/*! The primary [`TokenSource`](crate::engine::mouth::TeXMouthSource) for TeX, which reads from a [`String`]
    and converts it to [`Token`]s with the correct [`CategoryCode`](crate::tex::catcodes::CategoryCode)s.
*/

use std::vec::IntoIter;
use crate::debug_log;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::InvalidCharacter;
use crate::utils::Ptr;
use crate::utils::strings::{AllCharsTrait, CharType};

/// A [`StringSource`] is in one of three states
#[derive(Copy,Clone,PartialEq,Debug)]
enum MouthState {
    /// Beginning of line
    N,
    /// After a space (or control sequence)
    S,
    /// In the middle of a line
    M
}
pub struct StringSourceState<C:CharType> {
    state : MouthState,
    line : usize,
    col : usize,
    string:IntoIter<u8>,
    charbuffer:Vec<(C, usize, usize)>,
    eof:bool
}
impl<C:CharType> StringSourceState<C> {
    pub fn new(string: Vec<u8>) -> StringSourceState<C> {
        StringSourceState {
            state: MouthState::N,
            line: 1,
            col: 0,
            string: string.into_iter(),
            charbuffer: Vec::new(),
            eof: false
        }
    }

    pub fn preview(&self) -> String {
        let mut ret = String::new();
        for (c,_,_) in self.charbuffer.iter().rev() {
            for u in c.char_str().into_bytes() {
                ret.push(u as char);
            }
        }
        let cp = self.string.clone();
        for c in cp {
            ret.push(c as char);
        }
        ret
    }

    /// Process an [`end-of-line`](crate::tex::catcodes::CategoryCode::EOL) character:
    /// If `\endline==255`, ignore it and obtain the next character. Otherwise, return the
    /// `\endlinechar` character.
    pub fn do_eol(&mut self,cc:&CategoryCodeScheme<C>,endline:Option<C>) -> Option<(C,usize,usize)> {
        match endline {
            None => {
                self.line += 1;
                self.col = 0;
                self.next_char(cc,endline)
            }
            Some(endline) => {
                let ret = Some((endline,self.line,self.col));
                self.line += 1;
                self.col = 0;
                ret
            }
        }
    }

    /// Get the next character from the [`StringSource`], or return [`None`]
    /// if the end of the [`StringSource`] has been reached.
    fn next_char(&mut self,cc:&CategoryCodeScheme<C>, endline: Option<C>) -> Option<(C, usize, usize)> {
        if let Some(c) = self.charbuffer.pop() {
            return Some(c)
        }
        match C::from_u8_iter(&mut self.string) {
            Some(a) => self.do_char(a,cc,endline),
            None if self.eof => None,
            None => { // insert \endlinechar at end of file
                self.eof = true;
                self.do_eol(cc,endline)
            }
        }
    }

    fn do_char(&mut self,a:C,cc:&CategoryCodeScheme<C>, endline: Option<C>) -> Option<(C, usize, usize)> {
        match a.is_eol() {
            Some(true) => // `\n`
                self.do_eol(cc,endline),
            None => { // possibly `\r\n`
                match C::from_u8_iter(&mut self.string) {
                    None => { // end of file
                        self.eof = true;
                        self.do_eol(cc,endline)
                    },
                    Some(b) if a.is_eol_pair(b) =>  // ab==`\r\n`
                        self.do_eol(cc,endline),
                    Some(b) => { // a==`\r`
                        match endline {
                            None => { // return b
                                self.line += 1;
                                self.col = 0;
                                Some((b,self.line,0))
                            }
                            Some(endline) => {
                                // return \endlinechar
                                let ret = Some((endline,self.line,self.col));
                                self.line += 1;
                                self.col = 1;
                                self.charbuffer.push((b, self.line, 0));
                                ret
                            }
                        }
                    }
                }
            }
            _ if *cc.get(a) == CategoryCode::EOL => {
                self.skip_line();
                self.state = MouthState::N;
                self.next_char(cc,endline)
            }
            _ if *cc.get(a) == CategoryCode::Superscript => self.maybe_superscript(a,cc,endline),
            _ => {
                let ret = Some((a,self.line,self.col));
                self.col += 1;
                ret
            }
        }
    }

    fn maybe_superscript(&mut self,a:C,cc:&CategoryCodeScheme<C>, endline: Option<C>) -> Option<(C, usize, usize)> {
        debug_log!(trace=>"next_char:   Superscript");
        if let Some(a2) = C::from_u8_iter(&mut self.string) {
            if a == a2 {
                //fn cond(i: u8) -> bool { (48 <= i && i <= 57) || (97 <= i && i <= 102) }
                let (l,c) = (self.line,self.col);
                self.col += 2;
                if let Some((first, lf, cf)) = self.next_char(cc, endline) {
                    if lf != l {
                        self.charbuffer.push((first, lf, cf));
                        self.charbuffer.push((a2,l,c+1));
                        Some((a,l,c))
                    } else {
                        if let Some((second,ls,cs)) = self.next_char(cc,endline) {
                            if ls != l {
                                self.charbuffer.push((second, ls, cs));
                                let firstu = first.to_usize();
                                if firstu < 128 {
                                    let u = firstu as u8;
                                    let ch : C = (if u < 64 {u + 64} else {u - 64}).into();
                                    Some((ch,l,c))
                                } else {
                                    self.charbuffer.push((first,lf,cf));
                                    self.charbuffer.push((a2,l,c+1));
                                    Some((a,l,c))
                                }
                            } else {
                                let firstu = first.to_usize();
                                let secondu = second.to_usize();
                                fn cond(i: usize) -> bool { (48 <= i && i <= 57) || (97 <= i && i <= 102) }
                                if cond(firstu) && cond(secondu) {
                                    let char = u8::from_str_radix(
                                        std::str::from_utf8(&[firstu as u8, secondu as u8]).unwrap(),
                                        16
                                    ).unwrap();
                                    Some((char.into(),l,c))
                                } else {
                                    self.charbuffer.push((second, ls, cs));
                                    if firstu < 128 {
                                        let u = firstu as u8;
                                        let ch : C = (if u < 64 {u + 64} else {u - 64}).into();
                                        Some((ch,l,c))
                                    } else {
                                        self.charbuffer.push((first,lf,cf));
                                        self.charbuffer.push((a2,l,c+1));
                                        Some((a,l,c))
                                    }
                                }
                            }
                        } else {
                            let firstu = first.to_usize();
                            if firstu < 128 {
                                let u = firstu as u8;
                                let ch : C = (if u < 64 {u + 64} else {u - 64}).into();
                                Some((ch,l,c))
                            } else {
                                self.charbuffer.push((first,lf,cf));
                                self.charbuffer.push((a2,l,c+1));
                                Some((a,l,c))
                            }
                        }
                    }
                } else {
                    self.charbuffer.push((a2,l,c+1));
                    Some((a,l,c))
                }
            } else {
                let ret = Some((a,self.line,self.col));
                self.col += 1;
                if let Some(x) = self.do_char(a2,cc,endline) {
                    self.charbuffer.push(x);
                }
                ret
            }
        } else {
            let ret = Some((a,self.line,self.col));
            self.col += 1;
            ret
        }
    }

    /// Get the current line and column number of the [`StringSource`]
    pub fn get_next_lc(&self) -> (usize,usize) {
        match self.charbuffer.last() {
            Some((_,line,col)) => (*line,*col),
            None => (self.line,self.col)
        }
    }

    /// Skip the rest of the current line.
    fn skip_line(&mut self) {
        self.state = MouthState::S;
        self.line += 1;
        self.col = 0;
        while let Some(c) = C::from_u8_iter(&mut self.string) {
            match c.is_eol() {
                Some(true) => return (),
                Some(false) => (),
                None => {
                    match C::from_u8_iter(&mut self.string) {
                        Some(b) if c.is_eol_pair(b) => return (),
                        Some(c) => return self.charbuffer.push((c,self.line,0)),
                        None => return ()
                    }
                }
            }
        }
        /*
        let mut next:Option<(C, usize, usize)> = None;
        while self.get_next_lc().1 != 0 {
            match self.next_char(endline) {
                None => return (),
                o => next = o
            }
        }
        if let Some((b,line,col)) = next {
            self.charbuffer.push((b,line,col));
        }*/
    }


    /** Get the next [`BaseToken`] and its starting (line,column)-pair,
        using the given [`CategoryCodeScheme`] and `\endlinechar` value;
        including [`Comment`](CategoryCode::Comment) and
        [`Ignored`](CategoryCode::Ignored) characters, but with
        - no superfluous [`Space`](CategoryCode::Space) characters,
        - [`EOL`](CategoryCode::EOL) characters appropriately substituted (by
            [`Space`](CategoryCode::Space), `\par` or nothing), and
        - `^^<char>`-syntax resolved.
     */
    pub fn get_next_immediate(&mut self, cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Option<(BaseToken<C>,usize,usize)> {
        use crate::tex::catcodes::CategoryCode::*;
        match self.next_char(cc,endline) {
            None => {
                debug_log!(trace=>"get_next_immediate: No more characters");
                None
            },
            Some((a,line,col)) => {
                debug_log!(trace=>"get_next_immediate: Next character: {} (at {};{})",a.char_str(),line,col);
                match cc.get(a) {
                    EOL => {
                        debug_log!(trace=>"get_next_immediate:   EOL");
                        match self.state {
                            MouthState::N => {
                                debug_log!(trace=>"get_next_immediate: State:N; returning \\par");
                                Some((BaseToken::CS(C::par_token()), line, col))
                            }
                            MouthState::S => {
                                self.state = MouthState::N;
                                return self.get_next_immediate(cc,endline)
                            }
                            _ => {
                                self.state = MouthState::N;
                                debug_log!(trace=>"get_next_immediate: State:{:?}; returning Space",self.state);
                                Some((BaseToken::Char(a, Space),line, col))
                            }
                        }
                    }
                    Space => {
                        debug_log!(trace=>"get_next_immediate:   Space");
                        match self.state {
                            MouthState::S => self.get_next_immediate(cc, endline),
                            MouthState::N => {
                                self.get_next_immediate(cc, endline)
                            }
                            _ => {
                                self.state = MouthState::S;
                                Some((BaseToken::Char(a, Space),line, col))
                            }
                        }
                    }
                    Escape => {
                        debug_log!(trace=>"get_next_immediate:   Escape");
                        self.state = MouthState::S;
                        match self.next_char(cc,endline) {
                            Some((a,l,c)) if self.get_next_lc().1 == 0 && *cc.get(a) != EOL => {
                                self.charbuffer.push((a,l,c));
                                debug_log!(trace=>"get_next_immediate: EOL; returning empty CS");
                                Some((BaseToken::CS(C::empty_str()),line, col))
                            }
                            None => {
                                debug_log!(trace=>"get_next_immediate: Stream empty; returning empty CS");
                                Some((BaseToken::CS(C::empty_str()), line, col))
                            }
                            Some((a,_,_)) if *cc.get(a) == Letter => {
                                debug_log!(trace=>"get_next_immediate: Next character is Letter");
                                let mut v = vec!(a);
                                loop {
                                    match self.next_char(cc,endline) {
                                        Some((b,_,_)) if *cc.get(b) == Letter => v.push(b),
                                        Some(o) => {
                                            self.charbuffer.push(o);
                                            break
                                        }
                                        None => break
                                    }
                                }
                                let ret = BaseToken::CS(v.into());
                                debug_log!(trace=>"get_next_immediate: Returning {:?}",ret);
                                Some((ret,line, col))
                            }
                            Some((a,_,_)) => {
                                let ret = BaseToken::CS(vec!(a).into());
                                debug_log!(trace=>"get_next_immediate: Returning {:?}",ret);
                                Some((ret, line, col))
                            }
                        }
                    }
                    /*Comment => {
                        self.skip_line(endline);
                        self.get_next(cc, endline)
                    }
                    Ignored => self.get_next(cc, endline),
                    Invalid => ???
                    */
                    o => {
                        self.state = MouthState::M;
                        let ret = BaseToken::Char(a, *o);
                        debug_log!(trace=>"get_next_immediate: returning {:?}",ret);
                        Some((ret, line, col))
                    }
                }
            }
        }
    }

    pub fn get_next_valid<T:Token<Char=C>>(&mut self, cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Result<Option<(BaseToken<C>, usize, usize)>,InvalidCharacter<T>> {
        use CategoryCode::*;
        match self.get_next_immediate(cc,endline) {
            None => {
                debug_log!(trace=>"get_next_valid: returning None");
                Ok(None)
            }
            Some((BaseToken::CS(n),l,c)) => Ok(Some((BaseToken::CS(n),l,c))),
            Some((BaseToken::Char(c,code),l,cl)) => match code {
                Ignored => {
                    debug_log!(trace=>"get_next_valid: Ignored character");
                    self.get_next_valid(cc, endline)
                }
                Comment => {
                    debug_log!(trace=>"get_next_valid: Comment; skipping line");
                    self.skip_line();
                    self.get_next_valid(cc, endline)
                }
                Invalid => {
                    debug_log!(trace=>"get_next_valid: Invalid. Error");
                    Err(InvalidCharacter(c))
                }
                _ => {
                    debug_log!(trace=>"get_next_valid: valid. Returning");
                    Ok(Some((BaseToken::Char(c,code),l,cl)))
                }
            }
        }
    }

    /*pub fn get_next<T:Token<Char=C>>(&mut self, cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Result<Option<T>,InvalidCharacter<T>> {
        match self.get_next_valid(cc, endline)? {
            None => Ok(None),
            Some((n,_,_)) => Ok(Some(T::new(n))),
        }
    }*/
}

/// A [`StringSource`] is the primary [`TokenSource`](crate::engine::mouth::TeXMouthSource) for TeX, which reads from a [`String`]
pub struct StringSource<C:CharType> {
    state:StringSourceState<C>,
    pub source:Option<Ptr<String>>
}
impl<C:CharType> StringSource<C> {
    /// Create a new [`StringSource`] from a [`String`] and an optional source reference.
    /// `source` is usually a filename, used to construct [`crate::tex::token::SourceReference`]s for [`Token`]s.
    /// The [`StringSource`] keeps track of the current line and column number.
    pub fn new(string:Vec<u8>,source:Option<Ptr<String>>) -> StringSource<C> {
        StringSource {
            state:StringSourceState::new(string),
            source
        }
    }

    pub fn line(&self) -> usize { self.state.line }
    pub fn column(&self) -> usize { self.state.col }

    pub fn preview(&self) -> String { self.state.preview() }

    /*pub fn get_next_plain<T:Token<Char=C>>(&mut self, cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Result<Option<T>,InvalidCharacter<T>> {
        match self.state.get_next_valid(cc, endline)? {
            None => Ok(None),
            Some((n,_,_)) => Ok(Some(T::new(n))),
        }
    }*/

    pub fn get_next<T:Token<Char=C>>(&mut self, cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Result<Option<T>,InvalidCharacter<T>> {
        match self.state.get_next_valid(cc, endline)? {
            None => Ok(None),
            Some((n,l,c)) =>
                Ok(Some(T::new(n,self.source.clone().map(|s|
                    (s,(l,c),self.state.get_next_lc())
                ))))
                /*Ok(Some(TokenWithSourceref{base:n,
                sourceref:
                self.source.clone().map(|s| SourceReference::File { file:s,start:(l,c),end:self.state.get_next_lc() })*/
        }
    }

    /*
    fn sourceref(&self,line:usize,col:usize) -> Option<SourceReference<C>> {
        self.source.clone().map(|s| SourceReference::File { file:s,start:(line,col),end:self.state.get_next_lc() })
    }
    /** Get the next [`Token`] from the [`StringSource`], using the given [`CategoryCodeScheme`] and
     `\endlinechar` value. [`Token`]s are returned immediately with no additional tracking.
     Comments, characters with [`CategoryCode`](CategoryCode)
     [`Ignored`](CategoryCode::Ignored),
     superfluous spaces and end-of-line characters are skipped.
    */
    pub fn get_next(&mut self, cc: &CategoryCodeScheme<C>, endline: C) -> Option<Token<C>> {
        use crate::tex::catcodes::CategoryCode::*;
        match self.next_char(endline) {
            None => None,
            Some((a,line,col)) => {
                match cc.get(a) {
                    EOL => {
                        self.skip_line(endline);
                        match self.mstate {
                            MouthState::N => {
                                Some(Token{base: BaseToken::CS(C::par_token()), sourceref:self.sourceref(line, col)})
                            }
                            _ => {
                                self.mstate = MouthState::N;
                                Some(Token{base: BaseToken::Char(a, Space), sourceref:self.sourceref(line, col)})
                            }
                        }
                    }
                    Comment => {
                        self.skip_line(endline);
                        self.get_next(cc, endline)
                    }
                    Space => {
                        match self.mstate {
                            MouthState::S => self.get_next(cc, endline),
                            MouthState::N => {
                                self.get_next(cc, endline)
                            }
                            _ => {
                                self.mstate = MouthState::S;
                                Some(Token{base: BaseToken::Char(a, Space),sourceref:self.sourceref(line, col)})
                            }
                        }
                    }
                    Ignored => self.get_next(cc, endline),
                    Escape => {
                        match self.next_char(endline) {
                            Some((a,l,c)) if self.get_next_lc().1 == 0 && *cc.get(a) != EOL => {
                                self.mstate = MouthState::S;
                                self.charbuffer.push((a,l,c));
                                Some(Token{base: BaseToken::CS(C::empty_str()), sourceref:self.sourceref(line, col)})
                            }
                            None => {
                                Some(Token{base: BaseToken::CS(C::empty_str()), sourceref:self.sourceref(line, col)})
                            }
                            Some((a,_,_)) if *cc.get(a) == Letter => {
                                let mut v = vec!(a);
                                loop {
                                    match self.next_char(endline) {
                                        Some((b,_,_)) if *cc.get(b) == Letter => v.push(b),
                                        Some(o) => {
                                            self.charbuffer.push(o);
                                            self.mstate = MouthState::S;
                                            break
                                        }
                                        None => break
                                    }
                                }
                                Some(Token{base: BaseToken::CS(v.into()), sourceref:self.sourceref(line, col)})
                            }
                            Some((a,_,_)) => {
                                self.mstate = MouthState::S;
                                Some(Token{base: BaseToken::CS(vec!(a).into()), sourceref:self.sourceref(line, col)})
                            }
                        }
                    }
                    Superscript => { // Check for ^^
                        if let Some((b,bl,bc)) = self.next_char(endline) {
                            if a == b && bl == line {
                                fn cond(i: u8) -> bool { (48 <= i && i <= 57) || (97 <= i && i <= 102) }
                                if let Some((first,_,_)) = self.next_char(endline) {
                                    if let Some((second,sl,sc)) = self.next_char(endline) {
                                        let u1 : u8 = first.into() as u8;
                                        let u2 : u8 = second.into() as u8;
                                        if cond(u1) && cond(u2) {
                                            let char = u8::from_str_radix(std::str::from_utf8(&[u1, u2]).unwrap(), 16).unwrap();
                                            self.charbuffer.push((char.into(),line,col));
                                            self.get_next(cc, endline)
                                        } else {
                                            let u:usize = first.into();
                                            let c : C = (((u as i16) - 64) as u8).into();
                                            self.charbuffer.push((second,sl,sc));
                                            self.charbuffer.push((c,line,col));
                                            self.get_next(cc, endline)
                                        }
                                    } else {
                                        let u:usize = first.into();
                                        let c : C = (((u as i16) - 64) as u8).into();
                                        self.charbuffer.push((c,line,col));
                                        self.get_next(cc, endline)
                                    }
                                } else {
                                    self.charbuffer.push((b,bl,bc));
                                    self.mstate = MouthState::M;
                                    Some(Token{base: BaseToken::Char(a, Superscript), sourceref:self.sourceref(line, col)})
                                }
                            } else {
                                self.charbuffer.push((b,bl,bc));
                                self.mstate = MouthState::M;
                                Some(Token{base: BaseToken::Char(a, Superscript), sourceref:self.sourceref(line, col)})
                            }
                        } else {
                            Some(Token{base: BaseToken::Char(a, Superscript), sourceref:self.sourceref(line, col)})
                        }
                    }
                    o => {
                        self.mstate = MouthState::M;
                        Some(Token{base: BaseToken::Char(a, *o), sourceref:self.sourceref(line, col)})
                    }
                }
            }
        }
    }

     */
}