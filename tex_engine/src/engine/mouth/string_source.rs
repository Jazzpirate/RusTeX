/*! The primary [`TokenSource`](crate::engine::mouth::TeXMouthSource) for TeX, which reads from a [`String`]
    and converts it to [`Token`]s with the correct [`CategoryCode`](crate::tex::catcodes::CategoryCode)s.
*/

use std::vec::IntoIter;
use crate::{debug_log, throw};
use crate::engine::EngineType;
use crate::engine::memory::Memory;
use crate::engine::state::State;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::token::{BaseToken, FileReference, Token};
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::{AllCharsTrait, CharType, TeXStr};

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
#[derive(Clone)]
pub struct StringSourceState<C:CharType> {
    state : MouthState,
    line : usize,
    col : usize,
    string:IntoIter<u8>,
    charbuffer:Vec<(C, usize, usize)>,
    pub(crate) eof:bool
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
    /// If `\endline==None`, ignore it and obtain the next character. Otherwise, return the
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
            _ if *cc.get(&a) == CategoryCode::EOL => {
                self.skip_line();
                self.state = MouthState::N;
                self.next_char(cc,endline)
            }
            _ if *cc.get(&a) == CategoryCode::Superscript => self.maybe_superscript(a,cc,endline),
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
    }

    fn get_escape<ET:EngineType<Char=C>>(&mut self,memory:&mut Memory<ET>,cc:&CategoryCodeScheme<C>,endline:Option<C>,a:C,line:usize,col:usize) -> (BaseToken<C>,usize,usize) {
        use crate::tex::catcodes::CategoryCode::*;
        debug_log!(trace=>"get_next_immediate:   Escape");
        match self.next_char(cc,endline) {
            Some((a,l,c)) if self.get_next_lc().1 == 0 && *cc.get(&a) != EOL => {
                self.charbuffer.push((a,l,c));
                self.state = MouthState::N;
                debug_log!(trace=>"get_next_immediate: EOL; returning empty CS");
                (BaseToken::CS(memory.empty_str),line, col)
            }
            None => {
                self.state = MouthState::N;
                debug_log!(trace=>"get_next_immediate: Stream empty; returning empty CS");
                (BaseToken::CS(memory.empty_str), line, col)
            }
            Some((a,_,_)) if *cc.get(&a) == Letter => {
                let mut str = memory.get_string();
                self.state = MouthState::S;
                debug_log!(trace=>"get_next_immediate: Next character is Letter");
                str.push(a.as_char());
                loop {
                    match self.next_char(cc,endline) {
                        Some((a,l,c)) if self.get_next_lc().1 == 0 && *cc.get(&a) != EOL => {
                            self.charbuffer.push((a,l,c));
                            self.state = MouthState::N;
                            break
                        }
                        Some((b,_,_)) if *cc.get(&b) == Letter =>
                            str.push(b.as_char()),
                        Some(o) => {
                            self.charbuffer.push(o);
                            break
                        }
                        None => break
                    }
                }
                debug_log!(trace=>"get_next_immediate: Returning \\{:}",str);
                let ret = BaseToken::CS(TeXStr::from_string(&str,memory));
                memory.return_string(str);
                (ret,line, col)
            }
            Some((a,_,_)) => {
                self.state = MouthState::M;
                let mut str = memory.get_string();
                str.push(a.as_char());
                debug_log!(trace=>"get_next_immediate: Returning \\{}",str);
                let ret = BaseToken::CS(    TeXStr::from_string(&str,memory));
                memory.return_string(str);
                (ret, line, col)
            }
        }
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
    pub fn get_next_immediate<ET:EngineType<Char=C>>(&mut self, memory:&mut Memory<ET>,cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Option<(BaseToken<C>,usize,usize)> {
        use crate::tex::catcodes::CategoryCode::*;
        match self.next_char(cc,endline) {
            None => {
                debug_log!(trace=>"get_next_immediate: No more characters");
                None
            },
            Some((a,line,col)) => {
                debug_log!(trace=>"get_next_immediate: Next character: {} (at {};{})",a.char_str(),line,col);
                match cc.get(&a) {
                    EOL => {
                        debug_log!(trace=>"get_next_immediate:   EOL");
                        match self.state {
                            MouthState::N => {
                                debug_log!(trace=>"get_next_immediate: State:N; returning \\par");
                                Some((BaseToken::CS(memory.par), line, col))
                            }
                            MouthState::S => {
                                self.state = MouthState::N;
                                return self.get_next_immediate(memory,cc,endline)
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
                            MouthState::S => self.get_next_immediate(memory,cc, endline),
                            MouthState::N => {
                                self.get_next_immediate(memory,cc, endline)
                            }
                            _ => {
                                self.state = MouthState::S;
                                Some((BaseToken::Char(a, Space),line, col))
                            }
                        }
                    }
                    Escape => Some(self.get_escape(memory,cc,endline,a,line,col)),
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

    pub fn get_next_valid<ET:EngineType<Char=C>>(&mut self,memory:&mut Memory<ET> ,cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Result<Option<(BaseToken<C>, usize, usize)>,TeXError<ET>> {
        use CategoryCode::*;
        match self.get_next_immediate(memory,cc,endline) {
            None => {
                debug_log!(trace=>"get_next_valid: returning None");
                Ok(None)
            }
            Some((BaseToken::CS(n),l,c)) => Ok(Some((BaseToken::CS(n),l,c))),
            Some((BaseToken::Char(c,code),l,cl)) => match code {
                Ignored => {
                    debug_log!(trace=>"get_next_valid: Ignored character");
                    self.get_next_valid(memory,cc, endline)
                }
                Comment => {
                    debug_log!(trace=>"get_next_valid: Comment; skipping line");
                    if self.line == l {self.skip_line();}
                    self.get_next_valid(memory,cc, endline)
                }
                Invalid => {
                    debug_log!(trace=>"get_next_valid: Invalid. Error");
                    throw!("Invalid character {}",c)
                }
                _ => {
                    debug_log!(trace=>"get_next_valid: valid. Returning");
                    Ok(Some((BaseToken::Char(c,code),l,cl)))
                }
            }
        }
    }

    pub fn read<ET:EngineType<Char=C>,F:FnMut(Token<ET>)>(&mut self,memory:&mut Memory<ET> ,cc: &CategoryCodeScheme<C>, endline: Option<C>,mut f:F)
        -> Result<(),TeXError<ET>> {
        let mut done = false;
        use CategoryCode::*;
        let currline = self.line;
        while let Some((next,l,c)) = self.next_char(cc,endline) {
            if l != currline {
                self.state = MouthState::N;
                self.charbuffer.push((next,l,c));
                return Ok(())
            }
            match cc.get(&next) {
                Ignored => (),
                Comment => {
                    self.skip_line();
                    if done {
                        self.state = MouthState::N;
                        return Ok(())
                    }
                    return self.read(memory,cc,endline,f)
                }
                Invalid => {
                    debug_log!(trace=>"get_next_valid: Invalid. Error");
                    throw!("Invalid character {}",c)
                }
                EOL => {
                    if done {
                        return Ok(())
                    }
                    return self.read(memory,cc,endline,f)
                }
                Space => {
                    match self.state {
                        MouthState::S => (),
                        MouthState::N => (),
                        _ => {
                            self.state = MouthState::S;
                            f(Token::new(BaseToken::Char(next,Space),None));
                            done = true;
                        }
                    }
                }
                Escape => {
                    done = true;
                    f(Token::new(self.get_escape(memory,cc,endline,next,l,c).0,None))
                }
                o => {
                    self.state = MouthState::M;
                    done = true;
                    f(Token::new(BaseToken::Char(next,*o),None))
                }
            }
        }
        Ok(())
    }

    pub fn eof<ET:EngineType<Char=C>>(&mut self,state:&ET::State) -> bool {
        match self.charbuffer.last() {
            Some(_) => false,
            None => match self.next_char(state.get_catcode_scheme(), state.get_endlinechar()) {
                None => true,
                Some((c, l, co)) => {
                    self.charbuffer.push((c, l, co));
                    false
                }
            }
        }
    }
}

/// A [`StringSource`] is the primary [`TokenSource`](crate::engine::mouth::TeXMouthSource) for TeX, which reads from a [`String`]
#[derive(Clone)]
pub struct StringSource<C:CharType> {
    pub(crate) state:StringSourceState<C>,
    pub source:Option<string_interner::DefaultSymbol>
}
impl<C:CharType> StringSource<C> {
    /// Create a new [`StringSource`] from a [`String`] and an optional source reference.
    /// `source` is usually a filename, used to construct [`crate::tex::token::SourceReference`]s for [`Token`]s.
    /// The [`StringSource`] keeps track of the current line and column number.
    pub fn new(string:Vec<u8>,source:Option<string_interner::DefaultSymbol>) -> StringSource<C> {
        StringSource {
            state:StringSourceState::new(string),
            source
        }
    }

    pub fn read<ET:EngineType<Char=C>,F:FnMut(Token<ET>)>(&mut self,memory:&mut Memory<ET>,cc:&CategoryCodeScheme<C>,endline:Option<C>,f:F) -> Result<(),TeXError<ET>> {
        self.state.read(memory,cc,endline,f)
    }

    pub fn line(&self) -> usize { self.state.line }
    pub fn column(&self) -> usize { self.state.col }

    pub fn preview(&self) -> String { self.state.preview() }
    pub fn eof<ET:EngineType<Char=C>>(&mut self,state:&ET::State) -> bool { self.state.eof::<ET>(state) }

    pub fn get_next<ET:EngineType<Char=C>>(&mut self,memory:&mut Memory<ET> ,cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Result<Option<Token<ET>>,TeXError<ET>> {
        match self.state.get_next_valid(memory,cc, endline)? {
            None => Ok(None),
            Some((n,l,c)) =>
                Ok(Some(Token::new(n,self.source.clone().map(|s|
                                                                 FileReference {
                                                                     filename:s,start:(l,c),end:self.state.get_next_lc()
                                                                 }
                    )
                )))
        }
    }
}