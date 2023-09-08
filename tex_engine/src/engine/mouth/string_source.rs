/*! The primary [`TokenSource`](crate::engine::mouth::TeXMouthSource) for TeX, which reads from a [`String`]
    and converts it to [`Token`]s with the correct [`CategoryCode`](crate::tex::catcodes::CategoryCode)s.
*/

use std::marker::PhantomData;
use std::vec::IntoIter;
use crate::{debug_log, throw};
use crate::engine::EngineType;
use crate::engine::memory::{Interner, Memory};
use crate::engine::state::State;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::token::Token;
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::{AllCharsTrait, CharType, TeXStr};

/// A [`StringSource`] is in one of three states
#[derive(Copy,Clone,PartialEq,Debug)]
pub enum MouthState {
    /// Beginning of line
    N,
    /// After a space (or control sequence)
    S,
    /// In the middle of a line
    M
}

#[derive(Clone)]
struct Char<C:CharType>{char:C,start:(usize,usize),end:(usize,usize)}

#[derive(Clone)]
pub struct StringSource<C:CharType> {
    state : MouthState,
    line : usize,
    col : usize,
    pub string:Ptr<[Box<[u8]>]>,
    pub(crate) eof:bool,
    pub source:Option<TeXStr>,
    tempstr:String,phantom:PhantomData<C>
}

macro_rules! get_char {
    ($self:ident) => {{
        let line = &$self.string[$self.line];
        if $self.col >= line.len() {None} else {
            C::from_u8_iter(&mut $self.string[$self.line][$self.col..].iter().map(|u:&u8| *u),&mut $self.col)
        }
    }}
}
impl<C:CharType> StringSource<C> {
    pub fn new(string: Ptr<[Box<[u8]>]>,source:Option<TeXStr>) -> StringSource<C> {
        StringSource {
            state: MouthState::N,
            line: 0,
            col: 0,
            string: string,
            eof: false,
            tempstr:String::new(),
            source,phantom:PhantomData
        }
    }
    pub fn from_str(string:&Vec<u8>) -> Ptr<[Box<[u8]>]> {
        if string.is_empty() {
            return Ptr::new([]);
        }
        let mut ret = vec!();
        let mut curr = vec!();
        let mut iter = string.iter();
        while let Some(next) = iter.next() {
            match next {
                b'\n' => {
                    let mut next = vec!();
                    std::mem::swap(&mut curr, &mut next);
                    ret.push(next.into_boxed_slice())
                }
                b'\r' => {
                    let mut next = vec!();
                    std::mem::swap(&mut curr, &mut next);
                    ret.push(next.into_boxed_slice());
                    match iter.next() {
                        Some(b'\n') => (),
                        Some(b) => {
                            curr.push(*b);
                        }
                        None => ()
                    }
                }
                o => curr.push(*o)
            }
        }
        if !curr.is_empty() {
            ret.push(curr.into_boxed_slice());
        }
        ret.into()
    }


    pub fn line(&self) -> usize { self.line+1 }
    pub fn column(&self) -> usize { self.col+1 }

    pub fn preview(&self,len:usize) -> String {
        let mut ret = String::new();
        if self.line >= self.string.len() {
            return ret
        }
        if self.string[self.line].len() > self.col {
            for c in &self.string[self.line][self.col..] { ret.push(*c as char)}
        }
        let mut line = self.line + 1;
        while line < self.string.len() && ret.len() < len {
            ret.push_str(std::str::from_utf8(&*self.string[line]).unwrap());
            line += 1;
        }
        ret
    }

    pub fn eof<ET:EngineType<Char=C>>(&mut self,state:&ET::State) -> bool {
        self.line >= self.string.len() && self.eof
        /*
        match self.charbuffer.last() {
            Some(_) => false,
            None => match self.next_char(state.get_catcode_scheme(), state.get_endlinechar()) {
                None => true,
                Some((c, l, co,_)) => {
                    self.charbuffer.push((c, l, co));
                    false
                }
            }
        }

         */
    }

    fn return_endline<ET:EngineType<Char=C>>(&mut self,cc: &CategoryCodeScheme<C>, endline: Option<C>,par:TeXStr) -> Option<ET::Token> {
        use CategoryCode::*;
        let ret = endline.map(|c| {
            match cc.get(c) {
                Space | EOL if self.state == MouthState::S => None,
                Space if self.state == MouthState::N => None,
                EOL if self.state == MouthState::N => {
                    Some(ET::Token::new_cs_from_string(par,self.source,(self.line+1,self.col+1),(self.line+1,self.col+1)))
                }
                Ignored => None,
                EOL => Some(ET::Token::new_space_from_string(self.source,(self.line+1,self.col+1),(self.line+1,self.col+1))),
                o => Some(ET::Token::new_char_from_string(c,*o,self.source,(self.line+1,self.col+1),(self.line+1,self.col+1)))
            }
        }).flatten();
        self.line += 1;
        self.col = 0;
        self.state = MouthState::N;
        ret
        /*endline.map(|c| )*/
    }

    pub fn readline<ET:EngineType<Char=C>,F:FnMut(ET::Token)>(&mut self,interner:&mut Interner,mut f:F) {
        if self.line >= self.string.len() {self.eof=true;return ()}
        let mut iter = self.string[self.line].iter().map(|u:&u8| *u);
        while let Some(next) = C::from_u8_iter(&mut iter,&mut self.col) {
            let t = if next.as_bytes() == [32] {
                ET::Token::new_space_from_string(self.source,(self.line+1,self.col+1),(self.line+1,self.col+1))
            } else {
                ET::Token::new_char_from_string(next,CategoryCode::Other,self.source,(self.line+1,self.col+1),(self.line+1,self.col+1))
            };
            f(t)
        }
        self.line += 1;
    }

    pub fn read<ET:EngineType<Char=C>,F:FnMut(ET::Token)>(&mut self,interner:&mut Interner ,cc: &CategoryCodeScheme<C>, endline: Option<C>,mut f:F) {
        if self.line >= self.string.len() {
            if self.eof {
                return ()
            } else {
                self.eof = true;
                if let Some(n) =self.return_endline::<ET>(cc,endline,interner.par) {
                    f(n)
                }
                return ()
            }
        }
        let mut ingroups = 0;
        let mut line = self.line;
        while self.line == line {
            let start = (self.line+1,self.col+1);
            match get_char!(self) {
                None => {
                    if let Some(n) =self.return_endline::<ET>(cc,endline,interner.par) {
                        f(n)
                    }
                    return ()
                }
                Some(c) => match self.check_char::<ET>(interner,cc,endline,start,c) {
                    None if self.line == line => (),
                    None if ingroups > 0 => {
                        line = self.line;
                        if self.line >= self.string.len() {
                            if self.eof {
                                return ()
                            } else {
                                self.eof = true;
                                if let Some(n) =self.return_endline::<ET>(cc,endline,interner.par) {
                                    f(n)
                                }
                                return ()
                            }
                        }
                    },
                    None => return (),
                    Some(tk) => {
                        if tk.is_begin_group() {
                            ingroups += 1
                        } else if tk.is_end_group() {
                            ingroups -= 1
                        }
                        f(tk)
                    }
                }
            }
        }
    }


    pub fn get_next<ET:EngineType<Char=C>>(&mut self,interner:&mut Interner ,cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Option<ET::Token> { loop {
        if self.line >= self.string.len() {
            if self.eof {
                return None
            } else {
                self.eof = true;
                self.state = MouthState::M;
                return match self.return_endline::<ET>(cc,endline,interner.par) {
                    Some(t) => {
                        debug_log!(trace=>"Returning endline {}",t.printable(&interner));
                        Some(t)
                    }
                    None => {
                        debug_log!(trace=>"(No endlinechar)");
                        None
                    }
                }
            }
        }
        let start = (self.line+1,self.col+1);
        match get_char!(self) {
            None => match self.return_endline::<ET>(cc,endline,interner.par) {
                Some(e) => {
                    debug_log!(trace=>"Returning endline {}",e.printable(&interner));
                    return Some(e)
                }
                None => ()
            }
            Some(c) => match self.check_char::<ET>(interner,cc,endline,start,c) {
                Some(t) => {
                    debug_log!(trace=>"Returning {}",t.printable(&interner));
                    return Some(t)
                },
                None => ()
            }
        };
    }}

    fn check_char<ET:EngineType<Char=C>>(&mut self,interner:&mut Interner,cc:&CategoryCodeScheme<C>,endline:Option<C>,start:(usize,usize),c:C) -> Option<ET::Token> {
        use CategoryCode::*;
        match cc.get(c) {
            EOL if self.state == MouthState::N => {
                self.line += 1;
                self.col = 0;
                Some(ET::Token::new_cs_from_string(interner.par,self.source,start,(self.line+1,self.col+1)))
            }
            EOL => self.return_endline::<ET>(cc,endline,interner.par),
            Space if self.state == MouthState::S => None,
            Space if self.state == MouthState::N => None,
            Space => {
                self.state = MouthState::S;
                Some(ET::Token::new_space_from_string(self.source,start,(self.line+1,self.col+1)))
            }
            Ignored => None,
            Comment => {
                self.line +=1;
                self.col = 0;
                self.state = MouthState::N;
                None
            }
            Invalid => throw!("Invalid character {}",c),
            Escape => Some(self.get_escape::<ET>(interner,cc,start)),
            Superscript => match self.maybe_superscript(c) {
                Some(c) => self.check_char::<ET>(interner,cc,endline,start,c),
                None => {
                    self.state = MouthState::M;
                    Some(ET::Token::new_char_from_string(c,Superscript,self.source,start,(self.line+1,self.col+1)))
                }
            }
            cc => {
                self.state = MouthState::M;
                Some(ET::Token::new_char_from_string(c,*cc,self.source,start,(self.line+1,self.col+1)))
            }
        }
    }

    fn maybe_superscript(&mut self, firstsup:C) -> Option<C> {
        match get_char!(self) {
            None => None,
            Some(c) if c != firstsup => {
                self.col -= c.as_bytes().len();
                None
            }
            Some(secondsup) => {
                match get_char!(self) {
                    None => {
                        self.col -= secondsup.as_bytes().len();
                        None
                    }
                    Some(first) => match get_char!(self) {
                        None => {
                            let firstu = first.to_usize();
                            if firstu < 128 {
                                let u = firstu as u8;
                                let ch: C = (if u < 64 { u + 64 } else { u - 64 }).into();
                                Some(ch)
                            } else {
                                self.col -= first.as_bytes().len();
                                self.col -= secondsup.as_bytes().len();
                                None
                            }
                        }
                        Some(second) => {
                            let firstu = first.to_usize();
                            let secondu = second.to_usize();
                            fn cond(i: usize) -> bool { (48 <= i && i <= 57) || (97 <= i && i <= 102) }
                            if cond(firstu) && cond(secondu) {
                                let char = u8::from_str_radix(
                                    std::str::from_utf8(&[firstu as u8, secondu as u8]).unwrap(),
                                    16
                                ).unwrap();
                                Some(char.into())
                            } else {
                                self.col -= second.as_bytes().len();
                                let firstu = first.to_usize();
                                if firstu < 128 {
                                    let u = firstu as u8;
                                    let ch: C = (if u < 64 { u + 64 } else { u - 64 }).into();
                                    Some(ch)
                                } else {
                                    self.col -= first.as_bytes().len();
                                    self.col -= secondsup.as_bytes().len();
                                    None
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn get_escape<ET:EngineType<Char=C>>(&mut self,interner:&mut Interner,cc:&CategoryCodeScheme<C>,start:(usize,usize)) -> ET::Token {
        let name = match get_char!(self) {
            None => {
                self.state = MouthState::N;
                interner.empty_str
            },
            Some(next) => self.check_escape(interner,cc,next)
        };
        ET::Token::new_cs_from_string(name,self.source,start,(self.line+1,self.col+1))
    }

    fn check_escape(&mut self,interner:&mut Interner,cc:&CategoryCodeScheme<C>,next:C) -> TeXStr {
        use CategoryCode::*;
        match cc.get(next) {
            Superscript => {
                match self.maybe_superscript(next) {
                    Some(c) => self.check_escape(interner,cc,c),
                    None => {
                        self.state = MouthState::M;
                        self.tempstr.clear();
                        self.tempstr.push(next.as_char());
                        TeXStr::from_string(&self.tempstr,interner)
                    }
                }
            }
            Letter => self.get_cs_name(interner,cc,next),
            o => {
                self.state = MouthState::M;
                self.tempstr.clear();
                self.tempstr.push(next.as_char());
                TeXStr::from_string(&self.tempstr,interner)
            }
        }
    }

    fn get_cs_name(&mut self,interner:&mut Interner,cc:&CategoryCodeScheme<C>,first:C) -> TeXStr {
        self.tempstr.clear();
        self.tempstr.push(first.as_char());
        self.state = MouthState::S;
        loop {
            match get_char!(self) {
                None => {
                    break
                }
                Some(next) => match cc.get(next) {
                    CategoryCode::Letter => self.tempstr.push(next.as_char()),
                    CategoryCode::Superscript => {
                        let curr = self.col;
                        match self.maybe_superscript(next) {
                            Some(c) if *cc.get(c) == CategoryCode::Letter => self.tempstr.push(c.as_char()),
                            _ => {
                                self.col = curr;
                                self.col -= next.as_bytes().len();
                                break
                            }
                        }
                    }
                    _ => {
                        self.col -= next.as_bytes().len();
                        break
                    }
                }
            }
        }
        TeXStr::from_string(&self.tempstr,interner)
    }

/*
    /// Process an [`end-of-line`](crate::tex::catcodes::CategoryCode::EOL) character:
    /// If `\endline==None`, ignore it and obtain the next character. Otherwise, return the
    /// `\endlinechar` character.
    pub fn do_eol(&mut self,cc:&CategoryCodeScheme<C>,endline:Option<C>) -> Option<(C,usize,usize,Option<MouthState>)> {
        match endline {
            None => {
                self.line += 1;
                self.col = 0;
                self.state = MouthState::N;
                self.next_char(cc,endline)
            }
            Some(endline) => {
                let ret = Some((endline,self.line,self.col,Some(MouthState::N)));
                self.line += 1;
                self.col = 0;
                ret
            }
        }
    }

    /// Get the next character from the [`StringSource`], or return [`None`]
    /// if the end of the [`StringSource`] has been reached.
    fn next_char(&mut self,cc:&CategoryCodeScheme<C>, endline: Option<C>) -> Option<(C, usize, usize,Option<MouthState>)> {
        if let Some((c,a,b)) = self.charbuffer.pop() {
            return Some((c,a,b,None))
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

    fn do_char(&mut self,a:C,cc:&CategoryCodeScheme<C>, endline: Option<C>) -> Option<(C, usize, usize,Option<MouthState>)> {
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
                                self.state =MouthState::N;
                                Some((b,self.line,0,None))
                            }
                            Some(endline) => {
                                // return \endlinechar
                                let ret = Some((endline,self.line,self.col,Some(MouthState::N)));
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
                let ret = Some((a,self.line,self.col,None));
                self.col += 1;
                ret
            }
        }
    }

    fn maybe_superscript(&mut self,a:C,cc:&CategoryCodeScheme<C>, endline: Option<C>) -> Option<(C, usize, usize,Option<MouthState>)> {
        debug_log!(trace=>"next_char:   Superscript");
        if let Some(a2) = C::from_u8_iter(&mut self.string) {
            if a == a2 {
                //fn cond(i: u8) -> bool { (48 <= i && i <= 57) || (97 <= i && i <= 102) }
                let (l,c) = (self.line,self.col);
                self.col += 2;
                if let Some((first, lf, cf,_)) = self.next_char(cc, endline) {
                    if lf != l {
                        self.charbuffer.push((first, lf, cf));
                        self.charbuffer.push((a2,l,c+1));
                        Some((a,l,c,None))
                    } else {
                        if let Some((second,ls,cs,_)) = self.next_char(cc,endline) {
                            if ls != l {
                                self.charbuffer.push((second, ls, cs));
                                let firstu = first.to_usize();
                                if firstu < 128 {
                                    let u = firstu as u8;
                                    let ch : C = (if u < 64 {u + 64} else {u - 64}).into();
                                    Some((ch,l,c,None))
                                } else {
                                    self.charbuffer.push((first,lf,cf));
                                    self.charbuffer.push((a2,l,c+1));
                                    Some((a,l,c,None))
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
                                    Some((char.into(),l,c,None))
                                } else {
                                    self.charbuffer.push((second, ls, cs));
                                    if firstu < 128 {
                                        let u = firstu as u8;
                                        let ch : C = (if u < 64 {u + 64} else {u - 64}).into();
                                        Some((ch,l,c,None))
                                    } else {
                                        self.charbuffer.push((first,lf,cf));
                                        self.charbuffer.push((a2,l,c+1));
                                        Some((a,l,c,None))
                                    }
                                }
                            }
                        } else {
                            let firstu = first.to_usize();
                            if firstu < 128 {
                                let u = firstu as u8;
                                let ch : C = (if u < 64 {u + 64} else {u - 64}).into();
                                Some((ch,l,c,None))
                            } else {
                                self.charbuffer.push((first,lf,cf));
                                self.charbuffer.push((a2,l,c+1));
                                Some((a,l,c,None))
                            }
                        }
                    }
                } else {
                    self.charbuffer.push((a2,l,c+1));
                    Some((a,l,c,None))
                }
            } else {
                let ret = Some((a,self.line,self.col,None));
                self.col += 1;
                if let Some((x,s,e,_)) = self.do_char(a2,cc,endline) {
                    self.charbuffer.push((x,s,e));
                }
                ret
            }
        } else {
            let ret = Some((a,self.line,self.col,None));
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
        self.charbuffer.clear();
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

    fn get_escape(&mut self,interner:&mut Interner<C>,cc:&CategoryCodeScheme<C>,endline:Option<C>,a:C,line:usize,col:usize) -> (BaseToken<C>,usize,usize) {
        use crate::tex::catcodes::CategoryCode::*;
        debug_log!(trace=>"get_next_immediate:   Escape");
        match self.next_char(cc,endline) {
            Some((a,l,c,_)) if self.get_next_lc().1 == 0 && *cc.get(&a) != EOL => {
                self.charbuffer.push((a,l,c));
                self.state = MouthState::N;
                debug_log!(trace=>"get_next_immediate: EOL; returning empty CS");
                (BaseToken::CS(interner.empty_str),line, col)
            }
            None => {
                self.state = MouthState::N;
                debug_log!(trace=>"get_next_immediate: Stream empty; returning empty CS");
                (BaseToken::CS(interner.empty_str), line, col)
            }
            Some((a,_,_,_)) if *cc.get(&a) == Letter => {
                self.tempstr.clear();
                self.state = MouthState::S;
                debug_log!(trace=>"get_next_immediate: Next character is Letter");
                self.tempstr.push(a.as_char());
                loop {
                    match self.next_char(cc,endline) {
                        Some((a,l,c,_)) if self.get_next_lc().1 == 0 && *cc.get(&a) != EOL => {
                            self.charbuffer.push((a,l,c));
                            self.state = MouthState::N;
                            break
                        }
                        Some((b,_,_,_)) if *cc.get(&b) == Letter =>
                            self.tempstr.push(b.as_char()),
                        Some((o,s,l,_)) => {
                            self.charbuffer.push((o,s,l));
                            break
                        }
                        None => break
                    }
                }
                debug_log!(trace=>"get_next_immediate: Returning \\{:}",self.tempstr);
                let ret = BaseToken::CS(TeXStr::from_string(&self.tempstr,interner));
                (ret,line, col)
            }
            Some((a,_,_,_)) => {
                self.state = MouthState::M;
                self.tempstr.clear();
                self.tempstr.push(a.as_char());
                debug_log!(trace=>"get_next_immediate: Returning \\{}",self.tempstr);
                let ret = BaseToken::CS(TeXStr::from_string(&self.tempstr,interner));
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
    pub fn get_next_immediate(&mut self, interner:&mut Interner<C>,cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Option<(BaseToken<C>,usize,usize)> {
        use crate::tex::catcodes::CategoryCode::*;
        match self.next_char(cc,endline) {
            None => {
                debug_log!(trace=>"get_next_immediate: No more characters");
                None
            },
            Some((a,line,col,nst)) => {
                debug_log!(trace=>"get_next_immediate: Next character: {} (at {};{})",a.char_str(),line,col);
                match cc.get(&a) {
                    EOL => {
                        debug_log!(trace=>"get_next_immediate:   EOL");
                        match self.state {
                            MouthState::N => {
                                debug_log!(trace=>"get_next_immediate: State:N; returning \\par");
                                if let Some(st) = nst { self.state = st}
                                Some((BaseToken::CS(interner.par), line, col))
                            }
                            MouthState::S => {
                                self.state = MouthState::N;
                                if let Some(st) = nst { self.state = st}
                                return self.get_next_immediate(interner,cc,endline)
                            }
                            _ => {
                                self.state = MouthState::N;
                                if let Some(st) = nst { self.state = st}
                                debug_log!(trace=>"get_next_immediate: State:{:?}; returning Space",self.state);
                                Some((BaseToken::Char(a, Space),line, col))
                            }
                        }
                    }
                    Space => {
                        debug_log!(trace=>"get_next_immediate:   Space");
                        match self.state {
                            MouthState::S => {
                                if let Some(st) = nst { self.state = st};
                                self.get_next_immediate(interner,cc, endline)
                            },
                            MouthState::N => {
                                if let Some(st) = nst { self.state = st};
                                self.get_next_immediate(interner,cc, endline)
                            }
                            _ => {
                                self.state = MouthState::S;
                                if let Some(st) = nst { self.state = st}
                                Some((BaseToken::Char(a, Space),line, col))
                            }
                        }
                    }
                    Escape => Some(self.get_escape(interner,cc,endline,a,line,col)),
                    Ignored => {
                        let ret = BaseToken::Char(a, Ignored);
                        if let Some(st) = nst { self.state = st}
                        debug_log!(trace=>"get_next_immediate: returning {:?}",ret);
                        Some((ret, line, col))
                    }
                    o => {
                        self.state = MouthState::M;
                        if let Some(st) = nst { self.state = st}
                        let ret = BaseToken::Char(a, *o);
                        debug_log!(trace=>"get_next_immediate: returning {:?}",ret);
                        Some((ret, line, col))
                    }
                }
            }
        }
    }

    pub fn get_next_valid<ET:EngineType<Char=C>>(&mut self,interner:&mut Interner<C> ,cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Option<(BaseToken<C>, usize, usize)> {
        use CategoryCode::*;
        match self.get_next_immediate(interner,cc,endline) {
            None => {
                debug_log!(trace=>"get_next_valid: returning None");
                None
            }
            Some((BaseToken::CS(n),l,c)) => Some((BaseToken::CS(n),l,c)),
            Some((BaseToken::Char(c,code),l,cl)) => match code {
                Ignored => {
                    debug_log!(trace=>"get_next_valid: Ignored character");
                    self.get_next_valid::<ET>(interner,cc, endline)
                }
                Comment => {
                    debug_log!(trace=>"get_next_valid: Comment; skipping line");
                    if self.line == l {self.skip_line();}
                    self.get_next_valid::<ET>(interner,cc, endline)
                }
                Invalid => {
                    debug_log!(trace=>"get_next_valid: Invalid. Error");
                    throw!("Invalid character {}",c)
                }
                _ => {
                    debug_log!(trace=>"get_next_valid: valid. Returning");
                    Some((BaseToken::Char(c,code),l,cl))
                }
            }
        }
    }

    pub fn read<ET:EngineType<Char=C>,F:FnMut(ET::Token)>(&mut self,interner:&mut Interner<C> ,cc: &CategoryCodeScheme<C>, endline: Option<C>,mut f:F)
        -> Result<(),TeXError<ET>> {
        let mut done = false;
        use CategoryCode::*;
        let currline = self.line;
        while let Some((next,l,c,nst)) = self.next_char(cc,endline) {
            if l != currline {
                self.state = MouthState::N;
                if let Some(st) = nst { self.state = st}
                self.charbuffer.push((next,l,c));
                return Ok(())
            }
            match cc.get(&next) {
                Ignored => { if let Some(st) = nst { self.state = st}},
                Comment => {
                    self.skip_line();
                    if done {
                        self.state = MouthState::N;
                        return Ok(())
                    }
                    return self.read(interner,cc,endline,f)
                }
                Invalid => {
                    debug_log!(trace=>"get_next_valid: Invalid. Error");
                    throw!("Invalid character {}",c)
                }
                EOL => {
                    if let Some(st) = nst { self.state = st}
                    if done {
                        return Ok(())
                    }
                    return self.read(interner,cc,endline,f)
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
                    if let Some(st) = nst { self.state = st}
                }
                Escape => {
                    done = true;
                    f(Token::new(self.get_escape(interner,cc,endline,next,l,c).0,None))
                }
                o => {
                    self.state = MouthState::M;
                    if let Some(st) = nst { self.state = st}
                    done = true;
                    f(Token::new(BaseToken::Char(next,*o),None))
                }
            }
        }
        Ok(())
    }


 */
}
/*
/// A [`StringSource`] is the primary [`TokenSource`](crate::engine::mouth::TeXMouthSource) for TeX, which reads from a [`String`]
#[derive(Clone)]
pub struct StringSource<C:CharType> {
    pub(crate) state:StringSourceState<C>,
}
impl<C:CharType> StringSource<C> {
    /// Create a new [`StringSource`] from a [`String`] and an optional source reference.
    /// `source` is usually a filename, used to construct [`crate::tex::token::SourceReference`]s for [`Token`]s.
    /// The [`StringSource`] keeps track of the current line and column number.
    pub fn new(string:Vec<u8>,source:Option<TeXStr>) -> StringSource<C> {
        StringSource {
            state:StringSourceState::new(string,source),
        }
    }

    pub fn read<ET:EngineType<Char=C>,F:FnMut(ET::Token)>(&mut self,interner:&mut Interner<C>,cc:&CategoryCodeScheme<C>,endline:Option<C>,f:F) -> Result<(),TeXError<ET>> {
        self.state.read(interner,cc,endline,f)
    }

    pub fn line(&self) -> usize { self.state.line }
    pub fn column(&self) -> usize { self.state.col }

    pub fn preview(&self,len:usize) -> String { self.state.preview(len) }
    pub fn eof<ET:EngineType<Char=C>>(&mut self,state:&ET::State) -> bool { self.state.eof::<ET>(state) }

    pub fn get_next<ET:EngineType<Char=C>>(&mut self,interner:&mut Interner<C> ,cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Option<ET::Token> {
        match self.state.get_next_valid::<ET>(interner,cc, endline) {
            None => None,
            Some((n,l,c)) =>
                Some(Token::new(n,self.source.clone().map(|s|
                                                                 FileReference {
                                                                     filename:s.symbol(),start:(l,c),end:self.state.get_next_lc()
                                                                 }
                    )
                ))
        }
    }
}

 */