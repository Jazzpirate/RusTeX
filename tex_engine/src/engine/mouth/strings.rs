/*! String tokenizer for TeX input, primarily from files.
*/
use crate::prelude::*;
use crate::tex::input_text::{TextLine, TextLineSource};
use crate::utils::errors::ErrorHandler;

/// A [`StringTokenizer`] is in one of three states
#[derive(Copy,Clone,PartialEq,Eq,Debug)]
pub enum MouthState {
    /// Beginning of line
    NewLine,
    /// After a space (or control sequence)
    SkipBlank,
    /// In the middle of a line
    MidLine
}

/** Takes a [`TextLineSource`] and lazily turns it into [`Token`]s

  *Example:*
```rust
use tex_engine::utils::errors::ErrorThrower;
use tex_engine::engine::mouth::strings::StringTokenizer;
use tex_engine::tex::token::StandardToken;
use tex_engine::tex::catcodes::DEFAULT_SCHEME_U8;
use tex_engine::utils::Ptr;
use tex_engine::tex::token::Token;
use tex_engine::tex::catcodes::CommandCode;
use tex_engine::tex::input_text::StringLineSource;

type T = StandardToken<u8,Ptr<str>>;
let eh = ErrorThrower;
let mut cs_handler = ();
let cc = &*DEFAULT_SCHEME_U8;

let string = "\\foo   \n  \n   {a}{!}";
let input: StringLineSource<u8> = string.into();
let mut tokenizer = StringTokenizer::new(input);
let eol = Some(b'\r');
let next = tokenizer.get_next(&eh,&mut cs_handler,cc,None); // \foo
assert!(matches!(next,Some(T::ControlSequence(s)) if &*s == "foo"));
let next = tokenizer.get_next(&eh,&mut cs_handler,cc,eol); // \par
assert!(matches!(next,Some(T::ControlSequence(s)) if &*s == "par"));
let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // {
assert_eq!(next.command_code(), CommandCode::BeginGroup);
let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // a
assert_eq!(next.command_code(), CommandCode::Letter);
let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // }
assert_eq!(next.command_code(), CommandCode::EndGroup);
let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // {
assert_eq!(next.command_code(), CommandCode::BeginGroup);
let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // !
assert_eq!(next.command_code(), CommandCode::Other);
let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // }
assert_eq!(next.command_code(), CommandCode::EndGroup);
let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // end of line => space
assert_eq!(next.command_code(), CommandCode::Space);
assert!(tokenizer.get_next::<T,_>(&eh,&mut cs_handler,cc,eol).is_none()); // EOF
```
*/
#[derive(Clone,Debug)]
pub struct StringTokenizer<C:Character,S:TextLineSource<C>> {
    state : MouthState,
    line : usize,
    col : usize,
    current_line:TextLine<C>,
    pub source:S,
    pub(crate) eof:bool,
    tempstr:Vec<C>
}

pub struct InvalidCharacterError<C:Character>(pub C);

type CSH<T> = <<T as Token>::CS as CSName<<T as Token>::Char>>::Handler;

impl<C:Character,S:TextLineSource<C>> StringTokenizer<C,S> {
    /// Create a new [`StringTokenizer`] from a [`TextLineSource`]
    pub fn new(mut source:S) -> Self {
        Self {
            state: MouthState::NewLine,
            line: 1,
            col: 0,
            current_line:source.get_line().unwrap_or(TextLine::default()),
            source,
            eof: false,
            tempstr:Vec::new()
        }
    }
    /// The current line
    #[inline(always)]
    pub fn line(&self) -> usize { self.line }
    /// The current column
    #[inline(always)]
    pub fn column(&self) -> usize { self.col + 1 }
    /// whether the file end has been reached
    #[inline(always)]
    pub fn eof(&self) -> bool {
        self.eof
    }

    #[inline(always)]
    fn get_char(&mut self) -> Option<C> {
        if self.col >= self.current_line.len() {None} else {
            let next = self.current_line[self.col];
            self.col += 1;
            Some(next)
        }
    }

    pub fn readline<T:Token<Char=C>,F:FnMut(T)>(&mut self,mut f:F) {
        while self.col < self.current_line.len() {
            let next = self.current_line[self.col];
            self.col += 1;
            match next.try_into() {
                Ok(b' ') => f(T::space()),
                _ => f(T::from_char_cat(next,CommandCode::Other))
            }
        }
        self.next_line();
    }

    pub fn read<T:Token<Char=C>,E:ErrorHandler,F:FnMut(T)>(&mut self, eh:&E, handler:&mut CSH<T>, cc: &CategoryCodeScheme<C>, endline: Option<C>, mut f:F) {
        let mut ingroups = 0;
        let line = self.line;
        while self.line == line || ingroups > 0 {
            match self.get_char() {
                None => {
                    if self.eof {return}
                    if let Some(n) =self.return_endline::<T>(cc, endline, handler.par()) {
                        f(n)
                    }
                    return ()
                }
                Some(c) => match self.check_char::<T, E>(eh, handler, cc, endline, c) {
                    Ok(None) if self.line == line || ingroups > 0 => (),
                    Ok(None) => return (),
                    Ok(Some(tk)) => {
                        if tk.is_begin_group() {
                            ingroups += 1
                        } else if tk.is_end_group() {
                            ingroups -= 1
                        }
                        f(tk)
                    }
                    _ => todo!("invalid character")
                }
            }
        }
    }

    /// Get the next [`Token`] from the [`StringTokenizer`].
    pub fn get_next<T:Token<Char=C>, E: ErrorHandler>(&mut self, eh:&E, handler: &mut CSH<T>, cc: &CategoryCodeScheme<C>, endline: Option<C>) -> Result<Option<T>,InvalidCharacterError<C>> { loop {
        match self.get_char() {
            None if self.eof => return Ok(None),
            None => match self.return_endline::<T>(cc, endline, handler.par()) {
                Some(e) => {
                    //debug_log!(trace=>"Returning endline {}",e.printable(&interner));
                    return Ok(Some(e))
                }
                None => ()
            }
            Some(c) => match self.check_char::<T, E>(eh, handler, cc, endline, c)? {
                Some(t) => return Ok(Some(t)),
                None => ()
            }
        };
    }}

    fn next_line(&mut self) {
        if let Some(next) = self.source.get_line() {
            self.current_line = next;
            self.line += 1;
            self.col = 0;
        } else {
            self.eof = true;
            self.col = self.current_line.len();
            self.state = MouthState::MidLine;
        }
    }

    fn do_par<T:Token<Char=C>>(&mut self,par:T::CS) -> T {
        if self.current_line.is_empty() {
            while let Some(line) = self.source.get_line() {
                self.line += 1;
                if !line.is_empty() {
                    self.current_line = line;
                    break
                }
            }
        }
        T::from_cs(par)
    }

    fn return_endline<T:Token<Char=C>>(&mut self,cc: &CategoryCodeScheme<C>, endline: Option<C>,par:T::CS) -> Option<T> {
        use CategoryCode::*;
        self.next_line();
        let ret = match endline {
            None => None,
            Some(c) => match cc.get(c) {
                Space | EOL if self.state == MouthState::SkipBlank => None,
                Space if self.state == MouthState::NewLine => None,
                EOL if self.state == MouthState::NewLine => Some(self.do_par(par)),
                EOL => Some(T::space()),
                Ignored | Invalid | Comment => None,
                o => Some(T::from_char_cat(c,(*o).into()))
            }
        };
        self.state = MouthState::NewLine;
        ret
    }

    fn check_char<T:Token<Char=C>, E: ErrorHandler>(&mut self, eh:&E, handler:&mut CSH<T>, cc:&CategoryCodeScheme<C>, endline:Option<C>, c:C) -> Result<Option<T>,InvalidCharacterError<C>> {
        use CategoryCode::*;
        match cc.get(c) {
            EOL if self.state == MouthState::NewLine => {
                self.next_line();
                Ok(Some(self.do_par(handler.par())))
            }
            EOL => Ok(self.return_endline::<T>(cc, endline, handler.par())),
            Space if self.state == MouthState::SkipBlank => Ok(None),
            Space if self.state == MouthState::NewLine => Ok(None),
            Space => {
                self.state = MouthState::SkipBlank;
                Ok(Some(T::space()))
            }
            Ignored => Ok(None),
            Comment => {
                self.next_line();
                self.state = MouthState::NewLine;
                Ok(None)
            }
            Invalid => Err(InvalidCharacterError(c)),
            Escape => Ok(Some(self.get_escape::<T>(handler, cc, endline))),
            Superscript => match self.maybe_superscript(c) {
                Some(c) => self.check_char::<T, E>(eh, handler, cc, endline, c),
                None => {
                    self.state = MouthState::MidLine;
                    Ok(Some(T::from_char_cat(c,CommandCode::Superscript)))
                }
            }
            cc => {
                self.state = MouthState::MidLine;
                Ok(Some(T::from_char_cat(c,(*cc).into())))
            }
        }
    }

    fn get_escape<T:Token<Char=C>>(&mut self, handler:&mut CSH<T>, cc:&CategoryCodeScheme<C>, endline:Option<C>) -> T {
        let name = match self.get_char() {
            None => {
                self.state = MouthState::NewLine;
                match endline {
                    None => handler.empty_str(),
                    Some(c) => {
                        self.tempstr.clear();
                        self.tempstr.push(c);
                        handler.from_chars(&self.tempstr)
                    }
                }
            },
            Some(next) => self.check_escape::<T>(handler, cc, endline, next)
        };
        T::from_cs(name)
    }

    fn check_escape<T:Token<Char=C>>(&mut self, handler:&mut CSH<T>, cc:&CategoryCodeScheme<C>, endline:Option<C>, next:C) -> T::CS {
        use CategoryCode::*;
        match cc.get(next) {
            Superscript => {
                match self.maybe_superscript(next) {
                    Some(c) => self.check_escape::<T>(handler, cc, endline, c),
                    None => {
                        self.state = MouthState::MidLine;
                        self.tempstr.clear();
                        self.tempstr.push(next);
                        handler.from_chars(&self.tempstr)
                    }
                }
            }
            Letter => self.get_cs_name::<T>(handler, cc, next),
            _ => {
                self.state = MouthState::MidLine;
                self.tempstr.clear();
                self.tempstr.push(next);
                handler.from_chars(&self.tempstr)
            }
        }
    }

    fn get_cs_name<T:Token<Char=C>>(&mut self, handler:&mut CSH<T>, cc:&CategoryCodeScheme<C>, first:C) -> T::CS {
        self.tempstr.clear();
        self.tempstr.push(first);
        self.state = MouthState::SkipBlank;
        loop {
            match self.get_char() {
                None => break,
                Some(next) => match cc.get(next) {
                    CategoryCode::Letter => self.tempstr.push(next),
                    CategoryCode::Superscript => {
                        let curr = self.col;
                        match self.maybe_superscript(next) {
                            Some(c) if *cc.get(c) == CategoryCode::Letter => self.tempstr.push(c),
                            _ => {
                                self.col = curr;
                                self.col -= 1;
                                break
                            }
                        }
                    }
                    _ => {
                        self.col -= 1;
                        break
                    }
                }
            }
        }
        handler.from_chars(&self.tempstr)
    }

    #[inline(always)]
    fn cond(i: C) -> bool {
        (Into::<C>::into(48u8) <= i && i <= Into::<C>::into(57u8)) ||
            (Into::<C>::into(97u8) <= i && i <= Into::<C>::into(102u8))
    }

    fn maybe_superscript(&mut self, firstsup:C) -> Option<C> {
        match self.get_char() {
            None => None,
            Some(c) if c != firstsup => {
                self.col -= 1;
                None
            }
            Some(_) => {
                match self.get_char() {
                    None => {
                        self.col -= 1;
                        None
                    }
                    Some(first) => match self.get_char() {
                        None => {
                            if first < (128).into() {
                                let u : u8 = match first.try_into() {
                                    Ok(u) => u,
                                    Err(_) => {
                                        self.col -= 1;
                                        return None
                                    }
                                };
                                let ch: C = (if u < 64 { u + 64 } else { u - 64 }).into();
                                Some(ch)
                            } else {
                                self.col -= 2;
                                None
                            }
                        }
                        Some(second) => {
                            if Self::cond(first) && Self::cond(second) {
                                let ufirst : u8 = match first.try_into() {
                                    Ok(u) => u,
                                    Err(_) => {
                                        self.col -= 2;
                                        return None
                                    }
                                };
                                let usecond : u8 = match second.try_into() {
                                    Ok(u) => u,
                                    Err(_) => {
                                        self.col -= 2;
                                        return None
                                    }
                                };
                                let char = u8::from_str_radix(
                                    std::str::from_utf8(&[ufirst, usecond]).unwrap(),
                                    16
                                ).unwrap();
                                Some(char.into())
                            } else {
                                self.col -= 1;
                                if first < (128).into() {
                                    let u: u8 = match first.try_into() {
                                        Ok(u) => u,
                                        Err(_) => {
                                            self.col -= 2;
                                            return None
                                        }
                                    };
                                    let ch: C = (if u < 64 { u + 64 } else { u - 64 }).into();
                                    Some(ch)
                                } else {
                                    self.col -= 2;
                                    None
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn preview<W:std::fmt::Write>(&self,len:&mut usize,mut f: W) -> std::fmt::Result {
        if self.current_line.is_empty() {
            return Ok(())
        }
        if self.current_line.len() > self.col {
            for c in &self.current_line[self.col..] {
                *len -=1;
                c.display(&mut f);
                if *len == 0 {return Ok(())}
            }
        }
        Ok(())
    }

}