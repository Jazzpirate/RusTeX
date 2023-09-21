use std::fmt::{Debug, Display, Formatter};
use crate::engine::{EngineRef, EngineType};
use crate::engine::memory::{Interner, Memory, Symbol};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::{BaseCommand, CommandSource};
use crate::utils::Ptr;
use crate::utils::strings::{CharType, TeXStr};
use std::fmt::Write;
use crate::engine::state::State;
use crate::utils::strings::AllCharsTrait;

/// Tokens are the basic building blocks of TeX. They are either control sequences or characters.
pub trait Token:Clone+Debug+Send+PartialEq {
    type Char:CharType;
    fn clone_with<ET:EngineType<Token=Self>>(&self,rf:&CommandSource<ET>) -> Self;
    fn new_cs_from_command<ET:EngineType<Token=Self>>(name:TeXStr, rf:&CommandSource<ET>) -> Self;
    fn new_space_from_command<ET:EngineType<Token=Self>>(rf:&CommandSource<ET>) -> Self;
    fn new_char_from_command<ET:EngineType<Token=Self>>(c:Self::Char,cat:CategoryCode,rf:&CommandSource<ET>) -> Self;
    fn new_cs_from_string(name:TeXStr,file:Option<TeXStr>,start:(usize,usize),end:(usize,usize)) -> Self;
    fn new_space_from_string(file:Option<TeXStr>,start:(usize,usize),end:(usize,usize)) -> Self;
    fn new_char_from_string(c:Self::Char,cat:CategoryCode,file:Option<TeXStr>,start:(usize,usize),end:(usize,usize)) -> Self;
    fn trace<W:Write>(&self,interner:&Interner,w:&mut W) -> std::fmt::Result;
    fn eof() -> Self;
    fn to_str<W:Write>(&self,interner:&Interner,escapechar:Option<Self::Char>,cc:Option<&CategoryCodeScheme<Self::Char>>,mut w:W) -> std::fmt::Result {
        match self.name_or_char() {
            Ok(name) => {
                match escapechar {
                    Some(c) => w.write_char(c.as_char())?,
                    None => ()
                }
                let name = name.to_str(interner);
                w.write_str(name)?;
                match cc {
                    Some(cc) => match Self::Char::single_char(name) {
                        Some(first) if *cc.get(first) != CategoryCode::Letter => Ok(()),
                        _ => w.write_char(' ')
                    }
                    _ => w.write_char(' '),
                }
            }
            Err(c) => write!(w, "{}", c.as_char())
        }
    }
    /// Should be true iff this is a control sequence or an active character
    fn as_cs_like(&self) -> Option<CSLike<Self::Char>>;
    fn is_parameter(&self) -> bool;
    fn is_noexpand_marker(&self,interner:&Interner) -> bool;
    fn is_begin_group(&self) -> bool;
    fn is_end_group(&self) -> bool;
    fn is_eof(&self) -> bool;
    fn is_space(&self) -> bool;
    fn is_mathshift(&self) -> bool;
    fn is_align_tab(&self) -> bool;
    fn get_char(&self) -> Option<Self::Char>;
    fn get_catcode(&self) -> CategoryCode;
    fn split_char_and_catcode(&self) -> Option<(Self::Char,CategoryCode)>;
    fn get_cs_name(&self) -> Option<TeXStr>;
    fn to_command<ET: EngineType<Token=Self,Char=Self::Char>>(&self) -> BaseCommand<ET>;
    /// if `self.catcode == CategoryCode::Space`, should return `Err(Self::Char::space())`
    fn name_or_char(&self) -> Result<TeXStr,Self::Char>;
    fn printable<'a>(&'a self,interner:&'a Interner) -> PrintToken<'a,Self> { PrintToken(self, interner) }
    /*match tk.sourceref.as_ref().map(|t| t.trace(interner)).flatten() {
        Some(trace) => {
            ret.push_str(format!(": {} - {}",tk.to_str(interner,Some(ET::Char::backslash())),trace).as_str());
        }
        None => {
            ret.push_str(format!(": {}",tk.to_str(interner,Some(ET::Char::backslash()))).as_str());
        }
    },*/
}

#[derive(Copy,Clone,Debug)]
pub enum PlainToken<C:CharType> {
    CS(TeXStr),
    Character(C, CategoryCode)
}
impl<C:CharType> PartialEq for PlainToken<C> {
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (PlainToken::CS(a),PlainToken::CS(b)) => a == b,
            (PlainToken::Character(_,CategoryCode::Space),PlainToken::Character(_,CategoryCode::Space)) => true,
            (PlainToken::Character(a,ca),PlainToken::Character(b,cb)) => a == b && ca == cb,
            _ => false
        }
    }
}
impl<C:CharType> Token for PlainToken<C> {
    type Char = C;
    fn trace<W: Write>(&self, interner: &Interner, w: &mut W) -> std::fmt::Result {
        Ok(())
    }
    fn as_cs_like(&self) -> Option<CSLike<Self::Char>> {
        match self {
            PlainToken::CS(name) => Some(CSLike::CS(*name)),
            PlainToken::Character(c, CategoryCode::Active) => Some(CSLike::ActiveChar(*c)),
            _ => None
        }
    }
    fn name_or_char(&self) -> Result<TeXStr, Self::Char> {
        match self {
            PlainToken::CS(name) => Ok(*name),
            PlainToken::Character(c, _) => Err(*c)
        }
    }
    fn is_noexpand_marker(&self, interner: &Interner) -> bool {
        match self {
            PlainToken::CS(name) => name == &interner.noexpand_tk,
            _ => false
        }
    }
    fn is_space(&self) -> bool {
        match self {
            PlainToken::Character(_, CategoryCode::Space) => true,
            _ => false,
        }
    }
    fn is_parameter(&self) -> bool {
        match self {
            PlainToken::Character(_, CategoryCode::Parameter) => true,
            _ => false,
        }
    }
    fn is_align_tab(&self) -> bool {
        match self {
            PlainToken::Character(_,CategoryCode::AlignmentTab) => true,
            _ => false
        }
    }
    fn is_mathshift(&self) -> bool {
        match self {
            PlainToken::Character(_, CategoryCode::MathShift) => true,
            _ => false,
        }
    }
    fn is_eof(&self) -> bool {
        match self {
            PlainToken::Character(_, CategoryCode::EOL) => true,
            _ => false,
        }
    }
    fn is_begin_group(&self) -> bool {
        match self {
            PlainToken::Character(_, CategoryCode::BeginGroup) => true,
            _ => false,
        }
    }
    fn is_end_group(&self) -> bool {
        match self {
            PlainToken::Character(_, CategoryCode::EndGroup) => true,
            _ => false,
        }
    }
    fn get_cs_name(&self) -> Option<TeXStr> {
        match self {
            PlainToken::CS(name) => Some(*name),
            _ => None
        }
    }
    fn eof() -> Self {
        PlainToken::Character(Self::Char::newline(),CategoryCode::EOL)
    }
    fn get_char(&self) -> Option<Self::Char> {
        match self {
            PlainToken::Character(c,_) => Some(*c),
            _ => None,
        }
    }
    fn get_catcode(&self) -> CategoryCode {
        match self {
            PlainToken::Character(_,cc) => *cc,
            PlainToken::CS(_) => CategoryCode::Escape,
        }
    }
    fn clone_with<ET: EngineType<Token=Self>>(&self, rf: &CommandSource<ET>) -> Self { *self }
    fn new_char_from_command<ET: EngineType<Token=Self>>(c: Self::Char, cat: CategoryCode, rf: &CommandSource<ET>) -> Self {
        PlainToken::Character(c,cat)
    }
    fn new_space_from_command<ET: EngineType<Token=Self>>(rf: &CommandSource<ET>) -> Self {
        PlainToken::Character(Self::Char::space(),CategoryCode::Space)
    }
    fn new_cs_from_command<ET: EngineType<Token=Self>>(name: TeXStr, rf: &CommandSource<ET>) -> Self {
        PlainToken::CS(name)
    }
    fn new_char_from_string(c: Self::Char, cat: CategoryCode, file: Option<TeXStr>, start: (usize, usize), end: (usize, usize)) -> Self {
        PlainToken::Character(c,cat)
    }
    fn new_space_from_string(file: Option<TeXStr>, start: (usize, usize), end: (usize, usize)) -> Self {
        PlainToken::Character(Self::Char::space(),CategoryCode::Space)
    }
    fn new_cs_from_string(name: TeXStr, file: Option<TeXStr>, start: (usize, usize), end: (usize, usize)) -> Self {
        PlainToken::CS(name)
    }
    fn split_char_and_catcode(&self) -> Option<(Self::Char, CategoryCode)> {
        match self {
            PlainToken::Character(c,cc) => Some((*c,*cc)),
            _ => None
        }
    }
    fn to_command<ET: EngineType<Token=Self,Char=Self::Char>>(&self) -> BaseCommand<ET> {
        match self {
            PlainToken::CS(name) => BaseCommand::None,
            PlainToken::Character(c,cc) => BaseCommand::Char{char:*c,catcode:*cc}
        }
    }
}

#[derive(Copy,Clone,Debug)]
pub struct CompactToken(u32);
impl CompactToken {
    fn is_string(&self) -> bool { self.0 < 0x8000_0000 }
    fn as_string(&self) -> Option<TeXStr> {
        use string_interner::Symbol as OSymbol;
        if self.is_string() {
            Some(TeXStr::from_primitive(OSymbol::try_from_usize(self.0 as usize).unwrap()))
        } else {
            None
        }
    }
    fn catcode(&self) -> CategoryCode {
        //assert!(self.0 >= 0x8000_0000);
        CategoryCode::try_from(((self.0 & 0x0000_FF00) >> 8) as u8).unwrap()
    }
    fn u8(&self) -> u8 {
        //assert!(self.0 >= 0x8000_0000);
        (self.0 & 0x0000_00FF) as u8
    }
    fn from_char_cat(c:u8,cat:CategoryCode) -> Self {
        let c = u32::from(c);
        let cat = u32::from(Into::<u8>::into(cat));
        let r = 0x8000_0000 | (cat << 8) |c;
        //assert!(r >= 0x8000_0000);
        Self(r)
    }
}
impl PartialEq for CompactToken {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 || {
            if self.is_string() || other.is_string() { return false}
            let cc1 = self.catcode();
            let cc2 = other.catcode();
            if cc1 == CategoryCode::Space && cc2 == CategoryCode::Space {return true}
            if cc1 != cc2 {return false}
            let c1 = self.u8();
            let c2 = other.u8();
            c1 == c2
        }
    }
}

impl Token for CompactToken {
    type Char = u8;
    fn trace<W: Write>(&self, interner: &Interner, w: &mut W) -> std::fmt::Result {
        Ok(())
    }

    fn get_catcode(&self) -> CategoryCode {
        if self.is_string() {
            CategoryCode::Escape
        } else {
            self.catcode()
        }
    }

    fn is_noexpand_marker(&self, interner: &Interner) -> bool {
        use string_interner::Symbol as OSymbol;
        self.0 == (interner.noexpand_tk.0.to_usize() as u32)
    }

    fn as_cs_like(&self) -> Option<CSLike<Self::Char>> {
        match self.as_string() {
            Some(s) => Some(CSLike::CS(s)),
            _ => {
                let cc = self.catcode();
                match cc {
                    CategoryCode::Active => Some(CSLike::ActiveChar(self.u8())),
                    _ => None
                }
            }
        }
    }
    fn name_or_char(&self) -> Result<TeXStr, Self::Char> {
        match self.as_string() {
            Some(s) => Ok(s),
            _ => Err(self.u8())
        }
    }
    fn is_space(&self) -> bool {
        !self.is_string() && self.catcode() == CategoryCode::Space
    }
    fn is_parameter(&self) -> bool {
        !self.is_string() && self.catcode() == CategoryCode::Parameter
    }
    fn is_align_tab(&self) -> bool {
        !self.is_string() && self.catcode() == CategoryCode::AlignmentTab
    }
    fn is_mathshift(&self) -> bool {
        !self.is_string() && self.catcode() == CategoryCode::MathShift
    }
    fn is_eof(&self) -> bool {
        !self.is_string() && self.catcode() == CategoryCode::EOL
    }
    fn is_begin_group(&self) -> bool {
        !self.is_string() && self.catcode() == CategoryCode::BeginGroup
    }
    fn is_end_group(&self) -> bool {
        !self.is_string() && self.catcode() == CategoryCode::EndGroup
    }
    fn get_cs_name(&self) -> Option<TeXStr> {
        self.as_string()
    }
    fn eof() -> Self {
        Self::from_char_cat(b'\n',CategoryCode::EOL)
    }
    fn get_char(&self) -> Option<Self::Char> {
        if self.is_string() { None } else { Some(self.u8()) }
    }
    fn clone_with<ET: EngineType<Token=Self>>(&self, rf: &CommandSource<ET>) -> Self { *self }
    fn new_char_from_command<ET: EngineType<Token=Self>>(c: Self::Char, cat: CategoryCode, rf: &CommandSource<ET>) -> Self {
        Self::from_char_cat(c,cat)
    }
    fn new_space_from_command<ET: EngineType<Token=Self>>(rf: &CommandSource<ET>) -> Self {
        Self::from_char_cat(b' ',CategoryCode::Space)
    }
    fn new_cs_from_command<ET: EngineType<Token=Self>>(name: TeXStr, rf: &CommandSource<ET>) -> Self {
        //assert!(name.0.to_usize() < 0x8000_0000);
        use string_interner::Symbol as OSymbol;
        Self(name.0.to_usize() as u32)
    }
    fn new_char_from_string(c: Self::Char, cat: CategoryCode, file: Option<TeXStr>, start: (usize, usize), end: (usize, usize)) -> Self {
        Self::from_char_cat(c,cat)
    }
    fn new_space_from_string(file: Option<TeXStr>, start: (usize, usize), end: (usize, usize)) -> Self {
        Self::from_char_cat(b' ',CategoryCode::Space)
    }
    fn new_cs_from_string(name: TeXStr, file: Option<TeXStr>, start: (usize, usize), end: (usize, usize)) -> Self {
        //assert!(name.0.to_usize() < 0x8000_0000);
        use string_interner::Symbol as OSymbol;
        Self(name.0.to_usize() as u32)
    }
    fn split_char_and_catcode(&self) -> Option<(Self::Char, CategoryCode)> {
        if self.is_string() { None } else { Some((self.u8(),self.catcode())) }
    }
    fn to_command<ET: EngineType<Token=Self,Char=Self::Char>>(&self) -> BaseCommand<ET> {
        match self.as_string() {
            Some(name) => BaseCommand::None,
            _ => BaseCommand::Char{char:self.u8(),catcode:self.catcode()}
        }
    }
}

pub trait TokenConsumer<ET:EngineType> {
    fn consume_tk(&mut self,token:&ET::Token,int:&Interner);
    fn consume_str(&mut self,string:&str,int:&Interner);
}
pub struct StringConsumer<'a,W:Write,ET:EngineType>{write:W,esc:Option<ET::Char>,cc:Option<&'a CategoryCodeScheme<ET::Char>>}
impl<'a,W:Write,ET:EngineType> StringConsumer<'a,W,ET> {
    pub fn simple(w:W) -> StringConsumer<'a,W,ET> {
        StringConsumer{write:w,esc:Some(ET::Char::backslash()),cc:None}
    }
}
impl<'a,W:Write,ET:EngineType> TokenConsumer<ET> for StringConsumer<'a,W,ET> {
    fn consume_str(&mut self, string: &str,_:&Interner) {
        self.write.write_str(string).unwrap();
    }
    fn consume_tk(&mut self, token: &ET::Token,int:&Interner) {
        token.to_str(int,self.esc,self.cc,&mut self.write).unwrap();
    }
}
pub struct DeTokenizer<'a,ET:EngineType,F :FnMut(ET::Token)> {
    f:F,
    esc:Option<ET::Char>,
    cc:&'a CategoryCodeScheme<ET::Char>,
    src:&'a CommandSource<ET>
}
pub fn tokenize_string<ET:EngineType,F:FnMut(ET::Token)>(string: &str, src:&CommandSource<ET>, mut f:F) {
    let mut iter = string.as_bytes().iter().map(|c| *c);
    while let Some(c) = ET::Char::from_u8_iter(&mut iter,&mut 0) {
        match c {
            s if s == ET::Char::space() => f(ET::Token::new_space_from_command::<ET>(src)),
            _ => f(ET::Token::new_char_from_command::<ET>(c,CategoryCode::Other,src))
        }
    }
}
pub fn detokenize<ET:EngineType,F:FnMut(ET::Token)>(insert_space:bool,state:&ET::State,int:&Interner,token:&ET::Token,src:&CommandSource<ET>,mut f:F) {
    detokenizeI(insert_space,token,int,state.get_escapechar(),state.get_catcode_scheme(),src,f)
}
fn detokenizeI<ET:EngineType,F:FnMut(ET::Token)>(insert_space:bool,token:&ET::Token,int:&Interner,esc:Option<ET::Char>,cc:&CategoryCodeScheme<ET::Char>,src:&CommandSource<ET>, mut f:F) {
    match token.name_or_char() {
        Ok(name) => {
            match esc {
                Some(c) => f(ET::Token::new_char_from_command(c,CategoryCode::Other,src)),
                None => ()
            }
            let name = name.to_str(int);
            let mut iter = name.as_bytes().iter().map(|c| *c);
            let mut len = 0;
            let mut first = None;
            while let Some(c) = ET::Char::from_u8_iter(&mut iter,&mut 0) {
                len += 1;
                match first {
                    None => first = Some(c),
                    _ => ()
                }
                if c == ET::Char::space() {
                    f(ET::Token::new_space_from_command(src));
                } else {
                    f(ET::Token::new_char_from_command(c, CategoryCode::Other, src));
                }
            }
            if insert_space {
                if len == 1 {
                    let first = first.unwrap();
                    if *cc.get(first) == CategoryCode::Letter {
                        f(ET::Token::new_space_from_command(src));
                    }
                } else {
                    f(ET::Token::new_space_from_command(src));
                }
            }
        }
        Err(c) if c == ET::Char::space() => f(ET::Token::new_space_from_command(src)),
        Err(c) => f(ET::Token::new_char_from_command(c,CategoryCode::Other,src))
    }
}

impl<'a,ET:EngineType,F:FnMut(ET::Token)> DeTokenizer<'a,ET,F> {
    fn new(f:F,escapechar:Option<ET::Char>,cc:&'a CategoryCodeScheme<ET::Char>,src:&'a CommandSource<ET>) -> Self {
        Self{f,esc:escapechar,cc,src}
    }
}
impl<'a,ET:EngineType,F:FnMut(ET::Token)> TokenConsumer<ET> for DeTokenizer<'a,ET,F> {
    fn consume_tk(&mut self, token: &ET::Token,int:&Interner) {
        detokenizeI(true,token,int,self.esc,self.cc,self.src,&mut self.f)
    }
    fn consume_str(&mut self, string: &str,int:&Interner) {
        tokenize_string(string, self.src, &mut self.f)
    }
}

pub struct PrintToken<'a,T:Token>(&'a T, &'a Interner);
impl<'a,T:Token> std::fmt::Display for PrintToken<'a,T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.to_str(self.1,Some(T::Char::backslash()),None,f)
    }
}
#[derive(Copy,Clone)]
pub enum CSLike<C:CharType> {
    CS(TeXStr),
    ActiveChar(C)
}
impl<C:CharType> CSLike<C> {
    pub fn printable<'a>(&'a self,interner:&'a Interner) -> PrintCSLike<'a,C> { PrintCSLike(self, interner) }
}
pub struct PrintCSLike<'a,C:CharType>(pub &'a CSLike<C>, &'a Interner);
impl<'a,C:CharType> std::fmt::Display for PrintCSLike<'a,C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            CSLike::CS(n) => write!(f, "\\{}", n.to_str(self.1)),
            CSLike::ActiveChar(c) => write!(f, "{}", c.as_char())
        }
    }
}
/*
/// A [`BaseToken`] bundles the actually TeX-relevant data of a [`Token`], which is cloned often
/// and required by all [`Token`] implementations
#[derive(Clone,Copy,Debug)]
pub enum BaseToken<C:CharType> {
    /// A control sequence token with the provided name
    CS(TeXStr),
    /// An active character token with the provided character
    Char(C, CategoryCode)
}
impl<C:CharType> BaseToken<C> {
    pub fn to_str(& self,interner:&Interner,escapechar:Option<C>) -> String {
        match self {
            BaseToken::Char(_, CategoryCode::Space) => " ".to_string(),
            BaseToken::Char(c, _) => c.as_char().to_string(),
            BaseToken::CS(n) => match escapechar {
                Some(c) => (c.as_char().to_string() + n.to_str(interner)).to_string(),
                None => n.to_str(interner).to_string()
            }
        }
    }
}
impl<C:CharType> PartialEq for BaseToken<C> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BaseToken::Char(_, CategoryCode::Space), BaseToken::Char(_,CategoryCode::Space)) => true,
            (BaseToken::Char(c1, cc1), BaseToken::Char(c2, cc2)) => c1 == c2 && cc1 == cc2,
            (BaseToken::CS(n1), BaseToken::CS(n2)) => n1 == n2,
            _ => false
        }
    }
}

 */

/*
impl<ET:EngineType> ET::Token {
    pub fn to_str(&self,interner:&Interner,escapechar:Option<ET::Char>) -> String {
        self.base.to_str(interner,escapechar)
    }
}
impl<ET:EngineType> PartialEq for ET::Token {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base
    }
}
impl<ET:EngineType> ET::Token {
    /// The [`CategoryCode`] of the [`Token`]
    pub fn catcode(&self) -> CategoryCode {
        match &self.base {
            BaseToken::Char(_, cat) => *cat,
            BaseToken::CS(_) => CategoryCode::Escape
        }
    }
    pub fn new(base:BaseToken<ET::Char>, sourceref:Option<FileReference>) -> Self {
        Self {
            sourceref:sourceref.map(|fr| ET::TokenReference::from_file(&base,fr)),
            base
        }
    }
    pub fn with_ref(self, cmd:&ET::TokenReference) -> Self {
        Self {
            sourceref: Some(cmd.with_ref(&self.base)),
            base: self.base
        }
    }
}

 */

/*
#[derive(Clone,Copy,Debug,PartialEq)]
pub struct FileReference {
    pub filename:Symbol,
    pub start:(usize,usize),
    pub end:(usize,usize)
}

pub trait TokenReference<ET:EngineType<TokenReference = Self>>:Clone + Debug+Send {
    fn from_expansion(cmd:&CommandSource<ET>) -> Self;
    fn from_file(base:&BaseToken<ET::Char>,fr:FileReference) -> Self;
    fn with_ref(&self,base:&BaseToken<ET::Char>) -> Self { self.clone() }
    fn trace(&self,interner:&Interner) -> Option<String> { None }
}
impl<ET:EngineType<TokenReference = Self>> TokenReference<ET> for () {
    fn from_expansion(_cmd: &CommandSource<ET>) -> Self { () }
    fn from_file(_base: &BaseToken<ET::Char>, _fr: FileReference) -> Self { () }
}

#[derive(Clone,Debug,PartialEq,Copy)]
pub enum FileReferenceOnly {
    File(FileReference), None
}
impl<ET:EngineType<TokenReference = Self>> TokenReference<ET> for FileReferenceOnly {
    fn from_expansion(cmd: &CommandSource<ET>) -> Self {
        match &cmd.cause.sourceref {
            None|Some(FileReferenceOnly::None) => FileReferenceOnly::None,
            Some(FileReferenceOnly::File(fr)) => FileReferenceOnly::File(fr.clone())
        }
    }
    fn from_file(_: &BaseToken<ET::Char>, fr: FileReference) -> Self { FileReferenceOnly::File(fr) }
}

#[derive(Clone,Debug,PartialEq)]
pub enum FileTokenReferenceI<ET:EngineType<TokenReference = FileTokenReference<ET>>> {
    File(FileReference),
    Expansion{cmd:Option<ET::CommandReference>, token: ET::Token}
}
impl<ET:EngineType<TokenReference = FileTokenReference<ET>>> FileTokenReferenceI<ET> {
    pub fn trace(&self,interner:&Interner) -> Option<String> {
        use FileTokenReferenceI::*;
        match self {
            File (FileReference{filename, start,end}) => Some(format!("File {} at {}:{} - {}:{}",interner.resolve(*filename), start.0,start.1,end.0,end.1)),
            Expansion {token, ..} => {
                let mut trace = format!("Expanded from {}",token.to_str(interner,Some(ET::Char::backslash())));
                match token.sourceref.as_ref().map(|r|r.trace(interner)).flatten() {
                    Some(s) => {
                        trace.push_str("\n - ");
                        trace.push_str(&s)
                    },
                    None => ()
                }
                Some(trace)
            }
        }
    }
}

pub type FileTokenReference<ET> = Ptr<FileTokenReferenceI<ET>>;
impl<ET:EngineType<TokenReference = Self>> TokenReference<ET> for FileTokenReference<ET> {
    fn from_expansion(cmd: &CommandSource<ET>) -> Self {
        Ptr::new(FileTokenReferenceI::Expansion{
            cmd: cmd.reference.clone(),
            token: cmd.cause.clone()
        })
    }
    fn from_file(_: &BaseToken<ET::Char>, fr: FileReference) -> Self { Ptr::new(FileTokenReferenceI::File(fr)) }
    fn trace(&self, interner: &Interner) -> Option<String> {
        FileTokenReferenceI::trace(self,interner)
    }
}
*/

/// A list of [`Token`]s
pub struct PrintableTokenList<'a,ET:EngineType>(pub &'a [ET::Token], pub &'a Interner);
impl<'a,ET:EngineType> std::fmt::Display for PrintableTokenList<'a,ET> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for t in self.0 {
            t.to_str(self.1,Some(ET::Char::backslash()),None,&mut *f)?;
        }
        Ok(())
    }
}

/*
/// A [`Token`] bundling a [`BaseToken`] with a [`SourceReference`].
#[derive(Clone)]
pub struct TokenWithSourceref<ET:EngineType<Token=Self>>{
    /// The actual TeX-relevant data of the [`Token`]
    pub base: BaseToken<ET::Char>,
    /// The [`SourceReference`] of the [`Token`]
    pub sourceref: Option<SourceReference<ET>>
}
impl<ET:EngineType<Token=Self>> Token for TokenWithSourceref<ET> {
    type Char = ET::Char;
    fn base(&self) -> &BaseToken<Self::Char> { &self.base }
    fn take_base(self) -> BaseToken<Self::Char> {
        self.base
    }
    fn new(base:BaseToken<Self::Char>,sourceref:Option<(Ptr<String>,(usize,usize),(usize,usize))>) -> Self {
        Self { base, sourceref: sourceref.map(|tr| SourceReference::File {
            file: tr.0,
            start: tr.1,
            end: tr.2
        }) }
    }
    fn with_ref<IET:EngineType>(&self, cmd:&CommandSource<IET>) -> Self {
        use std::any::Any;
        let sourceref = match (cmd as &dyn Any).downcast_ref::<CommandSource<ET>>() {
            Some(cmd) => Some(SourceReference::Expansion {token:Box::new(cmd.cause.clone()),cmd:cmd.reference.clone()}),
            _ => None
        };
        Self { base: self.base.clone(), sourceref }
    }
    fn sourceref_trace(&self) -> Option<String> {
        match &self.sourceref {
            Some(SourceReference::File {file, start,end}) => Some(format!("File {} at {}:{} - {}:{}", file, start.0,start.1,end.0,end.1)),
            Some(SourceReference::Expansion {token, ..}) => {
                let mut trace = format!("Expanded from {}",token);
                match token.sourceref_trace() {
                    Some(s) => {
                        trace.push_str("\n - ");
                        trace.push_str(&s)
                    },
                    None => ()
                }
                Some(trace)
            },
            None => None
        }
    }
}
impl<ET:EngineType<Token=Self>> PartialEq for TokenWithSourceref<ET> {
    fn eq(&self, other: &Self) -> bool { PartialEq::eq(&self.base,&other.base) }
}
impl<ET:EngineType<Token=Self>>Display for TokenWithSourceref<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { Display::fmt(&self.base,f) }
}
impl<ET:EngineType<Token=Self>> Debug for TokenWithSourceref<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.sourceref {
            Some(ref s) => write!(f, "{:?} at {:?}", self.base, s),
            None => write!(f, "{:?}", self.base)
        }
    }
}


/// A [`SourceReference`] allows tracking the origin of a [`Token`]. It is either a file reference
/// or a reference to an expansion: The result of expanding a control sequence token.
#[derive(Clone)]
pub enum SourceReference<ET:EngineType> {
    /// A reference to a file, with the file name and the start and end position of the
    /// [`Token`] in the file.
    File{file: Ptr<String>,start:(usize,usize),end:(usize,usize)},
    /// A reference to an expansion, with the [`Token`] that was expanded via [`BaseCommand`].
    Expansion{token: Box<ET::Token>,cmd:Option<ET::CommandReference>}
}
impl<ET:EngineType> Debug for SourceReference<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceReference::File{file,start,end} => write!(f, "File {}; {:?} - {:?}", file, start, end),
            SourceReference::Expansion{token,cmd} => write!(f, "Expansion of {} via {:?}", token, cmd)
        }
    }
}

 */