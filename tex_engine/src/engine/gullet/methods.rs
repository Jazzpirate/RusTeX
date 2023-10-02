//! Default implementations for [`Gullet`] methods.
use crate::{debug_log, file_end, throw};
use crate::engine::{EngineRef, EngineType};
use crate::engine::gullet::Gullet;
use crate::engine::memory::Interner;
use crate::engine::state::State;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{BaseCommand, ResolvedToken, ConditionalFun, CommandSource, StomachCommand};
use crate::tex::numbers::{Int, MuSkip, Skip};
use crate::tex::token::{CSLike, StringConsumer, Token, TokenConsumer};
use crate::engine::mouth::Mouth;
use crate::tex::commands::etex::UNEXPANDED;
use crate::tex::commands::tex::{IFCASE, NOEXPAND, THE};
use crate::tex::ConditionalBranch;
use crate::tex::fonts::{Font,FontStore};
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;
use crate::utils::strings::AllCharsTrait;


/// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
/// (or [`None`] if the [`Mouth`] is empty)
pub fn get_next_unexpandable<ET:EngineType>(engine:&mut EngineRef<ET>) -> Option<ResolvedToken<ET>> {
    while let Some((next,e)) = engine.get_next_token() {
        if !e {return Some(ResolvedToken{command:BaseCommand::Relax,source:CommandSource{cause:next,reference:None},expand:false})}
        match engine.expand(resolve_token(&engine.state,next)) {
            Some(r) => return Some(r),
            _ => ()
        }
    }
    None
}

/// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
/// (or [`None`] if the [`Mouth`] is empty). Throws an error if the file ends in the process
pub fn get_next_unexpandable_same_file<ET:EngineType>(engine:&mut EngineRef<ET>) -> Option<ResolvedToken<ET>> {
    while let Some(next) = engine.mouth.get_next_simple(&engine.state,&mut engine.interner) {
        match engine.expand(resolve_token(&engine.state,next)) {
            Some(r) => return Some(r),
            _ => ()
        }
    }
    None
}


/// Resolves the given [`Token`]
pub fn resolve_token<ET:EngineType>(state:&ET::State,tk:ET::Token) -> ResolvedToken<ET> {
    let cmd = match tk.as_cs_like() {
        None => return ResolvedToken {
            command: tk.to_command(),
            source: CommandSource { cause: tk, reference: None },
            expand: true
        },
        Some(CSLike::CS(name)) => state.get_command(name),
        Some(CSLike::ActiveChar(c)) => state.get_ac_command(c)
    };
    match cmd {
        Some(c) => ResolvedToken {
            command: c.base.clone(),
            source: CommandSource { cause: tk, reference: c.reference.clone() },
            expand: true
        },
        None => ResolvedToken {
            command: BaseCommand::None,
            source: CommandSource { cause: tk, reference: None },
            expand: true
        }
    }
    /*
    match match &tk.base {
        BaseToken::Char(c, CategoryCode::Active) => state.get_ac_command(c),
        BaseToken::CS(name) => state.get_command(name),
        BaseToken::Char(char,catcode) =>
            return ResolvedToken {
                command: BaseCommand::Char { char: *char, catcode: *catcode },
                source: CommandSource { cause: tk, reference: None },
                expand: true
            }
    } {
        None => ResolvedToken {
            command:BaseCommand::None,
            source:CommandSource{cause:tk,reference:None},
            expand:true
        },
        Some(cmd) => ResolvedToken{
            command:cmd.base.clone(),
            source:CommandSource{cause:tk,reference:cmd.reference.clone()},
            expand:true
        }
    }

     */
}

pub fn do_conditional<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:CommandSource<ET>, name:&'static str, apply:ConditionalFun<ET>, unless:bool) {
    if name == IFCASE {
        debug_log!(trace=>"ifcase");
        let i = engine.gullet.new_conditional(IFCASE);
        let ret = engine.get_int().to_i64();
        debug_log!(trace=>"ifcase: {}",ret);
        engine.gullet.set_conditional(i, ConditionalBranch::Case(ret, 0));
        if ret == 0 {
            debug_log!(trace=>"True conditional");
        } else {
            false_loop::<ET>(engine,IFCASE,i)
        }
    } else {
        let i = engine.gullet.new_conditional(name);
        let b = apply(engine,cmd);
        if (b && !unless) || (!b && unless) {
            engine.gullet.set_conditional(i,ConditionalBranch::True(name));
            debug_log!(trace=>"True conditional");
        } else { false_loop::<ET>(engine,name,i) }
    }
}

pub fn false_loop<ET:EngineType>(engine:&mut EngineRef<ET>,condname:&'static str,condidx:usize) {
    debug_log!(trace=>"False conditional. Skipping...");
    let mut incond:usize = engine.gullet.current_conditional().1 - condidx;
    for i in 0..incond {
        engine.gullet.pop_conditional();
    }
    while let Some((next,_)) = engine.get_next_token() {
        match next.as_cs_like() {
            Some(_) => match resolve_token::<ET>(&engine.state,next).command {
                BaseCommand::Conditional {..} => incond += 1,
                BaseCommand::ExpandableNoTokens {name:"else",..} if incond == 0 => {
                    engine.gullet.set_top_conditional(ConditionalBranch::Else(condname));
                    debug_log!(trace=>"...else branch.");
                    return ()
                }
                BaseCommand::ExpandableNoTokens {name:"or",..} if incond == 0 && condname == "ifcase" => {
                    match engine.gullet.current_conditional() {
                        (Some(ConditionalBranch::Case(i, j)),_) => {
                            engine.gullet.set_top_conditional(ConditionalBranch::Case(i, j + 1));
                            if i == ((j+1) as i64) {
                                debug_log!(trace=>"...or branch.");
                                return ()
                            }
                        }
                        _ => unreachable!()
                    }
                }
                BaseCommand::ExpandableNoTokens {name:"fi",..} if incond > 0 => incond -= 1,
                BaseCommand::ExpandableNoTokens {name:"fi",..} => {
                    debug_log!(trace=>"...end of conditional.");
                    engine.gullet.pop_conditional();
                    return ()
                }
                _ => ()
            }
            _ => ()
        }
    }
    file_end!()
}

pub fn else_loop<ET:EngineType>(engine:&mut EngineRef<ET>,condname:&'static str,condidx:usize,allowelse:bool) {
    debug_log!(trace=>"\\else. Skipping...");
    engine.gullet.set_top_conditional(ConditionalBranch::Else(condname));
    let mut incond:usize = engine.gullet.current_conditional().1 - condidx;
    while let Some((next,exp)) = engine.get_next_token() {
        match next.as_cs_like() {
            Some(_) => match resolve_token::<ET>(&engine.state,next).command {
                BaseCommand::Conditional {..} => incond += 1,
                BaseCommand::ExpandableNoTokens {name:"else",..} if incond == 0 && !allowelse => {
                    throw!("Unexpected \\else")
                }
                BaseCommand::ExpandableNoTokens {name:"fi",..} if incond > 0 => incond -= 1,
                BaseCommand::ExpandableNoTokens {name:"fi",..} => {
                    debug_log!(trace=>"...end of conditional.");
                    engine.gullet.pop_conditional();
                    return ()
                }
                _ => ()
            }
            _ => ()
        }
    }
    file_end!()
}

pub fn get_keyword<'a,ET:EngineType>(engine:&mut EngineRef<ET>, kw:&'a str) -> bool {
    debug_log!(trace=>"Reading keyword {:?}: {}...\n at {}",kw,engine.preview(50).replace("\n","\\n"),engine.current_position());
    let mut current = engine.memory.get_string();
    let mut rs = engine.mouth.get_expansion();
    while let Some(next) = engine.get_next_unexpandable() {
        rs.push(next.source.cause);
        match next.command {
            BaseCommand::Char {char,..} if char.as_bytes().len() == 1 => {
                let us = char.as_bytes()[0];
                current.push(us as char);
                if current == kw {
                    rs.clear();
                    engine.mouth.push_expansion(rs);
                    engine.memory.return_string(current);
                    return true
                }
                else if !kw.starts_with(&current) {
                    engine.mouth.push_expansion(rs);
                    engine.memory.return_string(current);
                    return false
                }
            }
            _ => {
                engine.mouth.push_expansion(rs);
                engine.memory.return_string(current);
                return false
            }
        }
    }
    file_end!()
}

pub fn get_keywords<'a,ET:EngineType>(engine:&mut EngineRef<ET>, mut keywords:Vec<&'a str>) -> Option<&'a str> {
    debug_log!(trace=>"Reading keywords {:?}: {}...\n at {}",keywords,engine.preview(50).replace("\n","\\n"),engine.current_position());
    let mut current = String::new();
    let mut rs = engine.mouth.get_expansion();
    while let Some(next) = engine.get_next_unexpandable() {
        rs.push(next.source.cause.clone());
        match next.command {
            BaseCommand::Char{char,..} if char.as_bytes().len() == 1 => {
                let us = char.as_bytes()[0];
                let us = us.to_ascii_lowercase();
                if keywords.iter().any(|s| s.starts_with(&current) && s.len()>current.len() && s.as_bytes()[current.len()] == us) {
                    current.push(us as char);
                    keywords = keywords.into_iter().filter(|s| s.starts_with(&current)).collect();
                    if keywords.is_empty() {
                        engine.mouth.push_expansion(rs);
                        return None
                    }
                    else if keywords.len() == 1 && keywords[0] == current {
                        rs.clear();
                        engine.mouth.push_expansion(rs);
                        return Some(keywords[0])
                    }
                } else if keywords.contains(&current.as_str()) {
                    engine.mouth.requeue(next.source.cause);
                    rs.clear();
                    engine.mouth.push_expansion(rs);
                    keywords = keywords.into_iter().filter(|s| s == &current).collect();
                    return Some(keywords[0])
                } else {
                    engine.mouth.push_expansion(rs);
                    return None
                }
            }
            _ if keywords.contains(&current.as_str()) => {
                engine.mouth.requeue(next.source.cause);
                rs.clear();
                engine.mouth.push_expansion(rs);
                keywords = keywords.into_iter().filter(|s| s == &current).collect();
                return Some(keywords[0])
            }
            _ => {
                engine.mouth.push_expansion(rs);
                return None
            }
        }
    }
    file_end!()
}

pub fn get_string<ET:EngineType>(engine:&mut EngineRef<ET>, ret:&mut String) {
    debug_log!(trace=>"Reading string {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace();
    let mut quoted = false;
    while let Some(next) = engine.get_next_unexpandable() {
        match next.command {
            BaseCommand::Char {catcode:CategoryCode::Space,..} if quoted => ret.push(' '),
            BaseCommand::Char {catcode:CategoryCode::Space,..} => return (),
            BaseCommand::Char {char,..} if char.as_byte() == b'\"' => {
                quoted = !quoted;
            }
            BaseCommand::Char {char,..} => ret.push(char.as_char()), //(char.as_bytes())),//ret.push(char.to_char()),
            _ => {
                engine.mouth.requeue(next.source.cause);
                return ()
            }
        }
    }
}

pub fn get_braced_string<ET:EngineType>(engine:&mut EngineRef<ET>, ret:&mut String) {
    let mut s = StringConsumer::<&mut String,ET>::simple(ret);
    engine.get_expanded_group(false,false,true,|engine,t| s.consume_tk(&t,&engine.interner));
}

pub fn get_control_sequence<ET:EngineType>(engine:&mut EngineRef<ET>) -> CSLike<ET::Char> {
    engine.skip_whitespace();
    while let Some((next,e)) = engine.get_next_token() {
        let resolved = resolve_token::<ET>(&engine.state,next);
        match resolved.command {
            BaseCommand::Char{char,catcode:CategoryCode::Active} =>
                match get_cs_check_command::<ET>(engine, resolved) {
                    None => (),
                    Some(t) => return t,
                }
            BaseCommand::Char{..} => {
                throw!("Control sequence expected")
                // error
            }
            _ => match get_cs_check_command::<ET>(engine, resolved) {
                None => (),
                Some(t) => return t,
            }
        }
    }
    file_end!()
}

fn get_cs_check_command<ET:EngineType>(engine:&mut EngineRef<ET>, resolved:ResolvedToken<ET>)
                                       -> Option<CSLike<ET::Char>> {
    match resolved.command {
        BaseCommand::Expandable {..} | BaseCommand::Conditional { .. } | BaseCommand::ExpandableNoTokens {..} => {
            engine.expand(resolved);
            None
        },
        _ => Some(resolved.source.cause.as_cs_like().unwrap())
    }
}

pub fn get_font<ET:EngineType>(engine:&mut EngineRef<ET>) -> ET::FontRef {
    match engine.get_next_unexpandable() {
        None => file_end!(),
        Some(res) => match res.command {
            BaseCommand::Font(f) => f,
            BaseCommand::FontCommand {get,..} => {
                get(engine,res.source)
            }
            _ => throw!("Expected a font, got {:?}",res.command)
        }
    }
}


impl<ET:EngineType> EngineRef<ET> {
    #[inline(never)]
    pub fn eat_relax(&mut self) {
        match self.get_next_token() {
            Some((t,_)) => match t.as_cs_like() {
                Some(CSLike::CS(n)) => match self.state.get_command(n) {
                    Some(c) if c.base == BaseCommand::Relax => (),
                    _ => self.mouth.requeue(t)
                }
                Some(CSLike::ActiveChar(c)) => match self.state.get_ac_command(c) {
                    Some(c) if c.base == BaseCommand::Relax => (),
                    _ => self.mouth.requeue(t)
                }
                _ => self.mouth.requeue(t)
            }
            _ => ()
        }
    }

    /// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
    /// (or [`None`] if the [`Mouth`] is empty)
    pub fn get_next_unexpandable(&mut self) -> Option<ResolvedToken<ET>> {
        ET::Gullet::get_next_unexpandable(self)
    }


    /// Returns the next primitive to be processed by the [`Stomach`](crate::engine::stomach::Stomach) from
    /// the input stream, after expanding macros as necessary.
    pub fn get_next_stomach_command(&mut self) -> Option<StomachCommand<ET>> {
        ET::Gullet::get_next_stomach_command(self)
    }

    /// read a single keyword from the input stream; returns `true` if the keyword is found.
    pub fn get_keyword<'a>(&mut self, keyword: &'a str) -> bool {
        ET::Gullet::get_keyword(self, keyword)
    }

    /// reads one of several optional keywords from the input stream;
    /// returns `None` if none of the keywords are found.
    pub fn get_keywords<'a>(&mut self, keywords: Vec<&'a str>) -> Option<&'a str> {
        ET::Gullet::get_keywords(self, keywords)
    }

    /// Reads a number from the input stream.
    pub fn get_int(&mut self) -> ET::Int {
        ET::Gullet::get_int(self)
    }

    /// Reads a dimension from the input stream.
    pub fn get_dim(&mut self) -> ET::Dim {
        ET::Gullet::get_dim(self)
    }

    /// Reads a skip from the input stream.
    pub fn get_skip(&mut self) -> Skip<ET::SkipDim> {
        ET::Gullet::get_skip(self)
    }

    /// Reads a muskip from the input stream.
    pub fn get_muskip(&mut self) -> MuSkip<ET::MuDim, ET::MuStretchShrinkDim> {
        ET::Gullet::get_muskip(self)
    }

    pub fn get_mudim(&mut self) -> ET::MuDim {
        ET::Gullet::get_mudim(self)
    }

    pub fn get_control_sequence(&mut self) -> CSLike<ET::Char> {
        get_control_sequence::<ET>(self)
    }

    pub fn get_char(&mut self) -> ET::Char {
        let num = self.get_int();
        match ET::Char::from_i64(num.to_i64()) {
            Some(i) => i,
            None => throw!("Not a valid character: {}",num)
        }
    }

    pub fn get_string(&mut self, string: &mut String) {
        ET::Gullet::get_string(self, string)
    }

    pub fn get_braced_string(&mut self, string: &mut String) {
        get_braced_string::<ET>(self, string)
    }

    pub fn get_font(&mut self) -> ET::FontRef {
        ET::Gullet::get_font(self)
    }

    pub fn is_next_char_one_of(&mut self, chars: &'static [u8]) -> Option<u8> {
        match self.get_next_unexpandable() {
            None => file_end!(),
            Some(res) => match res.command {
                BaseCommand::Char { char, .. } if char.as_bytes().len() == 1 => {
                    let c = char.as_bytes()[0];
                    if chars.contains(&c) {
                        Some(c)
                    } else {
                        self.mouth.requeue(res.source.cause);
                        None
                    }
                }
                _ => {
                    self.mouth.requeue(res.source.cause);
                    None
                }
            }
        }
    }

    pub fn is_next_char(&mut self, char: u8) -> bool {
        match self.get_next_unexpandable() {
            None => file_end!(),
            Some(res) => match res.command {
                BaseCommand::Char { char: c, .. } if c.as_bytes() == [char] => true,
                _ => {
                    self.mouth.requeue(res.source.cause);
                    false
                }
            }
        }
    }

    pub fn expand(&mut self, r: ResolvedToken<ET>) -> Option<ResolvedToken<ET>> {
        ET::Gullet::expand(self, r)
    }

    pub fn get_expanded_group<F: FnMut(&mut EngineRef<ET>, ET::Token)>(&mut self, expand_protected: bool, edef_like: bool, err_on_unknowns: bool, mut f: F) {
        match self.mouth.get_next(&self.state, &mut self.interner, &mut self.outputs) {
            Some(t) if t.is_begin_group() => (),
            _ => throw!("begin group expected")
        }
        let mut ingroup = 0;
        let mut ok = false;
        while let Some(mut tk) = self.mouth.get_next(&self.state, &mut self.interner, &mut self.outputs) {
            let e = if tk.is_noexpand_marker(&self.interner) {
                tk = self.mouth.get_next(&self.state, &mut self.interner, &mut self.outputs).unwrap();
                false
            } else {
                if tk.is_begin_group() {
                    ingroup += 1;
                    f(self, tk);
                    continue
                }
                if tk.is_end_group() {
                    if ingroup == 0 {
                        ok = true;
                        break
                    } else { ingroup -= 1; }
                    f(self, tk);
                    continue
                }
                true
            };
            if e {
                let res = crate::engine::gullet::methods::resolve_token::<ET>(&self.state, tk);
                match res.command {
                    BaseCommand::Def(d) if d.protected && !expand_protected => {
                        f(self, res.source.cause)
                    }
                    BaseCommand::ExpandableNoTokens { name, .. } if name == crate::tex::commands::tex::NOEXPAND => {
                        match self.get_next_token() {
                            Some((t, _)) if t.is_eof() => (),
                            None => file_end!(),
                            Some((tk, _)) => { f(self, tk) }
                        }
                    },
                    BaseCommand::Expandable { name, apply } if name == crate::tex::commands::etex::UNEXPANDED && edef_like => {
                        self.expand_until_group(|engine,tk| {
                            if tk.is_parameter() {
                                f(engine,tk.clone())
                            }
                            f(engine,tk);
                        });
                    }
                    BaseCommand::Expandable { name, apply } if name == crate::tex::commands::etex::UNEXPANDED => {
                        self.expand_until_group(&mut f);
                    }
                    BaseCommand::Expandable { name, apply } if name == crate::tex::commands::tex::THE && edef_like =>
                        crate::tex::commands::tex::do_the::<ET,_>(self,&res.source,|engine,tk| {
                            if tk.is_parameter() {
                                f(engine,tk.clone())
                            }
                            f(engine,tk)
                        }),
                    BaseCommand::None if err_on_unknowns => match res.source.cause.as_cs_like() {
                        Some(crate::tex::token::CSLike::ActiveChar(c)) => throw!("Undefined active character {}",c.as_char()),
                        Some(crate::tex::token::CSLike::CS(name)) => throw!("Undefined control sequence {}",name.to_str(&self.interner)),
                        _ => unreachable!()
                    }
                    _ => match self.expand(res) {
                        Some(res) => {
                            f(self, res.source.cause)
                        },
                        _ => ()
                    }
                }
            } else { f(self, tk) }
        }
        if (!ok) { file_end!() }
    }

    pub fn expand_until_group<F: FnMut(&mut EngineRef<ET>, ET::Token)>(&mut self,f:F) {
        match self.get_next_unexpandable() {
            None => file_end!(),
            Some(res) => match res.command {
                BaseCommand::Char { catcode:CategoryCode::BeginGroup, .. } => {
                    self.get_until_endgroup(f)
                //ET::Mouth::get_until_endgroup(self,f)
                }
                _ => throw!("begin group expected; found: {}",res.source.cause.printable(&self.interner))
            }
        }
    }
}
