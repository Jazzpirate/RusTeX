use std::hint::unreachable_unchecked;
use crate::{catch, debug_log, file_end, throw};
use crate::engine::{EngineRef, EngineType};
use crate::engine::mouth::Mouth;
use crate::engine::gullet::methods::get_keywords;
use crate::engine::state::State;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{BaseCommand, ResolvedToken};
use crate::tex::fonts::Font;
use crate::utils::strings::CharType;
use crate::tex::numbers::{MuSkip, Skip, Int, Dim, SkipDim, MuDim, MuStretchShrinkDim};
use crate::tex::token::BaseToken;
use crate::utils::errors::TeXError;

fn is_ascii_digit(u:usize) -> bool {
    u >= 48 && u <= 57
}
fn is_ascii_oct_digit(u:usize) -> bool {
    u >= 48 && u <= 55
}
fn is_ascii_hex_digit(u:usize) -> bool {
    is_ascii_digit(u) || (u >= 65 && u <= 70) || (u >= 97 && u <= 102)
}

pub fn get_int<ET:EngineType>(engine:&mut EngineRef<ET>) -> ET::Int {
    debug_log!(trace=>"Reading number {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace();
    let mut isnegative = false;
    let mut ishex = false;
    let mut isoct = false;
    while let Some(next) = engine.get_next_unexpandable_same_file() {
        use crate::tex::commands::BaseCommand::*;
        match next.command {
            Def(_) | Conditional{..} | Expandable{..} => unsafe{unreachable_unchecked()},
            Char{char,catcode} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(engine.skip_whitespace() => next.source.cause);
                    }
                    43 => /* + */ catch!(engine.skip_whitespace()=> next.source.cause),
                    34 if !ishex && !isoct => ishex = true, // "
                    39 if !ishex && !isoct => isoct = true, // '
                    96 if !ishex && !isoct => { // `
                        match catch!(engine.get_next_token()=> next.source.cause) {
                            std::option::Option::None => file_end!(next.source.cause),
                            Some((tk,_)) => {
                                let c = match &tk.base {
                                    BaseToken::Char(c,_) => *c,
                                    BaseToken::CS(str) => {
                                        let str = ET::Char::tokenize(str.to_str(engine.interner));
                                        if str.len() != 1 { throw!("Number expected" => next.source.cause) }
                                            str[0]
                                    }
                                };
                                catch!(expand_until_space::<ET>(engine) => tk);
                                let us =  c.to_usize() as i64;
                                let us = if isnegative { -us } else { us };
                                return catch!(ET::Int::from_i64(us) => tk)
                            }
                        }
                    },
                    _ if is_ascii_hex_digit(us) && ishex =>
                    // TODO: texnically, this requires catcode 12 for 0-9 and catcode 11 or 12 for A-F
                        return read_hex_number::<ET>(engine,us as u8,isnegative),
                    _ if is_ascii_digit(us) && !isoct =>
                    // TODO: texnically, this requires catcode 12
                        return read_decimal_number::<ET>(engine,us as u8,isnegative),
                    _ if is_ascii_oct_digit(us) => // isoct == true
                    // TODO: texnically, this requires catcode 12
                        return read_oct_number::<ET>(engine,us as u8,isnegative),
                    _ => {
                        let c = ET::Int::from_i64(char.to_usize() as i64);
                        debug_log!(trace=>"Returning {}",c);
                        return c
                    }
                }
            }
            Int(ass) => {
                let val = ass.get(engine,next.source);
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return val
            }
            Dim(ass) => {
                let val = ET::Int::from_i64(ass.get(engine,next.source).to_sp());
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return val
            }
            Skip(ass) => {
                let val = ET::Int::from_i64(ass.get(engine,next.source).base.to_sp());
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return val
            }
            CharDef(char) => {
                let val = ET::Int::from_i64(char.to_usize() as i64);
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return val
            }
            MathChar(u) => {
                let val = ET::Int::from_i64(u as i64);
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return val
            }
            _ => throw!("Number expected" => next.source.cause)
        }
    }
    file_end!()
}

pub fn expand_until_space<ET:EngineType>(engine:&mut EngineRef<ET>) {
    match engine.get_next_unexpandable_same_file() {
        Some(cmd) => match cmd.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => (), // eat one space
            _ => {
                engine.mouth.requeue(cmd.source.cause);
            }
        }
        None => ()
    }
}

pub fn read_decimal_number<ET:EngineType>(engine:&mut EngineRef<ET>, firstchar:u8, isnegative:bool) -> ET::Int {
    debug_log!(trace=>"Reading decimal number {}...",(firstchar as char));
    let mut rets = vec!(firstchar);

    while let Some(next) = engine.get_next_unexpandable_same_file() {
        match next.command {
            BaseCommand::Char {catcode:CategoryCode::Space,..} => break, // eat one space
            BaseCommand::Char{char,..} => {
                let us = char.to_usize();
                if is_ascii_digit(us) {
                    rets.push(us as u8);
                } else {
                    engine.mouth.requeue(next.source.cause);
                    break
                }
            }
            _ => {
                engine.mouth.requeue(next.source.cause);
                break
            }
        }
    }
    use std::str::FromStr;
    let i = i64::from_str(std::str::from_utf8(&rets).unwrap()).unwrap();
    debug_log!(trace=>"Returning {}",if isnegative { -i } else { i });
    ET::Int::from_i64(if isnegative { -i } else { i })
}


pub fn read_oct_number<ET:EngineType>(engine:&mut EngineRef<ET>, firstchar:u8, isnegative:bool) -> ET::Int {
    debug_log!(trace=>"Reading hexadecimal number {}...",(firstchar as char));
    let mut rets = vec!(firstchar);
    while let Some(next) = engine.get_next_unexpandable_same_file() {
        match next.command {
            BaseCommand::Char {catcode:CategoryCode::Space,..} => break, // eat one space
            BaseCommand::Char{char,..} => {
                let us = char.to_usize();
                if is_ascii_oct_digit(us) {
                    rets.push(us as u8);
                } else {
                    engine.mouth.requeue(next.source.cause);
                    break
                }
            }
            _ => {
                engine.mouth.requeue(next.source.cause);
                break
            }
        }
    }
    let str = std::str::from_utf8(&rets).unwrap();
    let i = i64::from_str_radix(str,8).unwrap();
    debug_log!(trace=>"Returning {}",if isnegative { -i } else { i });
    ET::Int::from_i64(if isnegative { -i } else { i })
}


pub fn read_hex_number<ET:EngineType>(engine:&mut EngineRef<ET>, firstchar:u8, isnegative:bool) -> ET::Int {
    debug_log!(trace=>"Reading hexadecimal number {}...",(firstchar as char));
    let mut rets = vec!(firstchar);
    while let Some(next) = engine.get_next_unexpandable_same_file() {
        match next.command {
            BaseCommand::Char {catcode:CategoryCode::Space,..} => break, // eat one space
            BaseCommand::Char{char,..} => {
                let us = char.to_usize();
                if is_ascii_hex_digit(us) {
                    rets.push(us as u8);
                } else {
                    engine.mouth.requeue(next.source.cause);
                    break
                }
            }
            _ => {
                engine.mouth.requeue(next.source.cause);
                break
            }
        }
    }
    let str = std::str::from_utf8(&rets).unwrap();
    let i = i64::from_str_radix(str,16).unwrap();
    debug_log!(trace=>"Returning {}",if isnegative { -i } else { i });
    ET::Int::from_i64(if isnegative { -i } else { i })
}


pub fn get_dim<ET:EngineType>(engine:&mut EngineRef<ET>) -> ET::Dim {
    debug_log!(trace=>"Reading dimension {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace();
    let mut isnegative = false;
    while let Some(next) = engine.get_next_unexpandable_same_file() {
        match next.command {
            BaseCommand::Char{char,..} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(engine.skip_whitespace()
                            => next.source.cause
                        );
                    }
                    43 => /* + */ catch!(engine.skip_whitespace()
                        => next.source.cause
                    ),
                    _ => return get_dim_inner::<ET>(engine,isnegative,next)
                }
            }
            _ => return get_dim_inner::<ET>(engine,isnegative,next)
        }
    }
    file_end!()
}


pub fn get_dim_inner<ET:EngineType>(engine:&mut EngineRef<ET>, isnegative:bool, next:ResolvedToken<ET>)
                                    -> ET::Dim {
    match next.command {
        BaseCommand::Char { char,.. } => {
            let us = char.to_usize();
            match us {
                46 | 44 => /* . / ,*/ {
                    let f = catch!(read_float::<ET>(engine,us as u8,isnegative) => next.source.cause);
                    return read_unit::<ET>(engine, f)
                },
                _ if is_ascii_digit(us) => {
                    let f = catch!(read_float::<ET>(engine,us as u8,isnegative) => next.source.cause);
                    return read_unit::<ET>(engine, f)
                },
                _ => todo!("Non-digit in read_dimension: {}/{}\nat: {}", char.as_char(), us, engine.current_position())
            }
        }
        BaseCommand::Dim(ass) => {
            let val = ass.get(engine,next.source);
            return if isnegative { -val } else { val }
        }
        BaseCommand::Int(ass) => {
            let val = ass.get(engine,next.source).to_i64() as f64;
            let val = if isnegative { -val } else { val };
            return read_unit::<ET>(engine, val)
        }
        BaseCommand::Skip(ass) => {
            let val = ass.get(engine,next.source).base;
            return if isnegative { -val } else { val }
        }
        BaseCommand::CharDef(char) => {
            let val = char.to_usize() as f64;
            let val = if isnegative { -val } else { val };
            return read_unit::<ET>(engine, val)
        }
        o => todo!("Non-char in read_dim: {:?}\n{}\n at {}", o, engine.preview(50).replace("\n", "\\n"), engine.current_position())
    }
}

pub fn read_unit<ET:EngineType>(engine:&mut EngineRef<ET>, float:f64) -> ET::Dim {
    debug_log!(trace=>"Reading unit {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    match engine.get_next_unexpandable_same_file() {
        Some(cmd) => match cmd.command {
            BaseCommand::Dim(ass) => {
                let val = ass.get(engine,cmd.source);
                val.tex_mult(float)
            }
            BaseCommand::Skip(ass) => {
                let val = ass.get(engine,cmd.source).base;
                val.tex_mult(float)
            }
            BaseCommand::Char { .. } => {
                engine.mouth.requeue(cmd.source.cause);
                engine.skip_whitespace();
                if engine.get_keyword( "true") {
                    engine.skip_whitespace();
                    let mag = engine.state.get_primitive_int("mag").to_i64() as f64 / 1000.0;
                    let mut units =ET::Dim::units();
                    units.push("em");units.push("ex");
                    match engine.get_keywords( units) {
                        Some("em") => {
                            let d = engine.state.get_current_font().get_dim::<ET::Dim>(6);
                            d.tex_mult(float * mag)
                        }
                        Some("ex") => {
                            let d = engine.state.get_current_font().get_dim::<ET::Dim>(5);
                            d.tex_mult(float * mag)
                        }
                        Some(dim) => ET::Dim::from_float(dim, float * mag),
                        _ => throw!("Expected unit")
                    }
                } else {
                    let mut units =ET::Dim::units();
                    units.push("em");units.push("ex");
                    match engine.get_keywords( units) {
                        Some("em") => {
                            let d = engine.state.get_current_font().get_dim::<ET::Dim>(6);
                            d.tex_mult(float)
                        }
                        Some("ex") => {
                            let d = engine.state.get_current_font().get_dim::<ET::Dim>(5);
                            d.tex_mult(float)
                        }
                        Some(dim) => ET::Dim::from_float(dim, float),
                        _ => todo!("Non-unit in read_unit: {}\n at {}", engine.preview(50).replace("\n", "\\n"), engine.current_position())
                    }
                }
            }
            _ => todo!("Non-dimension in read_unit: {}\n at {}", engine.preview(50).replace("\n", "\\n"), engine.current_position())
        }
        None => file_end!()
    }
}

pub fn get_skip<ET:EngineType>(engine:&mut EngineRef<ET>) -> Skip<ET::SkipDim> {
    debug_log!(trace=>"Reading skip {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace();
    let mut isnegative = false;
    while let Some(next) = engine.get_next_unexpandable_same_file() {
        match next.command {
            BaseCommand::Char{char,..} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(engine.skip_whitespace() => next.source.cause);
                    }
                    43 => /* + */ catch!(engine.skip_whitespace() => next.source.cause),
                    _ => return get_skip_inner::<ET>(engine,isnegative,next)
                }
            }
            BaseCommand::Skip(ass) => {
                let val = ass.get(engine,next.source);
                return if isnegative { -val } else { val }
            }
            _ => return get_skip_inner::<ET>(engine,isnegative,next)
        }
    }
    file_end!()
}

fn get_skip_inner<ET:EngineType>(engine:&mut EngineRef<ET>, isnegative:bool, next:ResolvedToken<ET>)
                                 -> Skip<ET::SkipDim> {
    let base = get_dim_inner::<ET>(engine,isnegative,next);
    engine.skip_whitespace();
    let stretch = if engine.get_keyword("plus") {
        Some(get_skipdim::<ET>(engine))
    } else {None};
    engine.skip_whitespace();
    let shrink = if engine.get_keyword("minus") {
        Some(get_skipdim::<ET>(engine))
    } else {None};
    Skip{base,stretch,shrink}

}

pub fn get_skipdim<ET:EngineType>(engine:&mut EngineRef<ET>) -> ET::SkipDim {
    debug_log!(trace=>"Reading dimension {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace();
    let mut isnegative = false;
    while let Some(next) = engine.get_next_unexpandable_same_file() {
        match next.command {
            BaseCommand::Char {char,..} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(engine.skip_whitespace() => next.source.cause);
                    }
                    43 => /* + */ catch!(engine.skip_whitespace() => next.source.cause),
                    46 | 44 => /* . / ,*/ {
                        let f = catch!(read_float::<ET>(engine,us as u8,isnegative) => next.source.cause);
                        return read_skip_unit::<ET>(engine,f)
                    },
                    _ if is_ascii_digit(us) => {
                        let f = catch!(read_float::<ET>(engine,us as u8,isnegative) => next.source.cause);
                        return read_skip_unit::<ET>(engine,f)
                    },
                    _ => todo!("Non-digit in read_skipdim")
                }
            }
            BaseCommand::Dim(ass) => {
                let val = ass.get(engine,next.source);
                let ret = if isnegative { -val } else { val };
                return ET::SkipDim::from_dim(ret)
            }
            _ => todo!("Non-char in read_skipdim")
        }
    }
    file_end!()
}

pub fn read_skip_unit<ET:EngineType>(engine:&mut EngineRef<ET>, float:f64) -> ET::SkipDim {
    debug_log!(trace=>"Reading skip unit {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    match engine.get_next_unexpandable_same_file() {
        None => file_end!(),
        Some(next) => match next.command {
            BaseCommand::Char {char,..} => {
                engine.mouth.requeue(next.source.cause);
                engine.skip_whitespace();
                if engine.get_keyword("true") {
                    engine.skip_whitespace();
                    let mag = engine.state.get_primitive_int("mag").to_i64() as f64 / 1000.0;
                    match engine.get_keywords(ET::Dim::units()) {
                        Some(dim) => ET::SkipDim::from_float(dim,float * mag),
                        _ => throw!("Skip unit expected")
                    }
                } else {
                    match engine.get_keywords(ET::SkipDim::units()) {
                        Some(dim) => ET::SkipDim::from_float(dim,float),
                        _ => todo!("Non-unit in read_skip_unit: {}",engine.preview(50).replace("\n","\\n"))
                    }
                }
            }
            BaseCommand::Dim(ass) => {
                let val = ass.get(engine,next.source);
                ET::SkipDim::from_dim(val.tex_mult(float))
            }
            o => todo!("Non-unit in read_skip_unit: {:?}\n{}",o,engine.preview(50).replace("\n","\\n"))
        }
    }
}

pub fn get_muskip<ET:EngineType>(engine:&mut EngineRef<ET>)-> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
    debug_log!(trace=>"Reading muskip {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace();
    let mut isnegative = false;
    while let Some(next) = engine.get_next_unexpandable_same_file() {
        match next.command {
            BaseCommand::Char {char,..} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(engine.skip_whitespace() => next.source.cause);
                    }
                    43 => /* + */ catch!(engine.skip_whitespace() => next.source.cause),
                    _ => return get_muskip_inner::<ET>(engine,isnegative,next)
                }
            }
            BaseCommand::MuSkip(ass) => {
                let val = ass.get(engine,next.source);
                return if isnegative { -val } else { val }
            }
            _ => return get_muskip_inner::<ET>(engine,isnegative,next)
        }
    }
    file_end!()
}

fn get_muskip_inner<ET:EngineType>(engine:&mut EngineRef<ET>, isnegative:bool, next:ResolvedToken<ET>)
                                   -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
    let base = get_mudim::<ET>(engine, isnegative, next);
    engine.skip_whitespace();
    let stretch = if engine.get_keyword("plus") {
        Some(get_mustretchdim::<ET>(engine))
    } else {None};
    engine.skip_whitespace();
    let shrink = if engine.get_keyword("minus") {
        Some(get_mustretchdim::<ET>(engine))
    } else {None};
    MuSkip{base,stretch,shrink}
}

pub fn get_mudim<ET:EngineType>(engine:&mut EngineRef<ET>, isnegative:bool, next:ResolvedToken<ET>)
                                -> ET::MuDim {
    match next.command {
        BaseCommand::Char {char,..} => {
            let us = char.to_usize();
            match us {
                46 | 44 => /* . / ,*/ {
                    let f = catch!(read_float::<ET>(engine,us as u8,isnegative)
                            => next.source.cause
                        );
                    return read_muunit::<ET>(engine,f)
                },
                _ if is_ascii_digit(us) => {
                    let f = catch!(read_float::<ET>(engine,us as u8,isnegative)
                            => next.source.cause
                        );
                    return read_muunit::<ET>(engine,f)
                },
                _ => todo!("Non-digit in read_mudim")
            }
        }
        o => todo!("Non-char in read_mudim: {:?}",o)
    }
}

pub fn get_mustretchdim<ET:EngineType>(engine:&mut EngineRef<ET>) -> ET::MuStretchShrinkDim {
    debug_log!(trace=>"Reading mu stretch/shrink dimension {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace();
    let mut isnegative = false;
    while let Some(next) = engine.get_next_unexpandable_same_file() {
        match next.command {
            BaseCommand::Char{char,..} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(engine.skip_whitespace()=> next.source.cause);
                    }
                    43 => /* + */ catch!(engine.skip_whitespace()=> next.source.cause
                    ),
                    46 | 44 => /* . / ,*/ {
                        let f = catch!(read_float::<ET>(engine,us as u8,isnegative)
                            => next.source.cause
                        );
                        return read_mustretchunit::<ET>(engine,f)
                    },
                    _ if is_ascii_digit(us) => {
                        let f = catch!(read_float::<ET>(engine,us as u8,isnegative)
                            => next.source.cause
                        );
                        return read_mustretchunit::<ET>(engine,f)
                    },
                    _ => todo!("Non-digit in read_skipdim")
                }
            }
            _ => todo!("Non-char in read_skipdim")
        }
    }
    file_end!()
}

pub fn read_muunit<ET:EngineType>(engine:&mut EngineRef<ET>, float:f64) -> ET::MuDim {
    debug_log!(trace=>"Reading mu unit {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace();
    match engine.get_keywords(ET::MuDim::units()) {
        Some(dim) => ET::MuDim::from_float(dim,float),
        _ => todo!("Non-unit in read_unit")
    }
}

pub fn read_mustretchunit<ET:EngineType>(engine:&mut EngineRef<ET>, float:f64) -> ET::MuStretchShrinkDim {
    debug_log!(trace=>"Reading mu unit {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace();
    match get_keywords::<ET>(engine, ET::MuStretchShrinkDim::units()) {
        Some(dim) => ET::MuStretchShrinkDim::from_float(dim,float),
        _ => todo!("Non-unit in read_unit")
    }
}

pub fn read_float<ET:EngineType>(engine:&mut EngineRef<ET>, firstchar:u8, isnegative:bool) -> f64 {
    debug_log!(trace=>"Reading float {}...",(firstchar as char));
    let mut in_float = firstchar == b'.' || firstchar == b',';
    let mut rets = if in_float {vec!(b'0',b'.')} else {vec!(firstchar)};
    while let Some(next) = engine.get_next_unexpandable_same_file() {
        match next.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => break, // eat one space
            BaseCommand::Char{char,..} => {
                let us = char.to_usize();
                if is_ascii_digit(us) {
                    rets.push(us as u8);
                }
                else if !in_float && (us == 46 || us == 44) {
                    rets.push(b'.');
                    in_float = true;
                }
                else {
                    engine.mouth.requeue(next.source.cause);
                    break
                }
            }
            _ => {
                engine.mouth.requeue(next.source.cause);
                break
            }
        }
    }
    use std::str::FromStr;
    let f = f64::from_str(std::str::from_utf8(&rets).unwrap()).unwrap();
    debug_log!(trace=>"Returning {}",f);
    if isnegative {-f} else {f}
}

