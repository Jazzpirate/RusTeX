use std::marker::PhantomData;
use crate::{catch, debug_log, file_end};
use crate::engine::EngineType;
use crate::engine::gullet::Gullet;
use crate::engine::mouth::Mouth;
use crate::engine::gullet::methods::{get_keyword, get_keywords};
use crate::engine::state::State;
use crate::tex::catcodes::CategoryCode;
use crate::utils::strings::CharType;
use crate::tex::commands::{Assignable, GulletCommand, StomachCommand, StomachCommandInner};
use crate::tex::numbers::{MuSkip, NumSet, Skip, Int, Dim, SkipDim, MuDim, MuStretchShrinkDim, Numeric};
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{ExpectedInteger, ExpectedUnit, ImplementationError, TeXError};

fn is_ascii_digit(u:usize) -> bool {
    u >= 48 && u <= 57
}
fn is_ascii_oct_digit(u:usize) -> bool {
    u >= 48 && u <= 55
}
fn is_ascii_hex_digit(u:usize) -> bool {
    is_ascii_digit(u) || (u >= 65 && u <= 70) || (u >= 97 && u <= 102)
}

pub fn get_int<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State) -> Result<ET::Int,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading number {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace::<ET>(state)?;

    let mut isnegative = false;
    let mut ishex = false;
    let mut isoct = false;

    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace::<ET>(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace::<ET>(state)
                        => next.cause
                    ),
                    34 if !ishex && !isoct => ishex = true, // "
                    39 if !ishex && !isoct => isoct = true, // '
                    96 if !ishex && !isoct => { // `
                        match catch!(gullet.mouth().get_next::<ET>(state)
                            => next.cause
                        ) {
                            None => file_end!(next.cause),
                            Some((tk,_)) => {
                                let c = match tk.base() {
                                    BaseToken::Char(c,_) => *c,
                                    BaseToken::CS(str) if str.len() == 1 =>
                                        unsafe{ *str.as_vec().first().unwrap_unchecked() }
                                    _ => return Err(ExpectedInteger(tk,PhantomData).into())
                                };
                                catch!(expand_until_space::<ET>(gullet,state) => tk);
                                let us = c.to_usize() as i64;
                                return Ok(catch!(ET::Int::from_i64(us) => tk))
                            }
                        }
                    },
                    _ if is_ascii_hex_digit(us) && ishex =>
                    // TODO: texnically, this requires catcode 12 for 0-9 and catcode 11 or 12 for A-F
                        return read_hex_number::<ET>(gullet,state,us as u8,isnegative),
                    _ if is_ascii_digit(us) && !isoct =>
                    // TODO: texnically, this requires catcode 12
                        return read_decimal_number::<ET>(gullet,state,us as u8,isnegative),
                    _ if is_ascii_oct_digit(us) => // isoct == true
                    // TODO: texnically, this requires catcode 12
                        todo!("Octal digit in read_number"),
                    _ => {
                        let c = ET::Int::from_i64(char.to_usize() as i64)?;
                        debug_log!(trace=>"Returning {}",c);
                        return Ok(c)
                    }
                }
            }
            StomachCommandInner::ValueRegister(index,Assignable::Int) => {
                let val = state.get_int_register(index);
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return Ok(val)
            }
            StomachCommandInner::ValueRegister(index,Assignable::Dim) => {
                let val = state.get_dim_register(index);
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return Ok(ET::Int::from_i64(val.to_sp())?)
            }
            StomachCommandInner::Value {index,tp:Assignable::Int,..} => {
                match gullet.primitive_int(index) {
                    None => return Err(ImplementationError("Missing implementation for primitive int".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        let val = if isnegative { -val } else { val };
                        debug_log!(trace=>"Returning {}",val);
                        return Ok(val)
                    }
                }
            }
            StomachCommandInner::Value {index,tp:Assignable::Dim,..} => {
                match gullet.primitive_dim(index) {
                    None => return Err(ImplementationError("Missing implementation for primitive dim".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        let val = if isnegative { -val } else { val };
                        debug_log!(trace=>"Returning {}",val);
                        return Ok(ET::Int::from_i64(val.to_sp())?)
                    }
                }
            }
            StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Int,..} => {
                match gullet.primitive_int(value_index) {
                    None => return Err(ImplementationError("Missing implementation for primitive int".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        let val = if isnegative { -val } else { val };
                        debug_log!(trace=>"Returning {}",val);
                        return Ok(val)
                    }
                }
            }
            StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Dim,..} => {
                match gullet.primitive_dim(value_index) {
                    None => return Err(ImplementationError("Missing implementation for primitive int".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        let val = if isnegative { -val } else { val };
                        debug_log!(trace=>"Returning {}",val.to_sp());
                        return Ok( ET::Int::from_i64(val.to_sp())?)
                    }
                }
            }
            StomachCommandInner::AssignableValue {name,tp:Assignable::Int} => {
                let val = state.get_primitive_int(name);
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return Ok(val)
            }
            StomachCommandInner::Char{char,..} => {
                let c = ET::Int::from_i64(char.to_usize() as i64)?;
                debug_log!(trace=>"Returning {}",c);
                return Ok(c)
            }
            StomachCommandInner::MathChar(u) => {
                let c = ET::Int::from_i64(u as i64)?;
                debug_log!(trace=>"Returning {}",c);
                return Ok(c)
            }
            o => todo!("Non-char in read_number: {:?} at {}",o,gullet.mouth().file_line())
        }
    }
    file_end!()
}

pub fn expand_until_space<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State) -> Result<(),Box<dyn TeXError<ET::Token>>> {
    match gullet.get_next_stomach_command(state)? {
        Some(StomachCommand{cmd:StomachCommandInner::Space,..}) => Ok(()), // eat the space
        Some(o) => {
            gullet.mouth().requeue(o.cause);
            Ok(())
        }
        None => Ok(())
    }
}

pub fn read_decimal_number<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,firstchar:u8,isnegative:bool) -> Result<ET::Int,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading decimal number {}...",(firstchar as char));
    let mut rets = vec!(firstchar);

    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                if is_ascii_digit(us) {
                    rets.push(us as u8);
                } else {
                    gullet.mouth().requeue(next.cause);
                    break
                }
            }
            StomachCommandInner::Space => break, // eat one space
            _ => {
                gullet.mouth().requeue(next.cause);
                break
            }
        }
    }
    use std::str::FromStr;
    let i = i64::from_str(std::str::from_utf8(&rets).unwrap()).unwrap();
    debug_log!(trace=>"Returning {}",if isnegative { -i } else { i });
    Ok(ET::Int::from_i64(if isnegative { -i } else { i })?)
}

pub fn read_hex_number<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,firstchar:u8,isnegative:bool) -> Result<ET::Int,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading hexadecimal number {}...",(firstchar as char));
    let mut rets = vec!(firstchar);

    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                if is_ascii_hex_digit(us) {
                    rets.push(us as u8);
                } else {
                    gullet.mouth().requeue(next.cause);
                    break
                }
            }
            StomachCommandInner::Space => break, // eat one space
            _ => {
                gullet.mouth().requeue(next.cause);
                break
            }
        }
    }
    let str = std::str::from_utf8(&rets).unwrap();
    let i = i64::from_str_radix(str,16).unwrap();
    debug_log!(trace=>"Returning {}",i);
    Ok(ET::Int::from_i64(if isnegative { -i } else { i })?)
}


pub fn get_dim<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State) -> Result<ET::Dim,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading dimension {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace::<ET>(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace::<ET>(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace::<ET>(state)
                        => next.cause
                    ),
                    _ => return get_dim_inner::<ET>(gullet,state,isnegative,StomachCommandInner::Char{char,from_chardef:false},next.cause)
                }
            }
            o => return get_dim_inner::<ET>(gullet,state,isnegative,o,next.cause)
        }
    }
    file_end!()
}
pub fn get_dim_inner<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State,isnegative:bool,next:StomachCommandInner<ET::Char>,cause:ET::Token)
    -> Result<ET::Dim,Box<dyn TeXError<ET::Token>>> {
    match next {
        StomachCommandInner::Char{char,from_chardef:false} => {
            let us = char.to_usize();
            match us {
                46 | 44 => /* . / ,*/ {
                    let f = catch!(read_float::<ET>(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_unit::<ET>(gullet,state,f)
                },
                _ if is_ascii_digit(us) => {
                    let f = catch!(read_float::<ET>(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_unit::<ET>(gullet,state,f)
                },
                _ => todo!("Non-digit in read_dimension: {}/{}\nat: {}",char,us,gullet.mouth().file_line())
            }
        }
        StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Dim,..} => {
            match gullet.primitive_dim(value_index) {
                None => return Err(ImplementationError("Missing implementation for primitive dim".to_string(),PhantomData).into()),
                Some(f) => {
                    let val = f(state,gullet,GulletCommand{cause})?;
                    return Ok(if isnegative { -val } else { val })
                }
            }
        }
        StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Int,..} => {
            match gullet.primitive_int(value_index) {
                None => return Err(ImplementationError("Missing implementation for primitive int".to_string(),PhantomData).into()),
                Some(f) => {
                    let val = f(state,gullet,GulletCommand{cause})?.to_i64() as f64;
                    let val = if isnegative { -val } else { val };
                    return read_unit::<ET>(gullet,state,val)
                }
            }
        }
        StomachCommandInner::ValueRegister(i,Assignable::Dim) => {
            let val = state.get_dim_register(i);
            return Ok(if isnegative { -val } else { val })
        }
        StomachCommandInner::ValueRegister(i,Assignable::Int) => {
            let val = state.get_int_register(i).to_i64() as f64;
            let val = if isnegative { -val } else { val };
            return read_unit::<ET>(gullet,state,val)
        }
        StomachCommandInner::Value {index,tp:Assignable::Dim,..} => {
            match gullet.primitive_dim(index) {
                None => return Err(ImplementationError("Missing implementation for primitive dim".to_string(),PhantomData).into()),
                Some(f) => {
                    let val = f(state,gullet,GulletCommand{cause})?;
                    return Ok(if isnegative { -val } else { val })
                }
            }
        }
        StomachCommandInner::Char{char,..} => {
            let val = char.to_usize() as f64;
            let val = if isnegative { -val } else { val };
            return read_unit::<ET>(gullet,state,val)
        }
        o => todo!("Non-char in read_dim: {:?}\n{}\n at {}",o,gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line())
    }
}

pub fn read_unit<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,float:f64) -> Result<ET::Dim,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    match gullet.get_next_stomach_command(state)? {
        Some(cmd) => {
            match cmd.cmd {
                StomachCommandInner::Char { .. } => {
                    gullet.mouth().requeue(cmd.cause);
                    gullet.mouth().skip_whitespace::<ET>(state)?;
                    if get_keyword::<ET>(gullet, state, "true")? {
                        gullet.mouth().skip_whitespace::<ET>(state)?;
                        let mag = state.get_primitive_int("mag").to_i64() as f64 / 1000.0;
                        match get_keywords::<ET>(gullet, state, ET::Dim::units())? {
                            Some(dim) => Ok(ET::Dim::from_float(dim, float * mag)),
                            _ => Err(ExpectedUnit(PhantomData).into())
                        }
                    } else {
                        match get_keywords::<ET>(gullet, state, ET::Dim::units())? {
                            Some(dim) => Ok(ET::Dim::from_float(dim, float)),
                            _ => todo!("Non-unit in read_unit: {}\n at {}", gullet.mouth().preview(50).replace("\n", "\\n"), gullet.mouth().file_line())
                        }
                    }
                }
                StomachCommandInner::ValueRegister(i,Assignable::Dim) => {
                    let val = state.get_dim_register(i);
                    Ok(val.tex_mult(float))
                }
                _ => todo!("Non-unit in read_unit: {}\n at {}", gullet.mouth().preview(50).replace("\n", "\\n"), gullet.mouth().file_line())
            }
        }
        None => file_end!(),
    }
}

pub fn get_skip<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State) -> Result<Skip<ET::SkipDim>,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading skip {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace::<ET>(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace::<ET>(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace::<ET>(state)
                        => next.cause
                    ),
                    _ => return get_skip_inner::<ET>(gullet,state,isnegative,StomachCommandInner::Char{char,from_chardef:false},next.cause)
                }
            }
            StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Skip,..} => {
                match gullet.primitive_skip(value_index) {
                    None => return Err(ImplementationError("Missing implementation for primitive skip".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        return Ok(if isnegative { -val } else { val })
                    }
                }
            }
            _ => return get_skip_inner::<ET>(gullet,state,isnegative,next.cmd,next.cause)
        }
    }
    file_end!()
}

fn get_skip_inner<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State,isnegative:bool,next:StomachCommandInner<ET::Char>,cause:ET::Token)
    -> Result<Skip<ET::SkipDim>,Box<dyn TeXError<ET::Token>>> {
    let base = get_dim_inner::<ET>(gullet,state,isnegative,next,cause)?;
    gullet.mouth().skip_whitespace::<ET>(state)?;
    let stretch = if gullet.get_keyword(state,"plus")? {
        Some(get_skipdim::<ET>(gullet,state)?)
    } else {None};
    gullet.mouth().skip_whitespace::<ET>(state)?;
    let shrink = if gullet.get_keyword(state,"minus")? {
        Some(get_skipdim::<ET>(gullet,state)?)
    } else {None};
    Ok(Skip{base,stretch,shrink})
}

pub fn get_skipdim<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State) -> Result<ET::SkipDim,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading dimension {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace::<ET>(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace::<ET>(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace::<ET>(state)
                        => next.cause
                    ),
                    46 | 44 => /* . / ,*/ {
                        let f = catch!(read_float::<ET>(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_skip_unit::<ET>(gullet,state,f)
                    },
                    _ if is_ascii_digit(us) => {
                        let f = catch!(read_float::<ET>(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_skip_unit::<ET>(gullet,state,f)
                    },
                    _ => todo!("Non-digit in read_skipdim")
                }
            }
            StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Dim,..} => {
                match gullet.primitive_dim(value_index) {
                    None => return Err(ImplementationError("Missing implementation for primitive dim".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        let ret = if isnegative { -val } else { val };
                        return Ok(ET::SkipDim::from_dim(ret))
                    }
                }
            }
            _ => todo!("Non-char in read_skipdim")
        }
    }
    file_end!()
}

pub fn read_skip_unit<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,float:f64)
    -> Result<ET::SkipDim,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading skip unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace::<ET>(state)?;
    if get_keyword::<ET>(gullet, state, "true")? {
        gullet.mouth().skip_whitespace::<ET>(state)?;
        let mag = state.get_primitive_int("mag").to_i64() as f64 / 1000.0;
        match get_keywords::<ET>(gullet, state, ET::Dim::units())? {
            Some(dim) => Ok(ET::SkipDim::from_float(dim,float * mag)),
            _ => Err(ExpectedUnit(PhantomData).into())
        }
    } else {
        match get_keywords::<ET>(gullet, state, ET::SkipDim::units())? {
            Some(dim) => Ok(ET::SkipDim::from_float(dim,float)),
            _ => todo!("Non-unit in read_skip_unit: {}",gullet.mouth().preview(50).replace("\n","\\n"))
        }
    }
}

pub fn get_muskip<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State)
    -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading muskip {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace::<ET>(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace::<ET>(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace::<ET>(state)
                        => next.cause
                    ),
                    _ => return get_muskip_inner::<ET>(gullet,state,isnegative,StomachCommandInner::Char{char,from_chardef:false},next.cause)
                }
            }
            StomachCommandInner::ValueAssignment {value_index,tp:Assignable::MuSkip,..} => {
                match gullet.primitive_muskip(value_index) {
                    None => return Err(ImplementationError("Missing implementation for primitive muskip".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        return Ok(if isnegative { -val } else { val })
                    }
                }
            }
            _ => return get_muskip_inner::<ET>(gullet,state,isnegative,next.cmd,next.cause)
        }
    }
    file_end!()
}

fn get_muskip_inner<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State,isnegative:bool,next:StomachCommandInner<ET::Char>,cause:ET::Token)
    -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,Box<dyn TeXError<ET::Token>>> {
    let base = get_mudim::<ET>(gullet, state, isnegative, next, cause)?;
    gullet.mouth().skip_whitespace::<ET>(state);
    let stretch = if gullet.get_keyword(state,"plus")? {
        Some(get_mustretchdim::<ET>(gullet,state)?)
    } else {None};
    gullet.mouth().skip_whitespace::<ET>(state);
    let shrink = if gullet.get_keyword(state,"minus")? {
        Some(get_mustretchdim::<ET>(gullet,state)?)
    } else {None};
    Ok(MuSkip{base,stretch,shrink})
}

pub fn get_mudim<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State, isnegative:bool, next:StomachCommandInner<ET::Char>, cause:ET::Token)
    -> Result<ET::MuDim,Box<dyn TeXError<ET::Token>>> {
    match next {
        StomachCommandInner::Char{char,from_chardef:false} => {
            let us = char.to_usize();
            match us {
                46 | 44 => /* . / ,*/ {
                    let f = catch!(read_float::<ET>(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_muunit::<ET>(gullet,state,f)
                },
                _ if is_ascii_digit(us) => {
                    let f = catch!(read_float::<ET>(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_muunit::<ET>(gullet,state,f)
                },
                _ => todo!("Non-digit in read_mudim")
            }
        }
        o => todo!("Non-char in read_mudim: {:?}",o)
    }
}

pub fn get_mustretchdim<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State) -> Result<ET::MuStretchShrinkDim,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading mu stretch/shrink dimension {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace::<ET>(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace::<ET>(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace::<ET>(state)
                        => next.cause
                    ),
                    46 | 44 => /* . / ,*/ {
                        let f = catch!(read_float::<ET>(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_mustretchunit::<ET>(gullet,state,f)
                    },
                    _ if is_ascii_digit(us) => {
                        let f = catch!(read_float::<ET>(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_mustretchunit::<ET>(gullet,state,f)
                    },
                    _ => todo!("Non-digit in read_skipdim")
                }
            }
            _ => todo!("Non-char in read_skipdim")
        }
    }
    file_end!()
}

pub fn read_muunit<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,float:f64) -> Result<ET::MuDim,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading mu unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace::<ET>(state)?;
    match get_keywords::<ET>(gullet, state, ET::MuDim::units())? {
        Some(dim) => Ok(ET::MuDim::from_float(dim,float)),
        _ => todo!("Non-unit in read_unit")
    }
}

pub fn read_mustretchunit<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,float:f64)
    -> Result<ET::MuStretchShrinkDim,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading mu unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace::<ET>(state)?;
    match get_keywords::<ET>(gullet, state, ET::MuStretchShrinkDim::units())? {
        Some(dim) => Ok(ET::MuStretchShrinkDim::from_float(dim,float)),
        _ => todo!("Non-unit in read_unit")
    }
}

pub fn read_float<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,firstchar:u8,isnegative:bool) -> Result<f64,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading float {}...",(firstchar as char));
    let mut in_float = firstchar == b'.' || firstchar == b',';
    let mut rets = if in_float {vec!(b'0',b'.')} else {vec!(firstchar)};
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                if is_ascii_digit(us) {
                    rets.push(us as u8);
                }
                else if !in_float && (us == 46 || us == 44) {
                    rets.push(b'.');
                    in_float = true;
                }
                else {
                    gullet.mouth().requeue(next.cause.clone());
                    break
                }
            }
            StomachCommandInner::Space => break, // eat one space
            _ => {
                gullet.mouth().requeue(next.cause.clone());
                break
            }
        }
    }
    use std::str::FromStr;
    let f = f64::from_str(std::str::from_utf8(&rets).unwrap()).unwrap();
    debug_log!(trace=>"Returning {}",f);
    Ok(if isnegative {-f} else {f})
}
