use std::marker::PhantomData;
use crate::{catch, debug_log, file_end};
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

pub fn get_int<T:Token,Gu:Gullet<T>>(gullet:&mut Gu, state:&mut Gu::S) -> Result<<<Gu::S as State<T>>::NumSet as NumSet>::Int,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading number {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;

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
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    34 if !ishex && !isoct => ishex = true, // "
                    39 if !ishex && !isoct => isoct = true, // '
                    96 if !ishex && !isoct => { // `
                        match catch!(gullet.mouth().get_next(state)
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
                                catch!(expand_until_space(gullet,state) => tk);
                                let us = c.to_usize() as i64;
                                return Ok(catch!(<<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64(us) => tk))
                            }
                        }
                    },
                    _ if is_ascii_hex_digit(us) && ishex =>
                    // TODO: texnically, this requires catcode 12 for 0-9 and catcode 11 or 12 for A-F
                        return read_hex_number(gullet,state,us as u8,isnegative),
                    _ if is_ascii_digit(us) && !isoct =>
                    // TODO: texnically, this requires catcode 12
                        return read_decimal_number(gullet,state,us as u8,isnegative),
                    _ if is_ascii_oct_digit(us) => // isoct == true
                    // TODO: texnically, this requires catcode 12
                        todo!("Octal digit in read_number"),
                    _ => {
                        let c = <<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64(char.to_usize() as i64)?;
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
                return Ok(<<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64(val.to_sp())?)
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
                        return Ok(<<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64(val.to_sp())?)
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
            StomachCommandInner::AssignableValue {name,tp:Assignable::Int} => {
                let val = state.get_primitive_int(name);
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return Ok(val)
            }
            StomachCommandInner::Char{char,..} => {
                let c = <<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64(char.to_usize() as i64)?;
                debug_log!(trace=>"Returning {}",c);
                return Ok(c)
            }
            StomachCommandInner::MathChar(u) => {
                let c = <<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64(u as i64)?;
                debug_log!(trace=>"Returning {}",c);
                return Ok(c)
            }
            o => todo!("Non-char in read_number: {:?} at {}",o,gullet.mouth().file_line())
        }
    }
    file_end!()
}

pub fn expand_until_space<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S) -> Result<(),Box<dyn TeXError<T>>> {
    match gullet.get_next_stomach_command(state)? {
        Some(StomachCommand{cmd:StomachCommandInner::Space,..}) => Ok(()), // eat the space
        Some(o) => {
            gullet.mouth().requeue(o.cause);
            Ok(())
        }
        None => Ok(())
    }
}

pub fn read_decimal_number<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,firstchar:u8,isnegative:bool) -> Result<NS::Int,Box<dyn TeXError<T>>> {
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
    debug_log!(trace=>"Returning {}",i);
    Ok(NS::Int::from_i64(if isnegative { -i } else { i })?)
}

pub fn read_hex_number<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,firstchar:u8,isnegative:bool) -> Result<NS::Int,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading decimal number {}...",(firstchar as char));
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
    let i = i64::from_str_radix(std::str::from_utf8(&rets).unwrap(),16).unwrap();
    debug_log!(trace=>"Returning {}",i);
    Ok(NS::Int::from_i64(if isnegative { -i } else { i })?)
}


pub fn get_dim<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<NS::Dim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading dimension {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    _ => return get_dim_inner(gullet,state,isnegative,StomachCommandInner::Char{char,from_chardef:false},next.cause)
                }
            }
            o => return get_dim_inner(gullet,state,isnegative,o,next.cause)
        }
    }
    file_end!()
}
pub fn get_dim_inner<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S,isnegative:bool,next:StomachCommandInner<T::Char>,cause:T) -> Result<NS::Dim,Box<dyn TeXError<T>>> {
    match next {
        StomachCommandInner::Char{char,from_chardef:false} => {
            let us = char.to_usize();
            match us {
                46 | 44 => /* . / ,*/ {
                    let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_unit(gullet,state,f)
                },
                _ if is_ascii_digit(us) => {
                    let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_unit(gullet,state,f)
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
        StomachCommandInner::ValueRegister(i,Assignable::Dim) => {
            let val = state.get_dim_register(i);
            return Ok(if isnegative { -val } else { val })
        }
        StomachCommandInner::ValueRegister(i,Assignable::Int) => {
            let val = state.get_int_register(i).to_i64() as f64;
            let val = if isnegative { -val } else { val };
            return read_unit(gullet,state,val)
        }
        StomachCommandInner::Char{char,..} => {
            let val = char.to_usize() as f64;
            let val = if isnegative { -val } else { val };
            return read_unit(gullet,state,val)
        }
        o => todo!("Non-char in read_dim: {:?}\n{}\n at {}",o,gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line())
    }
}

pub fn read_unit<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,float:f64) -> Result<NS::Dim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    match gullet.get_next_stomach_command(state)? {
        Some(cmd) => {
            match cmd.cmd {
                StomachCommandInner::Char { .. } => {
                    gullet.mouth().requeue(cmd.cause);
                    gullet.mouth().skip_whitespace(state)?;
                    if get_keyword(gullet, state, "true")? {
                        gullet.mouth().skip_whitespace(state)?;
                        let mag = state.get_primitive_int("mag").to_i64() as f64 / 1000.0;
                        match get_keywords(gullet, state, NS::Dim::units())? {
                            Some(dim) => Ok(NS::Dim::from_float(dim, float * mag)),
                            _ => Err(ExpectedUnit(PhantomData).into())
                        }
                    } else {
                        match get_keywords(gullet, state, NS::Dim::units())? {
                            Some(dim) => Ok(NS::Dim::from_float(dim, float)),
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

pub fn get_skip<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<Skip<NS::SkipDim>,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading skip {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    _ => return get_skip_inner(gullet,state,isnegative,StomachCommandInner::Char{char,from_chardef:false},next.cause)
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
            _ => return get_skip_inner(gullet,state,isnegative,next.cmd,next.cause)
        }
    }
    file_end!()
}

fn get_skip_inner<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S,isnegative:bool,next:StomachCommandInner<T::Char>,cause:T) -> Result<Skip<NS::SkipDim>,Box<dyn TeXError<T>>> {
    let base = get_dim_inner(gullet,state,isnegative,next,cause)?;
    gullet.mouth().skip_whitespace(state)?;
    let stretch = if gullet.get_keyword(state,"plus")? {
        Some(get_skipdim(gullet,state)?)
    } else {None};
    gullet.mouth().skip_whitespace(state)?;
    let shrink = if gullet.get_keyword(state,"minus")? {
        Some(get_skipdim(gullet,state)?)
    } else {None};
    Ok(Skip{base,stretch,shrink})
}

pub fn get_skipdim<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<NS::SkipDim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading dimension {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    46 | 44 => /* . / ,*/ {
                        let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_skip_unit(gullet,state,f)
                    },
                    _ if is_ascii_digit(us) => {
                        let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_skip_unit(gullet,state,f)
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
                        return Ok(NS::SkipDim::from_dim(ret))
                    }
                }
            }
            _ => todo!("Non-char in read_skipdim")
        }
    }
    file_end!()
}

pub fn read_skip_unit<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,float:f64) -> Result<NS::SkipDim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading skip unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    if get_keyword(gullet, state, "true")? {
        gullet.mouth().skip_whitespace(state)?;
        let mag = state.get_primitive_int("mag").to_i64() as f64 / 1000.0;
        match get_keywords(gullet, state, NS::Dim::units())? {
            Some(dim) => Ok(NS::SkipDim::from_float(dim,float * mag)),
            _ => Err(ExpectedUnit(PhantomData).into())
        }
    } else {
        match get_keywords(gullet, state, NS::SkipDim::units())? {
            Some(dim) => Ok(NS::SkipDim::from_float(dim,float)),
            _ => todo!("Non-unit in read_skip_unit: {}",gullet.mouth().preview(50).replace("\n","\\n"))
        }
    }
}

pub fn get_muskip<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<MuSkip<NS::MuDim,NS::MuStretchShrinkDim>,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading muskip {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    _ => return get_muskip_inner(gullet,state,isnegative,StomachCommandInner::Char{char,from_chardef:false},next.cause)
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
            _ => return get_muskip_inner(gullet,state,isnegative,next.cmd,next.cause)
        }
    }
    file_end!()
}

fn get_muskip_inner<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S,isnegative:bool,next:StomachCommandInner<T::Char>,cause:T) -> Result<MuSkip<NS::MuDim,NS::MuStretchShrinkDim>,Box<dyn TeXError<T>>> {
    let base = get_mudim(gullet, state, isnegative, next, cause)?;
    gullet.mouth().skip_whitespace(state);
    let stretch = if gullet.get_keyword(state,"plus")? {
        Some(get_mustretchdim(gullet,state)?)
    } else {None};
    gullet.mouth().skip_whitespace(state);
    let shrink = if gullet.get_keyword(state,"minus")? {
        Some(get_mustretchdim(gullet,state)?)
    } else {None};
    Ok(MuSkip{base,stretch,shrink})
}

pub fn get_mudim<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S, isnegative:bool, next:StomachCommandInner<T::Char>, cause:T) -> Result<NS::MuDim,Box<dyn TeXError<T>>> {
    match next {
        StomachCommandInner::Char{char,from_chardef:false} => {
            let us = char.to_usize();
            match us {
                46 | 44 => /* . / ,*/ {
                    let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_muunit(gullet,state,f)
                },
                _ if is_ascii_digit(us) => {
                    let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_muunit(gullet,state,f)
                },
                _ => todo!("Non-digit in read_mudim")
            }
        }
        o => todo!("Non-char in read_mudim: {:?}",o)
    }
}

pub fn get_mustretchdim<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<NS::MuStretchShrinkDim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading mu stretch/shrink dimension {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
                let us = char.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    46 | 44 => /* . / ,*/ {
                        let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_mustretchunit(gullet,state,f)
                    },
                    _ if is_ascii_digit(us) => {
                        let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_mustretchunit(gullet,state,f)
                    },
                    _ => todo!("Non-digit in read_skipdim")
                }
            }
            _ => todo!("Non-char in read_skipdim")
        }
    }
    file_end!()
}

pub fn read_muunit<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,float:f64) -> Result<NS::MuDim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading mu unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    match get_keywords(gullet, state, NS::MuDim::units())? {
        Some(dim) => Ok(NS::MuDim::from_float(dim,float)),
        _ => todo!("Non-unit in read_unit")
    }
}

pub fn read_mustretchunit<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,float:f64) -> Result<NS::MuStretchShrinkDim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading mu unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    match get_keywords(gullet, state, NS::MuStretchShrinkDim::units())? {
        Some(dim) => Ok(NS::MuStretchShrinkDim::from_float(dim,float)),
        _ => todo!("Non-unit in read_unit")
    }
}

pub fn read_float<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,firstchar:u8,isnegative:bool) -> Result<f64,Box<dyn TeXError<T>>> {
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

