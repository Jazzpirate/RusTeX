//! Default implementations for [`Mouth`] methods

use crate::engine::mouth::Mouth;
use crate::{debug_log, file_end};
use crate::engine::EngineType;
use crate::engine::state::State;
use crate::tex::catcodes::CategoryCode;
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{InvalidCharacter, OtherError, TeXError};
use crate::utils::strings::CharType;

/// Default implementation for [`Mouth::skip_whitespace`]
pub fn skip_whitespace<ET:EngineType>(mouth:&mut ET::Mouth, state:&ET::State)
                                -> Result<(),InvalidCharacter<ET::Token>> {
    debug_log!(trace=>"skipping whitespace");
    while let Some((tk,_)) = mouth.get_next::<ET>(state)? {
        match tk.catcode() {
            CategoryCode::Space => (),
            _ => {
                mouth.requeue(tk);
                break
            }
        }
    }
    Ok(())
}

/// Default implementation for [`Mouth::skip_eq_char`]
pub fn skip_eq_char<ET:EngineType>(mouth:&mut ET::Mouth,state:&ET::State) -> Result<(),InvalidCharacter<ET::Token>> {
    mouth.skip_whitespace::<ET>(state)?;
    debug_log!(trace=>"skipping '='");
    if let Some((tk,_)) = mouth.get_next::<ET>(state)? {
        match tk.base() {
            BaseToken::Char(c,_) if c.to_usize() == 61 => {
                match mouth.get_next::<ET>(state)? {
                    Some((tk,_)) if tk.catcode() == CategoryCode::Space => (),
                    Some((tk,_)) => mouth.requeue(tk),
                    _ => ()
                }
            },
            _ => mouth.requeue(tk)
        }
    }
    Ok(())
}

/// Default implementation for [`Mouth::read_argument`]
pub fn read_argument<ET:EngineType>(mouth:&mut ET::Mouth, state:&ET::State) -> Result<Vec<ET::Token>,Box<dyn TeXError<ET::Token>>> {
    match mouth.get_next::<ET>(state)? {
        None => file_end!(),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => read_until_endgroup::<ET>(mouth,state),
        Some((o,_)) => Ok(vec!(o))
    }
}

pub fn read_argument_nopar<ET:EngineType>(mouth:&mut ET::Mouth, state:&ET::State)
    -> Result<Vec<ET::Token>,Box<dyn TeXError<ET::Token>>> {
    match mouth.get_next::<ET>(state)? {
        None => file_end!(),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => {
            let mut depth = 1;
            let mut tks = vec!();
            while let Some((tk,_)) = mouth.get_next::<ET>(state)? {
                match tk.base() {
                    BaseToken::Char(_,CategoryCode::BeginGroup) => depth += 1,
                    BaseToken::Char(_,CategoryCode::EndGroup) => {
                        depth -= 1;
                        if depth == 0 { return Ok(tks) }
                    },
                    BaseToken::CS(n) if *n == ET::Char::par_token() =>
                        return Err(OtherError{msg:format!("Paragraph ended while reading argument"),cause:Some(t),source:None}.into()),
                    _ => ()
                }
                tks.push(tk);
            }
            file_end!()
        }
        Some((t,_)) if t.catcode() == CategoryCode::EOF => file_end!(),
        Some((o,_)) => Ok(vec!(o))
    }
}

/// Default implementation for [`Mouth::read_until_endgroup`]
pub fn read_until_endgroup<ET:EngineType>(mouth: &mut ET::Mouth, state:&ET::State) -> Result<Vec<ET::Token>,Box<dyn TeXError<ET::Token>>> {
    let mut depth = 1;
    let mut tks = vec!();
    while let Some((tk,_)) = mouth.get_next::<ET>(state)? {
        match tk.catcode() {
            CategoryCode::BeginGroup => depth += 1,
            CategoryCode::EndGroup => {
                depth -= 1;
                if depth == 0 { return Ok(tks) }
            },
            CategoryCode::EOF => file_end!(),
            _ => ()
        }
        tks.push(tk);
    }
    file_end!()
}