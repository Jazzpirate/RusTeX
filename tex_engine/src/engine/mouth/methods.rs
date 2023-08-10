//! Default implementations for [`Mouth`] methods

use crate::engine::mouth::Mouth;
use crate::{debug_log, file_end, throw};
use crate::engine::EngineType;
use crate::engine::state::State;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::TokenCont;
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;
