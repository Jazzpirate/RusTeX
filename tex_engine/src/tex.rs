//! Basic TeX concepts, such as [`Token`](token::Token)s, [`CategoryCode`](catcodes::CategoryCode)s
//! and [`Command`](commands::Command)s.
pub mod catcodes;
pub mod token;
pub mod commands;
pub mod boxes;
pub mod numbers;
pub mod fonts;

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum ConditionalBranch { None(&'static str),True(&'static str),Else(&'static str),Case(i64,usize) }
