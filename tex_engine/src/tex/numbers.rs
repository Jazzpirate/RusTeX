use std::fmt::Display;
use std::marker::PhantomData;
use std::ops::{Add, Div, Mul, Neg};
use crate::engine::state::fields::IsDefault;
use crate::tex::token::Token;
use crate::utils::errors::NumericalError;

pub trait NumSet:'static {
    type Int:Int;
    type Dim:Dim;
    type Glue:Glue;
    type MuGlue:MuGlue;
}

pub trait Int:Default+Display+Clone+IsDefault+PartialOrd+TryInto<usize>+From<u8>+
Neg<Output=Self>+Div<Self,Output=Self>+Mul<Self,Output=Self>+Add<Self,Output=Self> {
    fn from_i64<T:Token>(i:i64) -> Result<Self,NumericalError<T>>;
    fn to_i64(&self) -> i64;
}
pub trait Dim:Default+Display+Clone+IsDefault+Neg<Output=Self> {
    fn units() -> Vec<&'static str>;
    fn from_float(dim:&str,float:f64) -> Self;
}
pub trait Glue:Default {}
pub trait MuGlue:Default {}

pub struct DefaultNumSet;
impl NumSet for DefaultNumSet {
    type Int = i32;
    type Dim = Dimi32;
    type Glue = Gluei32;
    type MuGlue = MuGluei32;
}

impl Int for i32 {
    fn from_i64<T:Token>(i:i64) -> Result<Self,NumericalError<T>> {
        if i < i32::MIN as i64 || i > (i32::MAX as i64) {
            Err(NumericalError(format!("Integer overflow: {}",i),PhantomData))
        } else {
            Ok(i as i32)
        }
    }
    fn to_i64(&self) -> i64 { *self as i64 }
}
#[derive(Clone,Copy)]
pub struct Dimi32(i32);
impl Dimi32 {
    fn round(&self) -> f64 {
        let mut i = 1.0 as f64;
        loop {
            let rounded = (((self.0 as f64) / 65536.0) * i).round() / i;
            if ((rounded * 65536.0).round() as i32) == self.0 {
                return rounded
            } else {
                i = i * 10.0;
            }
        }
    }
    fn to_string(&self) -> String {
        let mut ret = format!("{:.5}",self.round());
        loop {
            if ret.ends_with("0") {
                ret.pop();
            } else if ret.ends_with(".") {
                return ret + "0"
            }
            else {
                return ret
            }
        }
    }
}
impl Default for Dimi32 {
    fn default() -> Self {
        Self(0)
    }
}
impl IsDefault for Dimi32 {
    fn is_default(&self) -> bool {
        self.0 == 0
    }
}
impl Display for Dimi32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}pt",self.to_string())
    }
}

impl Dim for Dimi32 {
    fn units() -> Vec<&'static str> {vec!["pt","pc","in","bp","cm","mm","dd","cc","sp"]}
    fn from_float(dim: &str, float: f64) -> Self { match dim {
        "sp" => Self(float.round() as i32),
        "pt" => Self((float*65536.0).round() as i32),
        "pc" => Self((float*65536.0*12.0).round() as i32),
        "in" => Self((float*65536.0*72.27).round() as i32),
        "bp" => Self((float*65536.0*72.27/72.0).round() as i32),
        "cm" => Self((float*65536.0*72.27/2.54).round() as i32),
        "mm" => Self((float*65536.0*72.27/25.4).round() as i32),
        "dd" => Self((float*65536.0*1238.0/1157.0).round() as i32),
        "cc" => Self((float*65536.0*14856.0/1157.0).round() as i32),
        _ => unreachable!("Invalid dimension unit")
    } }
}
impl Neg for Dimi32 {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

pub struct Gluei32;
impl Default for Gluei32 {
    fn default() -> Self {
        todo!()
    }
}
impl Glue for Gluei32 {}

pub struct MuGluei32;
impl Default for MuGluei32 {
    fn default() -> Self {
        todo!()
    }
}
impl MuGlue for MuGluei32 {}