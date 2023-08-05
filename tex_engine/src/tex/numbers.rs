use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::ops::{Add, Div, Mul, Neg, Sub};
use crate::engine::state::fields::IsDefault;
use crate::tex::token::Token;
use crate::throw;
use crate::utils::errors::TeXError;

pub trait Numeric:Default+Display+Copy + Clone+IsDefault+Neg<Output=Self>+Add<Self,Output=Self>+Sub<Self,Output=Self>{
    fn scale(&self,times:i64,div:i64) -> Self;
}


#[derive(Clone,Copy,Debug)]
pub struct Frac(pub(crate) i64, pub(crate) i64);
impl Frac {
    pub fn new(n:i64,d:i64) -> Self {
        if d < 0 {return Frac::new(-n,-d)}
        if n % d == 0 {return Frac(n/d,1)}
        Frac(n,d)
    }
    pub fn inv(self) -> Self {
        Frac::new(self.1,self.0)
    }
}
impl Default for Frac {
    fn default() -> Self {
        Frac(0,1)
    }
}
impl IsDefault for Frac {
    fn is_default(&self) -> bool {
        self.0 == 0 && self.1 == 1
    }
}
impl Neg for Frac {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Frac(-self.0,self.1)
    }
}
impl Display for Frac {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.1 == 1 {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{}/{}", self.0, self.1)
        }
    }
}
impl Add for Frac {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Frac::new(self.0 * rhs.1 + rhs.0 * self.1, self.1 * rhs.1)
    }
}
impl Sub for Frac {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Frac::new(self.0 * rhs.1 - rhs.0 * self.1, self.1 * rhs.1)
    }
}

impl Numeric for Frac {
    fn scale(&self, times: i64, div: i64) -> Self {
        Frac::new(self.0 * times,self.1 * div)
    }
}

pub trait Int:Numeric +PartialOrd+TryInto<usize>+From<u8>+
Div<Self,Output=Self>+Mul<Self,Output=Self> {
    fn from_i64<T:Token>(i:i64) -> Result<Self,TeXError<T>>;
    fn to_i64(&self) -> i64;
}
pub trait Dim:Numeric+Add<Self,Output=Self>{
    fn units() -> Vec<&'static str>;
    fn from_float(dim:&str,float:f64) -> Self;
    fn from_sp(sp:i64) -> Self;
    fn to_sp(&self) -> i64;
    fn tex_mult(&self,other:f64) -> Self;
    fn tex_div(&self,other:i64) -> Self;
}
pub trait SkipDim:Display+Copy + Clone {
    type Dim:Dim;
    fn units() -> Vec<&'static str>;
    fn from_dim(dim:Self::Dim) -> Self;
    fn from_float(dim:&str,float:f64) -> Self;
}
pub trait MuDim:Numeric {
    fn units() -> Vec<&'static str>;
    fn from_float(dim:&str,float:f64) -> Self;
    fn tex_mult(&self,other:f64) -> Self;
    fn tex_div(&self,other:i64) -> Self;
}
pub trait MuStretchShrinkDim:Display+Copy + Clone {
    fn units() -> Vec<&'static str>;
    fn from_float(dim:&str,float:f64) -> Self;
}
impl Numeric for i32 {
    fn scale(&self, times: i64, div: i64) -> Self {
        ((*self as i64 * times) as f64 / (div as f64)).round() as i32
    }
}
impl Int for i32 {
    fn from_i64<T:Token>(i:i64) -> Result<Self,TeXError<T>> {
        if i < i32::MIN as i64 || i > (i32::MAX as i64) {
            throw!("Integer overflow: {}",i)
        } else {
            Ok(i as i32)
        }
    }
    fn to_i64(&self) -> i64 { *self as i64 }
}
#[derive(Clone,Copy)]
pub struct Dimi32(pub i32);
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

impl Add<Self> for Dimi32 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Dimi32(self.0 + rhs.0)
    }
}

impl Mul<i64> for Dimi32 {
    type Output = Self;

    fn mul(self, rhs: i64) -> Self::Output {
        Dimi32(self.0 * (rhs as i32))
    }
}
impl Div<i64> for Dimi32 {
    type Output = Self;

    fn div(self, rhs: i64) -> Self::Output {
        Dimi32(self.0 / (rhs as i32))
    }
}
impl Sub<Dimi32> for Dimi32 {
    type Output = Self;

    fn sub(self, rhs: Dimi32) -> Self::Output {
        Dimi32(self.0 - rhs.0)
    }
}

impl Numeric for Dimi32 {
    fn scale(&self, times: i64, div: i64) -> Self {
        Dimi32(((self.0 as i64 * times) / div) as i32)
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
    fn from_sp(s: i64) -> Self { Self(s as i32) }
    fn to_sp(&self) -> i64 { self.0 as i64 }
    fn tex_mult(&self, other: f64) -> Self { Dimi32(((self.0 as f64 * other * 65536.0).floor() / 65536.0).floor() as i32) }
    fn tex_div(&self, other: i64) -> Self { Dimi32(self.0 / other as i32) }
}
impl Neg for Dimi32 {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

#[derive(Clone,Copy)]
pub struct Skip<SD:SkipDim>{
    pub base:SD::Dim,
    pub stretch:Option<SD>,
    pub shrink:Option<SD>,
}
impl<SD:SkipDim> Skip<SD> {
    fn tex_mult(&self, other: f64) -> Self {
        Self{base:self.base.tex_mult(other),stretch:self.stretch.clone(),shrink:self.shrink.clone()}
    }
    fn tex_div(&self, other: i64) -> Self {
        Self{base:self.base.tex_div(other),stretch:self.stretch.clone(),shrink:self.shrink.clone()}
    }
}
impl<SD:SkipDim> Default for Skip<SD> {
    fn default() -> Self {
        Self{base:SD::Dim::default(),stretch:None,shrink:None}
    }
}
impl<SD:SkipDim> IsDefault for Skip<SD> {
    fn is_default(&self) -> bool {
        self.base.is_default() && self.stretch.is_none() && self.shrink.is_none()
    }
}
impl<SD:SkipDim> Display for Skip<SD> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.base,f)?;
        if let Some(stretch) = &self.stretch {
            write!(f," plus {}",stretch)?;
        }
        if let Some(shrink) = &self.shrink {
            write!(f," minus {}",shrink)?;
        }
        Ok(())
    }
}
impl<SD:SkipDim> Neg for Skip<SD> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self{base:-self.base,stretch:self.stretch,shrink:self.shrink}
    }
}
impl<SD:SkipDim> Add for Skip<SD> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self{base:self.base+rhs.base,stretch:self.stretch.or(rhs.stretch),shrink:self.shrink.or(rhs.shrink)}
    }
}
impl<SD:SkipDim> Sub for Skip<SD> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self{base:self.base-rhs.base,stretch:self.stretch.or(rhs.stretch),shrink:self.shrink.or(rhs.shrink)}
    }
}
impl<SD:SkipDim> Numeric for Skip<SD> {
    fn scale(&self, times: i64, div: i64) -> Self {
        Self{base:self.base.scale(times,div),stretch:self.stretch.clone(),shrink:self.shrink.clone()}
    }
}


#[derive(Clone,Copy)]
#[allow(non_camel_case_types)]
pub enum Fill {
    pt(i32),
    fil(i32),
    fill(i32),
}

impl Display for Fill {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Fill::pt(i) => Dimi32(*i).fmt(f),
            Fill::fil(i) => write!(f,"{}fil",Dimi32(*i).to_string()),
            Fill::fill(i) => write!(f,"{}fill",Dimi32(*i).to_string()),
        }
    }
}
impl Default for Fill {
    fn default() -> Self {
        todo!()
    }
}
impl SkipDim for Fill {
    type Dim = Dimi32;
    fn units() -> Vec<&'static str> {
        vec!["pt","pc","in","bp","cm","mm","dd","cc","sp","fil","fill"]
    }
    fn from_dim(dim: Self::Dim) -> Self {
        Self::pt(dim.0)
    }
    fn from_float(dim: &str, float: f64) -> Self {
        if dim == "fil" {
            Self::fil((float*65536.0).round() as i32)
        } else if dim == "fill" {
            Self::fill((float*65536.0).round() as i32)
        } else {
            Self::pt(Dimi32::from_float(dim,float).0)
        }
    }
}


#[derive(Clone,Copy)]
pub struct MuSkip<MD:MuDim,SD:MuStretchShrinkDim>{
    pub base: MD,
    pub stretch:Option<SD>,
    pub shrink:Option<SD>,
}
impl<MD:MuDim,SD:MuStretchShrinkDim> MuSkip<MD,SD> {
    fn tex_div(&self, other: i64) -> Self {
        Self{base:self.base.tex_div(other),stretch:self.stretch.clone(),shrink:self.shrink.clone()}
    }
    fn tex_mult(&self, other: f64) -> Self {
        Self{base:self.base.tex_mult(other),stretch:self.stretch.clone(),shrink:self.shrink.clone()}
    }
}
impl<MD:MuDim,SD:MuStretchShrinkDim> Default for MuSkip<MD,SD> {
    fn default() -> Self {
        Self{base:MD::default(),stretch:None,shrink:None}
    }
}
impl<MD:MuDim,SD:MuStretchShrinkDim> Display for MuSkip<MD,SD> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.base,f)?;
        if let Some(stretch) = &self.stretch {
            write!(f," plus {}",stretch)?;
        }
        if let Some(shrink) = &self.shrink {
            write!(f," minus {}",shrink)?;
        }
        Ok(())
    }
}
impl<MD:MuDim,SD:MuStretchShrinkDim> Add<Self> for MuSkip<MD,SD> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self{base:self.base+rhs.base,stretch:self.stretch.or(rhs.stretch),shrink:self.shrink.or(rhs.shrink)}
    }
}
impl<MD:MuDim,SD:MuStretchShrinkDim> IsDefault for MuSkip<MD,SD> {
    fn is_default(&self) -> bool {
        self.base.is_default() && self.stretch.is_none() && self.shrink.is_none()
    }
}
impl<MD:MuDim,SD:MuStretchShrinkDim> Neg for MuSkip<MD,SD> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self{base:-self.base,stretch:self.stretch,shrink:self.shrink}
    }
}
impl<MD:MuDim,SD:MuStretchShrinkDim> Sub<Self> for MuSkip<MD,SD> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self{base:self.base-rhs.base,stretch:self.stretch.or(rhs.stretch),shrink:self.shrink.or(rhs.shrink)}
    }
}
impl<MD:MuDim,SD:MuStretchShrinkDim> Numeric for MuSkip<MD,SD> {
    fn scale(&self, times: i64, div: i64) -> Self {
        Self{base:self.base.scale(times,div),stretch:self.stretch.clone(),shrink:self.shrink.clone()}
    }
}

#[derive(Clone,Copy,Debug)]
pub struct Mui32(i32);
impl Display for Mui32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}mu",Dimi32(self.0).to_string())
    }
}
impl Default for Mui32 {
    fn default() -> Self {
        Self(0)
    }
}
impl IsDefault for Mui32 {
    fn is_default(&self) -> bool {
        self.0 == 0
    }
}
impl Neg for Mui32 {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}
impl Add for Mui32 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}
impl Sub for Mui32 {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}
impl Numeric for Mui32 {
    fn scale(&self, times: i64, div: i64) -> Self {
        Self((self.0 as f64 * (times as f64) / (div as f64)).round() as i32)
    }
}
impl MuDim for Mui32 {
    fn units() -> Vec<&'static str> {vec!["mu"]}
    fn from_float(dim: &str, float: f64) -> Self { match dim {
        "mu" => Self((float*65536.0).round() as i32),
        _ => unreachable!("Invalid dimension unit")
    } }
    fn tex_mult(&self, other: f64) -> Self { Mui32(Dimi32(self.0).tex_mult(other).0) }
    fn tex_div(&self, other: i64) -> Self { Self((self.0 as i64 / other) as i32) }
}


#[derive(Clone,Copy)]
#[allow(non_camel_case_types)]
pub enum MuFill {
    mu(i32),
    fil(i32),
    fill(i32),
}
impl Display for MuFill {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MuFill::mu(i) => Mui32(*i).fmt(f),
            MuFill::fil(i) => write!(f,"{}fil",Dimi32(*i).to_string()),
            MuFill::fill(i) => write!(f,"{}fill",Dimi32(*i).to_string()),
        }
    }
}
impl MuStretchShrinkDim for MuFill {
    fn units() -> Vec<&'static str> {
        vec!["mu","fil","fill"]
    }
    fn from_float(dim: &str, float: f64) -> Self {
        if dim == "fil" {
            Self::fil((float*65536.0).round() as i32)
        } else if dim == "fill" {
            Self::fill((float*65536.0).round() as i32)
        } else {
            Self::mu((float*65536.0).round() as i32)
        }
    }
}