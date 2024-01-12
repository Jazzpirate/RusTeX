/*! Numerical values used in TeX, such as [integers](TeXInt) and [dimensions](TeXDimen).*/

use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::num::ParseIntError;
use std::ops::{Add, Div, Mul, Neg};
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::fontsystem::Font;
use crate::engine::state::State;
use std::ops::Sub;

/// Bundles the various numerical types used in some engine, and converts between them.
pub trait NumSet: Clone+Debug {
    type Int:TeXInt;
    type Dim:TeXDimen+Numeric<Self::Int>;
    type Skip:Skip<Base=Self::Dim>+Numeric<Self::Int>;
    type MuSkip:MuSkip+Numeric<Self::Int>;
    fn muskip_to_skip(muskip: Self::MuSkip, em: Self::Dim) -> Self::Skip;
    fn mudim_to_dim(mudim:<Self::MuSkip as MuSkip>::Base,em:Self::Dim) -> Self::Dim;
    fn dim_to_int(dim:Self::Dim) -> Self::Int;
}

pub trait Numeric<I:TeXInt>: Eq + Ord + Neg<Output=Self> + Add<Self,Output=Self> + Mul<I,Output=Self> + Div<I,Output=Self> + Copy + Default + Debug + Display {
    fn scale(&self,times:I,div:I) -> Self;
}

/// A TeX integer. By default `i32`.
pub trait TeXInt:Numeric<Self> + From<i32> + TryFrom<i64> + Into<i64> + TryInto<i32> + Debug + Display +
    std::str::FromStr {
    const MIN:Self;
    const MAX:Self;
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, std::num::ParseIntError>;
}
/// A TeX dimension. By default [`Dim32`].
pub trait TeXDimen:Copy + Eq + Ord + Default + Debug + Display + Add<Self,Output=Self> + Sub<Self,Output=Self> + Neg<Output=Self> + Into<i64> + std::iter::Sum {
    fn units() -> &'static[&'static [u8]];
    fn scale_float(&self,times:f64) -> Self;
    fn from_sp(sp:i32) -> Self;
    fn from_float<ET:EngineTypes<Dim=Self>>(engine: &EngineReferences<ET>,f:f64,dim:&[u8]) -> Self;
}

/// The default [`NumSet`] used in plain TeX.
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub struct DefaultNumSet;
impl NumSet for DefaultNumSet {
    type Int = i32;
    type Dim = Dim32;
    type Skip = Skip32<Dim32>;
    type MuSkip = MuSkip32;
    fn muskip_to_skip(muskip: Self::MuSkip, em: Self::Dim) -> Self::Skip {
        let base = Self::mudim_to_dim(muskip.base(),em);
        let stretch = muskip.stretch.map(|s| match s {
            MuFill::mu(i) => Fill::pt(Self::mudim_to_dim(Mu(i),em)),
            MuFill::fil(i) => Fill::fil(i),
            MuFill::fill(i) => Fill::fill(i),
        });
        let shrink = muskip.shrink.map(|s| match s {
            MuFill::mu(i) => Fill::pt(Self::mudim_to_dim(Mu(i),em)),
            MuFill::fil(i) => Fill::fil(i),
            MuFill::fill(i) => Fill::fill(i),
        });
        Self::Skip::new(base,stretch,shrink)
    }
    fn mudim_to_dim(mudim: <Self::MuSkip as MuSkip>::Base, em: Self::Dim) -> Self::Dim {
        Dim32(((mudim.0 as f64 / 65536.0) * (em.0 as f64) / 18.0).round() as i32)
    }

    fn dim_to_int(dim: Self::Dim) -> Self::Int {
        dim.0
    }
}

impl Numeric<i32> for i32 {

    fn scale(&self, times: i32, div: i32) -> Self {
        ((*self as f64 * times as f64) / (div as f64)).round() as i32
    }
}
impl TeXInt for i32 {
    const MIN: Self = i32::MIN;
    const MAX: Self = i32::MAX;

    fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError> {
        i32::from_str_radix(src, radix)
    }
}

/// A plain TeX dimension, represented as a 32-bit integer in *scaled points (sp)*, where 65536sp = 1pt.
#[derive(Clone,Copy,Eq,PartialEq,Ord,PartialOrd,Debug,Default)]
pub struct Dim32(pub i32);
impl Numeric<i32> for Dim32 {

    fn scale(&self, times: i32, div: i32) -> Self {
        Self((self.0 as i64 * (times as i64) / (div as i64)) as i32)
    }
}
impl Add for Dim32 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Dim32(self.0 + rhs.0)
    }
}
impl Sub for Dim32 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Dim32(self.0 - rhs.0)
    }
}
impl Div<i32> for Dim32 {
    type Output = Self;

    fn div(self, rhs: i32) -> Self::Output {
        self.scale(1,rhs)
    }
}
impl Mul<i32> for Dim32 {
    type Output = Self;

    fn mul(self, rhs: i32) -> Self::Output {
        self.scale(rhs,1)
    }
}
impl Dim32 {
    fn display_num(num:i32,unit:&str,f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut val = num;
        if val < 0 {
            write!(f,"-")?;
            val = -val;
        }
        write!(f,"{}.",val/65536)?;
        val = 10*(val%65536) + 5;
        let mut delta = 10;
        if val < delta {
            return write!(f,"0{}",unit)
        }
        while val > delta {
            if delta > 65536 { val = val + 32768 - 50000; }
            write!(f,"{}",val/65536)?;
            val = 10*(val%65536);
            delta = delta*10;
        }
        write!(f,"{}",unit)
    }
}
impl Neg for Dim32 {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Dim32(-self.0)
    }
}
impl Display for Dim32 {
    // B-Book §103
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::display_num(self.0,"pt",f)
    }
}
impl Into<i64> for Dim32 {

    fn into(self) -> i64 { self.0 as i64 }
}

const DEFAULT_UNITS:&[&[u8]] = &[b"pt",b"pc",b"in",b"bp",b"cm",b"mm",b"dd",b"cc",b"sp",b"em",b"ex"];

impl TeXDimen for Dim32 {

    fn scale_float(&self, times: f64) -> Self {
        Self((self.0 as f64 * times).floor() as i32)
    }

    fn from_sp(sp: i32) -> Self { Self(sp) }

    fn units() -> &'static[&'static [u8]] { DEFAULT_UNITS }
    fn from_float<ET:EngineTypes<Dim=Self>>(engine: &EngineReferences<ET>,float:f64,dim:&[u8]) -> Self {
        match dim {
            b"sp" => Self(float.round() as i32),
            b"pt" => Self((float*65536.0).round() as i32),
            b"pc" => Self((float*65536.0*12.0).round() as i32),
            b"in" => Self((float*65536.0*72.27).round() as i32),
            b"bp" => Self((float*65536.0*72.27/72.0).round() as i32),
            b"cm" => Self((float*65536.0*72.27/2.54).round() as i32),
            b"mm" => Self((float*65536.0*72.27/25.4).round() as i32),
            b"dd" => Self((float*65536.0*1238.0/1157.0).round() as i32),
            b"cc" => Self((float*65536.0*14856.0/1157.0).round() as i32),
            b"em" => {
                let f = engine.state.get_current_font();
                let em = f.get_dim(5);
                Self((float*(em.0 as f64)).round() as i32)
            }
            b"ex" => {
                let f = engine.state.get_current_font();
                let em = f.get_dim(4);
                Self((float*(em.0 as f64)).round() as i32)
            }
            _ => unreachable!()
        }
    }
}
impl std::iter::Sum for Dim32 {
    fn sum<I: Iterator<Item=Self>>(iter: I) -> Self {
        iter.fold(Self::default(), |a, b| a + b)
    }
}

const STRETCH_SHRINK_UNITS:&[&[u8]] = &[b"pt",b"pc",b"in",b"bp",b"cm",b"mm",b"dd",b"cc",b"sp",b"em",b"ex",b"fil",b"fill"];

pub trait Skip:Copy + Eq + Default + Debug + Display + Neg<Output=Self> {
    type Stretch;
    type Shrink;
    type Base : TeXDimen;
    fn stretch(&self) -> Option<Self::Stretch>;
    fn shrink(&self) -> Option<Self::Shrink>;
    fn base(self) -> Self::Base;
    fn add(self,other:Self::Base) -> Self;
    fn new(base:Self::Base,stretch:Option<Self::Stretch>,shrink:Option<Self::Shrink>) -> Self;
    fn stretch_units() -> &'static[&'static [u8]];
    fn shrink_units() -> &'static[&'static [u8]];
    fn stretch_from_dimen<ET:EngineTypes<Skip=Self,Dim=Self::Base>>(engine: &EngineReferences<ET>,float:f64,dimen:Self::Base) -> Self::Stretch;
    fn shrink_from_dimen<ET:EngineTypes<Skip=Self,Dim=Self::Base>>(engine: &EngineReferences<ET>,float:f64,dimen:Self::Base) -> Self::Shrink;
    fn stretch_from_float<ET:EngineTypes<Skip=Self,Dim=Self::Base>>(engine: &EngineReferences<ET>,float:f64,dim:&[u8]) -> Self::Stretch;
    fn shrink_from_float<ET:EngineTypes<Skip=Self,Dim=Self::Base>>(engine: &EngineReferences<ET>,float:f64,dim:&[u8]) -> Self::Shrink;
}

#[derive(Clone,Copy,Eq,PartialEq,Debug)]
#[allow(non_camel_case_types)]
pub enum Fill<D:TeXDimen> {
    pt(D),fil(i32),fill(i32)
}

#[derive(Clone,Copy,Eq,PartialEq,Debug,Default)]
pub struct Skip32<D:TeXDimen>{pub base:D,pub stretch:Option<Fill<D>>,pub shrink:Option<Fill<D>>}
impl<D:TeXDimen+Numeric<i32>> Numeric<i32> for Skip32<D> {
    fn scale(&self, times: i32, div: i32) -> Self {
        Self{
            base:self.base.scale(times,div),
            stretch:self.stretch.clone(),
            shrink:self.shrink.clone()
        }
    }
}
impl<D:TeXDimen> PartialOrd for Skip32<D> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.base.partial_cmp(&other.base)
    }
}
impl<D:TeXDimen> Ord for Skip32<D> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.base.cmp(&other.base)
    }
}
impl<D:TeXDimen+Numeric<i32>> Add<Self> for Skip32<D> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self{base:self.base+rhs.base,stretch:self.stretch.or(rhs.stretch),shrink:self.shrink.or(rhs.shrink)}
    }
}
impl<D:TeXDimen+Numeric<i32>> Div<i32> for Skip32<D> {
    type Output = Self;
    fn div(self, rhs: i32) -> Self::Output {
        self.scale(1,rhs)
    }
}
impl<D:TeXDimen+Numeric<i32>> Mul<i32> for Skip32<D> {
    type Output = Self;
    fn mul(self, rhs: i32) -> Self::Output {
        self.scale(rhs,1)
    }
}

impl<D:TeXDimen> Skip for Skip32<D> {
    type Base = D;
    type Stretch = Fill<D>;
    type Shrink = Fill<D>;

    fn base(self) -> Self::Base { self.base }

    fn stretch(&self) -> Option<Self::Stretch> { self.stretch }

    fn shrink(&self) -> Option<Self::Shrink> { self.shrink }

    fn new(base: D, stretch: Option<Fill<D>>, shrink: Option<Fill<D>>) -> Self {
        Self{base,stretch,shrink}
    }

    fn add(self, other: Self::Base) -> Self {
        Self{base:self.base+other,stretch:self.stretch,shrink:self.shrink}
    }

    fn stretch_units() -> &'static[&'static [u8]] { STRETCH_SHRINK_UNITS }

    fn shrink_units() -> &'static[&'static [u8]] { STRETCH_SHRINK_UNITS }
    fn stretch_from_float<ET: EngineTypes<Skip=Self,Dim=D>>(engine: &EngineReferences<ET>, float: f64, dim: &[u8]) -> Self::Stretch {
        match dim {
            b"fil" => Fill::fil((float * 65536.0).round() as i32),
            b"fill" => Fill::fill((float * 65536.0).round() as i32),
            _ => Fill::pt(D::from_float(engine,float,dim))
        }
    }

    fn shrink_from_float<ET: EngineTypes<Skip=Self,Dim=D>>(engine: &EngineReferences<ET>, float: f64, dim: &[u8]) -> Self::Shrink {
        Self::stretch_from_float(engine,float,dim)
    }

    fn stretch_from_dimen<ET: EngineTypes<Skip=Self,Dim=D>>(_engine: &EngineReferences<ET>, float: f64, dimen: Self::Base) -> Self::Stretch {
        let d = dimen.scale_float(float);
        Fill::pt(d)
    }

    fn shrink_from_dimen<ET: EngineTypes<Skip=Self,Dim=D>>(_engine: &EngineReferences<ET>, float: f64, dimen: Self::Base) -> Self::Stretch {
        let d = dimen.scale_float(float);
        Fill::pt(d)
    }
}
impl<D:TeXDimen> Display for Skip32<D> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result
    {
        write!(f,"{}",self.base)?;
        if let Some(stretch) = &self.stretch {
            write!(f," plus ")?;
            match stretch {
                Fill::pt(d) => write!(f,"{}",d)?,
                Fill::fil(i) => Dim32::display_num(*i,"fil",f)?,
                Fill::fill(i) => Dim32::display_num(*i,"fill",f)?,
            }
        }
        if let Some(shrink) = &self.shrink {
            write!(f," minus ")?;
            match shrink {
                Fill::pt(d) => write!(f,"{}",d)?,
                Fill::fil(i) => Dim32::display_num(*i,"fil",f)?,
                Fill::fill(i) => Dim32::display_num(*i,"fill",f)?,
            }
        }
        Ok(())
    }
}
impl<D:TeXDimen> Neg for Skip32<D> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Skip32 {
            base:-self.base,
            stretch:self.stretch.map(|s| match s {
                Fill::pt(d) => Fill::pt(-d),
                Fill::fil(i) => Fill::fil(-i),
                Fill::fill(i) => Fill::fill(-i),
            }),
            shrink:self.shrink.map(|s| match s {
                Fill::pt(d) => Fill::pt(-d),
                Fill::fil(i) => Fill::fil(-i),
                Fill::fill(i) => Fill::fill(-i),
            })
        }
    }
}

pub trait MuSkip:Copy + Eq + Ord + Default + Debug + Display + Neg<Output=Self> {
    type Base: Display+Debug+Clone+Copy;
    type Stretch;
    type Shrink;
    fn base(self) -> Self::Base;
    fn units() -> &'static[&'static [u8]];
    fn stretch_units() -> &'static[&'static [u8]];
    fn shrink_units() -> &'static[&'static [u8]];
    fn from_float<ET:EngineTypes>(engine: &EngineReferences<ET>,float:f64,dim:&[u8]) -> Self::Base;
    fn new(base:Self::Base,stretch:Option<Self::Stretch>,shrink:Option<Self::Shrink>) -> Self;
    fn stretch_from_float<ET:EngineTypes>(engine: &EngineReferences<ET>,float:f64,dim:&[u8]) -> Self::Stretch;
    fn shrink_from_float<ET:EngineTypes>(engine: &EngineReferences<ET>,float:f64,dim:&[u8]) -> Self::Shrink;
}

#[derive(Clone,Copy,Eq,PartialEq,Debug)]
#[allow(non_camel_case_types)]
pub enum MuFill {
    mu(i32),fil(i32),fill(i32)
}

#[derive(Clone,Copy,Eq,PartialEq,Debug,Default)]
pub struct Mu(pub i32);
impl Display for Mu {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Dim32::display_num(self.0,"mu",f)
    }
}

#[derive(Clone,Copy,Eq,PartialEq,Debug,Default)]
pub struct MuSkip32{pub base:Mu,pub stretch:Option<MuFill>,pub shrink:Option<MuFill>}
impl MuSkip for MuSkip32 {
    type Base = Mu;
    type Stretch = MuFill;
    type Shrink = MuFill;

    fn from_float<ET:EngineTypes>(_engine: &EngineReferences<ET>,float:f64,dim:&[u8]) -> Self::Base {
        match dim {
            b"mu" => Mu((float*65536.0).round() as i32),
            _ => unreachable!()
        }
    }

    fn base(self) -> Self::Base { self.base }

    fn units() -> &'static [&'static [u8]] { &[b"mu"]}

    fn stretch_units() -> &'static [&'static [u8]] { &[b"mu",b"fil",b"fill"] }

    fn shrink_units() -> &'static [&'static [u8]] { &[b"mu",b"fil",b"fill"] }

    fn new(base: Self::Base, stretch: Option<Self::Stretch>, shrink: Option<Self::Shrink>) -> Self {
        Self{base,stretch,shrink}
    }
    fn stretch_from_float<ET: EngineTypes>(engine: &EngineReferences<ET>, float: f64, dim: &[u8]) -> Self::Stretch {
        match dim {
            b"fil" => MuFill::fil((float * 65536.0).round() as i32),
            b"fill" => MuFill::fill((float * 65536.0).round() as i32),
            _ => MuFill::mu(Self::from_float(engine,float,dim).0)
        }
    }

    fn shrink_from_float<ET: EngineTypes>(engine: &EngineReferences<ET>, float: f64, dim: &[u8]) -> Self::Shrink {
        Self::stretch_from_float(engine,float,dim)
    }
}
impl PartialOrd for MuSkip32 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.base.0.partial_cmp(&other.base.0)
    }
}
impl Ord for MuSkip32 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.base.0.cmp(&other.base.0)
    }
}
impl Display for MuSkip32  {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Dim32::display_num(self.base.0,"mu",f)
    }
}
impl Neg for MuSkip32 {
    type Output = Self;
    fn neg(self) -> Self::Output {
        MuSkip32 {
            base:Mu(-self.base.0),
            stretch:self.stretch.map(|s| match s {
                MuFill::mu(d) => MuFill::mu(-d),
                MuFill::fil(i) => MuFill::fil(-i),
                MuFill::fill(i) => MuFill::fill(-i),
            }),
            shrink:self.shrink.map(|s| match s {
                MuFill::mu(d) => MuFill::mu(-d),
                MuFill::fil(i) => MuFill::fil(-i),
                MuFill::fill(i) => MuFill::fill(-i),
            })
        }
    }
}
impl Numeric<i32> for MuSkip32 {
    fn scale(&self, times: i32, div: i32) -> Self {
        Self{base:Mu(self.base.0.scale(times,div)),stretch:self.stretch.clone(),shrink:self.shrink.clone()}
    }
}

impl Add<Self> for MuSkip32 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self{base:Mu(self.base.0+rhs.base.0),stretch:self.stretch.or(rhs.stretch),shrink:self.shrink.or(rhs.shrink)}
    }
}
impl Div<i32> for MuSkip32 {
    type Output = Self;
    fn div(self, rhs: i32) -> Self::Output {
        self.scale(1,rhs)
    }
}
impl Mul<i32> for MuSkip32 {
    type Output = Self;
    fn mul(self, rhs: i32) -> Self::Output {
        self.scale(rhs, 1)
    }
}