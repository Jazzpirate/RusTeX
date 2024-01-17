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
    type MuDim:MuDim+Numeric<Self::Int>;
    fn muskip_to_skip(muskip: MuSkip<Self::MuDim>, em: Self::Dim) -> Skip<Self::Dim> {
        let base = Self::mudim_to_dim(muskip.base,em);
        let stretch = muskip.stretch.map(|s| match s {
            MuStretchShrink::Mu(i) => StretchShrink::Dim(Self::mudim_to_dim(i, em)),
            MuStretchShrink::Fil(i) => StretchShrink::Fil(i),
            MuStretchShrink::Fill(i) => StretchShrink::Fill(i),
            MuStretchShrink::Filll(i) => StretchShrink::Filll(i),
        });
        let shrink = muskip.shrink.map(|s| match s {
            MuStretchShrink::Mu(i) => StretchShrink::Dim(Self::mudim_to_dim(i, em)),
            MuStretchShrink::Fil(i) => StretchShrink::Fil(i),
            MuStretchShrink::Fill(i) => StretchShrink::Fill(i),
            MuStretchShrink::Filll(i) => StretchShrink::Filll(i),
        });
        Skip::new(base,stretch,shrink)
    }
    fn mudim_to_dim(mudim:Self::MuDim,em:Self::Dim) -> Self::Dim;
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
}
/// A TeX dimension. By default [`Dim32`].
pub trait TeXDimen:Copy + Eq + Ord + Default + Debug + Display + Add<Self,Output=Self> + Sub<Self,Output=Self> + Neg<Output=Self> + Into<i64> + std::iter::Sum {
    const UNITS: &'static[&'static [u8]] = DEFAULT_UNITS;
    fn scale_float(&self,times:f32) -> Self;
    fn from_sp(sp:i32) -> Self;
    fn from_float<ET:EngineTypes<Dim=Self>>(engine: &EngineReferences<ET>,f:f32,dim:&[u8]) -> Self;
}

/// The default [`NumSet`] used in plain TeX.
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub struct DefaultNumSet;
impl NumSet for DefaultNumSet {
    type Int = i32;
    type Dim = Dim32;
    type MuDim = Mu;
    fn mudim_to_dim(mudim: Mu, em: Dim32) -> Dim32 {
        Dim32(((mudim.0 as f32 / 65536.0) * (em.0 as f32) / 18.0).round() as i32)
    }

    fn dim_to_int(dim: Dim32) -> i32 {
        dim.0
    }
}

impl Numeric<i32> for i32 {
    fn scale(&self, times: i32, div: i32) -> Self {
        ((*self as f32 * times as f32) / (div as f32)).round() as i32
    }
}
impl TeXInt for i32 {
    const MIN: Self = i32::MIN;
    const MAX: Self = i32::MAX;
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
    fn scale_float(&self, times: f32) -> Self {
        Self((self.0 as f32 * times).floor() as i32)
    }
    fn from_sp(sp: i32) -> Self { Self(sp) }
    fn from_float<ET:EngineTypes<Dim=Self>>(engine: &EngineReferences<ET>,float:f32,dim:&[u8]) -> Self {
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
                Self((float*(em.0 as f32)).round() as i32)
            }
            b"ex" => {
                let f = engine.state.get_current_font();
                let em = f.get_dim(4);
                Self((float*(em.0 as f32)).round() as i32)
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

pub const STRETCH_SHRINK_UNITS:&[&[u8]] = &[b"fil",b"fill",b"filll"];

#[derive(Clone,Copy,Eq,PartialEq,Debug,Default)]
pub struct Skip<D:TeXDimen> {
    pub base:D,
    pub stretch:Option<StretchShrink<D>>,
    pub shrink:Option<StretchShrink<D>>
}
impl<D:TeXDimen> Add<D> for Skip<D> {
    type Output = Self;
    fn add(self, rhs: D) -> Self::Output {
        Self{base:self.base+rhs,stretch:self.stretch,shrink:self.shrink}
    }
}
impl<D:TeXDimen> Skip<D> {
    pub fn new(base: D, stretch: Option<StretchShrink<D>>, shrink: Option<StretchShrink<D>>) -> Self {
        Self{base,stretch:match stretch {
            Some(StretchShrink::Dim(d)) if d == D::default() => None,
            Some(StretchShrink::Fil(0) | StretchShrink::Fill(0) | StretchShrink::Filll(0)) => None,
            _ => stretch
        },shrink:match shrink {
            Some(StretchShrink::Dim(d)) if d == D::default() => None,
            Some(StretchShrink::Fil(0) | StretchShrink::Fill(0) | StretchShrink::Filll(0)) => None,
            _ => shrink
        }}
    }
}
/*
pub trait Skip:Copy + Eq + Default + Debug + Display + Neg<Output=Self> {
    type Base : TeXDimen;
    fn base(self) -> Self::Base;
    fn stretch(&self) -> Option<StretchShrink<Self::Base>>;
    fn shrink(&self) -> Option<StretchShrink<Self::Base>>;
    fn add(self,other:Self::Base) -> Self;
    fn new(base:Self::Base,stretch:Option<StretchShrink<Self::Base>>,shrink:Option<StretchShrink<Self::Base>>) -> Self;
    fn stretch_from_dimen<ET:EngineTypes<Skip=Self,Dim=Self::Base>>(engine: &EngineReferences<ET>,float:f32,dimen:Self::Base) -> StretchShrink<Self::Base>;
    fn stretch_from_float<ET:EngineTypes<Skip=Self,Dim=Self::Base>>(engine: &EngineReferences<ET>,float:f32,dim:&[u8]) -> StretchShrink<Self::Base>;
}

 */

#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub enum StretchShrink<D:TeXDimen> { Dim(D), Fil(i32), Fill(i32), Filll(i32) }
impl<D:TeXDimen> StretchShrink<D> {
    pub fn from_float<ET: EngineTypes<Dim=D>>(engine: &EngineReferences<ET>, float: f32, dim: &[u8]) -> Self {
        match dim {
            b"fil" => Self::Fil((float * 65536.0).round() as i32),
            b"fill" => Self::Fill((float * 65536.0).round() as i32),
            b"filll" => Self::Filll((float * 65536.0).round() as i32),
            _ => Self::Dim(D::from_float(engine, float, dim))
        }
    }
    pub fn from_dimen<ET: EngineTypes<Dim=D>>(_engine: &EngineReferences<ET>, float: f32, dimen: D) -> Self {
        let d = dimen.scale_float(float);
        Self::Dim(d)
    }
}


//#[derive(Clone,Copy,Eq,PartialEq,Debug,Default)]
//pub struct Skip32<D:TeXDimen>{pub base:D,pub stretch:Option<StretchShrink<D>>,pub shrink:Option<StretchShrink<D>>}
impl<I:TeXInt,D:TeXDimen+Numeric<I>> Numeric<I> for Skip<D> {
    fn scale(&self, times: I, div: I) -> Self {
        Self{
            base:self.base.scale(times,div),
            stretch:self.stretch.clone(),
            shrink:self.shrink.clone()
        }
    }
}
impl<D:TeXDimen> PartialOrd for Skip<D> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.base.partial_cmp(&other.base)
    }
}
impl<D:TeXDimen> Ord for Skip<D> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.base.cmp(&other.base)
    }
}
impl<D:TeXDimen> Add<Self> for Skip<D> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self{base:self.base+rhs.base,stretch:self.stretch.or(rhs.stretch),shrink:self.shrink.or(rhs.shrink)}
    }
}
impl<I:TeXInt,D:TeXDimen+Numeric<I>> Div<I> for Skip<D> {
    type Output = Self;
    fn div(self, rhs: I) -> Self::Output {
        self.scale(1.into(),rhs)
    }
}
impl<I:TeXInt,D:TeXDimen+Numeric<I>> Mul<I> for Skip<D> {
    type Output = Self;
    fn mul(self, rhs: I) -> Self::Output {
        self.scale(rhs,1.into())
    }
}
/*
impl<D:TeXDimen> Skip for Skip32<D> {
    type Base = D;
    fn base(self) -> Self::Base { self.base }
    fn stretch(&self) -> Option<Self::Stretch> { self.stretch }
    fn shrink(&self) -> Option<Self::Shrink> { self.shrink }
    fn new(base: D, stretch: Option<StretchShrink<D>>, shrink: Option<StretchShrink<D>>) -> Self {
        Self{base,stretch,shrink}
    }
    fn add(self, other: Self::Base) -> Self {
        Self{base:self.base+other,stretch:self.stretch,shrink:self.shrink}
    }
    fn stretch_from_float<ET: EngineTypes<Skip=Self,Dim=D>>(engine: &EngineReferences<ET>, float: f32, dim: &[u8]) -> Self::Stretch {
        match dim {
            b"fil" => StretchShrink::Fil((float * 65536.0).round() as i32),
            b"fill" => StretchShrink::Fill((float * 65536.0).round() as i32),
            _ => StretchShrink::Dim(D::from_float(engine, float, dim))
        }
    }
    fn stretch_from_dimen<ET: EngineTypes<Skip=Self,Dim=D>>(_engine: &EngineReferences<ET>, float: f32, dimen: Self::Base) -> Self::Stretch {
        let d = dimen.scale_float(float);
        StretchShrink::Dim(d)
    }
}

 */
impl<D:TeXDimen> Display for Skip<D> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.base)?;
        if let Some(stretch) = &self.stretch {
            write!(f," plus ")?;
            match stretch {
                StretchShrink::Dim(d) => write!(f, "{}", d)?,
                StretchShrink::Fil(i) => Dim32::display_num(*i, "fil", f)?,
                StretchShrink::Fill(i) => Dim32::display_num(*i, "fill", f)?,
                StretchShrink::Filll(i) => Dim32::display_num(*i, "filll", f)?,
            }
        }
        if let Some(shrink) = &self.shrink {
            write!(f," minus ")?;
            match shrink {
                StretchShrink::Dim(d) => write!(f, "{}", d)?,
                StretchShrink::Fil(i) => Dim32::display_num(*i, "fil", f)?,
                StretchShrink::Fill(i) => Dim32::display_num(*i, "fill", f)?,
                StretchShrink::Filll(i) => Dim32::display_num(*i, "filll", f)?,
            }
        }
        Ok(())
    }
}
impl<D:TeXDimen> Neg for Skip<D> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self {
            base:-self.base,
            stretch:self.stretch.map(|s| match s {
                StretchShrink::Dim(d) => StretchShrink::Dim(-d),
                StretchShrink::Fil(i) => StretchShrink::Fil(-i),
                StretchShrink::Fill(i) => StretchShrink::Fill(-i),
                StretchShrink::Filll(i) => StretchShrink::Filll(-i),
            }),
            shrink:self.shrink.map(|s| match s {
                StretchShrink::Dim(d) => StretchShrink::Dim(-d),
                StretchShrink::Fil(i) => StretchShrink::Fil(-i),
                StretchShrink::Fill(i) => StretchShrink::Fill(-i),
                StretchShrink::Filll(i) => StretchShrink::Filll(-i),
            })
        }
    }
}

pub trait MuDim:Display+Debug+Clone+Copy+Neg<Output=Self>+Add<Output=Self>+PartialEq+Ord+Default {
    const UNITS:&'static[&'static [u8]] = &[b"mu"];
    fn from_float<ET:EngineTypes>(engine: &EngineReferences<ET>,float:f32,dim:&[u8]) -> Self;
}

#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub struct MuSkip<M:MuDim> {
    pub base:M,
    pub stretch:Option<MuStretchShrink<M>>,
    pub shrink:Option<MuStretchShrink<M>>,
}
impl<M:MuDim> MuSkip<M> {
    pub fn new(base: M, stretch: Option<MuStretchShrink<M>>, shrink: Option<MuStretchShrink<M>>) -> Self {
        Self{base,stretch:match stretch {
            Some(MuStretchShrink::Mu(d)) if d == M::default() => None,
            Some(MuStretchShrink::Fil(0) | MuStretchShrink::Fill(0) | MuStretchShrink::Filll(0)) => None,
            _ => stretch
        },shrink:match shrink {
            Some(MuStretchShrink::Mu(d)) if d == M::default() => None,
            Some(MuStretchShrink::Fil(0) | MuStretchShrink::Fill(0) | MuStretchShrink::Filll(0)) => None,
            _ => shrink
        }}
    }

}
/*
pub trait MuSkip:Copy + Eq + Ord + Default + Debug + Display + Neg<Output=Self> {
    type Base: MuDim;
    type Stretch;
    type Shrink;
    fn base(self) -> Self::Base;
    fn units() -> &'static[&'static [u8]];
    fn stretch_units() -> &'static[&'static [u8]];
    fn shrink_units() -> &'static[&'static [u8]];
    fn from_float<ET:EngineTypes>(engine: &EngineReferences<ET>,float:f32,dim:&[u8]) -> Self::Base;
    fn new(base:Self::Base,stretch:Option<Self::Stretch>,shrink:Option<Self::Shrink>) -> Self;
    fn stretch_from_float<ET:EngineTypes>(engine: &EngineReferences<ET>,float:f32,dim:&[u8]) -> Self::Stretch;
    fn shrink_from_float<ET:EngineTypes>(engine: &EngineReferences<ET>,float:f32,dim:&[u8]) -> Self::Shrink;
}
*/
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub enum MuStretchShrink<M:MuDim> {
    Mu(M),Fil(i32),Fill(i32),Filll(i32)
}
impl<M:MuDim> MuStretchShrink<M> {
    pub fn from_float<ET: EngineTypes>(engine: &EngineReferences<ET>, float: f32, dim: &[u8]) -> Self {
        match dim {
            b"fil" => Self::Fil((float * 65536.0).round() as i32),
            b"fill" => Self::Fill((float * 65536.0).round() as i32),
            b"filll" => Self::Filll((float * 65536.0).round() as i32),
            _ => Self::Mu(M::from_float(engine, float, dim))
        }
    }
}

#[derive(Clone,Copy,Eq,PartialEq,Debug,Default,PartialOrd,Ord)]
pub struct Mu(pub i32);
impl Numeric<i32> for Mu {
    fn scale(&self, times: i32, div: i32) -> Self {
        Self((self.0 as i64 * (times as i64) / (div as i64)) as i32)
    }
}
impl Div<i32> for Mu {
    type Output = Self;
    fn div(self, rhs: i32) -> Self::Output {
        Self(self.0 / rhs)
    }
}
impl Mul<i32> for Mu {
    type Output = Self;
    fn mul(self, rhs: i32) -> Self::Output {
        Self(self.0 * rhs)
    }
}
impl MuDim for Mu {
    fn from_float<ET:EngineTypes>(_engine: &EngineReferences<ET>,float:f32,dim:&[u8]) -> Self {
        match dim {
            b"mu" => Mu((float*65536.0).round() as i32),
            _ => unreachable!()
        }
    }
}
impl Neg for Mu {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Mu(-self.0)
    }
}
impl Add<Mu> for Mu {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Mu(self.0+rhs.0)
    }
}
impl Display for Mu {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Dim32::display_num(self.0,"mu",f)
    }
}
/*
#[derive(Clone,Copy,Eq,PartialEq,Debug,Default)]
pub struct MuSkip32{pub base:Mu,pub stretch:Option<MuStretchShrink>,pub shrink:Option<MuStretchShrink>}
impl MuSkip for MuSkip32 {
    type Base = Mu;
    type Stretch = MuStretchShrink;
    type Shrink = MuStretchShrink;

    fn base(self) -> Self::Base { self.base }
    fn units() -> &'static [&'static [u8]] { &[b"mu"]}
    fn stretch_units() -> &'static [&'static [u8]] { &[b"mu",b"fil",b"fill"] }
    fn shrink_units() -> &'static [&'static [u8]] { &[b"mu",b"fil",b"fill"] }
    fn new(base: Self::Base, stretch: Option<Self::Stretch>, shrink: Option<Self::Shrink>) -> Self {
        Self{base,stretch,shrink}
    }
    fn stretch_from_float<ET: EngineTypes>(engine: &EngineReferences<ET>, float: f32, dim: &[u8]) -> Self::Stretch {
        match dim {
            b"fil" => MuStretchShrink::fil((float * 65536.0).round() as i32),
            b"fill" => MuStretchShrink::fill((float * 65536.0).round() as i32),
            _ => MuStretchShrink::mu(Self::from_float(engine, float, dim).0)
        }
    }
    fn shrink_from_float<ET: EngineTypes>(engine: &EngineReferences<ET>, float: f32, dim: &[u8]) -> Self::Shrink {
        Self::stretch_from_float(engine,float,dim)
    }
}

 */
impl<M:MuDim> PartialOrd for MuSkip<M> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.base.partial_cmp(&other.base)
    }
}
impl<M:MuDim> Ord for MuSkip<M> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.base.cmp(&other.base)
    }
}
impl<M:MuDim> Display for MuSkip<M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.base)?;
        if let Some(stretch) = &self.stretch {
            write!(f," plus ")?;
            match stretch {
                MuStretchShrink::Mu(d) => write!(f, "{}", d)?,
                MuStretchShrink::Fil(i) => Dim32::display_num(*i, "fil", f)?,
                MuStretchShrink::Fill(i) => Dim32::display_num(*i, "fill", f)?,
                MuStretchShrink::Filll(i) => Dim32::display_num(*i, "filll", f)?,
            }
        }
        if let Some(shrink) = &self.shrink {
            write!(f," minus ")?;
            match shrink {
                MuStretchShrink::Mu(d) => write!(f, "{}", d)?,
                MuStretchShrink::Fil(i) => Dim32::display_num(*i, "fil", f)?,
                MuStretchShrink::Fill(i) => Dim32::display_num(*i, "fill", f)?,
                MuStretchShrink::Filll(i) => Dim32::display_num(*i, "filll", f)?,
            }
        }
        Ok(())
    }
}
impl<M:MuDim> Neg for MuSkip<M> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self {
            base:-self.base,
            stretch:self.stretch.map(|s| match s {
                MuStretchShrink::Mu(d) => MuStretchShrink::Mu(-d),
                MuStretchShrink::Fil(i) => MuStretchShrink::Fil(-i),
                MuStretchShrink::Fill(i) => MuStretchShrink::Fill(-i),
                MuStretchShrink::Filll(i) => MuStretchShrink::Filll(-i),
            }),
            shrink:self.shrink.map(|s| match s {
                MuStretchShrink::Mu(d) => MuStretchShrink::Mu(-d),
                MuStretchShrink::Fil(i) => MuStretchShrink::Fil(-i),
                MuStretchShrink::Fill(i) => MuStretchShrink::Fill(-i),
                MuStretchShrink::Filll(i) => MuStretchShrink::Filll(-i),
            })
        }
    }
}
impl<M:MuDim> Default for MuSkip<M> {
    fn default() -> Self {
        Self{base:M::default(),stretch:None,shrink:None}
    }
}
impl<I:TeXInt,M:MuDim+Numeric<I>> Numeric<I> for MuSkip<M> {
    fn scale(&self, times: I, div: I) -> Self {
        Self{base:self.base.scale(times,div),stretch:self.stretch.clone(),shrink:self.shrink.clone()}
    }
}

impl<M:MuDim> Add<Self> for MuSkip<M> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self{base:self.base+rhs.base,stretch:self.stretch.or(rhs.stretch),shrink:self.shrink.or(rhs.shrink)}
    }
}
impl<I:TeXInt,M:MuDim+Numeric<I>> Div<I> for MuSkip<M> {
    type Output = Self;
    fn div(self, rhs: I) -> Self::Output {
        self.scale(1.into(),rhs)
    }
}
impl<I:TeXInt,M:MuDim+Numeric<I>> Mul<I> for MuSkip<M> {
    type Output = Self;
    fn mul(self, rhs: I) -> Self::Output {
        self.scale(rhs, 1.into())
    }
}