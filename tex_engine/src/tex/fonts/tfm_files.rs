use std::fs;
use std::path::PathBuf;
use crate::utils::map::HMap;

pub struct TfmFile {
    pub hyphenchar:u8,
    pub skewchar:u8,
    pub dimen:[f64;256],
    pub size : i64,
    pub typestr : String,
    pub widths:[f64;256],
    pub heights: [f64;256],
    pub depths: [f64;256],
    pub ics:[f64;256],
    pub lps:[u8;256],
    pub rps:[u8;256],
    pub ligs:HMap<(u8,u8),u8>,
    pub name:String,
    pub filepath:String,
}
impl PartialEq for TfmFile {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

// https://www.tug.org/TUGboat/tb02-1/tb02fuchstfm.pdf

impl TfmFile {
    pub fn new(pb : PathBuf) -> TfmFile {
        let filepath = pb.as_path().to_str().unwrap().into();
        let name: String = pb.file_stem().unwrap().to_str().unwrap().into();
        let mut state = FontState {
            ret:fs::read(pb).unwrap(),
            i:0
        };
        state.ret.reverse();

        let hyphenchar : u8 = 45;
        let skewchar : u8 = 255;
        let mut dimen: [f64;256] = [0.0;256];
        let mut size: i64 = 65536;
        let mut typestr = "".to_string();
        let mut widths: [f64;256] = [0.0;256];
        let mut heights: [f64;256] = [0.0;256];
        let mut depths: [f64;256] = [0.0;256];
        let mut ics: [f64;256] = [0.0;256];
        let lps: [u8;256] = [0;256];
        let rps: [u8;256] = [0;256];
        let mut ligs:HMap<(u8,u8),u8> = HMap::default();

        let (lf,lh) = state.read_int();
        let (bc,ec) = state.read_int();
        let (nw,nh) = state.read_int();
        let (nd,ni) = state.read_int();
        let (nl,nk) = state.read_int();
        let (ne,np) = state.read_int();
        let lf = lf as usize;
        let lh = lh as usize;
        let bc = bc as usize;
        let ec = ec as usize;
        let nw = nw as usize;
        let nh = nh as usize;
        let nd = nd as usize;
        let ni = ni as usize;
        let nl = nl as usize;
        let nk = nk as usize;
        let ne = ne as usize;
        let np = np as usize;
        assert_eq!(lf,6+lh+(ec-bc+1)+nw+nh+nd+ni+nk+nl+ne+np);
        state.skip(1);

        size = ((size as f64) * state.read_float()).round() as i64;

        if lh >= 12 {
            let mut sv : Vec<u8> = vec!();
            let (ln,b,c,d) = state.pop();
            sv.push(b);
            sv.push(c);
            sv.push(d);
            for _ in 0..9 {
                let (a,b,c,d) = state.pop();
                sv.push(a);
                sv.push(b);
                sv.push(c);
                sv.push(d);
            }
            typestr = String::from_utf8(sv.get(0..(ln as usize)).unwrap().iter().map(|x| *x).collect()).unwrap();
        }
        state.skip((lh + 6) - state.i);

        let finfo_table : Vec<FInfoEntry> = (bc..(ec+1)).map(|i| state.read_fifo(i as u8)).collect();
        assert_eq!(state.i,lh + 6 + (ec-bc+1));

        let widthls : Vec<f64> = (0..nw).map(|_| state.read_float()).collect();
        let heightls: Vec<f64> = (0..nh).map(|_| state.read_float()).collect();
        let depthls: Vec<f64> = (0..nd).map(|_| state.read_float()).collect();
        let italicls: Vec<f64> = (0..ni).map(|_| state.read_float()).collect();

        let mut ligatures : Vec<(bool,u16,bool,u16)> = vec!();
        for _ in 0..nl {
            let (a,b,c,d) = state.pop();
            let stop = a >= 128;
            let tag = c >= 128;
            ligatures.push((stop,b as u16,tag,d as u16))
        }
        state.skip(nk + ne);
        assert_eq!(state.i,lh + 6 + (ec-bc+1) + nw + nh + nd + ni + nl + nk + ne);
        if np > 0 {
            dimen[0] = state.read_float();
        }
        for i in 2..(np+1) {
            dimen[i-1] = state.read_float();
        }

        let factor = match dimen[6 - 1] {
            0.0 => 1.0,
            f => f
        };

        for t in finfo_table {
            match widthls.get(t.width_index as usize) {
                Some(i) if *i == 0.0 => (),
                None => (),
                Some(f) => {widths[t.char as usize] = factor * f;}
            }
            match heightls.get(t.height_index as usize) {
                Some(i) if *i == 0.0 => (),
                None => (),
                Some(f) => {heights[t.char as usize] = factor * f;}
            }
            match depthls.get(t.depth_index as usize) {
                Some(i) if *i == 0.0 => (),
                None => (),
                Some(f) => {depths[t.char as usize] = factor * f;}
            }
            match italicls.get(t.char_ic_index as usize) {
                Some(i) if *i == 0.0 => (),
                None => (),
                Some(f) => {ics[t.char as usize] = factor * f;}
            }
            for (nc,rep) in t.ligature(&ligatures) {
                ligs.insert((t.char,nc),rep);
            }
        }
        assert_eq!(state.i,lf);

        TfmFile {
            hyphenchar,skewchar,dimen,size,typestr,widths,
            heights,depths,ics,lps,rps,ligs,name,filepath
        }
    }
}

struct FontState {
    pub ret : Vec<u8>,
    pub i : usize
}
impl FontState {
    fn pop(&mut self) -> (u8,u8,u8,u8) {
        self.i += 1;
        (self.ret.pop().unwrap(),self.ret.pop().unwrap(),self.ret.pop().unwrap(),self.ret.pop().unwrap())
    }
    fn read_int(&mut self) -> (u16,u16) {
        let (a,b,c,d) = self.pop();
        let i1 = ((a as u16) << 8) | (b as u16);
        let i2 = ((c as u16) << 8) | (d as u16);
        (i1,i2)
    }
    fn read_float(&mut self) -> f64 {
        let (a,b,c,d) = self.pop();
        let int = ((a as i32) << 24) | ((b as i32) << 16) |
            ((c as i32) << 8) | (d as i32);
        let f = ((int & 0x7fffffff) as f64) / ((1 << 20) as f64);
        if int < 0 {-f} else {f}
    }
    fn skip(&mut self, len:usize) {
        for _ in 0..len {
            self.pop();
        }
    }
    fn read_fifo(&mut self,char:u8) -> FInfoEntry {
        let (a,b,c,d) = self.pop();
        let width_index = 0x000000FF & a;
        let (height_index,depth_index) = {
            let byte = 0x000000FF & b;
            let second = byte % 16;
            let first = (byte - second) / 16;
            (first,second)
        };
        let (char_ic_index,tag_field) = {
            let full = 0x000000FF & c;
            let second = full % 4;
            let first = (full - second) / 4;
            (first,second)
        };
        let remainder = 0x000000FF & d;

        FInfoEntry {
            char,width_index,height_index,depth_index,char_ic_index,tag_field,remainder
        }
    }
}

struct FInfoEntry {
    char: u8,
    width_index: u8,
    height_index: u8,
    depth_index: u8,
    char_ic_index: u8,
    tag_field: u8,
    remainder: u8
}
impl FInfoEntry {
    pub fn ligature(&self,ligs:&Vec<(bool,u16,bool,u16)>) -> Vec<(u8,u8)> {
        match self.tag_field {
            1 => {
                let mut i = self.remainder as usize;
                let mut ret : Vec<(u8,u8)> = vec!();
                loop {
                    let e = ligs.get(i);
                    match e {
                        Some((stop, nc, false, rep)) => {
                            ret.push((*nc as u8,*rep as u8));
                            if *stop {return ret} else {i += 1}
                        }
                        Some((false,_,_,_)) => i += 1,
                        _ => return ret
                    }
                }
            }
            2 => vec!(),
            3 => vec!(),
            _ => vec!()
        }
    }
}