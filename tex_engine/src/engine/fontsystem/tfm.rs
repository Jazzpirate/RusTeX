use std::collections::BTreeMap;
use std::io::Read;
use std::path::PathBuf;

#[derive(Debug)]
pub struct TfmFile {
    pub hyphenchar:u8,
    pub skewchar:u8,
    pub dimen:Vec<f64>,
    pub size : i64,
    pub typestr : String,
    pub defined:[bool;256],
    pub widths:[f64;256],
    pub heights: [f64;256],
    pub depths: [f64;256],
    pub ics:[f64;256],
    pub ligs:BTreeMap<(u8,u8),u8>,
    pub filepath:PathBuf,
}

impl PartialEq for TfmFile {
    fn eq(&self, other: &Self) -> bool {
        self.filepath == other.filepath
    }
}

impl TfmFile {

    pub fn name(&self) -> &str {
        self.filepath.file_stem().unwrap().to_str().unwrap()
    }
    pub fn new(pb : PathBuf) -> TfmFile {
        let filepath = pb; // ec-lmr10: 102+41 = 5???
        let mut state = FontState::new(&filepath);
        //state.ret.reverse();

        let hyphenchar : u8 = 45;
        let skewchar : u8 = 255;
        let mut dimen = vec!();
        let mut size: i64 = 65536;
        let mut typestr = "".to_string();
        let mut widths: [f64;256] = [0.0;256];
        let mut heights: [f64;256] = [0.0;256];
        let mut depths: [f64;256] = [0.0;256];
        let mut ics: [f64;256] = [0.0;256];
        let mut defined = [false;256];
        let mut ligs:BTreeMap<(u8,u8),u8> = BTreeMap::new();

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

        let mut lig_kerns: Vec<(u8, u8, u8, u8)> = vec!();
        for _ in 0..nl {
            let (a,b,c,d) = state.pop();
            lig_kerns.push((a, b, c, d));
        }
        state.skip(nk + ne);
        assert_eq!(state.i,lh + 6 + (ec-bc+1) + nw + nh + nd + ni + nl + nk + ne);
        if np > 0 {
            dimen.push(state.read_float());
        }
        for _ in 2..(np+1) {
            dimen.push(state.read_float());
        }

        let factor = match dimen.get(5) {
            Some(p) if *p == 0.0 => 1.0,
            None => 1.0,
            Some(f) => *f
        };

        for t in finfo_table {
            defined[t.char as usize] = true;
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
            t.ligature(&lig_kerns, &mut ligs)
        }
        assert_eq!(state.i,lf);

        TfmFile {
            hyphenchar,skewchar,dimen,size,typestr,widths,
            heights,depths,ics,ligs,filepath,defined
        }
    }
}

struct FontState {
    ret : std::io::BufReader<std::fs::File>,
    buf:[u8;4],
    pub i:usize
}
impl FontState {
    fn new(path:&std::path::Path) -> Self {
        let file = std::fs::File::open(path).unwrap();
        let ret = std::io::BufReader::new(file);
        Self {
            ret,buf:[0,0,0,0],i:0
        }
    }
    fn read_word(&mut self) {
        self.ret.read(&mut self.buf).unwrap();
        self.i += 1;
    }
    fn pop(&mut self) -> (u8,u8,u8,u8) {
        self.i += 1;
        self.ret.read(&mut self.buf).unwrap();
        (self.buf[0],self.buf[1],self.buf[2],self.buf[3])
    }
    fn read_int(&mut self) -> (u16,u16) {
        self.read_word();
        let i1 = ((self.buf[0] as u16) << 8) | (self.buf[1] as u16);
        let i2 = ((self.buf[2] as u16) << 8) | (self.buf[3] as u16);
        (i1,i2)
    }
    fn read_float(&mut self) -> f64 {
        self.read_word();
        let int = ((self.buf[0] as i32) << 24) | ((self.buf[1] as i32) << 16) |
            ((self.buf[2] as i32) << 8) | (self.buf[3] as i32);
        let f = ((int & 0x7fffffff) as f64) / ((1 << 20) as f64);
        if int < 0 {-f} else {f}
    }
    fn skip(&mut self, len:usize) {
        for _ in 0..len {
            self.read_word();
        }
    }
    fn read_fifo(&mut self,char:u8) -> FInfoEntry {
        self.read_word();
        let width_index = self.buf[0];
        let (height_index,depth_index) = {
            let byte = self.buf[1];
            let first = (0b11110000 & byte) >> 4;
            let second = 0b00001111 & byte;
            (first,second)
        };
        let (char_ic_index,tag_field) = {
            let byte = self.buf[2];
            let first = (0b11111100 & byte) >> 2;
            let second = 0b00000011 & byte;
            (first,second)
        };
        let remainder = self.buf[3];

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
    pub fn ligature(&self,ligs:&Vec<(u8,u8,u8,u8)>,map: &mut BTreeMap<(u8,u8),u8>) {
        if self.tag_field == 1 {
            let mut i = self.remainder as usize;
            match ligs.get(i).copied() {
                Some((mut skip,mut next,mut op,mut rem)) => {
                    if skip > 128 {
                        i = (256 * (op as usize)) + (rem as usize);
                        match ligs.get(i).copied() {
                            Some((s,n,o,r)) => {
                                skip = s;next = n;op = o;rem = r;
                            }
                            None => todo!("should not be able to happen")
                        }
                    }
                    loop {
                        if op < 128 { map.insert((self.char, next), rem); }
                        if skip >= 128 { return } else {
                            i += 1 + (skip as usize);
                            match ligs.get(i).copied() {
                                Some((s,n,o,r)) => {
                                    skip = s;next = n;rem = r;op = o;
                                }
                                None => todo!("should not be able to happen")
                            }
                        }
                    }
                }
                None => todo!("should not be able to happen")
            }
        }
    }
}