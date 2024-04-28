use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::path::Path;
use std::io::Write;

static GLYPH_MAP : &[u8] = include_bytes!("src/resources/glyphmap.txt");
static PATCHES: &str = include_str!("src/resources/patches.md");
//static MISSING : &str = include_str!("src/resources/missing.txt");

struct State {
    glyphlist: String,
    glyphnames: String,
    glyphmap: phf_codegen::Map<String>,
    glyphlookup: phf_codegen::Map<String>,
    done_glyphs: Vec<String>,
    build_glyphlist:Vec<String>,
    idx: u16,
    build_map:HashMap<String,u16>,
    reader: std::io::Lines<std::io::BufReader<&'static [u8]>>
}

impl State {
    fn go() -> State {
        let mut st = State {
            glyphlist: "[\"???\"".to_string(),
            glyphnames: "[\".notdef\"".to_string(),
            glyphmap: phf_codegen::Map::new(),
            glyphlookup: phf_codegen::Map::new(),
            done_glyphs: vec!(),
            idx: 0u16,
            build_map: HashMap::new(),
            reader: std::io::BufReader::new(GLYPH_MAP).lines(),
            build_glyphlist: vec!(".notdef".to_string())
        };
        st.build_map.insert(".notdef".to_string(),0);
        // known unknowns
        /*for m in MISSING.split(' ') {
            st.glyphmap.entry(m.to_string(),"0");
        }*/

        while let Some(Ok(line)) = st.reader.next() {
            st.do_line(line)
        }
        
        st
    }

    fn do_line(&mut self,line:String) {
        self.idx += 1;
        let split = line.find(' ').unwrap();
        let glyphname = line[..split].to_string();
        let mut glyphcode = line[split + 1..].to_string();
        if glyphcode.is_empty() {
            match self.reader.next() {
                Some(Ok(s)) if s.is_empty() => glyphcode = "\\n".to_string(),
                Some(Ok(s)) => {
                    self.do_name_code(glyphname,glyphcode);
                    return self.do_line(s);
                }
                _ => unreachable!()
            }
        }
        self.do_name_code(glyphname,glyphcode);
    }
    fn do_name_code(&mut self,glyphname:String,glyphcode:String) {
        if !self.done_glyphs.contains(&glyphcode) {
            self.done_glyphs.push(glyphcode.clone());
            self.glyphlookup.entry(glyphcode.clone(), &format!("{}", self.idx));
        }
        self.glyphlist.push_str(&format!(",{:?}", glyphcode));
        self.glyphnames.push_str(&format!(",{:?}", glyphname));
        self.build_map.insert(glyphname.clone(), self.idx);
        self.build_glyphlist.push(glyphname.clone());
        self.glyphmap.entry(glyphname, &format!("{}", self.idx));
    }
}


#[derive(Copy,Clone,Debug,PartialEq,Eq,Default)]
pub struct ModifierSeq {
    blackboard:bool,
    fraktur:bool,
    script:bool,
    bold:bool,
    capitals:bool,
    monospaced:bool,
    italic:bool,
    oblique:bool,
    sans_serif:bool,
}

#[derive(Clone,Debug)]
struct FontMap {
    modifiers: ModifierSeq,
    table:Option<String>,
    fontmap:Option<(String,String)>
}
struct TableParser{
    s:&'static str,
    map:HashMap<String,FontMap>,
    tables:Vec<(String,[String;256])>,
    glyphmap:HashMap<String,u16>,
    glyphlist:Vec<String>
}
impl TableParser {
    fn get(glyphmap:HashMap<String,u16>,glyphlist:Vec<String>) -> Self {
        let mut patch =  PATCHES.trim_start();
        patch = patch[patch.find("<!--- START -->").unwrap() + 15..].trim_start();
        patch = &patch[patch.find('\n').unwrap()..].trim_start();
        patch = &patch[patch.find('\n').unwrap()..].trim_start();
        assert!(patch.starts_with('|'),"???: {}",patch);
        let mut ret = TableParser{s:patch,map:HashMap::default(),tables:vec!(),glyphmap,glyphlist};
        loop {
            if !ret.read_line() {break}
        }
        assert!(ret.s.starts_with("<!--- END -->"));

        ret.s = ret.s[ret.s.find("<!--- START -->").unwrap() + 15..].trim_start();
        loop {
            if !ret.read_table() {break}
        }
        assert!(ret.s.starts_with("<!--- END -->"));
        ret
    }
    fn read_table(&mut self) -> bool {
        if !self.s.starts_with('-') {return false}
        self.s = &self.s[1..].trim_start();
        let idx = self.s.find('\n').unwrap();
        let (mut name,rest) = self.s.split_at(idx);
        if name.ends_with('\r') {name = &name[..name.len()-1]}
        self.s = rest[1..].trim_start();
        assert!(self.s.starts_with('|'));
        self.s = self.s[self.s.find('\n').unwrap()..].trim_start();
        self.s = self.s[self.s.find('\n').unwrap()..].trim_start();
        assert!(self.s.starts_with('|'));
        let mut table = array_init::array_init::<_,String,256>(|_| String::new());
        for i in 0usize..16 {
            self.skip_field();
            for j in 0usize..16 {
                table[i*16 + j] = self.read_glyph();
            }
            self.s = self.s[1..].trim_start();
        }
        self.tables.push((name.to_string(),table));
        true
    }
    fn read_glyph(&mut self) -> String {
        assert!(self.s.starts_with('|'));
        self.s = &self.s[1..].trim_start();

        if self.s.starts_with('|') {return "UNDEFINED".to_string()}
        if self.s.starts_with("\\u") {
            let u = u32::from_str_radix(&self.s[2..6],16).unwrap();
            self.s = &self.s[6..].trim_start();
            assert!(self.s.starts_with('|'));
            return format!("Glyph(GlyphI::Unicode({:?}))",char::from_u32(u).unwrap());
        }
        if self.s.starts_with('/') {
            let idx = self.s.find('|').unwrap();
            let (glyph,rest) = self.s.split_at(idx);
            self.s = rest;
            let i = self.glyphmap.get(glyph).unwrap();
            return format!("Glyph(GlyphI::S({}))",i)
        }
        if self.s.starts_with('`') {
            self.s = &self.s[1..];
            let mut ret = String::new();
            loop {
                let n = self.s.chars().next().unwrap();
                self.s = &self.s[n.len_utf8()..];
                if n == '`' {
                    self.s = self.s.trim_start();
                    assert!(self.s.starts_with('|'));
                    if ret.chars().count() == 1 {
                        return format!("Glyph(GlyphI::Unicode({:?}))",ret.chars().next().unwrap())
                    }
                    let i = self.glyphlist.iter().enumerate().find(|p| p.1 == &ret).unwrap().0;
                    return format!("Glyph(GlyphI::S({}))",i);
                }
                if n == '\\' {
                    let n = self.s.chars().next().unwrap();
                    ret.push(n);
                    self.s = &self.s[n.len_utf8()..];
                } else {
                    ret.push(n);
                }
            }
        }
        panic!("Unexpected glyph in table: {}",self.s);
    }
    fn skip_field(&mut self) {
        assert!(self.s.starts_with('|'));
        self.s = &self.s[1..];
        let idx = self.s.find('|').unwrap();
        self.s = &self.s[idx..];
    }
    fn read_line(&mut self) -> bool {
        if !self.s.starts_with('|') {return false}
        self.s = &self.s[1..];
        let idx = self.s.find('|').unwrap();
        let (names,rest) = self.s.split_at(idx);
        self.s = rest[1..].trim_start();
        let names = names.split(',').map(|d| d.trim().to_string()).collect::<Vec<_>>();

        let mut modifiers = ModifierSeq::default();
        let idx = self.s.find('|').unwrap();
        let (mods,rest) = self.s.split_at(idx);
        self.s = rest[1..].trim_start();
        for b in mods.trim().as_bytes() { match b {
            b'B' => modifiers.blackboard = true,
            b'f' => modifiers.fraktur = true,
            b'S' => modifiers.script = true,
            b'b' => modifiers.bold = true,
            b'c' => modifiers.capitals = true,
            b'm' => modifiers.monospaced = true,
            b'i' => modifiers.italic = true,
            b'o' => modifiers.oblique = true,
            b's' => modifiers.sans_serif = true,
            _ => panic!("Unexpected modifier in patches.md: {}, {}",b,self.s)
        }}

        let idx = self.s.find('|').unwrap();
        let (table,rest) = self.s.split_at(idx);
        self.s = rest[1..].trim_start();
        let table = table.trim();
        let table = if table.is_empty() {None} else {Some(table.to_string())};

        let idx = self.s.find('|').unwrap();
        let (font,rest) = self.s.split_at(idx);
        self.s = rest[1..].trim_start();
        let font = font.trim();
        let fontmap = if font.is_empty() {None} else {
            let idx = font.rfind(char::is_whitespace).unwrap();
            let (name,link) = font.split_at(idx);
            Some((name.trim().to_string(),link.trim().to_string()))
        };

        let map = FontMap{modifiers,table,fontmap};
        for n in names {
            self.map.insert(n,map.clone());
        }

        true
    }
}

fn main() {
    let st = State::go();

    let path = Path::new(&std::env::var("OUT_DIR").unwrap()).join("codegen.rs");
    let mut file = std::io::BufWriter::new(File::create(path).unwrap());
    writeln!(file,"use crate::glyphs::{{GlyphI,UNDEFINED}};").unwrap();
    writeln!(&mut file,
        "static GLYPH_LIST: [&'static str;{}] = {}];",
        st.idx+1,st.glyphlist
    ).unwrap();
    writeln!(&mut file,
      "static GLYPH_NAMES: [&'static str;{}] = {}];",
      st.idx+1,st.glyphnames
    ).unwrap();
    writeln!(
        &mut file,
        "static GLYPH_MAP: phf::Map<&'static str, u16> = {};",
        st.glyphmap.build()
    ).unwrap();
    writeln!(
        &mut file,
        "static GLYPH_LOOKUP: phf::Map<&'static str, u16> = {};",
        st.glyphlookup.build()
    ).unwrap();

    let parser = TableParser::get(st.build_map,st.build_glyphlist);

    let path = Path::new(&std::env::var("OUT_DIR").unwrap()).join("codegen_patch.rs");
    let mut patchfile = std::io::BufWriter::new(File::create(path).unwrap());
    writeln!(&mut patchfile,"{{").unwrap();
    for (name,val) in parser.map.into_iter() {
        writeln!(&mut patchfile,"patch(&mut map,\"{}\",{:?},{:?},{:?});",name,
               val.modifiers,
               val.table.map(|n| parser.tables.iter().enumerate().find(|p| p.1.0 == n).unwrap().0),
               val.fontmap
        ).unwrap();
    }
    writeln!(&mut patchfile,"}}").unwrap();

    let tbl = &parser.tables.iter().find(|p| p.0 == "PostScript Standard Encoding").unwrap().1;
    writeln!(&mut file,"const STANDARD_ENCODING: GlyphList = GlyphList([").unwrap();
    for i in 0..256 {
        writeln!(&mut file,"    {},",tbl[i]).unwrap();
    }
    writeln!(&mut file,"]);").unwrap();

    writeln!(&mut file,"const PATCHED_TABLES: [GlyphList;{}] = [",parser.tables.len()).unwrap();
    for (_,table) in parser.tables.into_iter() {
        writeln!(&mut file,"    GlyphList([").unwrap();
        for i in 0..256 {
            writeln!(&mut file,"        {},",table[i]).unwrap();
        }
        writeln!(&mut file,"    ]),").unwrap();
    }
    writeln!(&mut file,"];").unwrap();

    //panic!("Here: Maps: {:?}\n\nTables: {:?}",parser.map,parser.tables);
    //panic!("Here: {}", glyphmap.display());

}