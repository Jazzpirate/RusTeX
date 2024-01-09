/*! An implementation of (the central part of) kpathsea, the path searching library used by TeX.
Used by instantiating a [Kpathsea] instance with the working directory.


**Example:**
```rust
use tex_engine::engine::filesystem::kpathsea::Kpathsea;
let kpse = Kpathsea::new(std::env::current_dir().unwrap());
assert!(kpse.kpsewhich("latex.ltx").path.to_str().unwrap().ends_with("tex/latex/base/latex.ltx"));
assert!(kpse.kpsewhich("article.cls").path.to_str().unwrap().ends_with("tex/latex/base/article.cls"));
// as expected, the `.tex` file extension is optional:
assert!(kpse.kpsewhich("expl3-code").path.to_str().unwrap().ends_with("tex/latex/l3kernel/expl3-code.tex"));
```
*/

use std::collections::hash_map::Entry;
use std::fmt::Debug;
use std::fs::File;
use std::io::BufRead;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use lazy_static::lazy_static;
use path_dedot::ParseDot;
use crate::debug_log;
use crate::tex::control_sequences::ResolvedCSName;
use crate::utils::HMap;

/*
/** A "database" of paths to search for files. Notably, the "global" part (e.g. the system-wide
`TEXINPUTS`, `TEXMF`, etc.) is shared between all instances of [`Kpathsea`].
and lazily computed on first use.
**/
#[derive(Clone)]
pub struct Kpathsea {pub pwd:PathBuf,pub local:Box<[PathBuf]>,pub global:Arc<KpathseaBase>}
impl Kpathsea {
    /// Create a new [`Kpathsea`] instance with the given working directory.
    pub fn new(pwd:PathBuf) -> Kpathsea {
        let global = KPATHSEA.clone();
        let mut local:Vec<PathBuf> = vec!();
        KpathseaBase::fill_set(&mut local,pwd.clone(),global.recdot);
        Kpathsea { pwd, local:local.into(), global }
    }

    /// Search for a file in the database.
    pub fn kpsewhich<S:AsRef<str>>(&self,filestr:S) -> KpseResult {
        let filestr = filestr.as_ref();
        if filestr.starts_with("|kpsewhich") {
            return KpseResult{path:self.pwd.join(filestr),local:true}
        }
        if filestr.starts_with("nul:") && cfg!(target_os = "windows") {
            return KpseResult{path:PathBuf::from(filestr),local:false}
        } else if filestr.starts_with("nul:") {
            return KpseResult{path:self.pwd.join(filestr),local:false}
        } else if filestr.is_empty() {
            todo!("Empty string in kpsewhich")
        }
        let pb = PathBuf::from(filestr);
        if pb.is_absolute() {
            if pb.is_file() {return KpseResult{path:pb,local:false} }
            else {return KpseResult{path:PathBuf::from(format!("{}.tex",filestr)),local:true} }
        }
        for l in &*self.local {
            let mut p = l.join(filestr);
            if p.is_file() { return KpseResult{path:p,local:true} }
            p = l.join(format!("{}.tex",filestr));
            if p.is_file() { return KpseResult{path:p,local:true} }
        }
        for l in &*self.global.set {
            let mut p = l.join(filestr);
            if p.is_file() { return KpseResult{path:p,local:false} }
            p = l.join(format!("{}.tex",filestr));
            if p.is_file() { return KpseResult{path:p,local:false} }
        }
        KpseResult{path:self.pwd.join(filestr),local:true}
    }

    /// Get the contents of a file in the database.
    pub fn get(&self,path:&PathBuf) -> Option<Vec<u8>> {
        match path.file_name().map(|s| s.to_str()).flatten() {
            Some(filename) if filename.starts_with("|kpsewhich ") => {
                let pwd = self.pwd.to_str().unwrap();
                let out = if cfg!(target_os = "windows") {
                    std::process::Command::new("cmd").current_dir(&pwd).env("PWD",&pwd).env("CD",&pwd).args(&["/C",&filename[1..]])//args.collect::<Vec<&str>>())
                        .output().expect("kpsewhich not found!")
                        .stdout
                } else {
                    let args = filename[11..].split(" ");
                    std::process::Command::new("kpsewhich").current_dir(&pwd).env("PWD",&pwd).env("CD",&pwd).args(args.collect::<Vec<&str>>())
                        .output().expect("kpsewhich not found!")
                        .stdout
                };
                Some(out)
            }
            _ => std::fs::read(path).ok()
        }
    }
}

lazy_static! {
    pub static ref KPATHSEA : Arc<KpathseaBase> = Arc::new(KpathseaBase::new());
}

/// The result of a [`Kpathsea`] search.
pub struct KpseResult {
    /// The path to the file.
    pub path : PathBuf,
    /// Whether the file was found in the "local" part (i.e. the PWD) of the database.
    /// Note that if the file does not exist, this is always `true`.
    pub local : bool
}

#[derive(Clone,Debug)]
pub struct KpathseaBase {
    pub set: Box<[PathBuf]>,
    recdot:bool
}

impl KpathseaBase {
    pub fn which<S:AsRef<str>>(&self,filestr:S) -> Option<PathBuf> {
        let pb = PathBuf::from(filestr.as_ref());
        let filestr = filestr.as_ref();
        if pb.is_absolute() {
            if pb.is_file() {return Some(pb) }
            else {return Some(pb) }
        }
        for l in self.set.iter() {
            let mut p = l.join(filestr);
            if p.is_file() { return Some(p) }
            p = l.join(format!("{}.tex",filestr));
            if p.is_file() { return Some(p) }
        }
        None
    }
    /*pub fn get(&self,filestr:&str) -> Option<KpseResult> {
        for l in &*self.set {
            let mut p = l.join(filestr);
            if p.exists() { return Some(KpseResult{path:p,local:false}) }
            p = l.join(format!("{}.tex",filestr));
            if p.exists() { return Some(KpseResult{path:p,local:true}) }
        }
        None
    }*/
    fn new() -> KpathseaBase {
        debug_log!(debug=>"Initializing kpathsea database");
        let mut vars = HMap::<String,String>::default();
        let loc = std::str::from_utf8(std::process::Command::new("kpsewhich")
            .args(vec!("-var-value","SELFAUTOLOC")).output().expect("kpsewhich not found!")
            .stdout.as_slice()).unwrap().trim().to_string();
        let selfautoloc = PathBuf::from(loc);
        match selfautoloc.parent() {
            Some(p) => {
                vars.insert("SELFAUTODIR".to_string(),p.to_str().unwrap().to_string());
                match p.parent() {
                    Some(pp) => {
                        vars.insert("SELFAUTOPARENT".to_string(),pp.to_str().unwrap().to_string());
                        match pp.parent() {
                            Some(ppp) => {vars.insert("SELFAUTOGRANDPARENT".to_string(),ppp.to_str().unwrap().to_string());},
                            _ => {vars.insert("SELFAUTOGRANDPARENT".to_string(),pp.to_str().unwrap().to_string());}
                        }
                    }
                    None => {
                        vars.insert("SELFAUTOPARENT".to_string(),p.to_str().unwrap().to_string());
                        vars.insert("SELFAUTOGRANDPARENT".to_string(),p.to_str().unwrap().to_string());
                    }
                }
            },
            _ => {
                vars.insert("SELFAUTODIR".to_string(),selfautoloc.to_str().unwrap().to_string());
                vars.insert("SELFAUTOPARENT".to_string(),selfautoloc.to_str().unwrap().to_string());
                vars.insert("SELFAUTOGRANDPARENT".to_string(),selfautoloc.to_str().unwrap().to_string());
            }
        }
        vars.insert("SELFAUTOLOC".to_string(),selfautoloc.to_str().unwrap().to_string());
        let rs : Vec<String> = std::str::from_utf8(std::process::Command::new("kpsewhich")
            .args(vec!("-a","texmf.cnf")).output().expect("kpsewhich not found!")
            .stdout.as_slice()).unwrap().split("\n").map(|x| x.trim().to_string()).filter(|s| !s.is_empty()).collect();
        for r in rs {
            let p = PathBuf::from(r);
            if p.exists() {
                let lines : Vec<String> = std::str::from_utf8(std::fs::read(p).unwrap().as_slice()).unwrap().split("\n").map(|x| x.trim().to_string()).collect();
                for l in lines {
                    if !l.starts_with("%") && !l.is_empty() {
                        let mut kb : Vec<String> = l.split("=").map(|x| x.trim().to_string()).collect();
                        if kb.len() == 2 {
                            let v = kb.pop().unwrap();
                            let k = kb.pop().unwrap();
                            match vars.entry(k) {
                                Entry::Occupied(_) => (),
                                Entry::Vacant(e) => { e.insert(v); }
                            }
                        }
                    }
                }
            }
        }
        let filestrs : Vec<String> = vec!(
            std::env::vars().find(|a| a.0 == "TEXINPUTS").map(|x| x.1.clone()),
            vars.get("TEXINPUTS").map(|x| x.clone()),
            vars.get("VARTEXFONTS").map(|x| x.clone()),
            vars.get("VFFONTS").map(|x| x.clone()),
            vars.get("TFMFONTS").map(|x| x.clone()),
            vars.get("T1FONTS").map(|x| x.clone()),
            vars.get("ENCFONTS").map(|x| x.clone()),
        ).into_iter().flatten().collect();
        vars.insert("progname".to_string(),"pdflatex".to_string());
        let home = if cfg!(target_os = "windows") {
            std::env::vars().find(|x| x.0 == "HOMEDRIVE").unwrap().1 +
                &std::env::vars().find(|x| x.0 == "HOMEPATH").unwrap().1
        } else {
            std::env::vars().find(|x| x.0 == "HOME").unwrap().1
        };
        let mut recdot = false;
        let dirs : Vec<String> = filestrs.into_iter().map(|x| KpathseaBase::parse_string(x, &vars)).flatten().collect();
        let mut paths : Vec<(PathBuf,bool)> = vec!();
        for mut d in dirs {
            if d.starts_with(".") {
                if d == ".//" {
                    recdot = true
                }
            }
            else {
                let mut recurse : bool = false;
                if d.starts_with("~") {
                    d = home.clone() + &d[1..]
                }
                if d.ends_with("//") {
                    d.pop();d.pop();
                    recurse = true
                }
                if !d.trim().is_empty() {
                    let pb = PathBuf::from(d.trim());
                    if pb.exists() && ! paths.contains(&(pb.clone(),recurse)) {
                        paths.push((pb,recurse))
                    }
                }
            }
        }
        let mut set: Vec<PathBuf> = vec!();
        for (path,recurse) in paths.into_iter() { KpathseaBase::fill_set(&mut set, path, recurse) }
        KpathseaBase { set:set.into(),recdot }
    }

    fn fill_set(set: &mut Vec<PathBuf>, path : PathBuf, recurse: bool) {
        if path.is_dir() && !path.ends_with(".git") {
            set.push(path.clone());
            if recurse {
                for entry in std::fs::read_dir(path).unwrap() {
                    let p = entry.unwrap().path();
                    KpathseaBase::fill_set(set, p, recurse)
                }
            }
        }
    }

    fn parse_string(s : String,vars:&HMap<String,String>) -> Vec<String> {
        let mut fin : Vec<String> = vec!();
        let mut ret : Vec<String> = vec!("".to_string());
        let mut i : usize = 0;
        let chars : Vec<char> = s.chars().collect();
        loop {
            match chars.get(i) {
                None => {
                    fin.append(&mut ret);
                    return fin
                },
                Some(';' | ':') => {
                    i += 1;
                    fin.append(&mut std::mem::take(&mut ret));
                    ret.push("".to_string())
                },
                Some('$') => {
                    i += 1;
                    let mut varname : String = "".to_string();
                    loop {
                        match chars.get(i) {
                            Some(c) if c.is_ascii_alphabetic() => {
                                i += 1;
                                varname.push(*c)
                            },
                            _ => break
                        }
                    }
                    match vars.get(&varname) {
                        None => panic!("unknown variable name"),
                        Some(s) => {
                            let rets = KpathseaBase::parse_string(s.clone(), vars);
                            let nrets = std::mem::take(&mut ret);
                            for o in nrets { for r in &rets { ret.push(o.clone() + r) }}
                        }
                    }
                },
                Some('!') => i += 1,
                Some('{') => {
                    i += 1;
                    let mut rets : Vec<String> = vec!("".to_string());
                    let mut inbracks : u8 = 0;
                    loop {
                        match chars.get(i) {
                            Some(',') if inbracks == 0 => {
                                i += 1;
                                rets.push("".to_string())
                            },
                            Some('{') => {
                                i += 1;
                                inbracks += 1
                            },
                            Some('}') if inbracks == 0 => {
                                i += 1;
                                break
                            }
                            Some('}') => {
                                i += 1;
                                inbracks -= 1
                            }
                            Some(c) => {
                                i += 1;
                                rets.last_mut().unwrap().push(*c)
                            }
                            None => panic!("Syntax error in texmf.cnf")
                        }
                    }
                    let allrets : Vec<String> = rets.into_iter().map(|x| KpathseaBase::parse_string(x, vars)).flatten().collect();
                    let nrets = std::mem::take(&mut ret);
                    for o in nrets { for r in &allrets { ret.push(o.clone() + r) }}
                },
                Some(c) => {
                    i += 1;
                    for s in &mut ret { s.push(*c) }
                }
            }
        }
    }
}
*/





// ------------------------------------------------------------------------------------------





/// The result of a [`Kpathsea`] search.
pub struct KpseResult {
    /// The path to the file.
    pub path : PathBuf,
    /// Whether the file exists.
    pub exists : bool
}

lazy_static! {
    pub static ref KPATHSEA : Arc<KpathseaBase> = Arc::new(KpathseaBase::new());
}

/** A "database" of paths to search for files. Notably, the "global" part (e.g. the system-wide
`TEXINPUTS`, `TEXMF`, etc.) is shared between all instances of [`Kpathsea`].
and lazily computed on first use.
 **/
#[derive(Clone)]
pub struct Kpathsea {pub pwd:PathBuf,pub local:HMap<String,PathBuf>,pub global:Arc<KpathseaBase>}
impl Kpathsea {
    /// Create a new [`Kpathsea`] instance with the given working directory.
    pub fn new(pwd:PathBuf) -> Kpathsea {
        let global = KPATHSEA.clone();
        let local = get_dot(global.recdot,&pwd);
        Self { pwd, local, global }
    }

    /// Search for a file in the database.
    pub fn kpsewhich<S:AsRef<str>>(&self,filestr:S) -> KpseResult {
        use path_dedot::*;
        let filestr = filestr.as_ref();
        if filestr.starts_with("|kpsewhich") {
            return KpseResult{path:self.pwd.join(filestr),exists:true}
        }
        if filestr.starts_with("nul:") && cfg!(target_os = "windows") {
            return KpseResult{path:PathBuf::from(filestr),exists:true}
        } else if filestr.starts_with("nul:") {
            return KpseResult{path:self.pwd.join(filestr),exists:false}
        } else if filestr.is_empty() {
            todo!("Empty string in kpsewhich")
        }
        if Path::new(filestr).is_absolute() {
            if Path::new(filestr).is_file() {return KpseResult{path:PathBuf::from(&filestr),exists:true} }
            let pb = PathBuf::from(filestr.to_string() + ".tex");
            if pb.is_file() {return KpseResult{path:pb,exists:true} }
            return KpseResult{path:PathBuf::from(&filestr),exists:false}
            //else {return KpseResult{path:PathBuf::from(format!("{}.tex",filestr)),exists:false} }
        }
        match self.global.pre.get(filestr) {
            Some(p) => return KpseResult{path:p.clone(),exists:true},
            None => ()
        }
        match self.local.get(filestr) {
            Some(p) => return KpseResult{path:p.clone(),exists:true},
            None => ()
        }
        match self.global.post.get(filestr) {
            Some(p) => return KpseResult{path:p.clone(),exists:true},
            None => ()
        }
        let p = self.pwd.join(Path::new(filestr)).parse_dot().unwrap().to_path_buf();
        KpseResult{exists:p.exists(),path:p}
    }
}

#[derive(Clone,Debug)]
pub struct KpathseaBase {
    pub pre: HMap<String,PathBuf>,
    pub recdot:bool,
    pub post: HMap<String,PathBuf>,
}
impl KpathseaBase {
    pub fn new() -> KpathseaBase {
        let vars = Self::get_vars();
        let home = if cfg!(target_os = "windows") {
            std::env::vars().find(|x| x.0 == "HOMEDRIVE").unwrap().1 +
                &std::env::vars().find(|x| x.0 == "HOMEPATH").unwrap().1
        } else {
            std::env::vars().find(|x| x.0 == "HOME").unwrap().1
        };

        let paths = Self::paths_to_scan(&vars);
        let mut parser = PathParser {
            vars,
            diddot:false,
            recdot:false,
            predot:vec!(),
            postdot:vec!(),
            home:PathBuf::from(home),
            resolved_vars:HMap::default()
        };

        for s in paths { parser.do_dir(&s); }
        let (pre,dot,post) = parser.close();
        KpathseaBase { pre, recdot:dot, post }
    }

    pub fn which<S:AsRef<str>>(&self,filestr:S) -> Option<PathBuf> {
        let filestr = filestr.as_ref();
        if Path::new(filestr).is_absolute() {
            return Some(PathBuf::from(filestr))
        }
        match self.pre.get(filestr) {
            Some(p) => return Some(p.clone()),
            None => ()
        }
        match self.post.get(filestr) {
            Some(p) => return Some(p.clone()),
            None => ()
        }
        None
    }

    fn get_vars() -> HMap<String,String> {
        let mut vars = HMap::<String,String>::default();
        let locout = std::process::Command::new("kpsewhich")
            .args(vec!("-var-value","SELFAUTOLOC")).output().expect("kpsewhich not found!")
            .stdout;
        let loc = std::str::from_utf8(locout.as_slice()).unwrap().trim();
        let selfautoloc = Path::new(loc);
        vars.insert("SELFAUTOLOC".to_string(),selfautoloc.to_str().unwrap().to_string());
        if let Some(autodir) = selfautoloc.parent() {
            vars.insert("SELFAUTODIR".to_string(), autodir.to_str().unwrap().to_string());
            if let Some(autoparent) = autodir.parent() {
                vars.insert("SELFAUTOPARENT".to_string(), autoparent.to_str().unwrap().to_string());
                if let Some(autograndparent) = autoparent.parent() {
                    vars.insert("SELFAUTOGRANDPARENT".to_string(), autograndparent.to_str().unwrap().to_string());
                } else {
                    vars.insert("SELFAUTOGRANDPARENT".to_string(), autoparent.to_str().unwrap().to_string());
                }
            } else {
                vars.insert("SELFAUTOPARENT".to_string(),autodir.to_str().unwrap().to_string());
                vars.insert("SELFAUTOGRANDPARENT".to_string(),autodir.to_str().unwrap().to_string());
            }
        } else {
            vars.insert("SELFAUTODIR".to_string(),selfautoloc.to_str().unwrap().to_string());
            vars.insert("SELFAUTOPARENT".to_string(),selfautoloc.to_str().unwrap().to_string());
            vars.insert("SELFAUTOGRANDPARENT".to_string(),selfautoloc.to_str().unwrap().to_string());
        }

        let out = std::process::Command::new("kpsewhich")
            .args(vec!("-a","texmf.cnf")).output().expect("kpsewhich not found!")
            .stdout;
        let outstr = std::str::from_utf8(out.as_slice()).unwrap();
        let rs = outstr.split("\n").map(|x| x.trim()).filter(|s| !s.is_empty());
        for r in rs {
            let p = Path::new(r);
            if let Ok(f) = File::open(p) {
                let lines = std::io::BufReader::new(f).lines();
                for l in lines {
                    if let Ok(l) = l {
                        if !l.starts_with("%") && !l.is_empty() {
                            let mut kb = l.split("=").map(|x| x.trim()).collect::<Vec<_>>();
                            if kb.len() == 2 {
                                let v = kb.pop().unwrap();
                                let mut k = kb.pop().unwrap();
                                if let Some(i) = k.find('.') {
                                    let pre = &k[..i];
                                    let post = &k[i+1..];
                                    if post != "pdflatex" { continue }
                                    k = pre;
                                }
                                if k.chars().any(|c| c.is_lowercase()) { continue }
                                match vars.entry(k.to_string()) {
                                    Entry::Occupied(_) => (),
                                    Entry::Vacant(e) => { e.insert(v.to_string()); }
                                }
                            }
                        }
                    }
                }
            }
        }
        vars.insert("progname".to_string(),"pdflatex".to_string());
        vars
    }

    fn paths_to_scan(vars:&HMap<String,String>) -> Vec<String> {
        let mut todo = [NamedVar("TEXINPUTS",false),NamedVar("VARTEXFONTS",false),NamedVar("VFFONTS",false),NamedVar("TFMFONTS",false),NamedVar("T1FONTS",false),NamedVar("ENCFONTS",false)];

        let mut ret = vec!();
        for (k,mut v) in std::env::vars() {
            if let Some(td) = todo.iter_mut().find(|x| x.0 == k) {
                td.1 = true;
                if v.ends_with(";") || v.ends_with(":") {
                    if let Some(oldv) = vars.get(td.0) {
                        v.push_str(oldv);
                    }
                }
                ret.push(v);
            }
        }
        for td in todo.iter().filter(|x| !x.1) {
            if let Some(v) = vars.get(td.0) {
                ret.push(v.clone());
            }
        }
        ret
    }
}

struct NamedVar(&'static str,bool);
struct PathParser {
    vars: HMap<String,String>,
    diddot: bool,
    recdot: bool,
    predot:Vec<(PathBuf,bool)>,
    postdot:Vec<(PathBuf,bool)>,
    home:PathBuf,
    resolved_vars:HMap<String,Vec<Vec<u8>>>,
}
struct StringSet(Vec<Vec<u8>>);
impl StringSet {
    fn push_string(&mut self,s:&[u8]) {
        if s.is_empty() { return }
        if self.0.is_empty() {
            self.0.push(s.to_vec())
        }
        else {
            for r in &mut self.0 { r.extend(s) }
        }
    }
    fn new() -> StringSet {
        StringSet(vec!())
    }
    fn split(&mut self,strs:&Vec<Vec<u8>>) {
        if strs.len() == 1 {
            self.push_string(&strs[0]);
            return
        }
        if strs.is_empty() {
            return
        }
        if self.0.is_empty() {
            self.0 = strs.clone();
            return
        }
        let mut ret = vec!();
        for s in &self.0 {
            for r in strs {
                let mut s = s.clone();
                s.extend(r);
                ret.push(s)
            }
        }
        self.0 = ret;
    }
    fn clear(&mut self) -> Vec<Vec<u8>> {
        std::mem::take(&mut self.0)
    }
}
impl PathParser {
    fn do_dir(&mut self,s:&str) {
        for mut s in self.parse_string(s).into_iter().map(|v| String::from_utf8(v).unwrap()) {
            let mut recurse = false;
            if s.ends_with("//") {
                recurse = true;
                s.pop();s.pop();
            }
            else if s.ends_with("/") {
                s.pop();
            }
            if s == "." {
                if self.diddot { continue }
                self.diddot = true;
                self.recdot = recurse;
                continue
            }
            self.push_path(&Path::new(&s),recurse)
        }
    }
    fn push_path(&mut self,p:&Path,rec:bool) {
        let map = if self.diddot {
            &mut self.postdot
        } else {
            &mut self.predot
        };
        for (ip,m) in map.iter_mut() {
            if ip == p {
                *m = *m || rec; return
            }
        }
        map.push((p.to_path_buf(),rec));
    }
    #[inline(always)]
    fn parse_string(&mut self,s:&str) -> Vec<Vec<u8>> {
        self.parse_bytes(s.as_bytes())
    }
    fn parse_bytes(&mut self,mut s:&[u8]) -> Vec<Vec<u8>> {
        let mut ret: Vec<Vec<u8>> = vec!();
        let mut currs = StringSet::new();
        let breaks = [b';',b':',b'$',b'!',b'{',b'~'];
        while !s.is_empty() {
            if let Some((i,b)) = s.iter().enumerate().find(|(_,c)| breaks.contains(*c)) {
                let first = &s[..i];
                currs.push_string(first);
                s = &s[i+1..];
                match b {
                    b'!' => (),
                    b'~' => currs.push_string(self.home.to_str().unwrap().as_bytes()),
                    b';' | b':' => ret.extend(currs.clear().into_iter()),
                    b'$' => {
                        let name = if let Some((i,_))= s.iter().enumerate().find(|(_,c)| !c.is_ascii_alphabetic()) {
                            let r = &s[..i];
                            s = &s[i..];
                            r
                        } else {
                            let r = s;
                            s = b"";
                            r
                        };
                        let name = std::str::from_utf8(name).unwrap();
                        let resolved = self.get_resolved_var(name);
                        currs.split(resolved);
                    }
                    b'{' => {
                        let mut inbracks = 0;
                        let mut ret = vec!(vec!());
                        while !s.is_empty() {
                            let b = s[0];
                            s = &s[1..];
                            if b == b'{' { inbracks += 1}
                            else if b == b'}' && inbracks == 0 { break }
                            else if b == b'}' { inbracks -= 1 }
                            else if b == b',' && inbracks == 0 { ret.push(vec!());continue }
                            ret.last_mut().unwrap().push(b)
                        }
                        let v = ret.into_iter().flat_map(|v| self.parse_bytes(v.as_slice()) ).collect::<Vec<_>>();
                        currs.split(&v);
                    }
                    _ => unreachable!()
                }
            } else {
                currs.push_string(s);
                break
            }
        }
        ret.extend(currs.clear());
        ret
    }
    fn get_resolved_var(&mut self,key:&str) -> &Vec<Vec<u8>> {
        if !self.resolved_vars.contains_key(key) {
            let val = self.vars.get(key).unwrap().clone();
            let resolved = self.parse_string(&val);
            self.resolved_vars.insert(key.to_string(),resolved);
        }
        self.resolved_vars.get(key).unwrap()
    }
    fn close(self) -> (HMap<String,PathBuf>,bool,HMap<String,PathBuf>) {
        (Self::close_i(self.predot),self.recdot,Self::close_i(self.postdot))
    }
    fn close_i(v:Vec<(PathBuf,bool)>) -> HMap<String,PathBuf> {
        let mut ret = HMap::default();
        for (p,rec) in v.into_iter().rev() {
            let len = p.to_str().unwrap().len() + 1;
            for e in walkdir::WalkDir::new(&p).min_depth(1).into_iter().filter_map(|e| match e.ok() {
                None => None,
                Some(s) if s.path().as_os_str().to_str().unwrap().contains(".git") => None,
                Some(e) => Some(e)
            }) {
                let sub = &e.path().to_str().unwrap()[len..];
                if sub.contains('.') {
                    let sub = sub.to_string();
                    let pb = e.path().to_path_buf();
                    if sub.ends_with(".tex") {
                        let sub = sub[..sub.len()-4].to_string();
                        ret.insert(sub,pb.clone());
                    }
                    if rec {
                        let filename = pb.file_name().unwrap().to_str().unwrap();
                        ret.insert(filename.to_string(),pb.clone());
                        if sub.ends_with(".tex") {
                            ret.insert(filename[..filename.len()-4].to_string(),pb.clone());
                        }
                    }
                    ret.insert(sub,pb);
                }
            }
        }
        ret
    }
}

fn get_dot(recdot:bool,pwd:&Path) -> HMap<String,PathBuf> {
    let mut ret = HMap::default();
    let len = pwd.to_str().unwrap().len() + 1;
    for e in walkdir::WalkDir::new(&pwd).min_depth(1).into_iter().filter_map(|e| match e.ok() {
        None => None,
        Some(s) if s.path().as_os_str().to_str().unwrap().contains(".git") => None,
        Some(e) => Some(e)
    }) {
        let sub = &e.path().to_str().unwrap()[len..];
        let sub = sub.to_string();
        let pb = e.path().to_path_buf();
        if e.file_type().is_file() {
            let filename = pb.file_name().unwrap().to_str().unwrap();
            if sub.ends_with(".tex") {
                let sub = sub[..sub.len() - 4].to_string();
                ret.insert(sub, pb.clone());
            }
            if recdot {
                ret.insert(filename.to_string(), pb.clone());
                //ret.insert(filename.to_uppercase(), pb.clone());
                if sub.ends_with(".tex") {
                    ret.insert(filename[..filename.len() - 4].to_string(), pb.clone());
                    //ret.insert(filename.to_uppercase()[..filename.len() - 4].to_string(), pb.clone());
                }
            }
            //let sub2 = sub[..sub.len() - filename.len()].to_string() + filename.to_uppercase().as_str();
            //ret.insert(sub2, pb.clone());
            ret.insert(sub, pb);
        }
    }
    ret
}