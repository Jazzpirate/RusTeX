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
use std::path::PathBuf;
use std::sync::Arc;
use lazy_static::lazy_static;
use crate::debug_log;
use crate::utils::HMap;

#[derive(Debug,Copy,Clone)]
pub struct SourceReference<FileId:Copy> {
    pub file: FileId,
    pub line: usize,
    pub column: usize
}

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
            else {return KpseResult{path:pb.join(".tex"),local:true} }
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