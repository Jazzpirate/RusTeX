use crate::parsing::Parser;

pub struct VFGlyphMap {
    pub deps: Vec<Box<str>>,
    pub maps: [Vec<(u8, u8)>; 256],
}

#[allow(clippy::cast_possible_truncation)]
pub fn parse_vf(f: &str) -> Option<VFGlyphMap> {
    let disas = match std::str::from_utf8(
        std::process::Command::new("vftovp")
            .args(vec![f])
            .output()
            .expect("vftovp not found!")
            .stdout
            .as_slice(),
    ) {
        Ok(s) => s.trim().to_string(),
        _ => return None,
    };
    if disas.is_empty() {
        return None;
    }
    let mut s = Parser::new(&disas);
    let mut deps: Vec<Box<str>> = Vec::new();
    while s.0.contains("(MAPFONT") {
        s.read_until_str("(MAPFONT");
        let idx = s.read_enc_num() as usize;
        s.read_until_str("(FONTNAME");
        let name = s.read_until(')');
        if deps.len() <= idx {
            deps.resize(idx + 1, "".into());
        }
        deps[idx] = name.into();
        s.read_until_parens();
    }
    let mut maps = array_init::array_init::<_, Vec<(u8, u8)>, 256>(|_| Vec::new());
    while s.0.contains("(CHARACTER") {
        let mut font = 0u8;
        s.read_until_str("(CHARACTER");
        let idx = s.read_enc_num() as usize;
        let mut had_map = false;
        'B: while !s.is_empty() {
            if s.drop(")") {
                break;
            }
            if s.drop("(CHARWD")
                || s.drop("(CHARHT")
                || s.drop("(CHARDP")
                || s.drop("(CHARIC")
                || s.drop("(COMMENT")
                || s.drop("(NEXTLARGER")
                || s.drop("(VARCHAR")
            {
                s.read_until_parens();
                continue;
            }
            if s.drop("(MAP") {
                had_map = true;
                while !s.is_empty() {
                    if s.drop(")") {
                        continue 'B;
                    }
                    if s.drop("(SELECTFONT") {
                        font = s.read_enc_num();
                        s.read_until_parens();
                        continue;
                    }
                    if s.drop("(SETCHAR") {
                        let num = s.read_enc_num();
                        s.read_until_parens();
                        maps[idx].push((font, num));
                        had_map = true;
                        continue;
                    }
                    if s.drop("(SETRULE")
                        || s.drop("(SPECIAL")
                        || s.drop("(MOVEDOWN")
                        || s.drop("(MOVERIGHT")
                        || s.drop("(PUSH")
                        || s.drop("(POP")
                    {
                        s.read_until_parens();
                        continue;
                    }
                    return None;
                }
            }
            return None;
        }
        if !had_map {
            maps[idx].push((font, idx as u8))
        }
    }
    Some(VFGlyphMap { deps, maps })
}
