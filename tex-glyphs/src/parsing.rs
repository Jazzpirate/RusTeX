pub struct Parser<'a>(pub &'a str);
impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        Parser(s.trim_start())
    }

    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn ends_with(&self, c: char) -> bool {
        self.0.ends_with(c)
    }

    pub fn starts_with(&self, c: char) -> bool {
        self.0.starts_with(c)
    }

    pub fn starts_with_str(&self, s: &str) -> bool {
        self.0.starts_with(s)
    }

    pub fn starts_with_digit(&self) -> bool {
        self.0.starts_with(|c: char| c.is_ascii_digit())
    }

    pub fn read_digit(&mut self) -> u32 {
        let mut ret = 0u32;
        while !self.is_empty() && self.starts_with_digit() {
            ret = ret * 10
                + self
                    .0
                    .chars()
                    .next()
                    .unwrap_or_else(|| unreachable!())
                    .to_digit(10)
                    .unwrap_or_else(|| unreachable!());
            self.0 = &self.0[1..];
        }
        self.0 = self.0.trim_start();
        ret
    }

    pub fn skip(&mut self, n: usize) {
        self.0 = self.0[n..].trim_start();
    }

    pub fn drop_right(&mut self, n: usize) {
        self.0 = &self.0[..self.0.len() - n];
    }

    pub fn read_until_ws(&mut self) -> &str {
        if let Some(i) = self.0.find(char::is_whitespace) {
            let (r, rest) = self.0.split_at(i);
            self.0 = rest.trim_start();
            r
        } else {
            let r = self.0;
            self.0 = "";
            r
        }
    }
    pub fn read_until_ws_or(&mut self, c: char) -> &str {
        if let Some(i) = self.0.find(|x| x == c || char::is_whitespace(x)) {
            let (r, rest) = self.0.split_at(i);
            self.0 = rest.trim_start();
            r
        } else {
            let ret = self.0;
            self.0 = "";
            ret
        }
    }
    pub fn read_until(&mut self, c: char) -> &str {
        if let Some(i) = self.0.find(c) {
            let (r, rest) = self.0.split_at(i);
            self.0 = rest[1..].trim_start();
            r
        } else {
            let ret = self.0;
            self.0 = "";
            ret
        }
    }
    pub fn read_until_str(&mut self, s: &str) -> &str {
        if let Some(i) = self.0.find(s) {
            let (r, rest) = self.0.split_at(i);
            self.0 = rest[s.len()..].trim_start();
            r
        } else {
            let ret = self.0;
            self.0 = "";
            ret
        }
    }
    pub fn read_until_strs(&mut self, s: &[&str]) -> &str {
        for s in s {
            match self.0.find(s) {
                Some(i) => {
                    let (r, rest) = self.0.split_at(i);
                    self.0 = rest[s.len()..].trim_start();
                    return r;
                }
                None => continue,
            }
        }
        let ret = self.0;
        self.0 = "";
        ret
    }

    pub fn drop(&mut self, s: &str) -> bool {
        if self.0.starts_with(s) {
            self.skip(s.len());
            true
        } else {
            false
        }
    }

    pub fn skip_until_endline(&mut self) {
        match self.0.find('\n') {
            Some(i) => {
                self.skip(i + 1);
            }
            None => {
                self.0 = "";
            }
        }
    }

    pub fn read_until_parens(&mut self) {
        let mut parens = 0usize;
        loop {
            if let Some(i) = self.0.find(['(', ')']) {
                if self.0.chars().nth(i).unwrap_or_else(|| unreachable!()) == '(' {
                    parens += 1;
                } else if parens == 0 {
                    self.skip(i + 1);
                    return;
                } else {
                    parens -= 1;
                }
                self.skip(i + 1);
            } else {
                self.0 = "";
                return;
            }
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn read_enc_num(&mut self) -> u8 {
        if self.starts_with('D') {
            self.skip(1);
            let d = self.read_digit();
            return d as u8;
        }
        if self.starts_with('O') {
            self.skip(1);
            let d = self.read_digit();
            return u8::from_str_radix(&format!("{d}"), 8).unwrap_or_else(|_| unreachable!());
        }
        if self.starts_with('H') {
            self.skip(1);
            let d = self.read_digit();
            return u8::from_str_radix(&format!("{d}"), 16).unwrap_or_else(|_| unreachable!());
        }
        if self.starts_with('C') {
            self.skip(1);
            let c = self.0.chars().next().unwrap_or_else(|| unreachable!());
            self.skip(1);
            return c as u8;
        }
        0
    }
}
