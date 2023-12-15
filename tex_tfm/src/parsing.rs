
pub(crate) struct Parser<'a>(pub(crate) &'a str);
impl<'a> Parser<'a> {
    #[inline(always)]
    pub fn new(s:&'a str) -> Self {
        Parser(s.trim_start())
    }
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    #[inline(always)]
    pub fn ends_with(&self, c:char) -> bool {
        self.0.ends_with(c)
    }
    #[inline(always)]
    pub fn starts_with(&self, c:char) -> bool {
        self.0.starts_with(c)
    }
    #[inline(always)]
    pub fn starts_with_str(&self, s:&str) -> bool {
        self.0.starts_with(s)
    }

    #[inline(always)]
    pub fn starts_with_digit(&self) -> bool {
        self.0.starts_with(|c:char| c.is_ascii_digit())
    }

    pub fn read_digit(&mut self) -> u32 {
        let mut ret = 0u32;
        while !self.is_empty() && self.starts_with_digit() {
            ret = ret*10 + self.0.chars().next().unwrap().to_digit(10).unwrap();
            self.0 = &self.0[1..];
        }
        self.0 = self.0.trim_start();
        ret
    }

    #[inline(always)]
    pub fn skip(&mut self, n: usize) {
        self.0 = &self.0[n..].trim_start();
    }

    #[inline(always)]
    pub fn drop_right(&mut self, n: usize) {
        self.0 = &self.0[..self.0.len() - n];
    }

    pub fn read_until_ws(&mut self) -> &str {
        match self.0.find(char::is_whitespace) {
            Some(i) => {
                let (ret,rest) = self.0.split_at(i);
                self.0 = rest.trim_start();
                ret
            }
            None => {
                let ret = self.0;
                self.0 = "";
                ret
            }
        }
    }
    pub fn read_until_ws_or(&mut self,c:char) -> &str {
        match self.0.find(|x| x == c || char::is_whitespace(x)) {
            Some(i) => {
                let (ret,rest) = self.0.split_at(i);
                self.0 = rest.trim_start();
                ret
            }
            None => {
                let ret = self.0;
                self.0 = "";
                ret
            }
        }
    }
    pub fn read_until(&mut self,c:char) -> &str {
        match self.0.find(c) {
            Some(i) => {
                let (ret,rest) = self.0.split_at(i);
                self.0 = rest[1..].trim_start();
                ret
            }
            None => {
                let ret = self.0;
                self.0 = "";
                ret
            }
        }
    }
    pub fn read_until_str(&mut self,s:&str) -> &str {
        match self.0.find(s) {
            Some(i) => {
                let (ret,rest) = self.0.split_at(i);
                self.0 = rest[s.len()..].trim_start();
                ret
            }
            None => {
                let ret = self.0;
                self.0 = "";
                ret
            }
        }
    }
    pub fn read_until_strs(&mut self,s:&[&str]) -> &str {
        for s in s {
            match self.0.find(s) {
                Some(i) => {
                    let (ret, rest) = self.0.split_at(i);
                    self.0 = rest[s.len()..].trim_start();
                    return ret
                }
                None => continue
            }
        }
        let ret = self.0;
        self.0 = "";
        ret
    }
    #[inline(always)]
    pub fn drop(&mut self,s:&str) -> bool {
        if self.0.starts_with(s) {
            self.skip(s.len());
            true
        } else {false }
    }
    #[inline(always)]
    pub fn skip_until_endline(&mut self) {
        match self.0.find('\n') {
            Some(i) => {
                self.skip(i+1);
            }
            None => {
                self.0 = "";
            }
        }
    }

}