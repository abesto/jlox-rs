pub type SourceIndex = usize;
pub type Number = f64;

#[derive(Debug)]
pub struct SourceLocation {
    line: SourceIndex,
    character: SourceIndex,
}

impl SourceLocation {
    pub fn new(source: &[u8], offset: SourceIndex) -> Self {
        let mut loc = Self {
            character: 0,
            line: 0,
        };

        for i in 0..=std::cmp::min(offset, source.len() - 1) {
            if source[i] == b'\n' {
                loc.character = 0;
                loc.line += 1;
            } else {
                loc.character += 1;
            }
        }

        loc
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.line, self.character))
    }
}
