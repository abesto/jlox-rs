pub type SourceIndex = usize;
pub type Number = f64;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SourceLocation {
    Offset(SourceIndex),
    Resolved {
        line: SourceIndex,
        character: SourceIndex,
    },
}

impl SourceLocation {
    #[must_use]
    pub fn new(offset: SourceIndex) -> Self {
        Self::Offset(offset)
    }

    pub fn resolve(&mut self, source: &[u8]) {
        if let Self::Offset(offset) = self {
            let mut character = 0;
            let mut line = 0;

            for c in source.iter().take(*offset) {
                if c == &b'\n' {
                    character = 0;
                    line += 1;
                } else {
                    character += 1;
                }
            }

            *self = Self::Resolved { character, line };
        }
    }
}

impl From<SourceIndex> for SourceLocation {
    fn from(index: SourceIndex) -> Self {
        Self::Offset(index)
    }
}
pub trait ResolveErrorLocation {
    fn resolve(&mut self, source: &[u8]);
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Resolved { line, character } => write!(f, "{}:{}", line, character),
            Self::Offset(offset) => write!(f, "byte {}", offset),
        }
    }
}
