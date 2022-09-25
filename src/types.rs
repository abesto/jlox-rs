use crate::resolver::CommandIndex;

pub type SourceIndex = usize;
pub type Number = f64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SourceLocation {
    Offset {
        command: CommandIndex,
        offset: SourceIndex,
    },
    Resolved {
        command: CommandIndex,
        line: SourceIndex,
        character: SourceIndex,
    },
}

impl SourceLocation {
    #[must_use]
    pub fn new(command: CommandIndex, offset: SourceIndex) -> Self {
        Self::Offset { command, offset }
    }

    pub fn resolve(&mut self, source: &[u8]) {
        if let Self::Offset { command, offset } = self {
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

            *self = Self::Resolved {
                command: *command,
                character,
                line,
            };
        }
    }

    pub fn command(&self) -> CommandIndex {
        match self {
            Self::Offset { command, .. } | Self::Resolved { command, .. } => *command,
        }
    }
}

pub trait ResolveErrorLocation {
    fn resolve(&mut self, source: &[u8]);
}

impl<T: ResolveErrorLocation> ResolveErrorLocation for Vec<T> {
    fn resolve(&mut self, source: &[u8]) {
        for e in self {
            e.resolve(source);
        }
    }
}

pub struct ErrorLocationResolver<'a> {
    source: &'a [u8],
}

impl<'a> ErrorLocationResolver<'a> {
    #[must_use]
    pub fn new(source: &'a [u8]) -> Self {
        Self { source }
    }

    pub fn resolve<E>(&self, mut e: E) -> E
    where
        E: ResolveErrorLocation,
    {
        e.resolve(self.source);
        e
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Resolved {
                line, character, ..
            } => write!(f, "{}:{}", line, character),
            Self::Offset { offset, .. } => write!(f, "byte {}", offset),
        }
    }
}
