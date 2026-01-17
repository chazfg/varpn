pub type VarpnResult<T> = Result<T, VarpnErr>;
pub trait Trace<T> {
    fn trace(self, line: u32, msg: impl ToString) -> VarpnResult<T>;
}
impl<T> Trace<T> for VarpnResult<T> {
    fn trace(self, line: u32, msg: impl ToString) -> VarpnResult<T> {
        self.map_err(|e| e.add_trace(line, msg.to_string()))
    }
}
#[derive(Debug)]
pub enum VarpnErr {
    Backtrace {
        line: u32,
        msg: String,
        inner: Box<Self>,
    },
    Source {
        line: u32,
        msg: String,
    },
}

impl VarpnErr {
    pub fn source(&self) -> &VarpnErr {
        match &self {
            VarpnErr::Backtrace { inner, .. } => inner.source(),
            VarpnErr::Source { .. } => self,
        }
    }
    pub fn write_chain(&self, f: &mut std::fmt::Formatter<'_>, depth: u32) -> std::fmt::Result {
        for _ in 0..depth.max(4) {
            write!(f, "  ")?;
        }
        match self {
            VarpnErr::Backtrace { line, msg, inner } => {
                writeln!(f, "trace on {line}: {msg}")?;
                inner.write_chain(f, depth + 1)
            }
            VarpnErr::Source { line, msg } => {
                writeln!(f, "source {line}: {msg}")
            }
        }
    }

    pub fn new(line: u32, msg: impl ToString) -> Self {
        Self::Source {
            line,
            msg: msg.to_string(),
        }
    }
    pub fn add_trace(self, line: u32, msg: String) -> Self {
        Self::Backtrace {
            line,
            msg,
            inner: Box::new(self),
        }
    }
}

impl std::fmt::Display for VarpnErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_chain(f, 0)
    }
}

impl std::error::Error for VarpnErr {}
