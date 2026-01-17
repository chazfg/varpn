pub type VarpnResult<T> = Result<T, VarpnErr>;
pub trait Trace<T> {
    fn trace(self, msg: impl ToString) -> VarpnResult<T>;
}
impl<T> Trace<T> for VarpnResult<T> {
    fn trace(self, msg: impl ToString) -> VarpnResult<T> {
        self.map_err(|e| e.add_trace(msg.to_string()))
    }
}
#[derive(Debug)]
pub enum VarpnErr {
    Backtrace(String, Box<Self>),
    Source(String),
}
impl VarpnErr {
    pub fn new(s: String) -> Self {
        Self::Source(s)
    }
    pub fn add_trace(self, s: String) -> Self {
        Self::Backtrace(s, Box::new(self))
    }
}

impl std::fmt::Display for VarpnErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_chain(
            err: &VarpnErr,
            f: &mut std::fmt::Formatter<'_>,
            depth: usize,
        ) -> std::fmt::Result {
            match err {
                VarpnErr::Source(msg) => {
                    for _ in 0..depth {
                        write!(f, "  ")?;
                    }
                    writeln!(f, "{msg}")
                }
                VarpnErr::Backtrace(msg, inner) => {
                    for _ in 0..depth {
                        write!(f, "  ")?;
                    }
                    writeln!(f, "{msg}")?;
                    write_chain(inner, f, depth + 1)
                }
            }
        }

        write_chain(self, f, 0)
    }
}

impl std::error::Error for VarpnErr {}
