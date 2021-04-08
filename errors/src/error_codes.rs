
pub struct ErrorCode {
    kind: char,
    code: u32,
}

impl std::fmt::Debug for ErrorCode {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        fmt.write_str(&format!("{}{:04}", self.kind, self.code))
    }
}

impl ErrorCode {
    pub fn new(
        kind: char,
        code: u32,
    ) -> Self {
        Self {
            kind,
            code,
        }
    }
}

macro_rules! def_error_code {
    ($($CODE:literal => $DESC:literal,)*) => {
        // test
    }
}

def_error_code! {
    "E0001" => "Error Code 1",
    "E0002" => "Error Code 2",
}