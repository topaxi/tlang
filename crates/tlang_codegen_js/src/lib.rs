#[derive(Debug, PartialEq)]
pub struct Codegen {}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}

impl Codegen {
    pub fn new() -> Self {
        Self {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_codegen() {
        let codegen = Codegen::default();
        assert_eq!(codegen, codegen);
    }
}
