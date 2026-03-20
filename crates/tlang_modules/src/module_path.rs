use std::fmt;

/// A module path like `string::parse` represented as segments.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModulePath {
    segments: Vec<String>,
}

impl ModulePath {
    pub fn root() -> Self {
        ModulePath {
            segments: Vec::new(),
        }
    }

    pub fn new(segments: Vec<String>) -> Self {
        ModulePath { segments }
    }

    pub fn from_str_segments(segments: &[&str]) -> Self {
        ModulePath {
            segments: segments.iter().map(|s| s.to_string()).collect(),
        }
    }

    pub fn segments(&self) -> &[String] {
        &self.segments
    }

    pub fn is_root(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn parent(&self) -> Option<ModulePath> {
        if self.segments.is_empty() {
            None
        } else {
            Some(ModulePath {
                segments: self.segments[..self.segments.len() - 1].to_vec(),
            })
        }
    }

    pub fn child(&self, name: &str) -> ModulePath {
        let mut segments = self.segments.clone();
        segments.push(name.to_string());
        ModulePath { segments }
    }

    pub fn name(&self) -> Option<&str> {
        self.segments.last().map(|s| s.as_str())
    }

    /// Returns true if `self` is a direct child of `other`.
    pub fn is_child_of(&self, other: &ModulePath) -> bool {
        self.segments.len() == other.segments.len() + 1
            && self.segments.starts_with(&other.segments)
    }

    /// Returns true if `self` is a descendant of `other`.
    pub fn is_descendant_of(&self, other: &ModulePath) -> bool {
        self.segments.len() > other.segments.len() && self.segments.starts_with(&other.segments)
    }

    /// Returns true if `self` is a sibling of `other` (same parent).
    pub fn is_sibling_of(&self, other: &ModulePath) -> bool {
        match (self.parent(), other.parent()) {
            (Some(a), Some(b)) => a == b,
            (None, None) => true,
            _ => false,
        }
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.segments.is_empty() {
            write!(f, "<root>")
        } else {
            write!(f, "{}", self.segments.join("::"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_root() {
        let root = ModulePath::root();
        assert!(root.is_root());
        assert_eq!(root.parent(), None);
    }

    #[test]
    fn test_child() {
        let root = ModulePath::root();
        let string = root.child("string");
        assert_eq!(string.segments(), &["string"]);
        assert!(string.is_child_of(&root));

        let parse = string.child("parse");
        assert_eq!(parse.segments(), &["string", "parse"]);
        assert!(parse.is_child_of(&string));
        assert!(parse.is_descendant_of(&root));
    }

    #[test]
    fn test_siblings() {
        let string = ModulePath::from_str_segments(&["string"]);
        let math = ModulePath::from_str_segments(&["math"]);
        assert!(string.is_sibling_of(&math));

        let parse = ModulePath::from_str_segments(&["string", "parse"]);
        let utils = ModulePath::from_str_segments(&["string", "utils"]);
        assert!(parse.is_sibling_of(&utils));
        assert!(!parse.is_sibling_of(&string));
    }

    #[test]
    fn test_display() {
        assert_eq!(ModulePath::root().to_string(), "<root>");
        assert_eq!(
            ModulePath::from_str_segments(&["string", "parse"]).to_string(),
            "string::parse"
        );
    }
}
