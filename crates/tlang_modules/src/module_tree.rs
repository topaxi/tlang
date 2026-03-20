use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use crate::ModulePath;

/// A node in the module tree, representing either a directory or a .tlang file.
#[derive(Debug)]
pub struct ModuleNode {
    /// The module path (e.g., `string::parse`).
    pub path: ModulePath,
    /// The filesystem path to the .tlang source file, if it exists.
    pub file_path: Option<PathBuf>,
    /// Child modules (directory entries).
    pub children: BTreeMap<String, ModuleNode>,
}

/// The module tree built from scanning the filesystem.
///
/// Represents the complete module hierarchy under a `src/` directory.
#[derive(Debug)]
pub struct ModuleTree {
    /// The root directory (parent of `src/`).
    pub root_dir: PathBuf,
    /// The root module node (corresponds to `src/lib.tlang`).
    pub root: ModuleNode,
}

impl ModuleTree {
    /// Scan a directory to build the module tree.
    ///
    /// `root_dir` should be the project root (containing `src/`).
    /// The entry point is `src/lib.tlang`.
    pub fn from_directory(root_dir: &Path) -> Result<Self, ModuleTreeError> {
        let src_dir = root_dir.join("src");
        if !src_dir.is_dir() {
            return Err(ModuleTreeError::NoSrcDirectory(root_dir.to_path_buf()));
        }

        let lib_path = src_dir.join("lib.tlang");
        if !lib_path.is_file() {
            return Err(ModuleTreeError::NoLibFile(src_dir));
        }

        let mut root = ModuleNode {
            path: ModulePath::root(),
            file_path: Some(lib_path),
            children: BTreeMap::new(),
        };

        scan_directory(&src_dir, &ModulePath::root(), &mut root)?;

        Ok(ModuleTree {
            root_dir: root_dir.to_path_buf(),
            root,
        })
    }

    /// Get all module paths in the tree (including root).
    pub fn all_module_paths(&self) -> Vec<&ModulePath> {
        let mut paths = Vec::new();
        collect_paths(&self.root, &mut paths);
        paths
    }

    /// Look up a module node by its path.
    pub fn get(&self, path: &ModulePath) -> Option<&ModuleNode> {
        if path.is_root() {
            return Some(&self.root);
        }

        let mut node = &self.root;
        for segment in path.segments() {
            node = node.children.get(segment)?;
        }
        Some(node)
    }
}

fn scan_directory(
    dir: &Path,
    parent_path: &ModulePath,
    parent_node: &mut ModuleNode,
) -> Result<(), ModuleTreeError> {
    let mut entries: Vec<_> = std::fs::read_dir(dir)
        .map_err(|e| ModuleTreeError::IoError(dir.to_path_buf(), e))?
        .collect::<Result<_, _>>()
        .map_err(|e| ModuleTreeError::IoError(dir.to_path_buf(), e))?;

    // Sort for deterministic ordering
    entries.sort_by_key(|e| e.file_name());

    for entry in entries {
        let path = entry.path();
        let file_name = entry.file_name();
        let name = file_name.to_string_lossy();

        if path.is_dir() {
            let module_path = parent_path.child(&name);
            // Look for a corresponding .tlang file for this directory
            let dir_module_file = dir.join(format!("{name}.tlang"));
            let file_path = if dir_module_file.is_file() {
                Some(dir_module_file)
            } else {
                None
            };

            let mut node = ModuleNode {
                path: module_path.clone(),
                file_path,
                children: BTreeMap::new(),
            };

            scan_directory(&path, &module_path, &mut node)?;

            // Only include directories that have at least one .tlang file in them
            if node.file_path.is_some() || !node.children.is_empty() {
                parent_node.children.insert(name.to_string(), node);
            }
        } else if let Some(stem) = path
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| s.to_string())
            && path.extension().is_some_and(|ext| ext == "tlang")
            && stem != "lib"
        {
            // Skip files that correspond to directories (handled above)
            if dir.join(&stem).is_dir() {
                continue;
            }

            let module_path = parent_path.child(&stem);
            parent_node.children.insert(
                stem,
                ModuleNode {
                    path: module_path,
                    file_path: Some(path),
                    children: BTreeMap::new(),
                },
            );
        }
    }

    Ok(())
}

fn collect_paths<'a>(node: &'a ModuleNode, paths: &mut Vec<&'a ModulePath>) {
    if node.file_path.is_some() {
        paths.push(&node.path);
    }
    for child in node.children.values() {
        collect_paths(child, paths);
    }
}

#[derive(Debug)]
pub enum ModuleTreeError {
    NoSrcDirectory(PathBuf),
    NoLibFile(PathBuf),
    IoError(PathBuf, std::io::Error),
}

impl std::fmt::Display for ModuleTreeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleTreeError::NoSrcDirectory(path) => {
                write!(f, "no `src/` directory found in `{}`", path.display())
            }
            ModuleTreeError::NoLibFile(path) => {
                write!(f, "no `lib.tlang` found in `{}`", path.display())
            }
            ModuleTreeError::IoError(path, err) => {
                write!(f, "I/O error reading `{}`: {}", path.display(), err)
            }
        }
    }
}

impl std::error::Error for ModuleTreeError {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn create_test_project(dir: &Path) {
        let src = dir.join("src");
        fs::create_dir_all(src.join("string")).unwrap();

        fs::write(src.join("lib.tlang"), "pub mod string;").unwrap();
        fs::write(src.join("string.tlang"), "pub mod parse, utils;").unwrap();
        fs::write(
            src.join("string").join("parse.tlang"),
            "pub fn from_char_code(n) { n }",
        )
        .unwrap();
        fs::write(
            src.join("string").join("utils.tlang"),
            "pub fn trim(s) { s }",
        )
        .unwrap();
    }

    #[test]
    fn test_scan_project() {
        let dir = std::env::temp_dir().join("tlang_test_module_tree");
        let _ = fs::remove_dir_all(&dir);
        create_test_project(&dir);

        let tree = ModuleTree::from_directory(&dir).unwrap();

        let paths: Vec<String> = tree
            .all_module_paths()
            .iter()
            .map(|p| p.to_string())
            .collect();

        assert_eq!(
            paths,
            vec!["<root>", "string", "string::parse", "string::utils"]
        );

        // Look up specific modules
        assert!(tree.get(&ModulePath::root()).unwrap().file_path.is_some());
        assert!(
            tree.get(&ModulePath::from_str_segments(&["string"]))
                .unwrap()
                .file_path
                .is_some()
        );
        assert!(
            tree.get(&ModulePath::from_str_segments(&["string", "parse"]))
                .unwrap()
                .file_path
                .is_some()
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_no_src_directory() {
        let dir = std::env::temp_dir().join("tlang_test_no_src");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        let result = ModuleTree::from_directory(&dir);
        assert!(result.is_err());

        let _ = fs::remove_dir_all(&dir);
    }
}
