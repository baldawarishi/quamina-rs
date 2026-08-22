//! Extracts function-level fragments from a repo's `.rs` files and
//! normalizes each into a token-kind symbol sequence.

use std::fs;
use std::path::{Path, PathBuf};

use tree_sitter::{Node, Parser};

use crate::tokenize::{self, assert_ascii_symbols, NormalizedFragment, Vocab};
use crate::FragmentId;

/// Fragments shorter than this many normalized tokens are discarded — too
/// short to say anything interesting about clone recall, and they'd also
/// make `w`-gram windows up to 32 tokens wide degenerate.
pub const MIN_NORMALIZED_TOKENS: usize = 40;

/// Top-level directory names excluded from the corpus walk. `experiments`
/// excludes this crate's own (self-referential, still-being-edited) source;
/// `playground` is generated wasm glue; `target` is build output.
const EXCLUDED_DIRS: &[&str] = &["target", "experiments", "playground", ".git"];

#[derive(Debug, Clone)]
pub struct Fragment {
    pub id: FragmentId,
    /// Path relative to the repo root, for provenance in reports.
    pub file: String,
    /// Function name, best-effort, for human-readable output only — never
    /// part of the normalized symbol stream.
    pub name: String,
    pub start_line: usize,
    pub normalized: NormalizedFragment,
}

pub struct Corpus {
    pub fragments: Vec<Fragment>,
    pub vocab: Vocab,
}

/// Walks `repo_root` for `.rs` files (skipping [`EXCLUDED_DIRS`]),
/// extracts every `function_item` node from each, normalizes it, and keeps
/// fragments with at least [`MIN_NORMALIZED_TOKENS`] symbols.
///
/// File order is sorted for determinism; within a file, fragments are kept
/// in tree-sitter's document order. Fragment ids are assigned sequentially
/// over that order, so a given repo snapshot always produces the same ids.
pub fn build(repo_root: &Path) -> Corpus {
    let mut files = collect_rs_files(repo_root);
    files.sort();

    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_rust::LANGUAGE.into())
        .expect("loading the tree-sitter Rust grammar should never fail");

    let mut vocab = Vocab::new();
    let mut fragments = Vec::new();

    for path in &files {
        let source = match fs::read(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("fuzzyclone: skipping {}: {e}", path.display());
                continue;
            }
        };
        let Some(tree) = parser.parse(&source, None) else {
            eprintln!("fuzzyclone: tree-sitter failed to parse {}", path.display());
            continue;
        };

        let mut fn_nodes = Vec::new();
        collect_function_items(tree.root_node(), &mut fn_nodes);

        let rel = path
            .strip_prefix(repo_root)
            .unwrap_or(path)
            .to_string_lossy()
            .into_owned();

        for func in fn_nodes {
            let normalized = tokenize::normalize_function(func, &mut vocab);
            if normalized.symbols.len() < MIN_NORMALIZED_TOKENS {
                continue;
            }
            assert_ascii_symbols(&normalized.symbols);

            let name = func
                .child_by_field_name("name")
                .and_then(|n| n.utf8_text(&source).ok())
                .unwrap_or("<unknown>")
                .to_string();
            let start_line = func.start_position().row + 1;

            #[allow(
                clippy::cast_possible_truncation,
                reason = "corpus is nowhere near u32::MAX fragments"
            )]
            let id = fragments.len() as FragmentId;
            fragments.push(Fragment {
                id,
                file: rel.clone(),
                name,
                start_line,
                normalized,
            });
        }
    }

    Corpus { fragments, vocab }
}

fn collect_rs_files(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    walk(root, &mut out);
    out
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let file_name = entry.file_name();
        let name = file_name.to_string_lossy();
        if path.is_dir() {
            if EXCLUDED_DIRS.contains(&name.as_ref()) {
                continue;
            }
            walk(&path, out);
        } else if name.ends_with(".rs") {
            out.push(path);
        }
    }
}

fn collect_function_items<'a>(node: Node<'a>, out: &mut Vec<Node<'a>>) {
    if node.kind() == "function_item" {
        out.push(node);
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_function_items(child, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TempRepo {
        dir: PathBuf,
    }

    impl TempRepo {
        fn new(files: &[(&str, &str)]) -> Self {
            let dir = std::env::temp_dir().join(format!(
                "fuzzyclone-corpus-test-{}-{}",
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_nanos()
            ));
            for (rel, contents) in files {
                let path = dir.join(rel);
                fs::create_dir_all(path.parent().unwrap()).unwrap();
                fs::write(&path, contents).unwrap();
            }
            Self { dir }
        }
    }

    impl Drop for TempRepo {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.dir);
        }
    }

    fn long_enough_body(n: usize) -> String {
        // n simple statements; well over MIN_NORMALIZED_TOKENS once normalized.
        let mut s = String::from("fn padded() {\n");
        for i in 0..n {
            s.push_str(&format!("    let v{i} = {i};\n"));
        }
        s.push_str("}\n");
        s
    }

    #[test]
    fn short_fragments_are_discarded() {
        let repo = TempRepo::new(&[("src/lib.rs", "fn tiny() { let x = 1; }\n")]);
        let corpus = build(&repo.dir);
        assert!(
            corpus.fragments.is_empty(),
            "a handful of tokens must not survive the MIN_NORMALIZED_TOKENS filter"
        );
    }

    #[test]
    fn long_fragments_survive_and_are_normalized() {
        let src = long_enough_body(20);
        let repo = TempRepo::new(&[("src/lib.rs", &src)]);
        let corpus = build(&repo.dir);
        assert_eq!(corpus.fragments.len(), 1);
        assert!(corpus.fragments[0].normalized.symbols.len() >= MIN_NORMALIZED_TOKENS);
        assert_eq!(corpus.fragments[0].name, "padded");
    }

    #[test]
    fn excluded_dirs_are_skipped() {
        let src = long_enough_body(20);
        let repo = TempRepo::new(&[
            ("target/debug/build.rs", &src),
            ("experiments/fuzzyclone/src/lib.rs", &src),
        ]);
        let corpus = build(&repo.dir);
        assert!(
            corpus.fragments.is_empty(),
            "target/ and experiments/ must be excluded from the corpus walk"
        );
    }

    #[test]
    fn fragment_ids_are_assigned_in_deterministic_sorted_file_order() {
        let src = long_enough_body(20);
        let repo = TempRepo::new(&[("src/z.rs", &src), ("src/a.rs", &src)]);
        let corpus = build(&repo.dir);
        assert_eq!(corpus.fragments.len(), 2);
        assert_eq!(
            corpus.fragments[0].file, "src/a.rs",
            "a.rs sorts before z.rs"
        );
        assert_eq!(corpus.fragments[0].id, 0);
        assert_eq!(corpus.fragments[1].id, 1);
    }

    #[test]
    fn nested_and_impl_functions_are_both_collected() {
        let mut src = String::from("struct S;\nimpl S {\n");
        src.push_str(
            &long_enough_body(20)
                .replace("padded", "method")
                .replace("fn method", "    fn method"),
        );
        src.push_str("\n}\n");
        // also a free-standing nested fn inside another fn
        src.push_str("fn outer() {\n");
        src.push_str(&long_enough_body(20).replace("padded", "inner"));
        src.push_str("}\n");
        let repo = TempRepo::new(&[("src/lib.rs", &src)]);
        let corpus = build(&repo.dir);
        let names: Vec<&str> = corpus.fragments.iter().map(|f| f.name.as_str()).collect();
        assert!(
            names.contains(&"method"),
            "impl methods must be collected: {names:?}"
        );
        assert!(
            names.contains(&"inner"),
            "nested fns must be collected: {names:?}"
        );
    }
}
