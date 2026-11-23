//! Regexp test cases from outside sources, read from `testdata/`.
//!
//! The `examples/gen_*.rs` generators, run by the `just` recipes in the
//! `data` group, write these files. They are gzipped text with one record per
//! line; regexps and subjects are hex-encoded UTF-8, since both may hold any
//! character.

use std::io::Read;

/// A case from the XSLT regex-syntax test catalog by Michael Kay, derived from
/// Microsoft's W3C XML Schema regex tests.
pub struct CatalogCase {
    /// Upstream test-case name, e.g. `regex-syntax-0542`.
    pub id: String,
    /// The regex in Quamina syntax.
    pub regex: String,
    pub matches: Vec<String>,
    pub nomatches: Vec<String>,
    /// Whether XPath accepts the regex.
    pub valid: bool,
}

/// Every case in the catalog.
///
/// Each line of `regexp-catalog.txt.gz` is tab-separated: the case id, `1` or
/// `0` for whether XPath accepts the regex, the regex, and the matching and
/// nonmatching subjects, each list comma-separated or `-` when empty.
pub fn catalog() -> Vec<CatalogCase> {
    let list = |field: &str| match field {
        "-" => Vec::new(),
        _ => field.split(',').map(unhex).collect(),
    };
    read_gz("regexp-catalog.txt.gz")
        .lines()
        .map(|line| {
            let fields: Vec<_> = line.split('\t').collect();
            let [id, valid, regex, matches, nomatches] = fields[..] else {
                panic!("malformed catalog line: {line}");
            };
            CatalogCase {
                id: id.into(),
                regex: unhex(regex),
                matches: list(matches),
                nomatches: list(nomatches),
                valid: valid == "1",
            }
        })
        .collect()
}

fn read_gz(name: &str) -> String {
    let path = format!("{}/testdata/{name}", env!("CARGO_MANIFEST_DIR"));
    let mut text = String::new();
    flate2::read::GzDecoder::new(std::fs::File::open(path).unwrap())
        .read_to_string(&mut text)
        .unwrap();
    text
}

fn unhex(hex: &str) -> String {
    let bytes = (0..hex.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
        .collect();
    String::from_utf8(bytes).unwrap()
}
