//! Regexp test cases from outside sources, read from `testdata/`.
//!
//! The `examples/gen_*.rs` generators, run by the `just` recipes in the
//! `data` group, write these files. They are gzipped text with one record per
//! line; regexps and subjects are hex-encoded UTF-8, since both may hold any
//! character.

use std::io::Read;
use std::rc::Rc;

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

/// A regexp from RE2's exhaustive tests, in Quamina syntax, with RE2's
/// whole-string match on each of its subjects.
pub struct Re2Case {
    pub regex: String,
    pub subjects: Rc<[String]>,
    pub full_match: Vec<bool>,
}

/// Every RE2 case kept by `examples/gen_re2_fullmatch.rs`.
///
/// `re2-exhaustive-fullmatch.txt.gz` is a run of stanzas: a line
/// `strings <s>,<s>,...` with the subjects, then a line `<regex> <bits>` per
/// regexp, holding a `1` or `0` per subject.
pub fn re2_fullmatch() -> Vec<Re2Case> {
    let mut subjects: Rc<[String]> = Rc::new([]);
    let mut cases = Vec::new();
    for line in read_gz("re2-exhaustive-fullmatch.txt.gz").lines() {
        let (head, tail) = line.split_once(' ').unwrap();
        if head == "strings" {
            subjects = tail.split(',').map(unhex).collect();
        } else {
            cases.push(Re2Case {
                regex: unhex(head),
                subjects: Rc::clone(&subjects),
                full_match: tail.bytes().map(|bit| bit == b'1').collect(),
            });
        }
    }
    cases
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
