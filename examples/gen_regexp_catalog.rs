//! Extract the XSLT regex-syntax test catalog for the regexp tests.
//!
//! ```text
//! curl -sL <catalog URL> | cargo run --example gen_regexp_catalog -- testdata/regexp-catalog.txt.gz
//! ```
//!
//! `just regexp-catalog` runs this against a pinned copy of
//! <https://github.com/qt4cg/xslt40-test/blob/master/tests/misc/regex-syntax/_regex-syntax-test-set.xml>,
//! Michael Kay's catalog derived from Microsoft's W3C XML Schema regex tests.
//! Its stylesheet `re.xsl` anchors each regex as `^(regex)$`, the same
//! whole-value match Quamina does, and splits the match and nonmatch lists
//! with XPath `tokenize`: an empty list holds no strings at all, and an absent
//! match list is the one string `----`.
//!
//! XSD escapes with `\`, Quamina with `~`, so `\X` becomes `~X` and a literal
//! `~` becomes `~~`.
//!
//! `src/regexp_test_data.rs` reads the output and documents its format.

use std::io::{Read, Write};

use flate2::{Compression, write::GzEncoder};
use regex::Regex;

fn main() {
    let out = std::env::args().nth(1).expect("output path");
    let mut xml = String::new();
    std::io::stdin().read_to_string(&mut xml).unwrap();

    // Some cases are commented out upstream.
    let xml = Regex::new(r"(?s)<!--.*?-->").unwrap().replace_all(&xml, "");
    let param = Regex::new(r#"<param name="(\w+)" as="xs:string" select="([^"]*)"/>"#).unwrap();
    let mut lines = String::new();
    for case in xml.split("<test-case name=\"").skip(1) {
        let (name, body) = case.split_once('"').unwrap();
        let body = &body[..body.find("</test-case>").unwrap()];
        let get = |wanted: &str| {
            param
                .captures_iter(body)
                .find(|c| &c[1] == wanted)
                .map(|c| xpath_string(&xml_attribute(&c[2])))
        };
        let valid = body.contains("<assert>");
        let delimiter = get("delimiter").unwrap_or_else(|| ",".into());
        let matches = tokenize(&get("match").unwrap_or_else(|| "----".into()), &delimiter);
        let nomatches = tokenize(&get("nonmatch").unwrap_or_default(), &delimiter);
        let regex = to_quamina(&get("regex").unwrap());
        lines += &format!(
            "{name}\t{}\t{}\t{}\t{}\n",
            u8::from(valid),
            hex(&regex),
            hex_list(&matches),
            hex_list(&nomatches)
        );
    }

    let mut gz = GzEncoder::new(std::fs::File::create(out).unwrap(), Compression::best());
    gz.write_all(lines.as_bytes()).unwrap();
    gz.finish().unwrap();
}

/// An attribute value as XML reads it: whitespace characters become spaces,
/// then entity and character references are replaced.
fn xml_attribute(raw: &str) -> String {
    let raw = raw.replace(['\t', '\n', '\r'], " ");
    let mut out = String::new();
    let mut rest = raw.as_str();
    while let Some(amp) = rest.find('&') {
        out += &rest[..amp];
        let semi = amp + rest[amp..].find(';').unwrap();
        let entity = &rest[amp + 1..semi];
        out.push(match entity {
            "amp" => '&',
            "lt" => '<',
            "gt" => '>',
            "quot" => '"',
            "apos" => '\'',
            _ => {
                let code = match entity.strip_prefix("#x") {
                    Some(hex) => u32::from_str_radix(hex, 16),
                    None => entity.strip_prefix('#').unwrap().parse(),
                };
                char::from_u32(code.unwrap()).unwrap()
            }
        });
        rest = &rest[semi + 1..];
    }
    out + rest
}

/// Unquote an XPath string literal such as `'it''s'`.
fn xpath_string(literal: &str) -> String {
    let quote = &literal[..1];
    assert!(
        quote == "'" || quote == "\"",
        "not a string literal: {literal}"
    );
    literal[1..literal.len() - 1].replace(&quote.repeat(2), quote)
}

/// XPath `fn:tokenize`, for delimiters with no regex metacharacters.
fn tokenize(value: &str, delimiter: &str) -> Vec<String> {
    assert!(!delimiter.contains(|c| "\\^$.|?*+()[]{}".contains(c)));
    if value.is_empty() {
        return Vec::new();
    }
    value.split(delimiter).map(String::from).collect()
}

fn to_quamina(regex: &str) -> String {
    let mut out = String::new();
    let mut chars = regex.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                out.push('~');
                out.extend(chars.next());
            }
            '~' => out.push_str("~~"),
            c => out.push(c),
        }
    }
    out
}

fn hex(s: &str) -> String {
    use std::fmt::Write;
    s.bytes().fold(String::new(), |mut out, b| {
        write!(out, "{b:02x}").unwrap();
        out
    })
}

fn hex_list(subjects: &[String]) -> String {
    if subjects.is_empty() {
        return "-".into();
    }
    subjects
        .iter()
        .map(|s| hex(s))
        .collect::<Vec<_>>()
        .join(",")
}
