//! Extract whole-string match results from RE2's exhaustive tests.
//!
//! ```text
//! bzcat "$(go env GOROOT)/src/regexp/testdata/re2-exhaustive.txt.bz2" \
//!     | cargo run --example gen_re2_fullmatch -- testdata/re2-exhaustive-fullmatch.txt.gz
//! ```
//!
//! RE2's exhaustive tests enumerate every regexp up to a size over a few atoms
//! and operators, and record how each matches every short string over a small
//! alphabet; Go ships the results with its `regexp` package. Each result line
//! starts with the whole-string match, `-` when there is none, which is the
//! answer Quamina gives for a field value.
//!
//! Only regexps whose RE2 meaning carries over to Quamina are kept. Anchors,
//! word boundaries, flags, `\C` and POSIX classes have no counterpart and are
//! dropped. `.` becomes `[^~n]`, since RE2's dot excludes a newline and
//! Quamina's does not, and `\s` spells out RE2's set, which adds form feed to
//! XSD's. Strings that are not valid UTF-8 can't appear in a JSON event and are
//! dropped too.
//!
//! `src/regexp_test_data.rs` reads the output and documents its format.

use std::io::{Read, Write};

use flate2::{Compression, write::GzEncoder};

/// One `strings`/`regexps` block of the RE2 results. A subject or regexp that
/// isn't valid UTF-8 is `None`.
#[derive(Default)]
struct Stanza {
    subjects: Vec<Option<String>>,
    /// Each regexp with RE2's whole-string match on every subject.
    regexps: Vec<(Option<String>, Vec<bool>)>,
}

fn main() {
    let out = std::env::args().nth(1).expect("output path");
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text).unwrap();

    let mut stanzas: Vec<Stanza> = Vec::new();
    let mut in_strings = false;
    for line in text.split('\n') {
        // Test-group names such as `EgrepLiterals.Lowercase` carry no results.
        if line.is_empty() || line.starts_with('#') || line.starts_with(|c: char| c.is_uppercase())
        {
            continue;
        }
        match line {
            "strings" => {
                stanzas.push(Stanza::default());
                in_strings = true;
            }
            "regexps" => in_strings = false,
            _ => {
                let stanza = stanzas.last_mut().unwrap();
                if in_strings {
                    stanza.subjects.push(unquote(line));
                } else if line.starts_with('"') {
                    stanza.regexps.push((unquote(line), Vec::new()));
                } else {
                    let full_match = !line.starts_with("-;");
                    stanza.regexps.last_mut().unwrap().1.push(full_match);
                }
            }
        }
    }

    let mut lines = String::new();
    for Stanza { subjects, regexps } in &stanzas {
        let kept: Vec<usize> = (0..subjects.len())
            .filter(|&i| subjects[i].is_some())
            .collect();
        let mut stanza = String::new();
        for (regexp, full_matches) in regexps {
            let Some(quamina) = regexp.as_deref().and_then(to_quamina) else {
                continue;
            };
            let bits: String = kept
                .iter()
                .map(|&i| if full_matches[i] { '1' } else { '0' })
                .collect();
            stanza += &format!("{} {bits}\n", hex(&quamina));
        }
        if !stanza.is_empty() {
            let subjects: Vec<String> = kept
                .iter()
                .map(|&i| hex(subjects[i].as_ref().unwrap()))
                .collect();
            lines += &format!("strings {}\n{stanza}", subjects.join(","));
        }
    }

    let mut gz = GzEncoder::new(std::fs::File::create(out).unwrap(), Compression::best());
    gz.write_all(lines.as_bytes()).unwrap();
    gz.finish().unwrap();
}

/// Decode a Go double-quoted string, or `None` if it isn't valid UTF-8.
fn unquote(quoted: &str) -> Option<String> {
    let mut bytes = Vec::new();
    let mut rest = &quoted.as_bytes()[1..quoted.len() - 1];
    while let Some((&b, tail)) = rest.split_first() {
        rest = tail;
        if b != b'\\' {
            bytes.push(b);
            continue;
        }
        let (&e, tail) = rest.split_first().unwrap();
        rest = tail;
        bytes.push(match e {
            b'a' => 0x07,
            b'b' => 0x08,
            b'f' => 0x0C,
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            b'v' => 0x0B,
            b'x' => {
                let (digits, tail) = rest.split_at(2);
                rest = tail;
                u8::from_str_radix(std::str::from_utf8(digits).unwrap(), 16).unwrap()
            }
            b'0'..=b'7' => {
                let (digits, tail) = rest.split_at(2);
                rest = tail;
                let octal = [&[e], digits].concat();
                u8::from_str_radix(std::str::from_utf8(&octal).unwrap(), 8).unwrap()
            }
            other => other,
        });
    }
    String::from_utf8(bytes).ok()
}

/// What an RE2 escape letter means in Quamina syntax.
const fn escape(letter: char) -> Option<&'static str> {
    Some(match letter {
        'n' => "~n",
        'r' => "~r",
        't' => "~t",
        'f' => "\u{c}",
        'v' => "\u{b}",
        'a' => "\u{7}",
        'd' => "~d",
        'D' => "~D",
        'w' => "~w",
        'W' => "~W",
        's' => "[ ~t~n~r\u{c}]",
        'S' => "[^ ~t~n~r\u{c}]",
        _ => return None,
    })
}

/// A literal character in Quamina syntax.
fn literal(c: char) -> String {
    if "()*+-.?[\\]^{|}~".contains(c) {
        format!("~{c}")
    } else {
        c.to_string()
    }
}

/// Translate an RE2 regexp, or `None` when it has no Quamina counterpart.
fn to_quamina(regexp: &str) -> Option<String> {
    let chars: Vec<char> = regexp.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            '\\' => {
                let e = chars[i + 1];
                match escape(e) {
                    Some(text) => out += text,
                    None if e.is_alphanumeric() => return None,
                    None => out += &literal(e),
                }
                i += 2;
                continue;
            }
            '[' => {
                let (class, end) = class_to_quamina(&chars, i)?;
                out += &class;
                i = end;
                continue;
            }
            '^' | '$' => return None,
            '.' => out += "[^~n]",
            '(' if chars.get(i + 1) == Some(&'?') => {
                if chars.get(i + 2) != Some(&':') {
                    return None;
                }
                out += "(?:";
                i += 2;
            }
            '{' => match repeat_count(&chars[i + 1..]) {
                Some(len) => {
                    out.extend(&chars[i..=i + len + 1]);
                    i += len + 1;
                }
                None => out += "~{",
            },
            ']' | '}' => out += &literal(chars[i]),
            '~' => out += "~~",
            c => out.push(c),
        }
        i += 1;
    }
    Some(out)
}

/// The length of a repeat count such as `2,3` before its `}`, if `chars`
/// starts with one; otherwise RE2 reads the `{` as a literal.
fn repeat_count(chars: &[char]) -> Option<usize> {
    let len = chars.iter().position(|&c| c == '}')?;
    let body: String = chars[..len].iter().collect();
    let mut parts = body.splitn(2, ',');
    let min = parts.next().unwrap();
    let digits = |s: &str| s.chars().all(|c| c.is_ascii_digit());
    (!min.is_empty() && digits(min) && parts.all(digits)).then_some(len)
}

/// Translate the class opening at `chars[i]`, returning it and the index past it.
fn class_to_quamina(chars: &[char], mut i: usize) -> Option<(String, usize)> {
    let mut out = String::from("[");
    i += 1;
    if chars[i] == '^' {
        out.push('^');
        i += 1;
    }
    let mut first = true;
    while first || chars[i] != ']' {
        first = false;
        if chars[i] == '[' && chars.get(i + 1) == Some(&':') {
            return None;
        }
        let (lo, next) = class_atom(chars, i)?;
        i = next;
        if chars[i] == '-' && chars[i + 1] != ']' {
            let (hi, next) = class_atom(chars, i + 1)?;
            i = next;
            out += &format!("{lo}-{hi}");
        } else {
            out += &lo;
        }
    }
    Some((out + "]", i + 1))
}

/// One character of a class, or a class escape, in Quamina class syntax.
fn class_atom(chars: &[char], i: usize) -> Option<(String, usize)> {
    if chars[i] != '\\' {
        return Some((literal(chars[i]), i + 1));
    }
    let e = chars[i + 1];
    let text = match e {
        // In a class `\s` is spliced in as its members; `\S` would need a
        // class subtraction, so regexps using it are dropped.
        's' => " ~t~n~r\u{c}".to_string(),
        'S' => return None,
        _ => match escape(e) {
            Some(text) => text.to_string(),
            None if e.is_alphanumeric() => return None,
            None => literal(e),
        },
    };
    Some((text, i + 2))
}

fn hex(s: &str) -> String {
    use std::fmt::Write;
    s.bytes().fold(String::new(), |mut out, b| {
        write!(out, "{b:02x}").unwrap();
        out
    })
}
