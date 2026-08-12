//! Minimal standard-alphabet, padded base64 encoding, shared by every
//! decoder's `TaggedBase64` binary-value policy (see
//! [`crate::format_policies::BinaryValuePolicy`]).

const BASE64_ALPHABET: &[u8; 64] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/// Encode `data` as standard, padded base64 text.
pub fn encode(data: &[u8]) -> String {
    let mut out = String::with_capacity(data.len().div_ceil(3) * 4);
    let mut chunks = data.chunks_exact(3);
    for chunk in &mut chunks {
        let n = (u32::from(chunk[0]) << 16) | (u32::from(chunk[1]) << 8) | u32::from(chunk[2]);
        push_sextets(&mut out, n, 4);
    }
    let rem = chunks.remainder();
    match rem.len() {
        1 => {
            let n = u32::from(rem[0]) << 16;
            push_sextets(&mut out, n, 2);
            out.push_str("==");
        }
        2 => {
            let n = (u32::from(rem[0]) << 16) | (u32::from(rem[1]) << 8);
            push_sextets(&mut out, n, 3);
            out.push('=');
        }
        _ => {}
    }
    out
}

/// Push the top `count` base64 sextets of `n` (a 24-bit group left-aligned
/// in the low 24 bits) onto `out`.
fn push_sextets(out: &mut String, n: u32, count: u8) {
    for i in 0..count {
        let shift = 18 - 6 * u32::from(i);
        let sextet = (n >> shift) & 0x3F;
        out.push(BASE64_ALPHABET[sextet as usize] as char);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matches_known_vectors() {
        assert_eq!(encode(&[0x00, 0xff]), "AP8=");
        assert_eq!(encode(b""), "");
        assert_eq!(encode(b"f"), "Zg==");
        assert_eq!(encode(b"fo"), "Zm8=");
        assert_eq!(encode(b"foo"), "Zm9v");
        assert_eq!(encode(b"foobar"), "Zm9vYmFy");
    }
}
