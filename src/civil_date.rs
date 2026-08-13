//! Proleptic Gregorian civil calendar conversion and RFC 3339 timestamp
//! formatting, shared by every decoder with an epoch-based date/time value
//! (MessagePack's timestamp extension, CBOR's epoch tag, Avro's date/time
//! logical types).

/// Convert a day count since the Unix epoch to a proleptic Gregorian
/// `(year, month, day)`, using Howard Hinnant's `civil_from_days` algorithm.
pub const fn civil_from_days(days: i64) -> (i64, u32, u32) {
    let z = days + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    // `d` is in [1, 31] and `mp` is in [0, 11] by construction of the
    // algorithm above, so both narrowing casts below are lossless.
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    let m = (if mp < 10 { mp + 3 } else { mp - 9 }) as u32;
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d)
}

/// Format `seconds`/`nanos` since the Unix epoch as an RFC 3339 UTC string.
#[cfg_attr(
    not(any(feature = "messagepack", feature = "cbor")),
    allow(dead_code, reason = "MessagePack/CBOR timestamp extensions only")
)]
pub fn format_rfc3339(seconds: i64, nanos: u32) -> String {
    let days = seconds.div_euclid(86_400);
    let secs_of_day = seconds.rem_euclid(86_400);
    let (y, m, d) = civil_from_days(days);
    let hh = secs_of_day / 3600;
    let mm = (secs_of_day % 3600) / 60;
    let ss = secs_of_day % 60;
    if nanos == 0 {
        format!("{y:04}-{m:02}-{d:02}T{hh:02}:{mm:02}:{ss:02}Z")
    } else {
        let mut frac = format!("{nanos:09}");
        while frac.ends_with('0') {
            frac.pop();
        }
        format!("{y:04}-{m:02}-{d:02}T{hh:02}:{mm:02}:{ss:02}.{frac}Z")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn epoch_and_one_second_after() {
        assert_eq!(format_rfc3339(0, 0), "1970-01-01T00:00:00Z");
        assert_eq!(format_rfc3339(1, 0), "1970-01-01T00:00:01Z");
    }

    #[test]
    fn civil_from_days_matches_known_dates() {
        assert_eq!(civil_from_days(0), (1970, 1, 1));
        assert_eq!(civil_from_days(-1), (1969, 12, 31));
    }
}
