//! Unicode category and block data for I-Regexp property matching.
//!
//! This module provides character ranges for Unicode general categories (~p{Lu}, ~p{Ll}, etc.)
//! and Unicode blocks (~p{IsBasicLatin}, etc.). The category tables are generated, in
//! `unicode_category_tables.rs`; the block ranges are listed here by hand.

use crate::regexp::{RunePair, invert_rune_range, simplify_rune_range};
use crate::unicode_category_tables::CATEGORIES;

/// The sorted, merged ranges of a general category named like `L` or `Lu`,
/// or `None` if there is no such category.
///
/// Only the two-letter categories other than `Cn` are stored: a one-letter
/// category is the union of its own, and `Cn` is every non-surrogate code
/// point no other category holds.
pub fn get_category_ranges(name: &str) -> Option<Vec<RunePair>> {
    let pairs =
        |&(_, table): &(_, &'static [(char, char)])| table.iter().map(|&(lo, hi)| rp(lo, hi));
    let covers = |stored: &str| stored == name || (name.len() == 1 && stored.starts_with(name));
    let mut ranges: Vec<RunePair> = CATEGORIES
        .iter()
        .filter(|(stored, _)| covers(stored))
        .flat_map(pairs)
        .collect();
    if covers("Cn") {
        let assigned = CATEGORIES.iter().flat_map(pairs).collect();
        ranges.extend(invert_rune_range(assigned));
    }
    (!ranges.is_empty()).then(|| simplify_rune_range(ranges))
}

/// Get character ranges for a Unicode block.
/// Returns None if the block is not recognized.
#[allow(clippy::too_many_lines)] // exhaustive Unicode block name lookup; one match arm per block
pub fn get_block_ranges(block_name: &str) -> Option<Vec<RunePair>> {
    // Normalize by removing "Is" prefix if present
    let name = block_name.strip_prefix("Is").unwrap_or(block_name);

    match name {
        "BasicLatin" => Some(vec![rp('\u{0000}', '\u{007F}')]),
        "Latin-1Supplement" => Some(vec![rp('\u{0080}', '\u{00FF}')]),
        "LatinExtended-A" => Some(vec![rp('\u{0100}', '\u{017F}')]),
        "LatinExtended-B" => Some(vec![rp('\u{0180}', '\u{024F}')]),
        "LatinExtendedAdditional" => Some(vec![rp('\u{1E00}', '\u{1EFF}')]),
        "IPAExtensions" => Some(vec![rp('\u{0250}', '\u{02AF}')]),
        "SpacingModifierLetters" => Some(vec![rp('\u{02B0}', '\u{02FF}')]),
        "CombiningDiacriticalMarks" => Some(vec![rp('\u{0300}', '\u{036F}')]),
        "CombiningDiacriticalMarksforSymbols" => Some(vec![rp('\u{20D0}', '\u{20FF}')]),
        "CombiningHalfMarks" => Some(vec![rp('\u{FE20}', '\u{FE2F}')]),
        "GreekandCoptic" | "Greek" => Some(vec![rp('\u{0370}', '\u{03FF}')]),
        "GreekExtended" => Some(vec![rp('\u{1F00}', '\u{1FFF}')]),
        "Cyrillic" => Some(vec![rp('\u{0400}', '\u{04FF}')]),
        "Armenian" => Some(vec![rp('\u{0530}', '\u{058F}')]),
        "Hebrew" => Some(vec![rp('\u{0590}', '\u{05FF}')]),
        "Arabic" => Some(vec![rp('\u{0600}', '\u{06FF}')]),
        "ArabicPresentationForms-A" => Some(vec![rp('\u{FB50}', '\u{FDFF}')]),
        "ArabicPresentationForms-B" => Some(vec![rp('\u{FE70}', '\u{FEFF}')]),
        "Syriac" => Some(vec![rp('\u{0700}', '\u{074F}')]),
        "Thaana" => Some(vec![rp('\u{0780}', '\u{07BF}')]),
        "Devanagari" => Some(vec![rp('\u{0900}', '\u{097F}')]),
        "Bengali" => Some(vec![rp('\u{0980}', '\u{09FF}')]),
        "Gurmukhi" => Some(vec![rp('\u{0A00}', '\u{0A7F}')]),
        "Gujarati" => Some(vec![rp('\u{0A80}', '\u{0AFF}')]),
        "Oriya" => Some(vec![rp('\u{0B00}', '\u{0B7F}')]),
        "Tamil" => Some(vec![rp('\u{0B80}', '\u{0BFF}')]),
        "Telugu" => Some(vec![rp('\u{0C00}', '\u{0C7F}')]),
        "Kannada" => Some(vec![rp('\u{0C80}', '\u{0CFF}')]),
        "Malayalam" => Some(vec![rp('\u{0D00}', '\u{0D7F}')]),
        "Sinhala" => Some(vec![rp('\u{0D80}', '\u{0DFF}')]),
        "Thai" => Some(vec![rp('\u{0E00}', '\u{0E7F}')]),
        "Lao" => Some(vec![rp('\u{0E80}', '\u{0EFF}')]),
        "Tibetan" => Some(vec![rp('\u{0F00}', '\u{0FFF}')]),
        "Myanmar" => Some(vec![rp('\u{1000}', '\u{109F}')]),
        "Georgian" => Some(vec![rp('\u{10A0}', '\u{10FF}')]),
        "HangulJamo" => Some(vec![rp('\u{1100}', '\u{11FF}')]),
        "HangulCompatibilityJamo" => Some(vec![rp('\u{3130}', '\u{318F}')]),
        "HangulSyllables" => Some(vec![rp('\u{AC00}', '\u{D7AF}')]),
        "Ethiopic" => Some(vec![rp('\u{1200}', '\u{137F}')]),
        "Cherokee" => Some(vec![rp('\u{13A0}', '\u{13FF}')]),
        "UnifiedCanadianAboriginalSyllabics" => Some(vec![rp('\u{1400}', '\u{167F}')]),
        "Ogham" => Some(vec![rp('\u{1680}', '\u{169F}')]),
        "Runic" => Some(vec![rp('\u{16A0}', '\u{16FF}')]),
        "Khmer" => Some(vec![rp('\u{1780}', '\u{17FF}')]),
        "Mongolian" => Some(vec![rp('\u{1800}', '\u{18AF}')]),
        "LetterlikeSymbols" => Some(vec![rp('\u{2100}', '\u{214F}')]),
        "NumberForms" => Some(vec![rp('\u{2150}', '\u{218F}')]),
        "Arrows" => Some(vec![rp('\u{2190}', '\u{21FF}')]),
        "MathematicalOperators" => Some(vec![rp('\u{2200}', '\u{22FF}')]),
        "MiscellaneousTechnical" => Some(vec![rp('\u{2300}', '\u{23FF}')]),
        "ControlPictures" => Some(vec![rp('\u{2400}', '\u{243F}')]),
        "OpticalCharacterRecognition" => Some(vec![rp('\u{2440}', '\u{245F}')]),
        "EnclosedAlphanumerics" => Some(vec![rp('\u{2460}', '\u{24FF}')]),
        "BoxDrawing" => Some(vec![rp('\u{2500}', '\u{257F}')]),
        "BlockElements" => Some(vec![rp('\u{2580}', '\u{259F}')]),
        "GeometricShapes" => Some(vec![rp('\u{25A0}', '\u{25FF}')]),
        "MiscellaneousSymbols" => Some(vec![rp('\u{2600}', '\u{26FF}')]),
        "Dingbats" => Some(vec![rp('\u{2700}', '\u{27BF}')]),
        "BraillePatterns" => Some(vec![rp('\u{2800}', '\u{28FF}')]),
        "CJKRadicalsSupplement" => Some(vec![rp('\u{2E80}', '\u{2EFF}')]),
        "KangxiRadicals" => Some(vec![rp('\u{2F00}', '\u{2FDF}')]),
        "IdeographicDescriptionCharacters" => Some(vec![rp('\u{2FF0}', '\u{2FFF}')]),
        "CJKSymbolsandPunctuation" => Some(vec![rp('\u{3000}', '\u{303F}')]),
        "Hiragana" => Some(vec![rp('\u{3040}', '\u{309F}')]),
        "Katakana" => Some(vec![rp('\u{30A0}', '\u{30FF}')]),
        "Bopomofo" => Some(vec![rp('\u{3100}', '\u{312F}')]),
        "BopomofoExtended" => Some(vec![rp('\u{31A0}', '\u{31BF}')]),
        "Kanbun" => Some(vec![rp('\u{3190}', '\u{319F}')]),
        "EnclosedCJKLettersandMonths" => Some(vec![rp('\u{3200}', '\u{32FF}')]),
        "CJKCompatibility" => Some(vec![rp('\u{3300}', '\u{33FF}')]),
        "CJKUnifiedIdeographsExtensionA" => Some(vec![rp('\u{3400}', '\u{4DBF}')]),
        "CJKUnifiedIdeographs" => Some(vec![rp('\u{4E00}', '\u{9FFF}')]),
        "CJKUnifiedIdeographsExtensionB" => Some(vec![rp('\u{20000}', '\u{2A6DF}')]),
        "CJKCompatibilityIdeographs" => Some(vec![rp('\u{F900}', '\u{FAFF}')]),
        "CJKCompatibilityIdeographsSupplement" => Some(vec![rp('\u{2F800}', '\u{2FA1F}')]),
        "CJKCompatibilityForms" => Some(vec![rp('\u{FE30}', '\u{FE4F}')]),
        "YiSyllables" => Some(vec![rp('\u{A000}', '\u{A48F}')]),
        "YiRadicals" => Some(vec![rp('\u{A490}', '\u{A4CF}')]),
        "AlphabeticPresentationForms" => Some(vec![rp('\u{FB00}', '\u{FB4F}')]),
        "SmallFormVariants" => Some(vec![rp('\u{FE50}', '\u{FE6F}')]),
        "HalfwidthandFullwidthForms" => Some(vec![rp('\u{FF00}', '\u{FFEF}')]),
        "Specials" => Some(vec![rp('\u{FFF0}', '\u{FFFF}')]),
        "GeneralPunctuation" => Some(vec![rp('\u{2000}', '\u{206F}')]),
        "SuperscriptsandSubscripts" => Some(vec![rp('\u{2070}', '\u{209F}')]),
        "CurrencySymbols" => Some(vec![rp('\u{20A0}', '\u{20CF}')]),
        "PrivateUseArea" => Some(vec![rp('\u{E000}', '\u{F8FF}')]),
        // HighSurrogates and LowSurrogates are UTF-16 surrogates, not valid
        // Rust char codepoints — return empty ranges since they cannot match.
        "HighSurrogates" | "LowSurrogates" => Some(vec![]),
        "Tags" => Some(vec![rp('\u{E0000}', '\u{E007F}')]),
        "SupplementaryPrivateUseArea-A" => Some(vec![rp('\u{F0000}', '\u{FFFFF}')]),
        "SupplementaryPrivateUseArea-B" => Some(vec![rp('\u{100000}', '\u{10FFFF}')]),
        "OldItalic" => Some(vec![rp('\u{10300}', '\u{1032F}')]),
        "Gothic" => Some(vec![rp('\u{10330}', '\u{1034F}')]),
        "Deseret" => Some(vec![rp('\u{10400}', '\u{1044F}')]),
        "ByzantineMusicalSymbols" => Some(vec![rp('\u{1D000}', '\u{1D0FF}')]),
        "MusicalSymbols" => Some(vec![rp('\u{1D100}', '\u{1D1FF}')]),
        "MathematicalAlphanumericSymbols" => Some(vec![rp('\u{1D400}', '\u{1D7FF}')]),
        _ => None,
    }
}

// Helper to create RunePair
#[inline]
const fn rp(lo: char, hi: char) -> RunePair {
    RunePair { lo, hi }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_category_lookup() {
        // Test general categories
        for name in ["L", "Lu", "Ll", "Nd", "C", "Cn"] {
            assert!(get_category_ranges(name).is_some(), "{name}");
        }
        for name in ["", "X", "Lx", "Cs", "Lu ", "LuX", "l"] {
            assert!(get_category_ranges(name).is_none(), "{name:?}");
        }
    }

    // MIRI SKIP RATIONALE: walks all 1.1M code points.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_categories_partition_unicode() {
        // Every code point except the surrogates sits in exactly one two-letter
        // category, and each one-letter category is the union of its own.
        const SUBCATEGORIES: &str = "Cc Cf Cn Co Ll Lm Lo Lt Lu Mc Me Mn Nd Nl No \
                                     Pc Pd Pe Pf Pi Po Ps Sc Sk Sm So Zl Zp Zs";
        let mut owner = vec![None; 0x11_0000];
        for name in SUBCATEGORIES.split_whitespace() {
            for pair in category(name) {
                let span = &mut owner[pair.lo as usize..=pair.hi as usize];
                assert!(span.iter().all(Option::is_none), "{name} overlaps another");
                span.fill(Some(name));
            }
        }
        for (cp, category) in owner.iter().enumerate() {
            assert_eq!(
                category.is_none(),
                (0xD800..=0xDFFF).contains(&cp),
                "U+{cp:04X}"
            );
        }
        for major in "CLMNPSZ".chars() {
            let members: usize = SUBCATEGORIES
                .split_whitespace()
                .filter(|name| name.starts_with(major))
                .flat_map(category)
                .map(|p| p.hi as usize - p.lo as usize + 1)
                .sum();
            let whole: usize = category(&major.to_string())
                .iter()
                .map(|p| p.hi as usize - p.lo as usize + 1)
                .sum();
            assert_eq!(whole, members, "{major}");
        }
        // Spot checks beyond the ASCII range.
        assert_eq!(owner['\u{100}' as usize], Some("Lu"));
        assert_eq!(owner['\u{101}' as usize], Some("Ll"));
        assert_eq!(owner['\u{378}' as usize], Some("Cn"));
        assert_eq!(owner['\u{10FFFF}' as usize], Some("Cn"));
    }

    #[test]
    fn test_block_lookup() {
        // Test block names
        assert!(get_block_ranges("IsBasicLatin").is_some());
        assert!(get_block_ranges("BasicLatin").is_some());
        assert!(get_block_ranges("IsHiragana").is_some());
        assert!(get_block_ranges("NonExistent").is_none());
    }

    fn category(name: &str) -> Vec<RunePair> {
        get_category_ranges(name).unwrap()
    }

    #[test]
    fn test_ascii_in_lu() {
        let lu = category("Lu");
        // 'A' should be in Lu
        let has_a = lu.iter().any(|rp| rp.lo <= 'A' && 'A' <= rp.hi);
        assert!(has_a, "Lu should contain 'A'");
    }

    #[test]
    fn test_ascii_in_ll() {
        let ll = category("Ll");
        // 'a' should be in Ll
        let has_a = ll.iter().any(|rp| rp.lo <= 'a' && 'a' <= rp.hi);
        assert!(has_a, "Ll should contain 'a'");
    }

    #[test]
    fn test_digit_in_nd() {
        let nd = category("Nd");
        // '0' should be in Nd
        let has_0 = nd.iter().any(|rp| rp.lo <= '0' && '0' <= rp.hi);
        assert!(has_0, "Nd should contain '0'");
    }
}
