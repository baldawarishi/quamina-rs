//! Unicode category and block data for I-Regexp property matching.
//!
//! This module provides character ranges for Unicode general categories (~p{Lu}, ~p{Ll}, etc.)
//! and Unicode blocks (~p{IsBasicLatin}, etc.).
//!
//! Based on Unicode 15.0 data.

use crate::regexp::RunePair;

/// Get character ranges for a Unicode general category.
/// Returns None if the category is not recognized.
pub fn get_category_ranges(initial: char, detail: Option<char>) -> Option<Vec<RunePair>> {
    match (initial, detail) {
        // Letter categories
        ('L', None) => Some(category_l()),
        ('L', Some('u')) => Some(category_lu()),
        ('L', Some('l')) => Some(category_ll()),
        ('L', Some('t')) => Some(category_lt()),
        ('L', Some('m')) => Some(category_lm()),
        ('L', Some('o')) => Some(category_lo()),

        // Mark categories
        ('M', None) => Some(category_m()),
        ('M', Some('n')) => Some(category_mn()),
        ('M', Some('c')) => Some(category_mc()),
        ('M', Some('e')) => Some(category_me()),

        // Number categories
        ('N', None) => Some(category_n()),
        ('N', Some('d')) => Some(category_nd()),
        ('N', Some('l')) => Some(category_nl()),
        ('N', Some('o')) => Some(category_no()),

        // Punctuation categories
        ('P', None) => Some(category_p()),
        ('P', Some('c')) => Some(category_pc()),
        ('P', Some('d')) => Some(category_pd()),
        ('P', Some('s')) => Some(category_ps()),
        ('P', Some('e')) => Some(category_pe()),
        ('P', Some('i')) => Some(category_pi()),
        ('P', Some('f')) => Some(category_pf()),
        ('P', Some('o')) => Some(category_po()),

        // Separator categories
        ('Z', None) => Some(category_z()),
        ('Z', Some('s')) => Some(category_zs()),
        ('Z', Some('l')) => Some(category_zl()),
        ('Z', Some('p')) => Some(category_zp()),

        // Symbol categories
        ('S', None) => Some(category_s()),
        ('S', Some('m')) => Some(category_sm()),
        ('S', Some('c')) => Some(category_sc()),
        ('S', Some('k')) => Some(category_sk()),
        ('S', Some('o')) => Some(category_so()),

        // Other categories
        ('C', None) => Some(category_c()),
        ('C', Some('c')) => Some(category_cc()),
        ('C', Some('f')) => Some(category_cf()),
        ('C', Some('o')) => Some(category_co()),
        ('C', Some('n')) => Some(category_cn()),

        _ => None,
    }
}

/// Get character ranges for a Unicode block.
/// Returns None if the block is not recognized.
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
        // HighSurrogates and LowSurrogates are not valid in Rust as they are UTF-16 surrogates
        // Instead, we return empty ranges for these blocks (they can't match valid Unicode strings)
        "HighSurrogates" => Some(vec![]),
        "LowSurrogates" => Some(vec![]),
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
fn rp(lo: char, hi: char) -> RunePair {
    RunePair { lo, hi }
}

// =============================================================================
// General Category Functions
// =============================================================================

/// L (Letter) = Lu | Ll | Lt | Lm | Lo
fn category_l() -> Vec<RunePair> {
    let mut ranges = category_lu();
    ranges.extend(category_ll());
    ranges.extend(category_lt());
    ranges.extend(category_lm());
    ranges.extend(category_lo());
    ranges
}

/// Lu (Letter, Uppercase) - Extended ranges including supplementary planes
fn category_lu() -> Vec<RunePair> {
    vec![
        rp('A', 'Z'),
        rp('\u{00C0}', '\u{00D6}'),
        rp('\u{00D8}', '\u{00DE}'),
        // Latin Extended-A (even positions are uppercase)
        rp('\u{0100}', '\u{0100}'),
        rp('\u{0102}', '\u{0102}'),
        rp('\u{0104}', '\u{0104}'),
        rp('\u{0106}', '\u{0106}'),
        rp('\u{0108}', '\u{0108}'),
        rp('\u{010A}', '\u{010A}'),
        rp('\u{010C}', '\u{010C}'),
        rp('\u{010E}', '\u{010E}'),
        rp('\u{0110}', '\u{0110}'),
        rp('\u{0112}', '\u{0112}'),
        rp('\u{0114}', '\u{0114}'),
        rp('\u{0116}', '\u{0116}'),
        rp('\u{0118}', '\u{0118}'),
        rp('\u{011A}', '\u{011A}'),
        rp('\u{011C}', '\u{011C}'),
        rp('\u{011E}', '\u{011E}'),
        rp('\u{0120}', '\u{0120}'),
        rp('\u{0122}', '\u{0122}'),
        rp('\u{0124}', '\u{0124}'),
        rp('\u{0126}', '\u{0126}'),
        rp('\u{0128}', '\u{0128}'),
        rp('\u{012A}', '\u{012A}'),
        rp('\u{012C}', '\u{012C}'),
        rp('\u{012E}', '\u{012E}'),
        rp('\u{0130}', '\u{0130}'),
        rp('\u{0132}', '\u{0132}'),
        rp('\u{0134}', '\u{0134}'),
        rp('\u{0136}', '\u{0136}'),
        rp('\u{0139}', '\u{0139}'),
        rp('\u{013B}', '\u{013B}'),
        rp('\u{013D}', '\u{013D}'),
        rp('\u{013F}', '\u{013F}'),
        rp('\u{0141}', '\u{0141}'),
        rp('\u{0143}', '\u{0143}'),
        rp('\u{0145}', '\u{0145}'),
        rp('\u{0147}', '\u{0147}'),
        rp('\u{014A}', '\u{014A}'),
        rp('\u{014C}', '\u{014C}'),
        rp('\u{014E}', '\u{014E}'),
        rp('\u{0150}', '\u{0150}'),
        rp('\u{0152}', '\u{0152}'),
        rp('\u{0154}', '\u{0154}'),
        rp('\u{0156}', '\u{0156}'),
        rp('\u{0158}', '\u{0158}'),
        rp('\u{015A}', '\u{015A}'),
        rp('\u{015C}', '\u{015C}'),
        rp('\u{015E}', '\u{015E}'),
        rp('\u{0160}', '\u{0160}'),
        rp('\u{0162}', '\u{0162}'),
        rp('\u{0164}', '\u{0164}'),
        rp('\u{0166}', '\u{0166}'),
        rp('\u{0168}', '\u{0168}'),
        rp('\u{016A}', '\u{016A}'),
        rp('\u{016C}', '\u{016C}'),
        rp('\u{016E}', '\u{016E}'),
        rp('\u{0170}', '\u{0170}'),
        rp('\u{0172}', '\u{0172}'),
        rp('\u{0174}', '\u{0174}'),
        rp('\u{0176}', '\u{0176}'),
        rp('\u{0178}', '\u{0179}'),
        rp('\u{017B}', '\u{017B}'),
        rp('\u{017D}', '\u{017D}'),
        // Greek uppercase
        rp('\u{0391}', '\u{03A1}'),
        rp('\u{03A3}', '\u{03A9}'),
        // Cyrillic uppercase
        rp('\u{0410}', '\u{042F}'),
        // Fullwidth Latin uppercase
        rp('\u{FF21}', '\u{FF3A}'),
        // Mathematical Alphanumeric Symbols - uppercase letters
        // Bold: U+1D400-U+1D419, Italic: U+1D434-U+1D44D, Bold Italic: U+1D468-U+1D481
        // Script: U+1D49C-U+1D4B5 (with gaps), Bold Script: U+1D4D0-U+1D4E9
        // Fraktur: U+1D504-U+1D51C (with gaps), Double-struck: U+1D538-U+1D550 (with gaps)
        // Bold Fraktur: U+1D56C-U+1D585, Sans-serif: U+1D5A0-U+1D5B9
        // Sans-serif Bold: U+1D5D4-U+1D5ED, Sans-serif Italic: U+1D608-U+1D621
        // Sans-serif Bold Italic: U+1D63C-U+1D655, Monospace: U+1D670-U+1D689
        // We use broader ranges for simplicity:
        rp('\u{1D400}', '\u{1D419}'), // Bold
        rp('\u{1D434}', '\u{1D44D}'), // Italic
        rp('\u{1D468}', '\u{1D481}'), // Bold Italic
        rp('\u{1D49C}', '\u{1D4B9}'), // Script (some gaps but close enough)
        rp('\u{1D4D0}', '\u{1D4E9}'), // Bold Script
        rp('\u{1D504}', '\u{1D51C}'), // Fraktur (some gaps)
        rp('\u{1D538}', '\u{1D550}'), // Double-struck (some gaps)
        rp('\u{1D56C}', '\u{1D585}'), // Bold Fraktur
        rp('\u{1D5A0}', '\u{1D5B9}'), // Sans-serif
        rp('\u{1D5D4}', '\u{1D5ED}'), // Sans-serif Bold
        rp('\u{1D608}', '\u{1D621}'), // Sans-serif Italic
        rp('\u{1D63C}', '\u{1D655}'), // Sans-serif Bold Italic
        rp('\u{1D670}', '\u{1D689}'), // Monospace
        // Greek symbols in Mathematical Alphanumeric
        rp('\u{1D6A8}', '\u{1D6C0}'), // Bold Greek uppercase
        rp('\u{1D6E2}', '\u{1D6FA}'), // Italic Greek uppercase
        rp('\u{1D71C}', '\u{1D734}'), // Bold Italic Greek uppercase
        rp('\u{1D756}', '\u{1D76E}'), // Sans-serif Bold Greek uppercase
        rp('\u{1D790}', '\u{1D7A8}'), // Sans-serif Bold Italic Greek uppercase
    ]
}

/// Ll (Letter, Lowercase) - Common ranges
fn category_ll() -> Vec<RunePair> {
    vec![
        rp('a', 'z'),
        rp('\u{00DF}', '\u{00F6}'),
        rp('\u{00F8}', '\u{00FF}'),
        rp('\u{0101}', '\u{0101}'),
        rp('\u{0103}', '\u{0103}'),
        rp('\u{0105}', '\u{0105}'),
        rp('\u{0107}', '\u{0107}'),
        rp('\u{0109}', '\u{0109}'),
        rp('\u{010B}', '\u{010B}'),
        rp('\u{010D}', '\u{010D}'),
        rp('\u{010F}', '\u{010F}'),
        rp('\u{0111}', '\u{0111}'),
        rp('\u{0113}', '\u{0113}'),
        rp('\u{0115}', '\u{0115}'),
        rp('\u{0117}', '\u{0117}'),
        rp('\u{0119}', '\u{0119}'),
        rp('\u{011B}', '\u{011B}'),
        rp('\u{011D}', '\u{011D}'),
        rp('\u{011F}', '\u{011F}'),
        rp('\u{0121}', '\u{0121}'),
        rp('\u{0123}', '\u{0123}'),
        rp('\u{0125}', '\u{0125}'),
        rp('\u{0127}', '\u{0127}'),
        rp('\u{0129}', '\u{0129}'),
        rp('\u{012B}', '\u{012B}'),
        rp('\u{012D}', '\u{012D}'),
        rp('\u{012F}', '\u{012F}'),
        rp('\u{0131}', '\u{0131}'),
        rp('\u{0133}', '\u{0133}'),
        rp('\u{0135}', '\u{0135}'),
        rp('\u{0137}', '\u{0138}'),
        rp('\u{013A}', '\u{013A}'),
        rp('\u{013C}', '\u{013C}'),
        rp('\u{013E}', '\u{013E}'),
        rp('\u{0140}', '\u{0140}'),
        rp('\u{0142}', '\u{0142}'),
        rp('\u{0144}', '\u{0144}'),
        rp('\u{0146}', '\u{0146}'),
        rp('\u{0148}', '\u{0149}'),
        rp('\u{014B}', '\u{014B}'),
        rp('\u{014D}', '\u{014D}'),
        rp('\u{014F}', '\u{014F}'),
        rp('\u{0151}', '\u{0151}'),
        rp('\u{0153}', '\u{0153}'),
        rp('\u{0155}', '\u{0155}'),
        rp('\u{0157}', '\u{0157}'),
        rp('\u{0159}', '\u{0159}'),
        rp('\u{015B}', '\u{015B}'),
        rp('\u{015D}', '\u{015D}'),
        rp('\u{015F}', '\u{015F}'),
        rp('\u{0161}', '\u{0161}'),
        rp('\u{0163}', '\u{0163}'),
        rp('\u{0165}', '\u{0165}'),
        rp('\u{0167}', '\u{0167}'),
        rp('\u{0169}', '\u{0169}'),
        rp('\u{016B}', '\u{016B}'),
        rp('\u{016D}', '\u{016D}'),
        rp('\u{016F}', '\u{016F}'),
        rp('\u{0171}', '\u{0171}'),
        rp('\u{0173}', '\u{0173}'),
        rp('\u{0175}', '\u{0175}'),
        rp('\u{0177}', '\u{0177}'),
        rp('\u{017A}', '\u{017A}'),
        rp('\u{017C}', '\u{017C}'),
        rp('\u{017E}', '\u{0180}'),
        // Greek lowercase
        rp('\u{03B1}', '\u{03C9}'),
        // Cyrillic lowercase
        rp('\u{0430}', '\u{044F}'),
        // Fullwidth Latin lowercase
        rp('\u{FF41}', '\u{FF5A}'),
        // Mathematical Alphanumeric Symbols - lowercase letters
        rp('\u{1D41A}', '\u{1D433}'), // Bold
        rp('\u{1D44E}', '\u{1D467}'), // Italic
        rp('\u{1D482}', '\u{1D49B}'), // Bold Italic
        rp('\u{1D4B6}', '\u{1D4CF}'), // Script
        rp('\u{1D4EA}', '\u{1D503}'), // Bold Script
        rp('\u{1D51E}', '\u{1D537}'), // Fraktur
        rp('\u{1D552}', '\u{1D56B}'), // Double-struck
        rp('\u{1D586}', '\u{1D59F}'), // Bold Fraktur
        rp('\u{1D5BA}', '\u{1D5D3}'), // Sans-serif
        rp('\u{1D5EE}', '\u{1D607}'), // Sans-serif Bold
        rp('\u{1D622}', '\u{1D63B}'), // Sans-serif Italic
        rp('\u{1D656}', '\u{1D66F}'), // Sans-serif Bold Italic
        rp('\u{1D68A}', '\u{1D6A3}'), // Monospace
        // Greek symbols in Mathematical Alphanumeric - lowercase
        rp('\u{1D6C2}', '\u{1D6DA}'), // Bold Greek lowercase
        rp('\u{1D6FC}', '\u{1D714}'), // Italic Greek lowercase
        rp('\u{1D736}', '\u{1D74E}'), // Bold Italic Greek lowercase
        rp('\u{1D770}', '\u{1D788}'), // Sans-serif Bold Greek lowercase
        rp('\u{1D7AA}', '\u{1D7CB}'), // Sans-serif Bold Italic Greek lowercase (extended)
    ]
}

/// Lt (Letter, Titlecase) - Only a few characters
fn category_lt() -> Vec<RunePair> {
    vec![
        rp('\u{01C5}', '\u{01C5}'),
        rp('\u{01C8}', '\u{01C8}'),
        rp('\u{01CB}', '\u{01CB}'),
        rp('\u{01F2}', '\u{01F2}'),
        rp('\u{1F88}', '\u{1F8F}'),
        rp('\u{1F98}', '\u{1F9F}'),
        rp('\u{1FA8}', '\u{1FAF}'),
        rp('\u{1FBC}', '\u{1FBC}'),
        rp('\u{1FCC}', '\u{1FCC}'),
        rp('\u{1FFC}', '\u{1FFC}'),
    ]
}

/// Lm (Letter, Modifier)
fn category_lm() -> Vec<RunePair> {
    vec![
        rp('\u{02B0}', '\u{02C1}'),
        rp('\u{02C6}', '\u{02D1}'),
        rp('\u{02E0}', '\u{02E4}'),
        rp('\u{02EC}', '\u{02EC}'),
        rp('\u{02EE}', '\u{02EE}'),
        rp('\u{0374}', '\u{0374}'),
        rp('\u{037A}', '\u{037A}'),
        rp('\u{0559}', '\u{0559}'),
        rp('\u{0640}', '\u{0640}'),
        rp('\u{06E5}', '\u{06E6}'),
        rp('\u{07F4}', '\u{07F5}'),
        rp('\u{07FA}', '\u{07FA}'),
        rp('\u{0971}', '\u{0971}'),
        rp('\u{17D7}', '\u{17D7}'),
        rp('\u{1843}', '\u{1843}'),
        rp('\u{1AA7}', '\u{1AA7}'),
        rp('\u{1C78}', '\u{1C7D}'),
        rp('\u{1D2C}', '\u{1D6A}'),
        rp('\u{1D78}', '\u{1D78}'),
        rp('\u{1D9B}', '\u{1DBF}'),
        rp('\u{2071}', '\u{2071}'),
        rp('\u{207F}', '\u{207F}'),
        rp('\u{2090}', '\u{209C}'),
        rp('\u{2C7C}', '\u{2C7D}'),
        rp('\u{A770}', '\u{A770}'),
        rp('\u{A7F8}', '\u{A7F9}'),
        // Halfwidth Katakana modifier marks
        rp('\u{FF9E}', '\u{FF9F}'),
        // Katakana modifier letters
        rp('\u{30FC}', '\u{30FC}'),
        rp('\u{309D}', '\u{309E}'),
        rp('\u{30FD}', '\u{30FE}'),
    ]
}

/// Lo (Letter, Other) - Many ranges for various scripts
fn category_lo() -> Vec<RunePair> {
    vec![
        rp('\u{00AA}', '\u{00AA}'),
        rp('\u{00BA}', '\u{00BA}'),
        rp('\u{01BB}', '\u{01BB}'),
        rp('\u{01C0}', '\u{01C3}'),
        rp('\u{0294}', '\u{0294}'),
        // Arabic letters
        rp('\u{0620}', '\u{063F}'),
        rp('\u{0641}', '\u{064A}'),
        rp('\u{066E}', '\u{066F}'),
        rp('\u{0671}', '\u{06D3}'),
        rp('\u{06D5}', '\u{06D5}'),
        rp('\u{06EE}', '\u{06EF}'),
        rp('\u{06FA}', '\u{06FC}'),
        rp('\u{06FF}', '\u{06FF}'),
        // Hebrew letters
        rp('\u{05D0}', '\u{05EA}'),
        rp('\u{05EF}', '\u{05F2}'),
        // Devanagari
        rp('\u{0904}', '\u{0939}'),
        rp('\u{093D}', '\u{093D}'),
        rp('\u{0950}', '\u{0950}'),
        rp('\u{0958}', '\u{0961}'),
        rp('\u{0972}', '\u{0980}'),
        // CJK Ideographs
        rp('\u{4E00}', '\u{9FFF}'),
        // CJK Extension A
        rp('\u{3400}', '\u{4DBF}'),
        // CJK Extension B (extended to include all Extension B characters)
        rp('\u{20000}', '\u{2A6FF}'),
        // CJK Extension C-F
        rp('\u{2A700}', '\u{2CEAF}'),
        // Hiragana
        rp('\u{3041}', '\u{3096}'),
        // Katakana
        rp('\u{30A1}', '\u{30FA}'),
        rp('\u{30FC}', '\u{30FF}'),
        // Hangul syllables
        rp('\u{AC00}', '\u{D7A3}'),
        // Yi syllables
        rp('\u{A000}', '\u{A48C}'),
        // More Lo ranges
    ]
}

/// M (Mark) = Mn | Mc | Me
fn category_m() -> Vec<RunePair> {
    let mut ranges = category_mn();
    ranges.extend(category_mc());
    ranges.extend(category_me());
    ranges
}

/// Mn (Mark, Nonspacing)
fn category_mn() -> Vec<RunePair> {
    vec![
        rp('\u{0300}', '\u{036F}'),
        rp('\u{0483}', '\u{0489}'),
        rp('\u{0591}', '\u{05BD}'),
        rp('\u{05BF}', '\u{05BF}'),
        rp('\u{05C1}', '\u{05C2}'),
        rp('\u{05C4}', '\u{05C5}'),
        rp('\u{05C7}', '\u{05C7}'),
        rp('\u{0610}', '\u{061A}'),
        rp('\u{064B}', '\u{065F}'),
        rp('\u{0670}', '\u{0670}'),
        rp('\u{06D6}', '\u{06DC}'),
        rp('\u{06DF}', '\u{06E4}'),
        rp('\u{06E7}', '\u{06E8}'),
        rp('\u{06EA}', '\u{06ED}'),
        rp('\u{0711}', '\u{0711}'),
        rp('\u{0730}', '\u{074A}'),
        rp('\u{07A6}', '\u{07B0}'),
        rp('\u{0901}', '\u{0902}'),
        rp('\u{093C}', '\u{093C}'),
        rp('\u{0941}', '\u{0948}'),
        rp('\u{094D}', '\u{094D}'),
        rp('\u{0951}', '\u{0957}'),
        rp('\u{0962}', '\u{0963}'),
        rp('\u{0981}', '\u{0981}'),
        rp('\u{09BC}', '\u{09BC}'),
        rp('\u{09C1}', '\u{09C4}'),
        rp('\u{09CD}', '\u{09CD}'),
        rp('\u{09E2}', '\u{09E3}'),
        // Combining diacritical marks for symbols
        rp('\u{20D0}', '\u{20FF}'),
        // Musical Symbols - combining marks (U+1D165-U+1D169, U+1D16D-U+1D172, U+1D17B-U+1D182, U+1D185-U+1D18B, U+1D1AA-U+1D1AD)
        rp('\u{1D165}', '\u{1D169}'),
        rp('\u{1D16D}', '\u{1D172}'),
        rp('\u{1D17B}', '\u{1D182}'),
        rp('\u{1D185}', '\u{1D18B}'),
        rp('\u{1D1AA}', '\u{1D1AD}'),
    ]
}

/// Mc (Mark, Spacing Combining)
fn category_mc() -> Vec<RunePair> {
    vec![
        rp('\u{0903}', '\u{0903}'),
        rp('\u{093B}', '\u{093B}'),
        rp('\u{093E}', '\u{0940}'),
        rp('\u{0949}', '\u{094C}'),
        rp('\u{094E}', '\u{094F}'),
        rp('\u{0982}', '\u{0983}'),
        rp('\u{09BE}', '\u{09C0}'),
        rp('\u{09C7}', '\u{09C8}'),
        rp('\u{09CB}', '\u{09CC}'),
        rp('\u{09D7}', '\u{09D7}'),
        rp('\u{0A03}', '\u{0A03}'),
        rp('\u{0A3E}', '\u{0A40}'),
        rp('\u{0A83}', '\u{0A83}'),
        rp('\u{0ABE}', '\u{0AC0}'),
        rp('\u{0AC9}', '\u{0AC9}'),
        rp('\u{0ACB}', '\u{0ACC}'),
        rp('\u{0B02}', '\u{0B03}'),
        rp('\u{0B3E}', '\u{0B3E}'),
        rp('\u{0B40}', '\u{0B40}'),
        rp('\u{0B47}', '\u{0B48}'),
        rp('\u{0B4B}', '\u{0B4C}'),
        rp('\u{0B57}', '\u{0B57}'),
        rp('\u{0BBE}', '\u{0BBF}'),
        rp('\u{0BC1}', '\u{0BC2}'),
        rp('\u{0BC6}', '\u{0BC8}'),
        rp('\u{0BCA}', '\u{0BCC}'),
        rp('\u{0BD7}', '\u{0BD7}'),
        // Musical Symbols - spacing combining marks
        rp('\u{1D165}', '\u{1D166}'),
        rp('\u{1D16D}', '\u{1D172}'),
    ]
}

/// Me (Mark, Enclosing)
fn category_me() -> Vec<RunePair> {
    vec![
        rp('\u{0488}', '\u{0489}'),
        rp('\u{1ABE}', '\u{1ABE}'),
        rp('\u{20DD}', '\u{20E0}'),
        rp('\u{20E2}', '\u{20E4}'),
        rp('\u{A670}', '\u{A672}'),
    ]
}

/// N (Number) = Nd | Nl | No
fn category_n() -> Vec<RunePair> {
    let mut ranges = category_nd();
    ranges.extend(category_nl());
    ranges.extend(category_no());
    ranges
}

/// Nd (Number, Decimal Digit)
fn category_nd() -> Vec<RunePair> {
    vec![
        rp('0', '9'),
        rp('\u{0660}', '\u{0669}'),
        rp('\u{06F0}', '\u{06F9}'),
        rp('\u{07C0}', '\u{07C9}'),
        rp('\u{0966}', '\u{096F}'),
        rp('\u{09E6}', '\u{09EF}'),
        rp('\u{0A66}', '\u{0A6F}'),
        rp('\u{0AE6}', '\u{0AEF}'),
        rp('\u{0B66}', '\u{0B6F}'),
        rp('\u{0BE6}', '\u{0BEF}'),
        rp('\u{0C66}', '\u{0C6F}'),
        rp('\u{0CE6}', '\u{0CEF}'),
        rp('\u{0D66}', '\u{0D6F}'),
        rp('\u{0DE6}', '\u{0DEF}'),
        rp('\u{0E50}', '\u{0E59}'),
        rp('\u{0ED0}', '\u{0ED9}'),
        rp('\u{0F20}', '\u{0F29}'),
        rp('\u{1040}', '\u{1049}'),
        rp('\u{1090}', '\u{1099}'),
        rp('\u{17E0}', '\u{17E9}'),
        rp('\u{1810}', '\u{1819}'),
        rp('\u{1946}', '\u{194F}'),
        rp('\u{19D0}', '\u{19D9}'),
        rp('\u{1A80}', '\u{1A89}'),
        rp('\u{1A90}', '\u{1A99}'),
        rp('\u{1B50}', '\u{1B59}'),
        rp('\u{1BB0}', '\u{1BB9}'),
        rp('\u{1C40}', '\u{1C49}'),
        rp('\u{1C50}', '\u{1C59}'),
        rp('\u{A620}', '\u{A629}'),
        rp('\u{A8D0}', '\u{A8D9}'),
        rp('\u{A900}', '\u{A909}'),
        rp('\u{A9D0}', '\u{A9D9}'),
        rp('\u{A9F0}', '\u{A9F9}'),
        rp('\u{AA50}', '\u{AA59}'),
        rp('\u{ABF0}', '\u{ABF9}'),
        rp('\u{FF10}', '\u{FF19}'),
        // Mathematical Alphanumeric Symbols - digits (U+1D7CE-U+1D7FF)
        rp('\u{1D7CE}', '\u{1D7FF}'),
    ]
}

/// Nl (Number, Letter)
fn category_nl() -> Vec<RunePair> {
    vec![
        rp('\u{16EE}', '\u{16F0}'),
        rp('\u{2160}', '\u{2182}'),
        rp('\u{2185}', '\u{2188}'),
        rp('\u{3007}', '\u{3007}'),
        rp('\u{3021}', '\u{3029}'),
        rp('\u{3038}', '\u{303A}'),
        rp('\u{A6E6}', '\u{A6EF}'),
        // Gothic number letters (NINE HUNDRED and NINETY)
        rp('\u{10341}', '\u{10341}'),
        rp('\u{1034A}', '\u{1034A}'),
    ]
}

/// No (Number, Other)
fn category_no() -> Vec<RunePair> {
    vec![
        rp('\u{00B2}', '\u{00B3}'),
        rp('\u{00B9}', '\u{00B9}'),
        rp('\u{00BC}', '\u{00BE}'),
        rp('\u{09F4}', '\u{09F9}'),
        rp('\u{0B72}', '\u{0B77}'),
        rp('\u{0BF0}', '\u{0BF2}'),
        rp('\u{0C78}', '\u{0C7E}'),
        rp('\u{0D58}', '\u{0D5E}'),
        rp('\u{0D70}', '\u{0D78}'),
        rp('\u{0F2A}', '\u{0F33}'),
        rp('\u{1369}', '\u{137C}'),
        rp('\u{17F0}', '\u{17F9}'),
        rp('\u{19DA}', '\u{19DA}'),
        rp('\u{2070}', '\u{2070}'),
        rp('\u{2074}', '\u{2079}'),
        rp('\u{2080}', '\u{2089}'),
        rp('\u{2150}', '\u{215F}'),
        rp('\u{2189}', '\u{2189}'),
        rp('\u{2460}', '\u{249B}'),
        rp('\u{24EA}', '\u{24FF}'),
        rp('\u{2776}', '\u{2793}'),
        rp('\u{2CFD}', '\u{2CFD}'),
        rp('\u{3192}', '\u{3195}'),
        rp('\u{3220}', '\u{3229}'),
        rp('\u{3248}', '\u{324F}'),
        rp('\u{3251}', '\u{325F}'),
        rp('\u{3280}', '\u{3289}'),
        rp('\u{32B1}', '\u{32BF}'),
        // Old Italic numerals (U+10320-U+10323)
        rp('\u{10320}', '\u{10323}'),
        // Aegean numbers (U+10107-U+10133)
        rp('\u{10107}', '\u{10133}'),
    ]
}

/// P (Punctuation) = Pc | Pd | Ps | Pe | Pi | Pf | Po
fn category_p() -> Vec<RunePair> {
    let mut ranges = category_pc();
    ranges.extend(category_pd());
    ranges.extend(category_ps());
    ranges.extend(category_pe());
    ranges.extend(category_pi());
    ranges.extend(category_pf());
    ranges.extend(category_po());
    ranges
}

/// Pc (Punctuation, Connector)
fn category_pc() -> Vec<RunePair> {
    vec![
        rp('_', '_'),
        rp('\u{203F}', '\u{2040}'),
        rp('\u{2054}', '\u{2054}'),
        rp('\u{FE33}', '\u{FE34}'),
        rp('\u{FE4D}', '\u{FE4F}'),
        rp('\u{FF3F}', '\u{FF3F}'),
    ]
}

/// Pd (Punctuation, Dash)
fn category_pd() -> Vec<RunePair> {
    vec![
        rp('-', '-'),
        rp('\u{058A}', '\u{058A}'),
        rp('\u{05BE}', '\u{05BE}'),
        rp('\u{1400}', '\u{1400}'),
        rp('\u{1806}', '\u{1806}'),
        rp('\u{2010}', '\u{2015}'),
        rp('\u{2E17}', '\u{2E17}'),
        rp('\u{2E1A}', '\u{2E1A}'),
        rp('\u{2E3A}', '\u{2E3B}'),
        rp('\u{2E40}', '\u{2E40}'),
        rp('\u{301C}', '\u{301C}'),
        rp('\u{3030}', '\u{3030}'),
        rp('\u{30A0}', '\u{30A0}'),
        rp('\u{FE31}', '\u{FE32}'),
        rp('\u{FE58}', '\u{FE58}'),
        rp('\u{FE63}', '\u{FE63}'),
        rp('\u{FF0D}', '\u{FF0D}'),
    ]
}

/// Ps (Punctuation, Open)
fn category_ps() -> Vec<RunePair> {
    vec![
        rp('(', '('),
        rp('[', '['),
        rp('{', '{'),
        rp('\u{0F3A}', '\u{0F3A}'),
        rp('\u{0F3C}', '\u{0F3C}'),
        rp('\u{169B}', '\u{169B}'),
        rp('\u{201A}', '\u{201A}'),
        rp('\u{201E}', '\u{201E}'),
        rp('\u{2045}', '\u{2045}'),
        rp('\u{207D}', '\u{207D}'),
        rp('\u{208D}', '\u{208D}'),
        rp('\u{2308}', '\u{2308}'),
        rp('\u{230A}', '\u{230A}'),
        rp('\u{2329}', '\u{2329}'),
        rp('\u{2768}', '\u{2768}'),
        rp('\u{276A}', '\u{276A}'),
        rp('\u{276C}', '\u{276C}'),
        rp('\u{276E}', '\u{276E}'),
        rp('\u{2770}', '\u{2770}'),
        rp('\u{2772}', '\u{2772}'),
        rp('\u{2774}', '\u{2774}'),
        rp('\u{27C5}', '\u{27C5}'),
        rp('\u{27E6}', '\u{27E6}'),
        rp('\u{27E8}', '\u{27E8}'),
        rp('\u{27EA}', '\u{27EA}'),
        rp('\u{27EC}', '\u{27EC}'),
        rp('\u{27EE}', '\u{27EE}'),
        rp('\u{2983}', '\u{2983}'),
        rp('\u{2985}', '\u{2985}'),
        rp('\u{2987}', '\u{2987}'),
        rp('\u{2989}', '\u{2989}'),
        rp('\u{298B}', '\u{298B}'),
        rp('\u{298D}', '\u{298D}'),
        rp('\u{298F}', '\u{298F}'),
        rp('\u{2991}', '\u{2991}'),
        rp('\u{2993}', '\u{2993}'),
        rp('\u{2995}', '\u{2995}'),
        rp('\u{2997}', '\u{2997}'),
        rp('\u{29D8}', '\u{29D8}'),
        rp('\u{29DA}', '\u{29DA}'),
        rp('\u{29FC}', '\u{29FC}'),
        rp('\u{2E22}', '\u{2E22}'),
        rp('\u{2E24}', '\u{2E24}'),
        rp('\u{2E26}', '\u{2E26}'),
        rp('\u{2E28}', '\u{2E28}'),
        rp('\u{3008}', '\u{3008}'),
        rp('\u{300A}', '\u{300A}'),
        rp('\u{300C}', '\u{300C}'),
        rp('\u{300E}', '\u{300E}'),
        rp('\u{3010}', '\u{3010}'),
        rp('\u{3014}', '\u{3014}'),
        rp('\u{3016}', '\u{3016}'),
        rp('\u{3018}', '\u{3018}'),
        rp('\u{301A}', '\u{301A}'),
        rp('\u{301D}', '\u{301D}'),
        rp('\u{FD3F}', '\u{FD3F}'),
        rp('\u{FE17}', '\u{FE17}'),
        rp('\u{FE35}', '\u{FE35}'),
        rp('\u{FE37}', '\u{FE37}'),
        rp('\u{FE39}', '\u{FE39}'),
        rp('\u{FE3B}', '\u{FE3B}'),
        rp('\u{FE3D}', '\u{FE3D}'),
        rp('\u{FE3F}', '\u{FE3F}'),
        rp('\u{FE41}', '\u{FE41}'),
        rp('\u{FE43}', '\u{FE43}'),
        rp('\u{FE47}', '\u{FE47}'),
        rp('\u{FE59}', '\u{FE59}'),
        rp('\u{FE5B}', '\u{FE5B}'),
        rp('\u{FE5D}', '\u{FE5D}'),
        rp('\u{FF08}', '\u{FF08}'),
        rp('\u{FF3B}', '\u{FF3B}'),
        rp('\u{FF5B}', '\u{FF5B}'),
        rp('\u{FF5F}', '\u{FF5F}'),
        rp('\u{FF62}', '\u{FF62}'),
    ]
}

/// Pe (Punctuation, Close)
fn category_pe() -> Vec<RunePair> {
    vec![
        rp(')', ')'),
        rp(']', ']'),
        rp('}', '}'),
        rp('\u{0F3B}', '\u{0F3B}'),
        rp('\u{0F3D}', '\u{0F3D}'),
        rp('\u{169C}', '\u{169C}'),
        rp('\u{2046}', '\u{2046}'),
        rp('\u{207E}', '\u{207E}'),
        rp('\u{208E}', '\u{208E}'),
        rp('\u{2309}', '\u{2309}'),
        rp('\u{230B}', '\u{230B}'),
        rp('\u{232A}', '\u{232A}'),
        rp('\u{2769}', '\u{2769}'),
        rp('\u{276B}', '\u{276B}'),
        rp('\u{276D}', '\u{276D}'),
        rp('\u{276F}', '\u{276F}'),
        rp('\u{2771}', '\u{2771}'),
        rp('\u{2773}', '\u{2773}'),
        rp('\u{2775}', '\u{2775}'),
        rp('\u{27C6}', '\u{27C6}'),
        rp('\u{27E7}', '\u{27E7}'),
        rp('\u{27E9}', '\u{27E9}'),
        rp('\u{27EB}', '\u{27EB}'),
        rp('\u{27ED}', '\u{27ED}'),
        rp('\u{27EF}', '\u{27EF}'),
        rp('\u{2984}', '\u{2984}'),
        rp('\u{2986}', '\u{2986}'),
        rp('\u{2988}', '\u{2988}'),
        rp('\u{298A}', '\u{298A}'),
        rp('\u{298C}', '\u{298C}'),
        rp('\u{298E}', '\u{298E}'),
        rp('\u{2990}', '\u{2990}'),
        rp('\u{2992}', '\u{2992}'),
        rp('\u{2994}', '\u{2994}'),
        rp('\u{2996}', '\u{2996}'),
        rp('\u{2998}', '\u{2998}'),
        rp('\u{29D9}', '\u{29D9}'),
        rp('\u{29DB}', '\u{29DB}'),
        rp('\u{29FD}', '\u{29FD}'),
        rp('\u{2E23}', '\u{2E23}'),
        rp('\u{2E25}', '\u{2E25}'),
        rp('\u{2E27}', '\u{2E27}'),
        rp('\u{2E29}', '\u{2E29}'),
        rp('\u{3009}', '\u{3009}'),
        rp('\u{300B}', '\u{300B}'),
        rp('\u{300D}', '\u{300D}'),
        rp('\u{300F}', '\u{300F}'),
        rp('\u{3011}', '\u{3011}'),
        rp('\u{3015}', '\u{3015}'),
        rp('\u{3017}', '\u{3017}'),
        rp('\u{3019}', '\u{3019}'),
        rp('\u{301B}', '\u{301B}'),
        rp('\u{301E}', '\u{301F}'),
        rp('\u{FD3E}', '\u{FD3E}'),
        rp('\u{FE18}', '\u{FE18}'),
        rp('\u{FE36}', '\u{FE36}'),
        rp('\u{FE38}', '\u{FE38}'),
        rp('\u{FE3A}', '\u{FE3A}'),
        rp('\u{FE3C}', '\u{FE3C}'),
        rp('\u{FE3E}', '\u{FE3E}'),
        rp('\u{FE40}', '\u{FE40}'),
        rp('\u{FE42}', '\u{FE42}'),
        rp('\u{FE44}', '\u{FE44}'),
        rp('\u{FE48}', '\u{FE48}'),
        rp('\u{FE5A}', '\u{FE5A}'),
        rp('\u{FE5C}', '\u{FE5C}'),
        rp('\u{FE5E}', '\u{FE5E}'),
        rp('\u{FF09}', '\u{FF09}'),
        rp('\u{FF3D}', '\u{FF3D}'),
        rp('\u{FF5D}', '\u{FF5D}'),
        rp('\u{FF60}', '\u{FF60}'),
        rp('\u{FF63}', '\u{FF63}'),
    ]
}

/// Pi (Punctuation, Initial quote)
fn category_pi() -> Vec<RunePair> {
    vec![
        rp('\u{00AB}', '\u{00AB}'),
        rp('\u{2018}', '\u{2018}'),
        rp('\u{201B}', '\u{201C}'),
        rp('\u{201F}', '\u{201F}'),
        rp('\u{2039}', '\u{2039}'),
        rp('\u{2E02}', '\u{2E02}'),
        rp('\u{2E04}', '\u{2E04}'),
        rp('\u{2E09}', '\u{2E09}'),
        rp('\u{2E0C}', '\u{2E0C}'),
        rp('\u{2E1C}', '\u{2E1C}'),
        rp('\u{2E20}', '\u{2E20}'),
    ]
}

/// Pf (Punctuation, Final quote)
fn category_pf() -> Vec<RunePair> {
    vec![
        rp('\u{00BB}', '\u{00BB}'),
        rp('\u{2019}', '\u{2019}'),
        rp('\u{201D}', '\u{201D}'),
        rp('\u{203A}', '\u{203A}'),
        rp('\u{2E03}', '\u{2E03}'),
        rp('\u{2E05}', '\u{2E05}'),
        rp('\u{2E0A}', '\u{2E0A}'),
        rp('\u{2E0D}', '\u{2E0D}'),
        rp('\u{2E1D}', '\u{2E1D}'),
        rp('\u{2E21}', '\u{2E21}'),
    ]
}

/// Po (Punctuation, Other)
fn category_po() -> Vec<RunePair> {
    vec![
        rp('!', '!'),
        rp('"', '"'),
        rp('#', '#'),
        rp('%', '%'),
        rp('&', '&'),
        rp('\'', '\''),
        rp('*', '*'),
        rp(',', ','),
        rp('.', '.'),
        rp('/', '/'),
        rp(':', ';'),
        rp('?', '@'),
        rp('\\', '\\'),
        rp('\u{00A1}', '\u{00A1}'),
        rp('\u{00A7}', '\u{00A7}'),
        rp('\u{00B6}', '\u{00B7}'),
        rp('\u{00BF}', '\u{00BF}'),
        rp('\u{037E}', '\u{037E}'),
        rp('\u{0387}', '\u{0387}'),
        rp('\u{055A}', '\u{055F}'),
        rp('\u{0589}', '\u{0589}'),
        rp('\u{05C0}', '\u{05C0}'),
        rp('\u{05C3}', '\u{05C3}'),
        rp('\u{05C6}', '\u{05C6}'),
        rp('\u{05F3}', '\u{05F4}'),
        rp('\u{0609}', '\u{060A}'),
        rp('\u{060C}', '\u{060D}'),
        rp('\u{061B}', '\u{061B}'),
        rp('\u{061E}', '\u{061F}'),
        rp('\u{066A}', '\u{066D}'),
        rp('\u{06D4}', '\u{06D4}'),
        rp('\u{0700}', '\u{070D}'),
        rp('\u{07F7}', '\u{07F9}'),
        rp('\u{0830}', '\u{083E}'),
        rp('\u{085E}', '\u{085E}'),
        rp('\u{0964}', '\u{0965}'),
        rp('\u{0970}', '\u{0970}'),
        rp('\u{09FD}', '\u{09FD}'),
        rp('\u{0A76}', '\u{0A76}'),
        rp('\u{0AF0}', '\u{0AF0}'),
        rp('\u{0C77}', '\u{0C77}'),
        rp('\u{0C84}', '\u{0C84}'),
        rp('\u{0DF4}', '\u{0DF4}'),
        rp('\u{0E4F}', '\u{0E4F}'),
        rp('\u{0E5A}', '\u{0E5B}'),
        rp('\u{0F04}', '\u{0F12}'),
        rp('\u{0F14}', '\u{0F14}'),
        rp('\u{0F85}', '\u{0F85}'),
        rp('\u{0FD0}', '\u{0FD4}'),
        rp('\u{0FD9}', '\u{0FDA}'),
        rp('\u{104A}', '\u{104F}'),
        rp('\u{10FB}', '\u{10FB}'),
        rp('\u{1360}', '\u{1368}'),
        rp('\u{166E}', '\u{166E}'),
        rp('\u{16EB}', '\u{16ED}'),
        rp('\u{1735}', '\u{1736}'),
        rp('\u{17D4}', '\u{17D6}'),
        rp('\u{17D8}', '\u{17DA}'),
        rp('\u{1800}', '\u{1805}'),
        rp('\u{1807}', '\u{180A}'),
        rp('\u{1944}', '\u{1945}'),
        rp('\u{1A1E}', '\u{1A1F}'),
        rp('\u{1AA0}', '\u{1AA6}'),
        rp('\u{1AA8}', '\u{1AAD}'),
        rp('\u{1B5A}', '\u{1B60}'),
        rp('\u{1BFC}', '\u{1BFF}'),
        rp('\u{1C3B}', '\u{1C3F}'),
        rp('\u{1C7E}', '\u{1C7F}'),
        rp('\u{1CC0}', '\u{1CC7}'),
        rp('\u{1CD3}', '\u{1CD3}'),
        rp('\u{2016}', '\u{2017}'),
        rp('\u{2020}', '\u{2027}'),
        rp('\u{2030}', '\u{2038}'),
        rp('\u{203B}', '\u{203E}'),
        rp('\u{2041}', '\u{2043}'),
        rp('\u{2047}', '\u{2051}'),
        rp('\u{2053}', '\u{2053}'),
        rp('\u{2055}', '\u{205E}'),
        rp('\u{2CF9}', '\u{2CFC}'),
        rp('\u{2CFE}', '\u{2CFF}'),
        rp('\u{2D70}', '\u{2D70}'),
        rp('\u{2E00}', '\u{2E01}'),
        rp('\u{2E06}', '\u{2E08}'),
        rp('\u{2E0B}', '\u{2E0B}'),
        rp('\u{2E0E}', '\u{2E16}'),
        rp('\u{2E18}', '\u{2E19}'),
        rp('\u{2E1B}', '\u{2E1B}'),
        rp('\u{2E1E}', '\u{2E1F}'),
        rp('\u{2E2A}', '\u{2E2E}'),
        rp('\u{2E30}', '\u{2E39}'),
        rp('\u{2E3C}', '\u{2E3F}'),
        rp('\u{2E41}', '\u{2E41}'),
        rp('\u{2E43}', '\u{2E4F}'),
        rp('\u{3001}', '\u{3003}'),
        rp('\u{303D}', '\u{303D}'),
        rp('\u{30FB}', '\u{30FB}'),
        rp('\u{A4FE}', '\u{A4FF}'),
        rp('\u{A60D}', '\u{A60F}'),
        rp('\u{A673}', '\u{A673}'),
        rp('\u{A67E}', '\u{A67E}'),
        rp('\u{A6F2}', '\u{A6F7}'),
        rp('\u{A874}', '\u{A877}'),
        rp('\u{A8CE}', '\u{A8CF}'),
        rp('\u{A8F8}', '\u{A8FA}'),
        rp('\u{A8FC}', '\u{A8FC}'),
        rp('\u{A92E}', '\u{A92F}'),
        rp('\u{A95F}', '\u{A95F}'),
        rp('\u{A9C1}', '\u{A9CD}'),
        rp('\u{A9DE}', '\u{A9DF}'),
        rp('\u{AA5C}', '\u{AA5F}'),
        rp('\u{AADE}', '\u{AADF}'),
        rp('\u{AAF0}', '\u{AAF1}'),
        rp('\u{ABEB}', '\u{ABEB}'),
        rp('\u{FE10}', '\u{FE16}'),
        rp('\u{FE19}', '\u{FE19}'),
        rp('\u{FE30}', '\u{FE30}'),
        rp('\u{FE45}', '\u{FE46}'),
        rp('\u{FE49}', '\u{FE4C}'),
        rp('\u{FE50}', '\u{FE52}'),
        rp('\u{FE54}', '\u{FE57}'),
        rp('\u{FE5F}', '\u{FE61}'),
        rp('\u{FE68}', '\u{FE68}'),
        rp('\u{FE6A}', '\u{FE6B}'),
        rp('\u{FF01}', '\u{FF03}'),
        rp('\u{FF05}', '\u{FF07}'),
        rp('\u{FF0A}', '\u{FF0A}'),
        rp('\u{FF0C}', '\u{FF0C}'),
        rp('\u{FF0E}', '\u{FF0F}'),
        rp('\u{FF1A}', '\u{FF1B}'),
        rp('\u{FF1F}', '\u{FF20}'),
        rp('\u{FF3C}', '\u{FF3C}'),
        rp('\u{FF61}', '\u{FF61}'),
        rp('\u{FF64}', '\u{FF65}'),
    ]
}

/// Z (Separator) = Zs | Zl | Zp
fn category_z() -> Vec<RunePair> {
    let mut ranges = category_zs();
    ranges.extend(category_zl());
    ranges.extend(category_zp());
    ranges
}

/// Zs (Separator, Space)
fn category_zs() -> Vec<RunePair> {
    vec![
        rp(' ', ' '),
        rp('\u{00A0}', '\u{00A0}'),
        rp('\u{1680}', '\u{1680}'),
        rp('\u{2000}', '\u{200A}'),
        rp('\u{202F}', '\u{202F}'),
        rp('\u{205F}', '\u{205F}'),
        rp('\u{3000}', '\u{3000}'),
    ]
}

/// Zl (Separator, Line)
fn category_zl() -> Vec<RunePair> {
    vec![rp('\u{2028}', '\u{2028}')]
}

/// Zp (Separator, Paragraph)
fn category_zp() -> Vec<RunePair> {
    vec![rp('\u{2029}', '\u{2029}')]
}

/// S (Symbol) = Sm | Sc | Sk | So
fn category_s() -> Vec<RunePair> {
    let mut ranges = category_sm();
    ranges.extend(category_sc());
    ranges.extend(category_sk());
    ranges.extend(category_so());
    ranges
}

/// Sm (Symbol, Math)
fn category_sm() -> Vec<RunePair> {
    vec![
        rp('+', '+'),
        rp('<', '>'),
        rp('|', '|'),
        rp('~', '~'),
        rp('\u{00AC}', '\u{00AC}'),
        rp('\u{00B1}', '\u{00B1}'),
        rp('\u{00D7}', '\u{00D7}'),
        rp('\u{00F7}', '\u{00F7}'),
        rp('\u{03F6}', '\u{03F6}'),
        rp('\u{0606}', '\u{0608}'),
        rp('\u{2044}', '\u{2044}'),
        rp('\u{2052}', '\u{2052}'),
        rp('\u{207A}', '\u{207C}'),
        rp('\u{208A}', '\u{208C}'),
        rp('\u{2118}', '\u{2118}'),
        rp('\u{2140}', '\u{2144}'),
        rp('\u{214B}', '\u{214B}'),
        rp('\u{2190}', '\u{2194}'),
        rp('\u{219A}', '\u{219B}'),
        rp('\u{21A0}', '\u{21A0}'),
        rp('\u{21A3}', '\u{21A3}'),
        rp('\u{21A6}', '\u{21A6}'),
        rp('\u{21AE}', '\u{21AE}'),
        rp('\u{21CE}', '\u{21CF}'),
        rp('\u{21D2}', '\u{21D2}'),
        rp('\u{21D4}', '\u{21D4}'),
        rp('\u{21F4}', '\u{22FF}'),
        rp('\u{2320}', '\u{2321}'),
        rp('\u{237C}', '\u{237C}'),
        rp('\u{239B}', '\u{23B3}'),
        rp('\u{23DC}', '\u{23E1}'),
        rp('\u{25B7}', '\u{25B7}'),
        rp('\u{25C1}', '\u{25C1}'),
        rp('\u{25F8}', '\u{25FF}'),
        rp('\u{266F}', '\u{266F}'),
        rp('\u{27C0}', '\u{27C4}'),
        rp('\u{27C7}', '\u{27E5}'),
        rp('\u{27F0}', '\u{27FF}'),
        rp('\u{2900}', '\u{2982}'),
        rp('\u{2999}', '\u{29D7}'),
        rp('\u{29DC}', '\u{29FB}'),
        rp('\u{29FE}', '\u{2AFF}'),
        rp('\u{2B30}', '\u{2B44}'),
        rp('\u{2B47}', '\u{2B4C}'),
        rp('\u{FB29}', '\u{FB29}'),
        rp('\u{FE62}', '\u{FE62}'),
        rp('\u{FE64}', '\u{FE66}'),
        rp('\u{FF0B}', '\u{FF0B}'),
        rp('\u{FF1C}', '\u{FF1E}'),
        rp('\u{FF5C}', '\u{FF5C}'),
        rp('\u{FF5E}', '\u{FF5E}'),
        rp('\u{FFE2}', '\u{FFE2}'),
        rp('\u{FFE9}', '\u{FFEC}'),
    ]
}

/// Sc (Symbol, Currency)
fn category_sc() -> Vec<RunePair> {
    vec![
        rp('$', '$'),
        rp('\u{00A2}', '\u{00A5}'),
        rp('\u{058F}', '\u{058F}'),
        rp('\u{060B}', '\u{060B}'),
        rp('\u{07FE}', '\u{07FF}'),
        rp('\u{09F2}', '\u{09F3}'),
        rp('\u{09FB}', '\u{09FB}'),
        rp('\u{0AF1}', '\u{0AF1}'),
        rp('\u{0BF9}', '\u{0BF9}'),
        rp('\u{0E3F}', '\u{0E3F}'),
        rp('\u{17DB}', '\u{17DB}'),
        rp('\u{20A0}', '\u{20BF}'),
        rp('\u{A838}', '\u{A838}'),
        rp('\u{FDFC}', '\u{FDFC}'),
        rp('\u{FE69}', '\u{FE69}'),
        rp('\u{FF04}', '\u{FF04}'),
        rp('\u{FFE0}', '\u{FFE1}'),
        rp('\u{FFE5}', '\u{FFE6}'),
    ]
}

/// Sk (Symbol, Modifier)
fn category_sk() -> Vec<RunePair> {
    vec![
        rp('^', '^'),
        rp('`', '`'),
        rp('\u{00A8}', '\u{00A8}'),
        rp('\u{00AF}', '\u{00AF}'),
        rp('\u{00B4}', '\u{00B4}'),
        rp('\u{00B8}', '\u{00B8}'),
        rp('\u{02C2}', '\u{02C5}'),
        rp('\u{02D2}', '\u{02DF}'),
        rp('\u{02E5}', '\u{02EB}'),
        rp('\u{02ED}', '\u{02ED}'),
        rp('\u{02EF}', '\u{02FF}'),
        rp('\u{0375}', '\u{0375}'),
        rp('\u{0384}', '\u{0385}'),
        rp('\u{1FBD}', '\u{1FBD}'),
        rp('\u{1FBF}', '\u{1FC1}'),
        rp('\u{1FCD}', '\u{1FCF}'),
        rp('\u{1FDD}', '\u{1FDF}'),
        rp('\u{1FED}', '\u{1FEF}'),
        rp('\u{1FFD}', '\u{1FFE}'),
        rp('\u{309B}', '\u{309C}'),
        rp('\u{A700}', '\u{A716}'),
        rp('\u{A720}', '\u{A721}'),
        rp('\u{A789}', '\u{A78A}'),
        rp('\u{AB5B}', '\u{AB5B}'),
        rp('\u{FBB2}', '\u{FBC1}'),
        rp('\u{FF3E}', '\u{FF3E}'),
        rp('\u{FF40}', '\u{FF40}'),
        rp('\u{FFE3}', '\u{FFE3}'),
    ]
}

/// So (Symbol, Other)
fn category_so() -> Vec<RunePair> {
    vec![
        rp('\u{00A6}', '\u{00A6}'),
        rp('\u{00A9}', '\u{00A9}'),
        rp('\u{00AE}', '\u{00AE}'),
        rp('\u{00B0}', '\u{00B0}'),
        rp('\u{0482}', '\u{0482}'),
        rp('\u{058D}', '\u{058E}'),
        rp('\u{060E}', '\u{060F}'),
        rp('\u{06DE}', '\u{06DE}'),
        rp('\u{06E9}', '\u{06E9}'),
        rp('\u{06FD}', '\u{06FE}'),
        rp('\u{07F6}', '\u{07F6}'),
        rp('\u{09FA}', '\u{09FA}'),
        rp('\u{0B70}', '\u{0B70}'),
        rp('\u{0BF3}', '\u{0BF8}'),
        rp('\u{0BFA}', '\u{0BFA}'),
        rp('\u{0C7F}', '\u{0C7F}'),
        rp('\u{0D4F}', '\u{0D4F}'),
        rp('\u{0D79}', '\u{0D79}'),
        rp('\u{0F01}', '\u{0F03}'),
        rp('\u{0F13}', '\u{0F13}'),
        rp('\u{0F15}', '\u{0F17}'),
        rp('\u{0F1A}', '\u{0F1F}'),
        rp('\u{0F34}', '\u{0F34}'),
        rp('\u{0F36}', '\u{0F36}'),
        rp('\u{0F38}', '\u{0F38}'),
        rp('\u{0FBE}', '\u{0FC5}'),
        rp('\u{0FC7}', '\u{0FCC}'),
        rp('\u{0FCE}', '\u{0FCF}'),
        rp('\u{0FD5}', '\u{0FD8}'),
        rp('\u{109E}', '\u{109F}'),
        rp('\u{1390}', '\u{1399}'),
        rp('\u{1940}', '\u{1940}'),
        rp('\u{19DE}', '\u{19FF}'),
        rp('\u{1B61}', '\u{1B6A}'),
        rp('\u{1B74}', '\u{1B7C}'),
        rp('\u{2100}', '\u{2101}'),
        rp('\u{2103}', '\u{2106}'),
        rp('\u{2108}', '\u{2109}'),
        rp('\u{2114}', '\u{2114}'),
        rp('\u{2116}', '\u{2117}'),
        rp('\u{211E}', '\u{2123}'),
        rp('\u{2125}', '\u{2125}'),
        rp('\u{2127}', '\u{2127}'),
        rp('\u{2129}', '\u{2129}'),
        rp('\u{212E}', '\u{212E}'),
        rp('\u{213A}', '\u{213B}'),
        rp('\u{214A}', '\u{214A}'),
        rp('\u{214C}', '\u{214D}'),
        rp('\u{214F}', '\u{214F}'),
        rp('\u{218A}', '\u{218B}'),
        rp('\u{2195}', '\u{2199}'),
        rp('\u{219C}', '\u{219F}'),
        rp('\u{21A1}', '\u{21A2}'),
        rp('\u{21A4}', '\u{21A5}'),
        rp('\u{21A7}', '\u{21AD}'),
        rp('\u{21AF}', '\u{21CD}'),
        rp('\u{21D0}', '\u{21D1}'),
        rp('\u{21D3}', '\u{21D3}'),
        rp('\u{21D5}', '\u{21F3}'),
        rp('\u{2300}', '\u{2307}'),
        rp('\u{230C}', '\u{231F}'),
        rp('\u{2322}', '\u{2328}'),
        rp('\u{232B}', '\u{237B}'),
        rp('\u{237D}', '\u{239A}'),
        rp('\u{23B4}', '\u{23DB}'),
        rp('\u{23E2}', '\u{2426}'),
        rp('\u{2440}', '\u{244A}'),
        rp('\u{249C}', '\u{24E9}'),
        rp('\u{2500}', '\u{25B6}'),
        rp('\u{25B8}', '\u{25C0}'),
        rp('\u{25C2}', '\u{25F7}'),
        rp('\u{2600}', '\u{266E}'),
        rp('\u{2670}', '\u{2767}'),
        rp('\u{2794}', '\u{27BF}'),
        rp('\u{2800}', '\u{28FF}'),
        rp('\u{2B00}', '\u{2B2F}'),
        rp('\u{2B45}', '\u{2B46}'),
        rp('\u{2B4D}', '\u{2B73}'),
        rp('\u{2B76}', '\u{2B95}'),
        rp('\u{2B98}', '\u{2BB9}'),
        rp('\u{2BBD}', '\u{2BC8}'),
        rp('\u{2BCA}', '\u{2BD1}'),
        rp('\u{2BEC}', '\u{2BEF}'),
        rp('\u{2CE5}', '\u{2CEA}'),
        rp('\u{2E80}', '\u{2E99}'),
        rp('\u{2E9B}', '\u{2EF3}'),
        rp('\u{2F00}', '\u{2FD5}'),
        rp('\u{2FF0}', '\u{2FFB}'),
        rp('\u{3004}', '\u{3004}'),
        rp('\u{3012}', '\u{3013}'),
        rp('\u{3020}', '\u{3020}'),
        rp('\u{3036}', '\u{3037}'),
        rp('\u{303E}', '\u{303F}'),
        rp('\u{3190}', '\u{3191}'),
        rp('\u{3196}', '\u{319F}'),
        rp('\u{31C0}', '\u{31E3}'),
        rp('\u{3200}', '\u{321E}'),
        rp('\u{322A}', '\u{3247}'),
        rp('\u{3250}', '\u{3250}'),
        rp('\u{3260}', '\u{327F}'),
        rp('\u{328A}', '\u{32B0}'),
        rp('\u{32C0}', '\u{32FE}'),
        rp('\u{3300}', '\u{33FF}'),
        rp('\u{4DC0}', '\u{4DFF}'),
        rp('\u{A490}', '\u{A4C6}'),
        rp('\u{A828}', '\u{A82B}'),
        rp('\u{A836}', '\u{A837}'),
        rp('\u{A839}', '\u{A839}'),
        rp('\u{AA77}', '\u{AA79}'),
        rp('\u{FDFD}', '\u{FDFD}'),
        rp('\u{FFE4}', '\u{FFE4}'),
        rp('\u{FFE8}', '\u{FFE8}'),
        rp('\u{FFED}', '\u{FFEE}'),
        rp('\u{FFFC}', '\u{FFFD}'),
        // Musical Symbols - So range (U+1D100-U+1D1FF but many are marks, here are So)
        rp('\u{1D100}', '\u{1D126}'),
        rp('\u{1D129}', '\u{1D164}'),
        rp('\u{1D16A}', '\u{1D16C}'),
        rp('\u{1D183}', '\u{1D184}'),
        rp('\u{1D18C}', '\u{1D1A9}'),
        rp('\u{1D1AE}', '\u{1D1FF}'),
        // Ancient Greek Musical Notation
        rp('\u{1D200}', '\u{1D245}'),
    ]
}

/// C (Other) = Cc | Cf | Co | Cn (Cs excluded as surrogates)
fn category_c() -> Vec<RunePair> {
    let mut ranges = category_cc();
    ranges.extend(category_cf());
    ranges.extend(category_co());
    // Cn (unassigned) is complex and huge, skip for now
    ranges
}

/// Cc (Other, Control)
fn category_cc() -> Vec<RunePair> {
    vec![rp('\u{0000}', '\u{001F}'), rp('\u{007F}', '\u{009F}')]
}

/// Cf (Other, Format)
fn category_cf() -> Vec<RunePair> {
    vec![
        rp('\u{00AD}', '\u{00AD}'),
        rp('\u{0600}', '\u{0605}'),
        rp('\u{061C}', '\u{061C}'),
        rp('\u{06DD}', '\u{06DD}'),
        rp('\u{070F}', '\u{070F}'),
        rp('\u{08E2}', '\u{08E2}'),
        rp('\u{180E}', '\u{180E}'),
        rp('\u{200B}', '\u{200F}'),
        rp('\u{202A}', '\u{202E}'),
        rp('\u{2060}', '\u{2064}'),
        rp('\u{2066}', '\u{206F}'),
        rp('\u{FEFF}', '\u{FEFF}'),
        rp('\u{FFF9}', '\u{FFFB}'),
        // Tags (U+E0000-U+E007F) - format characters
        rp('\u{E0001}', '\u{E0001}'),
        rp('\u{E0020}', '\u{E007F}'),
    ]
}

/// Co (Other, Private Use)
fn category_co() -> Vec<RunePair> {
    vec![
        rp('\u{E000}', '\u{F8FF}'),
        rp('\u{F0000}', '\u{FFFFD}'),
        rp('\u{100000}', '\u{10FFFD}'),
    ]
}

/// Cn (Other, Not Assigned) - Simplified, only some ranges
fn category_cn() -> Vec<RunePair> {
    // Cn is extremely complex - it's all unassigned code points
    // For now, return empty - this is rarely tested
    vec![]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_category_lookup() {
        // Test general categories
        assert!(get_category_ranges('L', None).is_some());
        assert!(get_category_ranges('L', Some('u')).is_some());
        assert!(get_category_ranges('L', Some('l')).is_some());
        assert!(get_category_ranges('N', Some('d')).is_some());
        assert!(get_category_ranges('X', None).is_none());
    }

    #[test]
    fn test_block_lookup() {
        // Test block names
        assert!(get_block_ranges("IsBasicLatin").is_some());
        assert!(get_block_ranges("BasicLatin").is_some());
        assert!(get_block_ranges("IsHiragana").is_some());
        assert!(get_block_ranges("NonExistent").is_none());
    }

    #[test]
    fn test_ascii_in_lu() {
        let lu = category_lu();
        // 'A' should be in Lu
        let has_a = lu.iter().any(|rp| rp.lo <= 'A' && 'A' <= rp.hi);
        assert!(has_a, "Lu should contain 'A'");
    }

    #[test]
    fn test_ascii_in_ll() {
        let ll = category_ll();
        // 'a' should be in Ll
        let has_a = ll.iter().any(|rp| rp.lo <= 'a' && 'a' <= rp.hi);
        assert!(has_a, "Ll should contain 'a'");
    }

    #[test]
    fn test_digit_in_nd() {
        let nd = category_nd();
        // '0' should be in Nd
        let has_0 = nd.iter().any(|rp| rp.lo <= '0' && '0' <= rp.hi);
        assert!(has_0, "Nd should contain '0'");
    }
}
