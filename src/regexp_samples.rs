//! Regexp samples ported from Go quamina.
//! Original source: https://github.com/qt4cg/xslt40-test
//! Total samples: 992

#[derive(Debug)]
pub struct RegexpSample {
    pub regex: &'static str,
    pub matches: &'static [&'static str],
    pub nomatches: &'static [&'static str],
    pub valid: bool,
}

pub static REGEXP_SAMPLES: &[RegexpSample] = &[
    // Sample 1
    RegexpSample {
        regex: "",
        matches: &["", ""],
        nomatches: &["a", " ", "\r", "\t", "\n"],
        valid: true,
    },
    // Sample 2
    RegexpSample {
        regex: "a",
        matches: &["a"],
        nomatches: &["aa", "b", ""],
        valid: true,
    },
    // Sample 3
    RegexpSample {
        regex: "a|a",
        matches: &["a"],
        nomatches: &["aa", "b", ""],
        valid: true,
    },
    // Sample 4
    RegexpSample {
        regex: "a|b",
        matches: &["a", "b"],
        nomatches: &["aa", "bb", "ab", ""],
        valid: true,
    },
    // Sample 5
    RegexpSample {
        regex: "ab",
        matches: &["ab"],
        nomatches: &["a", "b", "aa", "bb", ""],
        valid: true,
    },
    // Sample 6
    RegexpSample {
        regex: "a|b|a|c|b|d|a",
        matches: &["a", "b", "c", "d"],
        nomatches: &["aa", "ac", "e"],
        valid: true,
    },
    // Sample 7
    RegexpSample {
        regex: "       a|b      ",
        matches: &["       a"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 8
    RegexpSample {
        regex: "ab?c",
        matches: &["ac", "abc"],
        nomatches: &["a", "ab", "bc", ""],
        valid: true,
    },
    // Sample 9
    RegexpSample {
        regex: "abc?",
        matches: &["ab", "abc"],
        nomatches: &["a", "bc", "abcc", ""],
        valid: true,
    },
    // Sample 10
    RegexpSample {
        regex: "ab+c",
        matches: &["abc", "abbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbc"],
        nomatches: &["ac", "bbbc", "abbb", ""],
        valid: true,
    },
    // Sample 11
    RegexpSample {
        regex: "abc+",
        matches: &["abc", "abccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"],
        nomatches: &["a", "ab", "abcd"],
        valid: true,
    },
    // Sample 12
    RegexpSample {
        regex: "ab*c",
        matches: &["abc", "abbbbbbbc", "ac"],
        nomatches: &["a", "ab", "bc", "c", "abcb", ""],
        valid: true,
    },
    // Sample 13
    RegexpSample {
        regex: "abc*",
        matches: &["abc", "ab", "abccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"],
        nomatches: &["a", "abcd", "abbc", ""],
        valid: true,
    },
    // Sample 14
    RegexpSample {
        regex: "a?b+c*",
        matches: &["b", "ab", "bcccccc", "abc", "abbbc"],
        nomatches: &["aabc", "a", "c", "ac", ""],
        valid: true,
    },
    // Sample 15
    RegexpSample {
        regex: "(ab+c)a?~?~??",
        matches: &["abc?", "abbbc??", "abca??", "abbbbca?"],
        nomatches: &["ac??", "bc??", "abc", "abc???"],
        valid: true,
    },
    // Sample 16
    RegexpSample {
        regex: "?a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 17
    RegexpSample {
        regex: "+a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 18
    RegexpSample {
        regex: "*a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 19
    RegexpSample {
        regex: "{1}a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 20
    RegexpSample {
        regex: "a{0}",
        matches: &["", ""],
        nomatches: &["a"],
        valid: true,
    },
    // Sample 21
    RegexpSample {
        regex: "a{2,1}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 22
    RegexpSample {
        regex: "a{1,0}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 23
    RegexpSample {
        regex: "((ab){2})?",
        matches: &["abab", ""],
        nomatches: &["a", "ab", "ababa", "abababab"],
        valid: true,
    },
    // Sample 24
    RegexpSample {
        regex: "(a{2})+",
        matches: &["aa", "aaaa", "aaaaaaaaaaaaaaaaaaaa"],
        nomatches: &["", "a", "a2", "aaa"],
        valid: true,
    },
    // Sample 25
    RegexpSample {
        regex: "(a{2})*",
        matches: &["", "aa", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"],
        nomatches: &["a", "aaa", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"],
        valid: true,
    },
    // Sample 26
    RegexpSample {
        regex: "ab{2}c",
        matches: &["abbc"],
        nomatches: &["ac", "abc", "abbbc", "a", ""],
        valid: true,
    },
    // Sample 27
    RegexpSample {
        regex: "abc{2}",
        matches: &["abcc"],
        nomatches: &["abc", "abccc", ""],
        valid: true,
    },
    // Sample 28
    RegexpSample {
        regex: "a*b{2,4}c{0}",
        matches: &["aaabbb", "bb", "bbb", "bbbb"],
        nomatches: &["ab", "abbc", "bbc", "abbbbb", ""],
        valid: true,
    },
    // Sample 29
    RegexpSample {
        regex: "((ab)(ac){0,2})?",
        matches: &["ab", "abac", "abacac"],
        nomatches: &["ac", "abacacac", "abaca", "abab", "abacabac"],
        valid: true,
    },
    // Sample 30
    RegexpSample {
        regex: "(a~sb){0,2}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 31
    RegexpSample {
        regex: "(ab){2,}",
        matches: &["abab", "ababab", "ababababababababababababababababababababababababababababababababab"],
        nomatches: &["ab", "ababa", "ababaa", "ababababa", "abab abab", ""],
        valid: true,
    },
    // Sample 32
    RegexpSample {
        regex: "a{,2}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 33
    RegexpSample {
        regex: "(ab){2,0}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 34
    RegexpSample {
        regex: "(ab){0,0}",
        matches: &[""],
        nomatches: &["a", "ab"],
        valid: true,
    },
    // Sample 35
    RegexpSample {
        regex: "a{0,1}b{1,2}c{2,3}",
        matches: &["abcc", "abccc", "abbcc", "abbccc", "bbcc", "bbccc"],
        nomatches: &["aabcc", "bbbcc", "acc", "aabcc", "abbc", "abbcccc"],
        valid: true,
    },
    // Sample 36
    RegexpSample {
        regex: "(((((boy)|(girl))[0-1][x-z]{2})?)|(man|woman)[0-1]?[y|n])*",
        matches: &["", "boy0xx", "woman1y", "girl1xymany", "boy0xxwoman1ygirl1xymany", "boy0xxwoman1ygirl1xymanyboy0xxwoman1ygirl1xymany"],
        nomatches: &["boy0xxwoman1ygirl1xyman", "boyxx"],
        valid: true,
    },
    // Sample 37
    RegexpSample {
        regex: "((a)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 38
    RegexpSample {
        regex: "(a))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 39
    RegexpSample {
        regex: "ab|(d))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 40
    RegexpSample {
        regex: "((a*(b*)((a))*(a))))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 41
    RegexpSample {
        regex: "~",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 42
    RegexpSample {
        regex: "?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 43
    RegexpSample {
        regex: "*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 44
    RegexpSample {
        regex: "+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 45
    RegexpSample {
        regex: "(",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 46
    RegexpSample {
        regex: ")",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 47
    RegexpSample {
        regex: "|",
        matches: &[""],
        nomatches: &[""],
        valid: true,
    },
    // Sample 48
    RegexpSample {
        regex: "[",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 49
    RegexpSample {
        regex: "~.~~~?~*~+~{~}~[~]~(~)~|",
        matches: &[".~?*+{}[]()|"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 50
    RegexpSample {
        regex: "(([~.~~~?~*~+~{~}~[~]~(~)~|]?)*)+",
        matches: &[".~?*+{}[]()|.~?*+{}[]()|.~?*+{}[]()|"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 51
    RegexpSample {
        regex: "[^2-9a-x]{2}",
        matches: &["1z"],
        nomatches: &["1x"],
        valid: true,
    },
    // Sample 52
    RegexpSample {
        regex: "[^~s]{3}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 53
    RegexpSample {
        regex: "[^@]{0,2}",
        matches: &["", "a", "ab", " a"],
        nomatches: &["@"],
        valid: true,
    },
    // Sample 54
    RegexpSample {
        regex: "[^-z]+",
        matches: &[""],
        nomatches: &["aaz", "a-z"],
        valid: true,
    },
    // Sample 55
    RegexpSample {
        regex: "[a-d-[b-c]]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 56
    RegexpSample {
        regex: "[^a-d-b-c]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 57
    RegexpSample {
        regex: "[^a-d-b-c]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 58
    RegexpSample {
        regex: "[a-~}]+",
        matches: &["abcxyz}"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 59
    RegexpSample {
        regex: "[a-b-[0-9]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 60
    RegexpSample {
        regex: "[a-c-[^a-c]]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 61
    RegexpSample {
        regex: "[a-z-[^a]]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 62
    RegexpSample {
        regex: "[^~p{IsBasicLatin}]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 63
    RegexpSample {
        regex: "[^~p{IsBasicLatin}]*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 64
    RegexpSample {
        regex: "[^~P{IsBasicLatin}]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 65
    RegexpSample {
        regex: "[^~?]",
        matches: &[""],
        nomatches: &["?"],
        valid: true,
    },
    // Sample 66
    RegexpSample {
        regex: "([^~?])*",
        matches: &["a+*abc"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 67
    RegexpSample {
        regex: "~c[^~d]~c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 68
    RegexpSample {
        regex: "~c[^~s]~c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 69
    RegexpSample {
        regex: "[^~^a]",
        matches: &[""],
        nomatches: &["^", "a"],
        valid: true,
    },
    // Sample 70
    RegexpSample {
        regex: "[a-abc]{3}",
        matches: &["abc"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 71
    RegexpSample {
        regex: "[a-~}-]+",
        matches: &["}-"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 72
    RegexpSample {
        regex: "[a--b]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 73
    RegexpSample {
        regex: "[^[a-b]]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 74
    RegexpSample {
        regex: "[a]",
        matches: &[""],
        nomatches: &["b", ""],
        valid: true,
    },
    // Sample 75
    RegexpSample {
        regex: "[1-3]{1,4}",
        matches: &["123"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 76
    RegexpSample {
        regex: "[a-a]",
        matches: &["a"],
        nomatches: &["b"],
        valid: true,
    },
    // Sample 77
    RegexpSample {
        regex: "[0-z]*",
        matches: &["1234567890:;<=>?@Azaz"],
        nomatches: &["{", "/"],
        valid: true,
    },
    // Sample 78
    RegexpSample {
        regex: "[~n]",
        matches: &[""],
        nomatches: &[""],
        valid: true,
    },
    // Sample 79
    RegexpSample {
        regex: "[~t]",
        matches: &[""],
        nomatches: &[""],
        valid: true,
    },
    // Sample 80
    RegexpSample {
        regex: "[~~~|~.~?~*~+~(~)~{~}~-~[~]~^]*",
        matches: &["~|.?*+(){}-[]^"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 81
    RegexpSample {
        regex: "[^a-z^]",
        matches: &[""],
        nomatches: &[""],
        valid: true,
    },
    // Sample 82
    RegexpSample {
        regex: "[\\-~{^]",
        matches: &[""],
        nomatches: &[""],
        valid: true,
    },
    // Sample 83
    RegexpSample {
        regex: "[~C~?a-c~?]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 84
    RegexpSample {
        regex: "[~c~?a-c~?]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 85
    RegexpSample {
        regex: "[~D~?a-c~?]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 86
    RegexpSample {
        regex: "[~S~?a-c~?]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 87
    RegexpSample {
        regex: "[a-c-1-4x-z-7-9]*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 88
    RegexpSample {
        regex: "[a-c-1-4x-z-7-9]*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 89
    RegexpSample {
        regex: "[a-\\]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 90
    RegexpSample {
        regex: "[a-~[]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 91
    RegexpSample {
        regex: "[~*a]*",
        matches: &["a*a****aaaaa*"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 92
    RegexpSample {
        regex: "[a-;]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 93
    RegexpSample {
        regex: "[1-~]]+",
        matches: &["1]"],
        nomatches: &["0", "^"],
        valid: true,
    },
    // Sample 94
    RegexpSample {
        regex: "[=->]",
        matches: &["=", ">"],
        nomatches: &["~?"],
        valid: true,
    },
    // Sample 95
    RegexpSample {
        regex: "[>-=]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 96
    RegexpSample {
        regex: "[@]",
        matches: &["@"],
        nomatches: &["a"],
        valid: true,
    },
    // Sample 97
    RegexpSample {
        regex: "[࿿]",
        matches: &["࿿"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 98
    RegexpSample {
        regex: "[𐀀]",
        matches: &["𐀀"],
        nomatches: &["𐀁"],
        valid: true,
    },
    // Sample 99
    RegexpSample {
        regex: "[~]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 100
    RegexpSample {
        regex: "[~~~[~]]{0,3}",
        matches: &["~", "[", "]", "~[", "~[]", "[]", "[~~", "~]~", "[]["],
        nomatches: &["~[][", "~]~]", "[][]"],
        valid: true,
    },
    // Sample 101
    RegexpSample {
        regex: "[-]",
        matches: &["-"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 102
    RegexpSample {
        regex: "[-a]+",
        matches: &["a--aa---"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 103
    RegexpSample {
        regex: "[a-]*",
        matches: &["a--aa---"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 104
    RegexpSample {
        regex: "[a-a-x-x]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 105
    RegexpSample {
        regex: "[a-a-x-x]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 106
    RegexpSample {
        regex: "[~n~r~t~~~|~.~-~^~?~*~+~{~}~[~]~(~)]*",
        matches: &["~|.-^?*+[]{}()*[[]{}}))\n\r\t\t\n\n\r*()"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 107
    RegexpSample {
        regex: "[a~*]*",
        matches: &["a**", "aa*", "a"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 108
    RegexpSample {
        regex: "[(a~?)?]+",
        matches: &["a?", "a?a?a?", "a", "a??", "aa?"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 109
    RegexpSample {
        regex: "~~t",
        matches: &["~t"],
        nomatches: &["t", "~~t", "\t"],
        valid: true,
    },
    // Sample 110
    RegexpSample {
        regex: "~~n",
        matches: &["~n"],
        nomatches: &["n", "~~n", "\n"],
        valid: true,
    },
    // Sample 111
    RegexpSample {
        regex: "~~r",
        matches: &["~r"],
        nomatches: &["r", "~~r", "\r"],
        valid: true,
    },
    // Sample 112
    RegexpSample {
        regex: "~n",
        matches: &[""],
        nomatches: &[""],
        valid: true,
    },
    // Sample 113
    RegexpSample {
        regex: "~t",
        matches: &[""],
        nomatches: &[""],
        valid: true,
    },
    // Sample 114
    RegexpSample {
        regex: "~~",
        matches: &["~"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 115
    RegexpSample {
        regex: "~|",
        matches: &["|"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 116
    RegexpSample {
        regex: "~.",
        matches: &["."],
        nomatches: &[""],
        valid: true,
    },
    // Sample 117
    RegexpSample {
        regex: "~-",
        matches: &["-"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 118
    RegexpSample {
        regex: "~^",
        matches: &["^"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 119
    RegexpSample {
        regex: "~?",
        matches: &["?"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 120
    RegexpSample {
        regex: "~*",
        matches: &["*"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 121
    RegexpSample {
        regex: "~+",
        matches: &["+"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 122
    RegexpSample {
        regex: "~{",
        matches: &["{"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 123
    RegexpSample {
        regex: "~}",
        matches: &["}"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 124
    RegexpSample {
        regex: "~(",
        matches: &["("],
        nomatches: &[""],
        valid: true,
    },
    // Sample 125
    RegexpSample {
        regex: "~)",
        matches: &[")"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 126
    RegexpSample {
        regex: "~[",
        matches: &["["],
        nomatches: &[""],
        valid: true,
    },
    // Sample 127
    RegexpSample {
        regex: "~]",
        matches: &["]"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 128
    RegexpSample {
        regex: "~n~~~r~|~t~.~-~^~?~*~+~{~}~(~)~[~]",
        matches: &[""],
        nomatches: &["\n~\r|\t.-^?*+{}()[", "~\r|\t.-^?*+{}()[]", "\n~\r|\t-^?*+{}()[]"],
        valid: true,
    },
    // Sample 129
    RegexpSample {
        regex: "~n~na~n~nb~n~n",
        matches: &[""],
        nomatches: &["\n\na\n\nb\n", "\na\n\nb\n\n", "\n\na\n\n\n\n", "\n\na\n\r\nb\n\n"],
        valid: true,
    },
    // Sample 130
    RegexpSample {
        regex: "~r~ra~r~rb~r~r",
        matches: &["\r\ra\r\rb\r\r"],
        nomatches: &["\r\ra\r\rb\r", "\ra\r\rb\r\r", "\r\ra\r\r\r\r", "\r\ra\r\n\rb\r\r"],
        valid: true,
    },
    // Sample 131
    RegexpSample {
        regex: "~t~ta~t~tb~t~t",
        matches: &[""],
        nomatches: &["\t\ta\t\tb\t", "\ta\t\tb\t\t", "\t\ta\t\t\t\t", "\t\ta\t\t\tb\t\t"],
        valid: true,
    },
    // Sample 132
    RegexpSample {
        regex: "a~r~nb",
        matches: &["a\r\nb"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 133
    RegexpSample {
        regex: "~n~ra~n~rb",
        matches: &["\n\ra\n\rb"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 134
    RegexpSample {
        regex: "~ta~tb~tc~t",
        matches: &["\ta\tb\tc\t"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 135
    RegexpSample {
        regex: "~na~nb~nc~n",
        matches: &["\na\nb\nc\n"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 136
    RegexpSample {
        regex: "(~t|~s)a(~r~n|~r|~n|~s)+(~s|~t)b(~s|~r~n|~r|~n)*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 137
    RegexpSample {
        regex: "~~c",
        matches: &["~c"],
        nomatches: &["~p{_xmlC}", "~~c", "~~"],
        valid: true,
    },
    // Sample 138
    RegexpSample {
        regex: "~~.,~~s,~~S,~~i,~~I,~~c,~~C,~~d,~~D,~~w,~~W",
        matches: &["~.,~s,~S,~i,~I,~c,~C,~d,~D,~w,~W"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 139
    RegexpSample {
        regex: "~~.*,~~s*,~~S*,~~i*,~~I?,~~c+,~~C+,~~d{0,3},~~D{1,1000},~~w*,~~W+",
        matches: &["~.abcd,~sssss,~SSSSSS,~iiiiiii,~,~c,~CCCCCC,~ddd,~D,~wwwwwww,~WWW"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 140
    RegexpSample {
        regex: "[~p{L}*]{0,2}",
        matches: &["aX"],
        nomatches: &["aBC"],
        valid: true,
    },
    // Sample 141
    RegexpSample {
        regex: "(~p{Ll}~p{Cc}~p{Nd})*",
        matches: &[""],
        nomatches: &[" "],
        valid: true,
    },
    // Sample 142
    RegexpSample {
        regex: "~p{L}*",
        matches: &[""],
        nomatches: &["⃝"],
        valid: true,
    },
    // Sample 143
    RegexpSample {
        regex: "~p{Lu}*",
        matches: &["A𝞨"],
        nomatches: &["a"],
        valid: true,
    },
    // Sample 144
    RegexpSample {
        regex: "~p{Ll}*",
        matches: &["a𝟉"],
        nomatches: &["ǅ"],
        valid: true,
    },
    // Sample 145
    RegexpSample {
        regex: "~p{Lt}*",
        matches: &["ǅῼ"],
        nomatches: &["ʰ"],
        valid: true,
    },
    // Sample 146
    RegexpSample {
        regex: "~p{Lm}*",
        matches: &["ʰﾟ"],
        nomatches: &["א"],
        valid: true,
    },
    // Sample 147
    RegexpSample {
        regex: "~p{Lo}*",
        matches: &["א𪘀"],
        nomatches: &["ً"],
        valid: true,
    },
    // Sample 148
    RegexpSample {
        regex: "~p{M}*",
        matches: &["ً𝆭ः𝅲ः𝅲⃝⃝⃠"],
        nomatches: &["ǅ"],
        valid: true,
    },
    // Sample 149
    RegexpSample {
        regex: "~p{Mn}*",
        matches: &["ً𝆭"],
        nomatches: &["ः"],
        valid: true,
    },
    // Sample 150
    RegexpSample {
        regex: "~p{Mc}*",
        matches: &["ः𝅲"],
        nomatches: &["⃝"],
        valid: true,
    },
    // Sample 151
    RegexpSample {
        regex: "~p{Me}*",
        matches: &["⃝⃠"],
        nomatches: &["０"],
        valid: true,
    },
    // Sample 152
    RegexpSample {
        regex: "~p{N}*",
        matches: &["０𝟿𐍊𐍊〥²²𐌣"],
        nomatches: &["ः"],
        valid: true,
    },
    // Sample 153
    RegexpSample {
        regex: "~p{Nd}*",
        matches: &["０𝟿"],
        nomatches: &["𐍊"],
        valid: true,
    },
    // Sample 154
    RegexpSample {
        regex: "~p{Nl}*",
        matches: &["𐍊〥"],
        nomatches: &["²"],
        valid: true,
    },
    // Sample 155
    RegexpSample {
        regex: "~p{No}*",
        matches: &["²𐌣"],
        nomatches: &["‿"],
        valid: true,
    },
    // Sample 156
    RegexpSample {
        regex: "~p{P}*",
        matches: &["‿･〜〜－〝〝｢〞〞｣««‹»»›¿¿､"],
        nomatches: &["²"],
        valid: true,
    },
    // Sample 157
    RegexpSample {
        regex: "~p{Pc}*",
        matches: &[""],
        nomatches: &["〜"],
        valid: true,
    },
    // Sample 158
    RegexpSample {
        regex: "~p{Pd}*",
        matches: &["〜－"],
        nomatches: &["〝"],
        valid: true,
    },
    // Sample 159
    RegexpSample {
        regex: "~p{Ps}*",
        matches: &["〝｢"],
        nomatches: &["〞"],
        valid: true,
    },
    // Sample 160
    RegexpSample {
        regex: "~p{Pe}*",
        matches: &["〞｣"],
        nomatches: &["«"],
        valid: true,
    },
    // Sample 161
    RegexpSample {
        regex: "~p{Pi}*",
        matches: &["«‹"],
        nomatches: &["»"],
        valid: true,
    },
    // Sample 162
    RegexpSample {
        regex: "~p{Pf}*",
        matches: &["»›"],
        nomatches: &["¿"],
        valid: true,
    },
    // Sample 163
    RegexpSample {
        regex: "~p{Po}*",
        matches: &["¿､"],
        nomatches: &[" "],
        valid: true,
    },
    // Sample 164
    RegexpSample {
        regex: "~p{Z}*",
        matches: &[" 　    "],
        nomatches: &["¿"],
        valid: true,
    },
    // Sample 165
    RegexpSample {
        regex: "~p{Zs}*",
        matches: &[" 　"],
        nomatches: &[" "],
        valid: true,
    },
    // Sample 166
    RegexpSample {
        regex: "~p{Zl}*",
        matches: &[" "],
        nomatches: &[" "],
        valid: true,
    },
    // Sample 167
    RegexpSample {
        regex: "~p{Zp}*",
        matches: &[" "],
        nomatches: &["⁄"],
        valid: true,
    },
    // Sample 168
    RegexpSample {
        regex: "~p{S}*",
        matches: &["⁄￢₠₠￦゛゛￣㆐㆐𝇝"],
        nomatches: &[" "],
        valid: true,
    },
    // Sample 169
    RegexpSample {
        regex: "~p{Sm}*",
        matches: &["⁄￢"],
        nomatches: &["₠"],
        valid: true,
    },
    // Sample 170
    RegexpSample {
        regex: "~p{Sc}*",
        matches: &["₠￦"],
        nomatches: &["゛"],
        valid: true,
    },
    // Sample 171
    RegexpSample {
        regex: "~p{Sk}*",
        matches: &["゛￣"],
        nomatches: &["㆐"],
        valid: true,
    },
    // Sample 172
    RegexpSample {
        regex: "~p{So}*",
        matches: &["㆐𝇝"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 173
    RegexpSample {
        regex: "~p{C}*",
        matches: &[""],
        nomatches: &["₠"],
        valid: true,
    },
    // Sample 174
    RegexpSample {
        regex: "~p{Cc}*",
        matches: &[""],
        nomatches: &["܏"],
        valid: true,
    },
    // Sample 175
    RegexpSample {
        regex: "~p{Cf}*",
        matches: &["܏󠁸"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 176
    RegexpSample {
        regex: "(~p{Co})*",
        matches: &["􀀀󰀀󿿽􏿽"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 177
    RegexpSample {
        regex: "~p{Co}*",
        matches: &[""],
        nomatches: &["⁄"],
        valid: true,
    },
    // Sample 178
    RegexpSample {
        regex: "~p{Cn}*",
        matches: &[""],
        nomatches: &[""],
        valid: true,
    },
    // Sample 179
    RegexpSample {
        regex: "~P{L}*",
        matches: &["_", "⃝"],
        nomatches: &["aAbB", "A𝞨aa𝟉ǅǅῼʰʰﾟאא𪘀"],
        valid: true,
    },
    // Sample 180
    RegexpSample {
        regex: "[~P{L}*]{0,2}",
        matches: &["", "#$"],
        nomatches: &["!$#", "A"],
        valid: true,
    },
    // Sample 181
    RegexpSample {
        regex: "~P{Lu}*",
        matches: &["a"],
        nomatches: &["A𝞨"],
        valid: true,
    },
    // Sample 182
    RegexpSample {
        regex: "~P{Ll}*",
        matches: &["ǅ"],
        nomatches: &["a𝟉"],
        valid: true,
    },
    // Sample 183
    RegexpSample {
        regex: "~P{Lt}*",
        matches: &["ʰ"],
        nomatches: &["ǅῼ"],
        valid: true,
    },
    // Sample 184
    RegexpSample {
        regex: "~P{Lm}*",
        matches: &["א"],
        nomatches: &["ʰﾟ"],
        valid: true,
    },
    // Sample 185
    RegexpSample {
        regex: "~P{Lo}*",
        matches: &["ً"],
        nomatches: &["א𪘀"],
        valid: true,
    },
    // Sample 186
    RegexpSample {
        regex: "~P{M}*",
        matches: &["ǅ"],
        nomatches: &["ً𝆭ः𝅲ः𝅲⃝⃝⃠"],
        valid: true,
    },
    // Sample 187
    RegexpSample {
        regex: "~P{Mn}*",
        matches: &["ः𝅲"],
        nomatches: &["ً𝆭"],
        valid: true,
    },
    // Sample 188
    RegexpSample {
        regex: "~P{Mc}*",
        matches: &["⃝"],
        nomatches: &["ः𝅲"],
        valid: true,
    },
    // Sample 189
    RegexpSample {
        regex: "~P{Me}*",
        matches: &["０"],
        nomatches: &["⃝⃠"],
        valid: true,
    },
    // Sample 190
    RegexpSample {
        regex: "~P{N}*",
        matches: &["ः"],
        nomatches: &["０𝟿𐍊𐍊〥²²𐌣"],
        valid: true,
    },
    // Sample 191
    RegexpSample {
        regex: "~P{Nd}*",
        matches: &["𐍊"],
        nomatches: &["０𝟿"],
        valid: true,
    },
    // Sample 192
    RegexpSample {
        regex: "~P{Nl}*",
        matches: &["²"],
        nomatches: &["𐍊〥"],
        valid: true,
    },
    // Sample 193
    RegexpSample {
        regex: "~P{No}*",
        matches: &["‿"],
        nomatches: &["²𐌣"],
        valid: true,
    },
    // Sample 194
    RegexpSample {
        regex: "~P{P}*",
        matches: &["²"],
        nomatches: &["‿･〜〜－〝〝｢〞〞｣««‹»»›¿¿､"],
        valid: true,
    },
    // Sample 195
    RegexpSample {
        regex: "~P{Pc}*",
        matches: &["〜"],
        nomatches: &["‿･"],
        valid: true,
    },
    // Sample 196
    RegexpSample {
        regex: "~P{Pd}*",
        matches: &["〝"],
        nomatches: &["〜－"],
        valid: true,
    },
    // Sample 197
    RegexpSample {
        regex: "~P{Ps}*",
        matches: &["〞"],
        nomatches: &["〝｢"],
        valid: true,
    },
    // Sample 198
    RegexpSample {
        regex: "~P{Pe}*",
        matches: &["«"],
        nomatches: &["〞｣"],
        valid: true,
    },
    // Sample 199
    RegexpSample {
        regex: "~P{Pi}*",
        matches: &["»"],
        nomatches: &["«‹"],
        valid: true,
    },
    // Sample 200
    RegexpSample {
        regex: "~P{Pf}*",
        matches: &["¿"],
        nomatches: &["»›"],
        valid: true,
    },
    // Sample 201
    RegexpSample {
        regex: "~P{Po}*",
        matches: &[" "],
        nomatches: &["¿､"],
        valid: true,
    },
    // Sample 202
    RegexpSample {
        regex: "~P{Z}*",
        matches: &["¿"],
        nomatches: &[" 　    "],
        valid: true,
    },
    // Sample 203
    RegexpSample {
        regex: "~P{Zs}*",
        matches: &[" "],
        nomatches: &[" 　"],
        valid: true,
    },
    // Sample 204
    RegexpSample {
        regex: "~P{Zl}*",
        matches: &[" "],
        nomatches: &[" "],
        valid: true,
    },
    // Sample 205
    RegexpSample {
        regex: "~P{Zp}*",
        matches: &["⁄"],
        nomatches: &[" "],
        valid: true,
    },
    // Sample 206
    RegexpSample {
        regex: "~P{S}*",
        matches: &[" "],
        nomatches: &["⁄￢₠₠￦゛゛￣㆐㆐𝇝"],
        valid: true,
    },
    // Sample 207
    RegexpSample {
        regex: "~P{Sm}*",
        matches: &["₠"],
        nomatches: &["⁄￢"],
        valid: true,
    },
    // Sample 208
    RegexpSample {
        regex: "~P{Sc}*",
        matches: &["゛"],
        nomatches: &["₠￦"],
        valid: true,
    },
    // Sample 209
    RegexpSample {
        regex: "~P{Sk}*",
        matches: &["㆐"],
        nomatches: &["゛￣"],
        valid: true,
    },
    // Sample 210
    RegexpSample {
        regex: "~P{So}*",
        matches: &[""],
        nomatches: &["㆐𝇝"],
        valid: true,
    },
    // Sample 211
    RegexpSample {
        regex: "~P{C}*",
        matches: &["₠"],
        nomatches: &["\t܏܏󠁸􀀀󰀀󿿽􏿽"],
        valid: true,
    },
    // Sample 212
    RegexpSample {
        regex: "~P{Cc}*",
        matches: &["܏"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 213
    RegexpSample {
        regex: "~P{Cf}*",
        matches: &[""],
        nomatches: &["܏󠁸"],
        valid: true,
    },
    // Sample 214
    RegexpSample {
        regex: "~P{Co}*",
        matches: &["⁄"],
        nomatches: &["􀀀󰀀󿿽􏿽"],
        valid: true,
    },
    // Sample 215
    RegexpSample {
        regex: "~p{~~L}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 216
    RegexpSample {
        regex: "~~~p{L}*",
        matches: &["~a"],
        nomatches: &["a"],
        valid: true,
    },
    // Sample 217
    RegexpSample {
        regex: "~p{Is}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 218
    RegexpSample {
        regex: "~P{Is}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 219
    RegexpSample {
        regex: "~p{IsaA0-a9}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 220
    RegexpSample {
        regex: "~p{IsBasicLatin}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 221
    RegexpSample {
        regex: "~p{IsLatin-1Supplement}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 222
    RegexpSample {
        regex: "~p{IsLatinExtended-A}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 223
    RegexpSample {
        regex: "~p{IsLatinExtended-B}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 224
    RegexpSample {
        regex: "~p{IsIPAExtensions}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 225
    RegexpSample {
        regex: "~p{IsSpacingModifierLetters}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 226
    RegexpSample {
        regex: "~p{IsArmenian}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 227
    RegexpSample {
        regex: "~p{IsHebrew}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 228
    RegexpSample {
        regex: "~p{IsArabic}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 229
    RegexpSample {
        regex: "~p{IsSyriac}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 230
    RegexpSample {
        regex: "~p{IsThaana}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 231
    RegexpSample {
        regex: "~p{IsDevanagari}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 232
    RegexpSample {
        regex: "~p{IsBengali}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 233
    RegexpSample {
        regex: "~p{IsGurmukhi}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 234
    RegexpSample {
        regex: "~p{IsGujarati}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 235
    RegexpSample {
        regex: "~p{IsOriya}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 236
    RegexpSample {
        regex: "~p{IsTamil}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 237
    RegexpSample {
        regex: "~p{IsTelugu}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 238
    RegexpSample {
        regex: "~p{IsKannada}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 239
    RegexpSample {
        regex: "~p{IsMalayalam}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 240
    RegexpSample {
        regex: "~p{IsSinhala}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 241
    RegexpSample {
        regex: "~p{IsThai}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 242
    RegexpSample {
        regex: "~p{IsLao}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 243
    RegexpSample {
        regex: "~p{IsTibetan}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 244
    RegexpSample {
        regex: "~p{IsMyanmar}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 245
    RegexpSample {
        regex: "~p{IsGeorgian}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 246
    RegexpSample {
        regex: "~p{IsHangulJamo}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 247
    RegexpSample {
        regex: "~p{IsEthiopic}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 248
    RegexpSample {
        regex: "~p{IsCherokee}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 249
    RegexpSample {
        regex: "~p{IsUnifiedCanadianAboriginalSyllabics}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 250
    RegexpSample {
        regex: "~p{IsOgham}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 251
    RegexpSample {
        regex: "~p{IsRunic}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 252
    RegexpSample {
        regex: "~p{IsKhmer}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 253
    RegexpSample {
        regex: "~p{IsMongolian}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 254
    RegexpSample {
        regex: "~p{IsLatinExtendedAdditional}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 255
    RegexpSample {
        regex: "~p{IsGreekExtended}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 256
    RegexpSample {
        regex: "~p{IsGeneralPunctuation}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 257
    RegexpSample {
        regex: "~p{IsSuperscriptsandSubscripts}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 258
    RegexpSample {
        regex: "~p{IsCurrencySymbols}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 259
    RegexpSample {
        regex: "~p{IsCombiningDiacriticalMarksforSymbols}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 260
    RegexpSample {
        regex: "~p{IsLetterlikeSymbols}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 261
    RegexpSample {
        regex: "~p{IsNumberForms}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 262
    RegexpSample {
        regex: "~p{IsArrows}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 263
    RegexpSample {
        regex: "~p{IsMathematicalOperators}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 264
    RegexpSample {
        regex: "~p{IsMiscellaneousTechnical}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 265
    RegexpSample {
        regex: "~p{IsControlPictures}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 266
    RegexpSample {
        regex: "~p{IsOpticalCharacterRecognition}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 267
    RegexpSample {
        regex: "~p{IsEnclosedAlphanumerics}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 268
    RegexpSample {
        regex: "~p{IsBoxDrawing}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 269
    RegexpSample {
        regex: "~p{IsBlockElements}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 270
    RegexpSample {
        regex: "~p{IsGeometricShapes}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 271
    RegexpSample {
        regex: "~p{IsMiscellaneousSymbols}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 272
    RegexpSample {
        regex: "~p{IsDingbats}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 273
    RegexpSample {
        regex: "~p{IsBraillePatterns}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 274
    RegexpSample {
        regex: "~p{IsCJKRadicalsSupplement}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 275
    RegexpSample {
        regex: "~p{IsKangxiRadicals}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 276
    RegexpSample {
        regex: "~p{IsIdeographicDescriptionCharacters}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 277
    RegexpSample {
        regex: "~p{IsCJKSymbolsandPunctuation}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 278
    RegexpSample {
        regex: "~p{IsHiragana}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 279
    RegexpSample {
        regex: "~p{IsKatakana}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 280
    RegexpSample {
        regex: "~p{IsBopomofo}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 281
    RegexpSample {
        regex: "~p{IsHangulCompatibilityJamo}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 282
    RegexpSample {
        regex: "~p{IsKanbun}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 283
    RegexpSample {
        regex: "~p{IsBopomofoExtended}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 284
    RegexpSample {
        regex: "~p{IsEnclosedCJKLettersandMonths}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 285
    RegexpSample {
        regex: "~p{IsCJKCompatibility}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 286
    RegexpSample {
        regex: "~p{IsCJKUnifiedIdeographsExtensionA}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 287
    RegexpSample {
        regex: "~p{IsCJKUnifiedIdeographs}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 288
    RegexpSample {
        regex: "~p{IsYiSyllables}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 289
    RegexpSample {
        regex: "~p{IsYiRadicals}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 290
    RegexpSample {
        regex: "~p{IsHangulSyllables}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 291
    RegexpSample {
        regex: "~p{IsPrivateUseArea}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 292
    RegexpSample {
        regex: "~p{IsSupplementaryPrivateUseArea-A}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 293
    RegexpSample {
        regex: "~p{IsSupplementaryPrivateUseArea-B}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 294
    RegexpSample {
        regex: "~p{IsCJKCompatibilityIdeographs}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 295
    RegexpSample {
        regex: "~p{IsAlphabeticPresentationForms}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 296
    RegexpSample {
        regex: "~p{IsArabicPresentationForms-A}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 297
    RegexpSample {
        regex: "~p{IsCombiningHalfMarks}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 298
    RegexpSample {
        regex: "~p{IsCJKCompatibilityForms}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 299
    RegexpSample {
        regex: "~p{IsSmallFormVariants}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 300
    RegexpSample {
        regex: "~p{IsArabicPresentationForms-B}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 301
    RegexpSample {
        regex: "~p{IsHalfwidthandFullwidthForms}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 302
    RegexpSample {
        regex: "~p{IsSpecials}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 303
    RegexpSample {
        regex: "~p{IsBasicLatin}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 304
    RegexpSample {
        regex: "~p{IsLatin-1Supplement}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 305
    RegexpSample {
        regex: "~p{IsLatinExtended-A}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 306
    RegexpSample {
        regex: "~p{IsLatinExtended-B}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 307
    RegexpSample {
        regex: "~p{IsIPAExtensions}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 308
    RegexpSample {
        regex: "~p{IsSpacingModifierLetters}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 309
    RegexpSample {
        regex: "~p{IsCyrillic}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 310
    RegexpSample {
        regex: "~p{IsArmenian}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 311
    RegexpSample {
        regex: "~p{IsHebrew}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 312
    RegexpSample {
        regex: "~p{IsArabic}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 313
    RegexpSample {
        regex: "~p{IsSyriac}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 314
    RegexpSample {
        regex: "~p{IsThaana}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 315
    RegexpSample {
        regex: "~p{IsDevanagari}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 316
    RegexpSample {
        regex: "~p{IsBengali}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 317
    RegexpSample {
        regex: "~p{IsGurmukhi}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 318
    RegexpSample {
        regex: "~p{IsGujarati}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 319
    RegexpSample {
        regex: "~p{IsOriya}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 320
    RegexpSample {
        regex: "~p{IsTamil}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 321
    RegexpSample {
        regex: "~p{IsTelugu}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 322
    RegexpSample {
        regex: "~p{IsKannada}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 323
    RegexpSample {
        regex: "~p{IsMalayalam}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 324
    RegexpSample {
        regex: "~p{IsSinhala}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 325
    RegexpSample {
        regex: "~p{IsThai}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 326
    RegexpSample {
        regex: "~p{IsLao}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 327
    RegexpSample {
        regex: "~p{IsTibetan}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 328
    RegexpSample {
        regex: "~p{IsMyanmar}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 329
    RegexpSample {
        regex: "~p{IsGeorgian}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 330
    RegexpSample {
        regex: "~p{IsHangulJamo}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 331
    RegexpSample {
        regex: "~p{IsEthiopic}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 332
    RegexpSample {
        regex: "~p{IsCherokee}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 333
    RegexpSample {
        regex: "~p{IsUnifiedCanadianAboriginalSyllabics}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 334
    RegexpSample {
        regex: "~p{IsOgham}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 335
    RegexpSample {
        regex: "~p{IsRunic}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 336
    RegexpSample {
        regex: "~p{IsKhmer}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 337
    RegexpSample {
        regex: "~p{IsMongolian}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 338
    RegexpSample {
        regex: "~p{IsLatinExtendedAdditional}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 339
    RegexpSample {
        regex: "~p{IsGreekExtended}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 340
    RegexpSample {
        regex: "~p{IsGeneralPunctuation}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 341
    RegexpSample {
        regex: "~p{IsSuperscriptsandSubscripts}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 342
    RegexpSample {
        regex: "~p{IsCurrencySymbols}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 343
    RegexpSample {
        regex: "~p{IsCombiningDiacriticalMarksforSymbols}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 344
    RegexpSample {
        regex: "~p{IsLetterlikeSymbols}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 345
    RegexpSample {
        regex: "~p{IsNumberForms}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 346
    RegexpSample {
        regex: "~p{IsArrows}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 347
    RegexpSample {
        regex: "~p{IsMathematicalOperators}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 348
    RegexpSample {
        regex: "~p{IsMiscellaneousTechnical}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 349
    RegexpSample {
        regex: "~p{IsControlPictures}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 350
    RegexpSample {
        regex: "~p{IsOpticalCharacterRecognition}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 351
    RegexpSample {
        regex: "~p{IsEnclosedAlphanumerics}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 352
    RegexpSample {
        regex: "~p{IsBoxDrawing}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 353
    RegexpSample {
        regex: "~p{IsBlockElements}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 354
    RegexpSample {
        regex: "~p{IsGeometricShapes}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 355
    RegexpSample {
        regex: "~p{IsMiscellaneousSymbols}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 356
    RegexpSample {
        regex: "~p{IsDingbats}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 357
    RegexpSample {
        regex: "~p{IsBraillePatterns}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 358
    RegexpSample {
        regex: "~p{IsCJKRadicalsSupplement}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 359
    RegexpSample {
        regex: "~p{IsKangxiRadicals}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 360
    RegexpSample {
        regex: "~p{IsIdeographicDescriptionCharacters}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 361
    RegexpSample {
        regex: "~p{IsCJKSymbolsandPunctuation}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 362
    RegexpSample {
        regex: "~p{IsHiragana}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 363
    RegexpSample {
        regex: "~p{IsKatakana}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 364
    RegexpSample {
        regex: "~p{IsBopomofo}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 365
    RegexpSample {
        regex: "~p{IsHangulCompatibilityJamo}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 366
    RegexpSample {
        regex: "~p{IsKanbun}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 367
    RegexpSample {
        regex: "~p{IsBopomofoExtended}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 368
    RegexpSample {
        regex: "~p{IsEnclosedCJKLettersandMonths}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 369
    RegexpSample {
        regex: "~p{IsCJKCompatibility}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 370
    RegexpSample {
        regex: "~p{IsCJKUnifiedIdeographsExtensionA}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 371
    RegexpSample {
        regex: "~p{IsCJKUnifiedIdeographs}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 372
    RegexpSample {
        regex: "~p{IsYiSyllables}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 373
    RegexpSample {
        regex: "~p{IsYiRadicals}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 374
    RegexpSample {
        regex: "~p{IsLowSurrogates}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 375
    RegexpSample {
        regex: "~p{IsPrivateUseArea}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 376
    RegexpSample {
        regex: "~p{IsSupplementaryPrivateUseArea-B}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 377
    RegexpSample {
        regex: "~p{IsCJKCompatibilityIdeographs}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 378
    RegexpSample {
        regex: "~p{IsAlphabeticPresentationForms}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 379
    RegexpSample {
        regex: "~p{IsArabicPresentationForms-A}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 380
    RegexpSample {
        regex: "~p{IsCombiningHalfMarks}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 381
    RegexpSample {
        regex: "~p{IsCJKCompatibilityForms}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 382
    RegexpSample {
        regex: "~p{IsSmallFormVariants}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 383
    RegexpSample {
        regex: "~p{IsSpecials}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 384
    RegexpSample {
        regex: "~p{IsHalfwidthandFullwidthForms}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 385
    RegexpSample {
        regex: "~p{IsOldItalic}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 386
    RegexpSample {
        regex: "~p{IsGothic}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 387
    RegexpSample {
        regex: "~p{IsDeseret}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 388
    RegexpSample {
        regex: "~p{IsByzantineMusicalSymbols}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 389
    RegexpSample {
        regex: "~p{IsMusicalSymbols}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 390
    RegexpSample {
        regex: "~p{IsMathematicalAlphanumericSymbols}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 391
    RegexpSample {
        regex: "~p{IsCJKUnifiedIdeographsExtensionB}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 392
    RegexpSample {
        regex: "~p{IsCJKCompatibilityIdeographsSupplement}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 393
    RegexpSample {
        regex: "~p{IsTags}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 394
    RegexpSample {
        regex: "~p{IsBasicLatin}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 395
    RegexpSample {
        regex: "~p{IsLatin-1Supplement}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 396
    RegexpSample {
        regex: "~p{IsLatinExtended-A}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 397
    RegexpSample {
        regex: "~p{IsLatinExtended-B}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 398
    RegexpSample {
        regex: "~p{IsIPAExtensions}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 399
    RegexpSample {
        regex: "~p{IsSpacingModifierLetters}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 400
    RegexpSample {
        regex: "~p{IsGreekandCoptic}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 401
    RegexpSample {
        regex: "~p{IsCyrillic}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 402
    RegexpSample {
        regex: "~p{IsArmenian}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 403
    RegexpSample {
        regex: "~p{IsHebrew}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 404
    RegexpSample {
        regex: "~p{IsArabic}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 405
    RegexpSample {
        regex: "~p{IsSyriac}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 406
    RegexpSample {
        regex: "~p{IsThaana}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 407
    RegexpSample {
        regex: "~p{IsDevanagari}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 408
    RegexpSample {
        regex: "~p{IsBengali}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 409
    RegexpSample {
        regex: "~p{IsGurmukhi}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 410
    RegexpSample {
        regex: "~p{IsGujarati}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 411
    RegexpSample {
        regex: "~p{IsOriya}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 412
    RegexpSample {
        regex: "~p{IsTamil}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 413
    RegexpSample {
        regex: "~p{IsTelugu}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 414
    RegexpSample {
        regex: "~p{IsKannada}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 415
    RegexpSample {
        regex: "~p{IsMalayalam}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 416
    RegexpSample {
        regex: "~p{IsSinhala}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 417
    RegexpSample {
        regex: "~p{IsThai}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 418
    RegexpSample {
        regex: "~p{IsLao}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 419
    RegexpSample {
        regex: "~p{IsTibetan}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 420
    RegexpSample {
        regex: "~p{IsMyanmar}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 421
    RegexpSample {
        regex: "~p{IsGeorgian}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 422
    RegexpSample {
        regex: "~p{IsHangulJamo}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 423
    RegexpSample {
        regex: "~p{IsEthiopic}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 424
    RegexpSample {
        regex: "~p{IsCherokee}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 425
    RegexpSample {
        regex: "~p{IsUnifiedCanadianAboriginalSyllabics}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 426
    RegexpSample {
        regex: "~p{IsOgham}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 427
    RegexpSample {
        regex: "~p{IsRunic}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 428
    RegexpSample {
        regex: "~p{IsKhmer}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 429
    RegexpSample {
        regex: "~p{IsMongolian}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 430
    RegexpSample {
        regex: "~p{IsLatinExtendedAdditional}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 431
    RegexpSample {
        regex: "~p{IsGreekExtended}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 432
    RegexpSample {
        regex: "~p{IsGeneralPunctuation}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 433
    RegexpSample {
        regex: "~p{IsSuperscriptsandSubscripts}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 434
    RegexpSample {
        regex: "~p{IsCurrencySymbols}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 435
    RegexpSample {
        regex: "~p{IsCombiningDiacriticalMarksforSymbols}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 436
    RegexpSample {
        regex: "~p{IsLetterlikeSymbols}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 437
    RegexpSample {
        regex: "~p{IsNumberForms}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 438
    RegexpSample {
        regex: "~p{IsArrows}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 439
    RegexpSample {
        regex: "~p{IsMathematicalOperators}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 440
    RegexpSample {
        regex: "~p{IsMiscellaneousTechnical}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 441
    RegexpSample {
        regex: "~p{IsControlPictures}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 442
    RegexpSample {
        regex: "~p{IsOpticalCharacterRecognition}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 443
    RegexpSample {
        regex: "~p{IsEnclosedAlphanumerics}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 444
    RegexpSample {
        regex: "~p{IsBoxDrawing}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 445
    RegexpSample {
        regex: "~p{IsBlockElements}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 446
    RegexpSample {
        regex: "~p{IsGeometricShapes}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 447
    RegexpSample {
        regex: "~p{IsMiscellaneousSymbols}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 448
    RegexpSample {
        regex: "~p{IsDingbats}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 449
    RegexpSample {
        regex: "~p{IsBraillePatterns}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 450
    RegexpSample {
        regex: "~p{IsCJKRadicalsSupplement}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 451
    RegexpSample {
        regex: "~p{IsKangxiRadicals}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 452
    RegexpSample {
        regex: "~p{IsIdeographicDescriptionCharacters}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 453
    RegexpSample {
        regex: "~p{IsCJKSymbolsandPunctuation}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 454
    RegexpSample {
        regex: "~p{IsHiragana}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 455
    RegexpSample {
        regex: "~p{IsKatakana}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 456
    RegexpSample {
        regex: "~p{IsBopomofo}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 457
    RegexpSample {
        regex: "~p{IsHangulCompatibilityJamo}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 458
    RegexpSample {
        regex: "~p{IsKanbun}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 459
    RegexpSample {
        regex: "~p{IsBopomofoExtended}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 460
    RegexpSample {
        regex: "~p{IsEnclosedCJKLettersandMonths}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 461
    RegexpSample {
        regex: "~p{IsCJKCompatibility}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 462
    RegexpSample {
        regex: "~p{IsCJKUnifiedIdeographsExtensionA}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 463
    RegexpSample {
        regex: "~p{IsCJKUnifiedIdeographs}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 464
    RegexpSample {
        regex: "~p{IsYiSyllables}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 465
    RegexpSample {
        regex: "~p{IsYiRadicals}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 466
    RegexpSample {
        regex: "~p{IsHangulSyllables}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 467
    RegexpSample {
        regex: "~p{IsHighSurrogates}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 468
    RegexpSample {
        regex: "~p{IsCJKCompatibilityIdeographs}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 469
    RegexpSample {
        regex: "~p{IsAlphabeticPresentationForms}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 470
    RegexpSample {
        regex: "~p{IsArabicPresentationForms-A}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 471
    RegexpSample {
        regex: "~p{IsCombiningHalfMarks}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 472
    RegexpSample {
        regex: "~p{IsCJKCompatibilityForms}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 473
    RegexpSample {
        regex: "~p{IsSmallFormVariants}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 474
    RegexpSample {
        regex: "~p{IsArabicPresentationForms-B}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 475
    RegexpSample {
        regex: "~p{IsSpecials}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 476
    RegexpSample {
        regex: "~p{IsHalfwidthandFullwidthForms}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 477
    RegexpSample {
        regex: "~p{IsOldItalic}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 478
    RegexpSample {
        regex: "~p{IsGothic}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 479
    RegexpSample {
        regex: "~p{IsDeseret}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 480
    RegexpSample {
        regex: "~p{IsByzantineMusicalSymbols}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 481
    RegexpSample {
        regex: "~p{IsMusicalSymbols}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 482
    RegexpSample {
        regex: "~p{IsMathematicalAlphanumericSymbols}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 483
    RegexpSample {
        regex: "~p{IsCJKUnifiedIdeographsExtensionB}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 484
    RegexpSample {
        regex: "~p{IsCJKCompatibilityIdeographsSupplement}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 485
    RegexpSample {
        regex: "~p{IsTags}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 486
    RegexpSample {
        regex: "~p{IsSupplementaryPrivateUseArea-A}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 487
    RegexpSample {
        regex: ".",
        matches: &["a", " "],
        nomatches: &["aa", ""],
        valid: true,
    },
    // Sample 488
    RegexpSample {
        regex: "~s",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 489
    RegexpSample {
        regex: "~s*~c~s?~c~s+~c~s*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 490
    RegexpSample {
        regex: "a~s{0,3}a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 491
    RegexpSample {
        regex: "a~sb",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 492
    RegexpSample {
        regex: "~S",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 493
    RegexpSample {
        regex: "~S+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 494
    RegexpSample {
        regex: "~S*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 495
    RegexpSample {
        regex: "~S?~s?~S?~s+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 496
    RegexpSample {
        regex: "~i",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 497
    RegexpSample {
        regex: "~i*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 498
    RegexpSample {
        regex: "~i+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 499
    RegexpSample {
        regex: "~c~i*a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 500
    RegexpSample {
        regex: "[~s~i]*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 501
    RegexpSample {
        regex: "~I",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 502
    RegexpSample {
        regex: "~I*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 503
    RegexpSample {
        regex: "a~I+~c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 504
    RegexpSample {
        regex: "~c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 505
    RegexpSample {
        regex: "~c?~?~d~s~c+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 506
    RegexpSample {
        regex: "~c?~c+~c*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 507
    RegexpSample {
        regex: "~C",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 508
    RegexpSample {
        regex: "~c~C?~c~C+~c~C*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 509
    RegexpSample {
        regex: "~d",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 510
    RegexpSample {
        regex: "~D",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 511
    RegexpSample {
        regex: "~w",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 512
    RegexpSample {
        regex: "~W",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 513
    RegexpSample {
        regex: "true",
        matches: &["true"],
        nomatches: &["false"],
        valid: true,
    },
    // Sample 514
    RegexpSample {
        regex: "false",
        matches: &["false"],
        nomatches: &["true"],
        valid: true,
    },
    // Sample 515
    RegexpSample {
        regex: "(true|false)",
        matches: &["true", "false"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 516
    RegexpSample {
        regex: "(1|true)",
        matches: &["1"],
        nomatches: &["0"],
        valid: true,
    },
    // Sample 517
    RegexpSample {
        regex: "(1|true|false|0|0)",
        matches: &["0"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 518
    RegexpSample {
        regex: "([0-1]{4}|(0|1){8})",
        matches: &["1111", "11001010"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 519
    RegexpSample {
        regex: "AF01D1",
        matches: &["AF01D1"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 520
    RegexpSample {
        regex: "~d*~.~d+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 521
    RegexpSample {
        regex: "http://~c*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 522
    RegexpSample {
        regex: "[~i~c]+:[~i~c]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 523
    RegexpSample {
        regex: "P~p{Nd}{4}Y~p{Nd}{2}M",
        matches: &["P1111Y12M"],
        nomatches: &["P111Y12M", "P1111Y1M", "P11111Y12M", "P1111Y", "P12M", "P11111Y00M", "P11111Y13M"],
        valid: true,
    },
    // Sample 524
    RegexpSample {
        regex: "~p{Nd}{4}-~d~d-~d~dT~d~d:~d~d:~d~d",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 525
    RegexpSample {
        regex: "~p{Nd}{2}:~d~d:~d~d(~-~d~d:~d~d)?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 526
    RegexpSample {
        regex: "~p{Nd}{4}-~p{Nd}{2}-~p{Nd}{2}",
        matches: &["1999-12-12"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 527
    RegexpSample {
        regex: "~p{Nd}{4}-~[{Nd}{2}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 528
    RegexpSample {
        regex: "~p{Nd}{4}",
        matches: &["1999"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 529
    RegexpSample {
        regex: "~p{Nd}{2}",
        matches: &[""],
        nomatches: &["1999"],
        valid: true,
    },
    // Sample 530
    RegexpSample {
        regex: "--0[123]~-(12|14)",
        matches: &["--03-14"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 531
    RegexpSample {
        regex: "---([123]0)|([12]?[1-9])|(31)",
        matches: &["---30"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 532
    RegexpSample {
        regex: "--((0[1-9])|(1(1|2)))--",
        matches: &["--12--"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 533
    RegexpSample {
        regex: "~c+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 534
    RegexpSample {
        regex: "~c{2,4}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 535
    RegexpSample {
        regex: "[~i~c]*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 536
    RegexpSample {
        regex: "~c[~c~d]*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 537
    RegexpSample {
        regex: "~p{Nd}+",
        matches: &["10000101", "10000201"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 538
    RegexpSample {
        regex: "~-~d~d",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 539
    RegexpSample {
        regex: "~-?~d",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 540
    RegexpSample {
        regex: "~d+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 541
    RegexpSample {
        regex: "~-?[0-3]{3}",
        matches: &["-300"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 542
    RegexpSample {
        regex: "((~-|~+)?[1-127])|(~-?128)",
        matches: &["-128"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 543
    RegexpSample {
        regex: "~p{Nd}~d+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 544
    RegexpSample {
        regex: "~d+~d+~d+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 545
    RegexpSample {
        regex: "~d+~d+~p{Nd}~d+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 546
    RegexpSample {
        regex: "~+?~d",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 547
    RegexpSample {
        regex: "++",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 548
    RegexpSample {
        regex: "[0-9]*",
        matches: &["9", "0"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 549
    RegexpSample {
        regex: "~-[0-9]*",
        matches: &["-11111", "-9"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 550
    RegexpSample {
        regex: "[13]",
        matches: &["1", "3"],
        nomatches: &["2"],
        valid: true,
    },
    // Sample 551
    RegexpSample {
        regex: "[123]+|[abc]+",
        matches: &["112233123", "abcaabbccabc"],
        nomatches: &["1a", "a1"],
        valid: true,
    },
    // Sample 552
    RegexpSample {
        regex: "([abc]+)|([123]+)",
        matches: &["112233123", "abcaabbccabc", "abab"],
        nomatches: &["1a", "1a", "x"],
        valid: true,
    },
    // Sample 553
    RegexpSample {
        regex: "[abxyz]+",
        matches: &["abab"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 554
    RegexpSample {
        regex: "(~p{Lu}~w*)~s(~p{Lu}~w*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 555
    RegexpSample {
        regex: "(~p{Lu}~p{Ll}*)~s(~p{Lu}~p{Ll}*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 556
    RegexpSample {
        regex: "(~P{Ll}~p{Ll}*)~s(~P{Ll}~p{Ll}*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 557
    RegexpSample {
        regex: "(~P{Lu}+~p{Lu})~s(~P{Lu}+~p{Lu})",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 558
    RegexpSample {
        regex: "(~p{Lt}~w*)~s(~p{Lt}*~w*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 559
    RegexpSample {
        regex: "(~P{Lt}~w*)~s(~P{Lt}*~w*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 560
    RegexpSample {
        regex: "[@-D]+",
        matches: &[""],
        nomatches: &["eE?@ABCDabcdeE"],
        valid: true,
    },
    // Sample 561
    RegexpSample {
        regex: "[>-D]+",
        matches: &[""],
        nomatches: &["eE=>?@ABCDabcdeE"],
        valid: true,
    },
    // Sample 562
    RegexpSample {
        regex: "[~u0554-~u0557]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 563
    RegexpSample {
        regex: "[X-~]]+",
        matches: &[""],
        nomatches: &["wWXYZxyz[~]^"],
        valid: true,
    },
    // Sample 564
    RegexpSample {
        regex: "[X-~u0533]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 565
    RegexpSample {
        regex: "[X-a]+",
        matches: &[""],
        nomatches: &["wWAXYZaxyz"],
        valid: true,
    },
    // Sample 566
    RegexpSample {
        regex: "[X-c]+",
        matches: &[""],
        nomatches: &["wWABCXYZabcxyz"],
        valid: true,
    },
    // Sample 567
    RegexpSample {
        regex: "[X-~u00C0]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 568
    RegexpSample {
        regex: "[~u0100~u0102~u0104]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 569
    RegexpSample {
        regex: "[B-D~u0130]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 570
    RegexpSample {
        regex: "[~u013B~u013D~u013F]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 571
    RegexpSample {
        regex: "(Foo) (Bar)",
        matches: &["Foo Bar", "Foo Bar"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 572
    RegexpSample {
        regex: "~p{klsak",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 573
    RegexpSample {
        regex: "{5",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 574
    RegexpSample {
        regex: "{5,",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 575
    RegexpSample {
        regex: "{5,6",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 576
    RegexpSample {
        regex: "(?r:foo)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 577
    RegexpSample {
        regex: "(?c:foo)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 578
    RegexpSample {
        regex: "(?n:(foo)(~s+)(bar))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 579
    RegexpSample {
        regex: "(?e:foo)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 580
    RegexpSample {
        regex: "(?+i:foo)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 581
    RegexpSample {
        regex: "foo([~d]*)bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 582
    RegexpSample {
        regex: "([~D]*)bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 583
    RegexpSample {
        regex: "foo([~s]*)bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 584
    RegexpSample {
        regex: "foo([~S]*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 585
    RegexpSample {
        regex: "foo([~w]*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 586
    RegexpSample {
        regex: "foo([~W]*)bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 587
    RegexpSample {
        regex: "([~p{Lu}]~w*)~s([~p{Lu}]~w*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 588
    RegexpSample {
        regex: "([~P{Ll}][~p{Ll}]*)~s([~P{Ll}][~p{Ll}]*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 589
    RegexpSample {
        regex: "foo([a-~d]*)bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 590
    RegexpSample {
        regex: "([5-~D]*)bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 591
    RegexpSample {
        regex: "foo([6-~s]*)bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 592
    RegexpSample {
        regex: "foo([c-~S]*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 593
    RegexpSample {
        regex: "foo([7-~w]*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 594
    RegexpSample {
        regex: "foo([a-~W]*)bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 595
    RegexpSample {
        regex: "([f-~p{Lu}]~w*)~s([~p{Lu}]~w*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 596
    RegexpSample {
        regex: "([1-~P{Ll}][~p{Ll}]*)~s([~P{Ll}][~p{Ll}]*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 597
    RegexpSample {
        regex: "[~p]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 598
    RegexpSample {
        regex: "[~P]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 599
    RegexpSample {
        regex: "([~pfoo])",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 600
    RegexpSample {
        regex: "([~Pfoo])",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 601
    RegexpSample {
        regex: "(~p{",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 602
    RegexpSample {
        regex: "(~p{Ll",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 603
    RegexpSample {
        regex: "(foo)([~x41]*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 604
    RegexpSample {
        regex: "(foo)([~u0041]*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 605
    RegexpSample {
        regex: "(foo)([~r]*)(bar)",
        matches: &[""],
        nomatches: &["foo   bar"],
        valid: true,
    },
    // Sample 606
    RegexpSample {
        regex: "(foo)([~o]*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 607
    RegexpSample {
        regex: "(foo)~d*bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 608
    RegexpSample {
        regex: "~D*(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 609
    RegexpSample {
        regex: "(foo)~s*(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 610
    RegexpSample {
        regex: "(foo)~S*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 611
    RegexpSample {
        regex: "(foo)~w*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 612
    RegexpSample {
        regex: "(foo)~W*(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 613
    RegexpSample {
        regex: "~p{Lu}(~w*)~s~p{Lu}(~w*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 614
    RegexpSample {
        regex: "~P{Ll}~p{Ll}*~s~P{Ll}~p{Ll}*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 615
    RegexpSample {
        regex: "foo(?(?#COMMENT)foo)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 616
    RegexpSample {
        regex: "foo(?(?afdfoo)bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 617
    RegexpSample {
        regex: "(foo) #foo        ~s+ #followed by 1 or more whitespace        (bar)  #followed by bar        ",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 618
    RegexpSample {
        regex: "(foo) #foo        ~s+ #followed by 1 or more whitespace        (bar)  #followed by bar",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 619
    RegexpSample {
        regex: "(foo) (?#foo) ~s+ (?#followed by 1 or more whitespace) (bar)  (?#followed by bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 620
    RegexpSample {
        regex: "(foo) (?#foo) ~s+ (?#followed by 1 or more whitespace",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 621
    RegexpSample {
        regex: "(foo)(~077)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 622
    RegexpSample {
        regex: "(foo)(~77)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 623
    RegexpSample {
        regex: "(foo)(~176)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 624
    RegexpSample {
        regex: "(foo)(~300)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 625
    RegexpSample {
        regex: "(foo)(~477)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 626
    RegexpSample {
        regex: "(foo)(~777)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 627
    RegexpSample {
        regex: "(foo)(~7770)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 628
    RegexpSample {
        regex: "(foo)(~7)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 629
    RegexpSample {
        regex: "(foo)(~40)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 630
    RegexpSample {
        regex: "(foo)(~040)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 631
    RegexpSample {
        regex: "(foo)(~377)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 632
    RegexpSample {
        regex: "(foo)(~400)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 633
    RegexpSample {
        regex: "(foo)(~x2a*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 634
    RegexpSample {
        regex: "(foo)(~x2b*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 635
    RegexpSample {
        regex: "(foo)(~x2c*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 636
    RegexpSample {
        regex: "(foo)(~x2d*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 637
    RegexpSample {
        regex: "(foo)(~x2e*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 638
    RegexpSample {
        regex: "(foo)(~x2f*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 639
    RegexpSample {
        regex: "(foo)(~x2A*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 640
    RegexpSample {
        regex: "(foo)(~x2B*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 641
    RegexpSample {
        regex: "(foo)(~x2C*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 642
    RegexpSample {
        regex: "(foo)(~x2D*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 643
    RegexpSample {
        regex: "(foo)(~x2E*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 644
    RegexpSample {
        regex: "(foo)(~x2F*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 645
    RegexpSample {
        regex: "(foo)(~c*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 646
    RegexpSample {
        regex: "(foo)~c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 647
    RegexpSample {
        regex: "(foo)(~c *)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 648
    RegexpSample {
        regex: "(foo)(~c?*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 649
    RegexpSample {
        regex: "(foo)(~c`*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 650
    RegexpSample {
        regex: "(foo)(~c~|*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 651
    RegexpSample {
        regex: "(foo)(~c~[*)(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 652
    RegexpSample {
        regex: "~A(foo)~s+(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 653
    RegexpSample {
        regex: "(foo)~s+(bar)~Z",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 654
    RegexpSample {
        regex: "(foo)~s+(bar)~z",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 655
    RegexpSample {
        regex: "~b@foo",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 656
    RegexpSample {
        regex: "~b,foo",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 657
    RegexpSample {
        regex: "~b~[foo",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 658
    RegexpSample {
        regex: "~B@foo",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 659
    RegexpSample {
        regex: "~B,foo",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 660
    RegexpSample {
        regex: "~B~[foo",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 661
    RegexpSample {
        regex: "(~w+)~s+(~w+)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 662
    RegexpSample {
        regex: "(foo~w+)~s+(bar~w+)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 663
    RegexpSample {
        regex: "([^{}]|~n)+",
        matches: &[""],
        nomatches: &["{{{{Hello  World  }END"],
        valid: true,
    },
    // Sample 664
    RegexpSample {
        regex: "(([0-9])|([a-z])|([A-Z]))*",
        matches: &[""],
        nomatches: &["{hello 1234567890 world}", "{HELLO 1234567890 world}", "{1234567890 hello  world}"],
        valid: true,
    },
    // Sample 665
    RegexpSample {
        regex: "(([0-9])|([a-z])|([A-Z]))+",
        matches: &[""],
        nomatches: &["{hello 1234567890 world}", "{HELLO 1234567890 world}", "{1234567890 hello world}"],
        valid: true,
    },
    // Sample 666
    RegexpSample {
        regex: "(([a-d]*)|([a-z]*))",
        matches: &["aaabbbcccdddeeefff"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 667
    RegexpSample {
        regex: "(([d-f]*)|([c-e]*))",
        matches: &["dddeeeccceee"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 668
    RegexpSample {
        regex: "(([c-e]*)|([d-f]*))",
        matches: &["dddeeeccceee"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 669
    RegexpSample {
        regex: "(([a-d]*)|(.*))",
        matches: &["aaabbbcccdddeeefff"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 670
    RegexpSample {
        regex: "(([d-f]*)|(.*))",
        matches: &["dddeeeccceee"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 671
    RegexpSample {
        regex: "(([c-e]*)|(.*))",
        matches: &["dddeeeccceee"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 672
    RegexpSample {
        regex: "CH",
        matches: &[""],
        nomatches: &["Ch", "Ch"],
        valid: true,
    },
    // Sample 673
    RegexpSample {
        regex: "cH",
        matches: &[""],
        nomatches: &["Ch", "Ch"],
        valid: true,
    },
    // Sample 674
    RegexpSample {
        regex: "AA",
        matches: &[""],
        nomatches: &["Aa", "Aa"],
        valid: true,
    },
    // Sample 675
    RegexpSample {
        regex: "aA",
        matches: &[""],
        nomatches: &["Aa", "Aa"],
        valid: true,
    },
    // Sample 676
    RegexpSample {
        regex: "ı",
        matches: &[""],
        nomatches: &["I", "I", "I", "i", "I", "i"],
        valid: true,
    },
    // Sample 677
    RegexpSample {
        regex: "İ",
        matches: &[""],
        nomatches: &["i", "i", "I", "i", "I", "i"],
        valid: true,
    },
    // Sample 678
    RegexpSample {
        regex: "([0-9]+?)([~w]+?)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 679
    RegexpSample {
        regex: "([0-9]+?)([a-z]+?)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 680
    RegexpSample {
        regex: "^[abcd]{0,16}*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 681
    RegexpSample {
        regex: "^[abcd]{1,}*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 682
    RegexpSample {
        regex: "^[abcd]{1}*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 683
    RegexpSample {
        regex: "^[abcd]{0,16}?*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 684
    RegexpSample {
        regex: "^[abcd]{1,}?*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 685
    RegexpSample {
        regex: "^[abcd]{1}?*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 686
    RegexpSample {
        regex: "^[abcd]*+$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 687
    RegexpSample {
        regex: "^[abcd]+*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 688
    RegexpSample {
        regex: "^[abcd]?*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 689
    RegexpSample {
        regex: "^[abcd]*?+$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 690
    RegexpSample {
        regex: "^[abcd]+?*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 691
    RegexpSample {
        regex: "^[abcd]??*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 692
    RegexpSample {
        regex: "^[abcd]*{0,5}$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 693
    RegexpSample {
        regex: "^[abcd]+{0,5}$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 694
    RegexpSample {
        regex: "^[abcd]?{0,5}$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 695
    RegexpSample {
        regex: "http://([a-zA-z0-9~-]*~.?)*?(:[0-9]*)??/",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 696
    RegexpSample {
        regex: "http://([a-zA-Z0-9~-]*~.?)*?/",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 697
    RegexpSample {
        regex: "([a-z]*?)([~w])",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 698
    RegexpSample {
        regex: "([a-z]*)([~w])",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 699
    RegexpSample {
        regex: "[abcd-[d]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 700
    RegexpSample {
        regex: "[~d-[357]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 701
    RegexpSample {
        regex: "[~w-[b-y]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 702
    RegexpSample {
        regex: "[~w-[~d]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 703
    RegexpSample {
        regex: "[~w-[~p{Ll}]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 704
    RegexpSample {
        regex: "[~d-[13579]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 705
    RegexpSample {
        regex: "[~p{Ll}-[ae-z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 706
    RegexpSample {
        regex: "[~p{Nd}-[2468]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 707
    RegexpSample {
        regex: "[~P{Lu}-[ae-z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 708
    RegexpSample {
        regex: "[abcd-[def]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 709
    RegexpSample {
        regex: "[~d-[357a-z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 710
    RegexpSample {
        regex: "[~d-[de357fgA-Z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 711
    RegexpSample {
        regex: "[~d-[357~p{Ll}]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 712
    RegexpSample {
        regex: "[~w-[b-y~s]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 713
    RegexpSample {
        regex: "[~w-[~d~p{Po}]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 714
    RegexpSample {
        regex: "[~w-[~p{Ll}~s]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 715
    RegexpSample {
        regex: "[~d-[13579a-zA-Z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 716
    RegexpSample {
        regex: "[~d-[13579abcd]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 717
    RegexpSample {
        regex: "[~d-[13579~s]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 718
    RegexpSample {
        regex: "[~w-[b-y~p{Po}]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 719
    RegexpSample {
        regex: "[~w-[b-y!.,]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 720
    RegexpSample {
        regex: "[~p{Ll}-[ae-z0-9]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 721
    RegexpSample {
        regex: "[~p{Nd}-[2468az]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 722
    RegexpSample {
        regex: "[~P{Lu}-[ae-zA-Z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 723
    RegexpSample {
        regex: "[abc-[defg]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 724
    RegexpSample {
        regex: "[~d-[abc]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 725
    RegexpSample {
        regex: "[~d-[a-zA-Z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 726
    RegexpSample {
        regex: "[~d-[~p{Ll}]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 727
    RegexpSample {
        regex: "[~w-[~p{Po}]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 728
    RegexpSample {
        regex: "[~d-[~D]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 729
    RegexpSample {
        regex: "[a-zA-Z0-9-[~s]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 730
    RegexpSample {
        regex: "[~p{Ll}-[A-Z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 731
    RegexpSample {
        regex: "[~p{Nd}-[a-z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 732
    RegexpSample {
        regex: "[~P{Lu}-[~p{Lu}]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 733
    RegexpSample {
        regex: "[~P{Lu}-[A-Z]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 734
    RegexpSample {
        regex: "[~P{Nd}-[~p{Nd}]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 735
    RegexpSample {
        regex: "[~P{Nd}-[2-8]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 736
    RegexpSample {
        regex: "([ ]|[~w-[0-9]])+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 737
    RegexpSample {
        regex: "([0-9-[02468]]|[0-9-[13579]])+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 738
    RegexpSample {
        regex: "([^0-9-[a-zAE-Z]]|[~w-[a-zAF-Z]])+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 739
    RegexpSample {
        regex: "([~p{Ll}-[aeiou]]|[^~w-[~s]])+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 740
    RegexpSample {
        regex: "98[~d-[9]][~d-[8]][~d-[0]]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 741
    RegexpSample {
        regex: "m[~w-[^aeiou]][~w-[^aeiou]]t",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 742
    RegexpSample {
        regex: "[abcdef-[^bce]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 743
    RegexpSample {
        regex: "[^cde-[ag]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 744
    RegexpSample {
        regex: "[~p{IsGreekandCoptic}-[~P{Lu}]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 745
    RegexpSample {
        regex: "[a-zA-Z-[aeiouAEIOU]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 746
    RegexpSample {
        regex: "[abcd~-d-[bc]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 747
    RegexpSample {
        regex: "[^a-f-[~x00-~x60~u007B-~uFFFF]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 748
    RegexpSample {
        regex: "[a-f-[]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 749
    RegexpSample {
        regex: "[~[~]a-f-[[]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 750
    RegexpSample {
        regex: "[~[~]a-f-[]]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 751
    RegexpSample {
        regex: "[ab~-~[cd-[-[]]]]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 752
    RegexpSample {
        regex: "[ab~-~[cd-[[]]]]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 753
    RegexpSample {
        regex: "[a-[a-f]]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 754
    RegexpSample {
        regex: "[a-[c-e]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 755
    RegexpSample {
        regex: "[a-d~--[bc]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 756
    RegexpSample {
        regex: "[[abcd]-[bc]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 757
    RegexpSample {
        regex: "[-[e-g]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 758
    RegexpSample {
        regex: "[-e-g]+",
        matches: &[""],
        nomatches: &["ddd---eeefffggghhh", "ddd---eeefffggghhh"],
        valid: true,
    },
    // Sample 759
    RegexpSample {
        regex: "[a-e - m-p]+",
        matches: &[""],
        nomatches: &["---a b c d e m n o p---"],
        valid: true,
    },
    // Sample 760
    RegexpSample {
        regex: "[^-[bc]]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 761
    RegexpSample {
        regex: "[A-[]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 762
    RegexpSample {
        regex: "[a~-[bc]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 763
    RegexpSample {
        regex: "[a~-[~-~-bc]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 764
    RegexpSample {
        regex: "[a~-~[~-~[~-bc]+",
        matches: &[""],
        nomatches: &["```bbbaaa---[[[cccddd"],
        valid: true,
    },
    // Sample 765
    RegexpSample {
        regex: "[abc~--[b]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 766
    RegexpSample {
        regex: "[abc~-z-[b]]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 767
    RegexpSample {
        regex: "[a-d~-[b]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 768
    RegexpSample {
        regex: "[abcd~-d~-[bc]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 769
    RegexpSample {
        regex: "[a - c - [ b ] ]+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 770
    RegexpSample {
        regex: "[a - c - [ b ] +",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 771
    RegexpSample {
        regex: "(?<first_name>~~S+)~~s(?<last_name>~~S+)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 772
    RegexpSample {
        regex: "(a+)(?:b*)(ccc)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 773
    RegexpSample {
        regex: "abc(?=XXX)~w+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 774
    RegexpSample {
        regex: "abc(?!XXX)~w+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 775
    RegexpSample {
        regex: "[^0-9]+(?>[0-9]+)3",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 776
    RegexpSample {
        regex: "^aa$",
        matches: &[""],
        nomatches: &["aA"],
        valid: true,
    },
    // Sample 777
    RegexpSample {
        regex: "^Aa$",
        matches: &[""],
        nomatches: &["aA"],
        valid: true,
    },
    // Sample 778
    RegexpSample {
        regex: "~s+~d+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 779
    RegexpSample {
        regex: "foo~d+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 780
    RegexpSample {
        regex: "foo~s+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 781
    RegexpSample {
        regex: "(hello)foo~s+bar(world)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 782
    RegexpSample {
        regex: "(hello)~s+(world)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 783
    RegexpSample {
        regex: "(foo)~s+(bar)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 784
    RegexpSample {
        regex: "(d)(o)(g)(~s)(c)(a)(t)(~s)(h)(a)(s)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 785
    RegexpSample {
        regex: "^([a-z0-9]+)@([a-z]+)~.([a-z]+)$",
        matches: &[""],
        nomatches: &["bar@bar.foo.com"],
        valid: true,
    },
    // Sample 786
    RegexpSample {
        regex: "^http://www.([a-zA-Z0-9]+)~.([a-z]+)$",
        matches: &[""],
        nomatches: &["http://www.foo.bar.com"],
        valid: true,
    },
    // Sample 787
    RegexpSample {
        regex: "(.*)",
        matches: &["abc~nsfc"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 788
    RegexpSample {
        regex: "            ((.)+)      ",
        matches: &[""],
        nomatches: &["abc"],
        valid: true,
    },
    // Sample 789
    RegexpSample {
        regex: " ([^/]+)       ",
        matches: &[" abc       "],
        nomatches: &[""],
        valid: true,
    },
    // Sample 790
    RegexpSample {
        regex: ".*~B(SUCCESS)~B.*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 791
    RegexpSample {
        regex: "~060(~061)?~061",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 792
    RegexpSample {
        regex: "(~x30~x31~x32)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 793
    RegexpSample {
        regex: "(~u0034)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 794
    RegexpSample {
        regex: "(a+)(b*)(c?)",
        matches: &[""],
        nomatches: &["aaabbbccc"],
        valid: true,
    },
    // Sample 795
    RegexpSample {
        regex: "(d+?)(e*?)(f??)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 796
    RegexpSample {
        regex: "(111|aaa)",
        matches: &["aaa"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 797
    RegexpSample {
        regex: "(abbc)(?(1)111|222)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 798
    RegexpSample {
        regex: ".*~b(~w+)~b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 799
    RegexpSample {
        regex: "a+~.?b*~.+c{2}",
        matches: &["ab.cc"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 800
    RegexpSample {
        regex: "(abra(cad)?)+",
        matches: &[""],
        nomatches: &["abracadabra1abracadabra2abracadabra3"],
        valid: true,
    },
    // Sample 801
    RegexpSample {
        regex: "^(cat|chat)",
        matches: &[""],
        nomatches: &["cats are bad"],
        valid: true,
    },
    // Sample 802
    RegexpSample {
        regex: "([0-9]+(~.[0-9]+){3})",
        matches: &["209.25.0.111"],
        nomatches: &[""],
        valid: true,
    },
    // Sample 803
    RegexpSample {
        regex: "qqq(123)*",
        matches: &[""],
        nomatches: &["Startqqq123123End"],
        valid: true,
    },
    // Sample 804
    RegexpSample {
        regex: "(~s)?(-)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 805
    RegexpSample {
        regex: "a(.)c(.)e",
        matches: &[""],
        nomatches: &["123abcde456aBCDe789"],
        valid: true,
    },
    // Sample 806
    RegexpSample {
        regex: "(~S+):~W(~d+)~s(~D+)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 807
    RegexpSample {
        regex: "a[b-a]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 808
    RegexpSample {
        regex: "a[]b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 809
    RegexpSample {
        regex: "a[",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 810
    RegexpSample {
        regex: "a]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 811
    RegexpSample {
        regex: "a[]]b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 812
    RegexpSample {
        regex: "a[^]b]c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 813
    RegexpSample {
        regex: "~ba~b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 814
    RegexpSample {
        regex: "~by~b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 815
    RegexpSample {
        regex: "~Ba~B",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 816
    RegexpSample {
        regex: "~By~b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 817
    RegexpSample {
        regex: "~by~B",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 818
    RegexpSample {
        regex: "~By~B",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 819
    RegexpSample {
        regex: "(*)b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 820
    RegexpSample {
        regex: "a~",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 821
    RegexpSample {
        regex: "abc)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 822
    RegexpSample {
        regex: "(abc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 823
    RegexpSample {
        regex: "a**",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 824
    RegexpSample {
        regex: "a.+?c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 825
    RegexpSample {
        regex: "))((",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 826
    RegexpSample {
        regex: "~10((((((((((a))))))))))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 827
    RegexpSample {
        regex: "~1(abc)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 828
    RegexpSample {
        regex: "~1([a-c]*)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 829
    RegexpSample {
        regex: "~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 830
    RegexpSample {
        regex: "~2",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 831
    RegexpSample {
        regex: "(a)|~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 832
    RegexpSample {
        regex: "(a)|~6",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 833
    RegexpSample {
        regex: "(~2b*?([a-c]))*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 834
    RegexpSample {
        regex: "(~2b*?([a-c])){3}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 835
    RegexpSample {
        regex: "(x(a)~3(~2|b))+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 836
    RegexpSample {
        regex: "((a)~3(~2|b)){2,}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 837
    RegexpSample {
        regex: "ab*?bc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 838
    RegexpSample {
        regex: "ab{0,}?bc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 839
    RegexpSample {
        regex: "ab+?bc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 840
    RegexpSample {
        regex: "ab{1,}?bc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 841
    RegexpSample {
        regex: "ab{1,3}?bc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 842
    RegexpSample {
        regex: "ab{3,4}?bc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 843
    RegexpSample {
        regex: "ab{4,5}?bc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 844
    RegexpSample {
        regex: "ab??bc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 845
    RegexpSample {
        regex: "ab{0,1}?bc",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 846
    RegexpSample {
        regex: "ab??c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 847
    RegexpSample {
        regex: "ab{0,1}?c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 848
    RegexpSample {
        regex: "a.*?c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 849
    RegexpSample {
        regex: "a.{0,5}?c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 850
    RegexpSample {
        regex: "(a+|b){0,1}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 851
    RegexpSample {
        regex: "(?:(?:(?:(?:(?:(?:(?:(?:(?:(a))))))))))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 852
    RegexpSample {
        regex: "(?:(?:(?:(?:(?:(?:(?:(?:(?:(a|b|c))))))))))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 853
    RegexpSample {
        regex: "(.)(?:b|c|d)a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 854
    RegexpSample {
        regex: "(.)(?:b|c|d)*a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 855
    RegexpSample {
        regex: "(.)(?:b|c|d)+?a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 856
    RegexpSample {
        regex: "(.)(?:b|c|d)+a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 857
    RegexpSample {
        regex: "(.)(?:b|c|d){2}a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 858
    RegexpSample {
        regex: "(.)(?:b|c|d){4,5}a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 859
    RegexpSample {
        regex: "(.)(?:b|c|d){4,5}?a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 860
    RegexpSample {
        regex: ":(?:",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 861
    RegexpSample {
        regex: "(.)(?:b|c|d){6,7}a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 862
    RegexpSample {
        regex: "(.)(?:b|c|d){6,7}?a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 863
    RegexpSample {
        regex: "(.)(?:b|c|d){5,6}a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 864
    RegexpSample {
        regex: "(.)(?:b|c|d){5,6}?a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 865
    RegexpSample {
        regex: "(.)(?:b|c|d){5,7}a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 866
    RegexpSample {
        regex: "(.)(?:b|c|d){5,7}?a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 867
    RegexpSample {
        regex: "(.)(?:b|(c|e){1,2}?|d)+?a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 868
    RegexpSample {
        regex: "^(a~1?){4}$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 869
    RegexpSample {
        regex: "^(a(?(1)~1)){4}$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 870
    RegexpSample {
        regex: "(?:(f)(o)(o)|(b)(a)(r))*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 871
    RegexpSample {
        regex: "(?:..)*a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 872
    RegexpSample {
        regex: "(?:..)*?a",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 873
    RegexpSample {
        regex: "(?:(?i)a)b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 874
    RegexpSample {
        regex: "((?i)a)b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 875
    RegexpSample {
        regex: "(?i:a)b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 876
    RegexpSample {
        regex: "((?i:a))b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 877
    RegexpSample {
        regex: "(?:(?-i)a)b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 878
    RegexpSample {
        regex: "((?-i)a)b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 879
    RegexpSample {
        regex: "(?-i:a)b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 880
    RegexpSample {
        regex: "((?-i:a))b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 881
    RegexpSample {
        regex: "((?-i:a.))b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 882
    RegexpSample {
        regex: "((?s-i:a.))b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 883
    RegexpSample {
        regex: "(?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 884
    RegexpSample {
        regex: "(?:c|d)(?:)(?:aaaaaaaa(?:)(?:bbbbbbbb)(?:bbbbbbbb(?:))(?:bbbbbbbb(?:)(?:bbbbbbbb)))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 885
    RegexpSample {
        regex: "~1~d(ab)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 886
    RegexpSample {
        regex: "x(~~)*(?:(?:F)?)?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 887
    RegexpSample {
        regex: "^a(?#xxx){3}c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 888
    RegexpSample {
        regex: "^a (?#xxx) (?#yyy) {3}c",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 889
    RegexpSample {
        regex: "^(?:a?b?)*$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 890
    RegexpSample {
        regex: "((?s)^a(.))((?m)^b$)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 891
    RegexpSample {
        regex: "((?m)^b$)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 892
    RegexpSample {
        regex: "(?m)^b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 893
    RegexpSample {
        regex: "(?m)^(b)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 894
    RegexpSample {
        regex: "((?m)^b)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 895
    RegexpSample {
        regex: "~n((?m)^b)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 896
    RegexpSample {
        regex: "((?s).)c(?!.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 897
    RegexpSample {
        regex: "((?s)b.)c(?!.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 898
    RegexpSample {
        regex: "((c*)(?(1)a|b))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 899
    RegexpSample {
        regex: "((q*)(?(1)b|a))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 900
    RegexpSample {
        regex: "(?(1)a|b)(x)?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 901
    RegexpSample {
        regex: "(?(1)b|a)(x)?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 902
    RegexpSample {
        regex: "(?(1)b|a)()?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 903
    RegexpSample {
        regex: "(?(1)b|a)()",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 904
    RegexpSample {
        regex: "(?(1)a|b)()?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 905
    RegexpSample {
        regex: "^(?(2)(~())blah(~))?$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 906
    RegexpSample {
        regex: "^(?(2)(~())blah(~)+)?$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 907
    RegexpSample {
        regex: "(?(1?)a|b)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 908
    RegexpSample {
        regex: "(?(1)a|b|c)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 909
    RegexpSample {
        regex: "(ba~2)(?=(a+?))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 910
    RegexpSample {
        regex: "ba~1(?=(a+?))$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 911
    RegexpSample {
        regex: "(?>a+)b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 912
    RegexpSample {
        regex: "([[:]+)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 913
    RegexpSample {
        regex: "([[=]+)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 914
    RegexpSample {
        regex: "([[.]+)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 915
    RegexpSample {
        regex: "[a[:xyz:",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 916
    RegexpSample {
        regex: "[a[:xyz:]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 917
    RegexpSample {
        regex: "([a[:xyz:]b]+)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 918
    RegexpSample {
        regex: "((?>a+)b)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 919
    RegexpSample {
        regex: "(?>(a+))b",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 920
    RegexpSample {
        regex: "((?>[^()]+)|~([^()]*~))+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 921
    RegexpSample {
        regex: "a{37,17}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 922
    RegexpSample {
        regex: "a~Z",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 923
    RegexpSample {
        regex: "b~Z",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 924
    RegexpSample {
        regex: "b~z",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 925
    RegexpSample {
        regex: "round~(((?>[^()]+))~)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 926
    RegexpSample {
        regex: "(a~1|(?(1)~1)){2}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 927
    RegexpSample {
        regex: "(a~1|(?(1)~1)){1,2}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 928
    RegexpSample {
        regex: "(a~1|(?(1)~1)){0,2}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 929
    RegexpSample {
        regex: "(a~1|(?(1)~1)){2,}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 930
    RegexpSample {
        regex: "(a~1|(?(1)~1)){1,2}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 931
    RegexpSample {
        regex: "(a~1|(?(1)~1)){0,2}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 932
    RegexpSample {
        regex: "(a~1|(?(1)~1)){2,}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 933
    RegexpSample {
        regex: "~1a(~d*){0,2}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 934
    RegexpSample {
        regex: "~1a(~d*){2,}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 935
    RegexpSample {
        regex: "~1a(~d*){0,2}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 936
    RegexpSample {
        regex: "~1a(~d*){2,}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 937
    RegexpSample {
        regex: "z~1a(~d*){2,}?",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 938
    RegexpSample {
        regex: "((((((((((a))))))))))~10",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 939
    RegexpSample {
        regex: "(abc)~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 940
    RegexpSample {
        regex: "([a-c]*)~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 941
    RegexpSample {
        regex: "(([a-c])b*?~2)*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 942
    RegexpSample {
        regex: "(([a-c])b*?~2){3}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 943
    RegexpSample {
        regex: "((~3|b)~2(a)x)+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 944
    RegexpSample {
        regex: "((~3|b)~2(a)){2,}",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 945
    RegexpSample {
        regex: "a(?!b).",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 946
    RegexpSample {
        regex: "a(?=d).",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 947
    RegexpSample {
        regex: "a(?=c|d).",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 948
    RegexpSample {
        regex: "a(?:b|c|d)(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 949
    RegexpSample {
        regex: "a(?:b|c|d)*(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 950
    RegexpSample {
        regex: "a(?:b|c|d)+?(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 951
    RegexpSample {
        regex: "a(?:b|c|d)+(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 952
    RegexpSample {
        regex: "a(?:b|c|d){2}(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 953
    RegexpSample {
        regex: "a(?:b|c|d){4,5}(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 954
    RegexpSample {
        regex: "a(?:b|c|d){4,5}?(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 955
    RegexpSample {
        regex: "a(?:b|c|d){6,7}(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 956
    RegexpSample {
        regex: "a(?:b|c|d){6,7}?(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 957
    RegexpSample {
        regex: "a(?:b|c|d){5,6}(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 958
    RegexpSample {
        regex: "a(?:b|c|d){5,6}?(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 959
    RegexpSample {
        regex: "a(?:b|c|d){5,7}(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 960
    RegexpSample {
        regex: "a(?:b|c|d){5,7}?(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 961
    RegexpSample {
        regex: "a(?:b|(c|e){1,2}?|d)+?(.)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 962
    RegexpSample {
        regex: "^(?:b|a(?=(.)))*~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 963
    RegexpSample {
        regex: "(ab)~d~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 964
    RegexpSample {
        regex: "((q*)(?(1)a|b))",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 965
    RegexpSample {
        regex: "(x)?(?(1)a|b)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 966
    RegexpSample {
        regex: "(x)?(?(1)b|a)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 967
    RegexpSample {
        regex: "()?(?(1)b|a)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 968
    RegexpSample {
        regex: "()(?(1)b|a)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 969
    RegexpSample {
        regex: "()?(?(1)a|b)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 970
    RegexpSample {
        regex: "^(~()?blah(?(1)(~)))$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 971
    RegexpSample {
        regex: "^(~(+)?blah(?(1)(~)))$",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 972
    RegexpSample {
        regex: "(?(?!a)a|b)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 973
    RegexpSample {
        regex: "(?(?!a)b|a)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 974
    RegexpSample {
        regex: "(?(?=a)b|a)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 975
    RegexpSample {
        regex: "(?(?=a)a|b)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 976
    RegexpSample {
        regex: "(?=(a+?))(~1ab)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 977
    RegexpSample {
        regex: "^(?=(a+?))~1ab",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 978
    RegexpSample {
        regex: "(~d*){0,2}a~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 979
    RegexpSample {
        regex: "(~d*){2,}a~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 980
    RegexpSample {
        regex: "(~d*){0,2}?a~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 981
    RegexpSample {
        regex: "(~d*){2,}?a~1",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 982
    RegexpSample {
        regex: "(~d*){2,}?a~1z",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 983
    RegexpSample {
        regex: "(?>~d+)3",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 984
    RegexpSample {
        regex: "(~w(?=aa)aa)",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 985
    RegexpSample {
        regex: "~p{IsCombiningDiacriticalMarks}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 986
    RegexpSample {
        regex: "~p{IsCyrillic}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 987
    RegexpSample {
        regex: "~p{IsHighSurrogates}+",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 988
    RegexpSample {
        regex: "^([0-9a-zA-Z]([-.~w]*[0-9a-zA-Z])*@(([0-9a-zA-Z])+([-~w]*[0-9a-zA-Z])*~.)+[a-zA-Z]{2,9})",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 989
    RegexpSample {
        regex: "[~w~-~.]+@.*",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 990
    RegexpSample {
        regex: "[~w]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 991
    RegexpSample {
        regex: "[~d]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
    // Sample 992
    RegexpSample {
        regex: "[~i]",
        matches: &[],
        nomatches: &[],
        valid: false,
    },
];
