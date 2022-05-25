// Expands bash braces to the full form.

// uses a recursive descent parser to expand the braces.

// based on https://stackoverflow.com/a/28528753 after porting it to C++
// Also based on braces.c from bash (9439ce09) after porting it to C++, since it makes sense.

use std::borrow::Cow;
use std::convert::Into;
use std::str::Chars;

fn brace_whitespace(c: Option<char>) -> bool {
    if c.is_none() {
        return false;
    }
    let c = c.unwrap();
    c == '\0' || c == ' ' || c == '\t' || c == '\n'
}

/* Basic idea:

Segregate the text into 3 sections: preamble (stuff before an open brace),
postamble (stuff after the matching close brace) and amble (stuff after
    preamble, and before postamble).  Expand amble, and then tack on the
    expansions to preamble.  Expand postamble, and tack on the expansions to
    the result so far.
    */
const BRACE_SEQ_SPECIFIER: &str = "..";
/* The character which is used to separate arguments. */
const BRACE_ARG_SEPARATOR: char = ',';

// Start at INDEX, and skip characters in TEXT.
// Set INDEX to the index of the character matching SATISFY.
// This understands about quoting.
// Return the character that caused us to stop searching;
// this is either the same as SATISFY, or None.
// If SATISFY is `}', we are looking for a brace expression, so we
// should enforce the rules that govern valid brace expansions:
// 1) to count as an arg separator, a comma or `..' has to be outside
//    an inner set of braces.

fn brace_gobbler(text: &str, indx: &mut usize, satisfy: char) -> Option<char> {
    let mut chars = text.chars();
    for _ in 0..*indx {
        chars.next();
    }
    // Stores None if not in quotes, else stores the character used to open the quote
    let mut quoted: Option<char> = None;

    // Counter for the number of nested braces we are in
    let mut level = 0;

    // indicates whether we are potentially looking at a brace sequence or looking to close one
    let mut needs_expanding = satisfy != '}';

    // holds text[i]
    let mut c = None;
    loop {
        // holds text[i-1]
        let prev_char = c;
        c = chars.next();
        if c.is_none() {
            break;
        }
        *indx += 1;
        // allows us to use nth without messing up the iteration and without needing to rebuild the iterator
        let mut chars_copy = chars.clone();
        // holds text[i+1]
        let next_char = chars_copy.next();

        // A backslash escapes the next character, if we are:
        // 1) either not in a quote
        // 2) or in a double-quoted string
        // 3) in a back-tick quoted string
        match quoted {
            None | Some('"') | Some('`') => {
                if c == Some('\\') {
                    *indx += 1;
                    chars.next();
                    continue;
                }
            }
            _ => {}
        }

        // SHELL SPECIFIC: treat ${...} like \{...} and skip it, as long as we are not in a single quote
        if c == Some('$') && next_char == Some('{') && quoted != Some('\'') {
            chars.next();
            *indx += 1;
            if quoted.is_none() {
                level += 1;
            }
            continue;
        }
        // END OF SHELL SPECIFIC

        // skip over the character if we are in the middle of a quoted string
        if quoted.is_some() {
            // reset quoted if we reach the end of the quote
            if c == quoted {
                quoted = None;
            }
            // original source has goto consub to handle shell substitutions, skipping that
            continue;
        }

        match c {
            // Check if we need to open a quoted section
            Some('"') | Some('\'') | Some('`') => {
                quoted = c;
                continue;
            }
            _ => {}
        }

        // SHELL SPECIFIC: original source has some code to handle shell substitutions, skipping that

        // We've hit the character we were looking for, and we are not in a quoted section (checked by a previous if)
        if c == Some(satisfy) && level == 0 && needs_expanding {
            // We ignore an open brace surounded by whitespace, and also an open brace followed by a close brace which is preceeded
            // by whitespace.
            if c == Some('{')
                && (*indx == 0 || brace_whitespace(prev_char))
                && (brace_whitespace(next_char) || next_char == Some('}'))
            {
                continue;
            }
            *indx -= 1;
            break;
        }

        // if we see a brace, we need to increment the level
        if c == Some('{') {
            level += 1;
        // if we see a close brace, we need to decrement the level
        } else if c == Some('}') && level > 0 {
            level -= 1;
        } else if satisfy == '}'
            && level == 0
            && (c == Some(BRACE_ARG_SEPARATOR)
                // check if text[i+2] == '}'
                || (chars_copy.next() != Some('}')
                    // should be fine to use .get here since we only care about ascii ".."
                    && chars.as_str().get(0..2) == Some(BRACE_SEQ_SPECIFIER)))
        {
            needs_expanding = true;
        }
    }
    c
}

fn expand_amble(text: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut i = 0;
    let mut start = 0;

    let chars = text.chars();

    loop {
        let c = brace_gobbler(text, &mut i, BRACE_ARG_SEPARATOR);
        for s in brace_expand(
            chars
                .clone()
                .skip(start)
                .take(i - start)
                .collect::<String>(),
        ) {
            result.push(s);
        }

        i += 1;
        start = i;
        if c.is_none() {
            break;
        }
    }
    result
}

#[derive(PartialEq, Copy, Clone, Debug)]
enum StTypes {
    Int,
    Char,
    ZInt,
}

// Make a sequence of strings starting from START to END, inclusive.
// Incremending by INCR, padding based on T and WIDTH.
fn mkseq(start: isize, end: isize, incr: isize, t: StTypes, width: usize) -> Vec<String> {
    let incr = {
        if incr == 0 {
            1_isize
        } else if start > end && incr > 0 {
            -incr
        } else if start < end && incr < 0 {
            if incr == isize::MIN {
                return vec![];
            }
            -incr
        } else {
            incr
        }
    };

    if end.checked_sub(start).is_none() {
        return vec![];
    }

    let mut result = Vec::with_capacity(end.abs_diff(start) / incr.abs() as usize + 1);

    let mut n = start;

    /* Make sure we go through the loop at least once, so {3..3} prints `3' */
    loop {
        let n_str = n.to_string();
        match t {
            StTypes::Int => {
                result.push(n_str);
            }
            StTypes::ZInt => {
                result.push("0".repeat(width.saturating_sub(n_str.len())) + (&n_str));
            }
            StTypes::Char => {
                result.push(String::from(n as u8 as char));
            }
        }
        if n.checked_add(incr).is_none() {
            break;
        }

        n += incr;

        if (incr < 0 && n < end) || (incr > 0 && n > end) {
            break;
        }
    }

    result
}

// splits lhs..rhs[..incr] into the three parts
// and determines of lhs and rhs is a number or a character
// and if incr, if present, is a number, and then returns the
// constructed sequence through mkseq
fn expand_seqterm(text: &str) -> Vec<String> {
    let mut chars = text.chars();
    fn get_hand_side(chars: &mut Chars, is_rhs: bool) -> Option<String> {
        let mut result = String::new();
        loop {
            match chars.next() {
                Some(c) => {
                    if c == '.' {
                        break;
                    }
                    result.push(c);
                }
                None => {
                    // if we are on the right hand side, it's fine if the string ends
                    // else, if we manage to get the the end of the string trying to find
                    // the '.', then we have an error
                    if !is_rhs {
                        return None;
                    }
                    break;
                }
            }
        }
        Some(result)
    }

    // get lhs
    let lhs = match get_hand_side(&mut chars, false) {
        Some(s) => s,
        None => return vec![],
    };

    assert!(!lhs.is_empty());

    // check if the next character is '.' to see if we have a range
    // get_hand_side() should have consumed the first '.' if it exists
    if chars.next() != Some('.') {
        return vec![];
    }

    // get rhs
    let rhs = match get_hand_side(&mut chars, true) {
        Some(s) => s,
        None => return vec![], // unreachable
    };

    assert!(!rhs.is_empty());

    // check if the next character is '.' to see if we have a increment, else it's just the default 1
    let incr = {
        let next = chars.next();
        if next == Some('.') {
            let tem = chars.collect::<String>().parse::<isize>();
            match tem {
                Ok(i) => i,
                Err(_) => return vec![],
            }
        } else if next.is_some() {
            return vec![];
        } else {
            1
        }
    };

    assert!(incr != 0);

    fn get_type_of_string(s: &str) -> Option<StTypes> {
        if s.is_empty() {
            return None;
        }
        let mut chars = s.chars();
        let c = chars.next().unwrap();
        if s.len() == 1 && c.is_alphabetic() {
            return Some(StTypes::Char);
        }
        if s.parse::<isize>().is_ok() {
            if s.len() > 2 && (c == '0' || (c == '-' && chars.next() == Some('0'))) {
                return Some(StTypes::ZInt);
            } else {
                return Some(StTypes::Int);
            }
        }
        None
    }

    // now we check if lhs and rhs are numbers or characters
    // if they are numbers, we need to check if they are integers or zeroes padded integers
    // if they are characters, we need to check if they are single characters
    // check if lhs is a number
    let lhs_type = {
        match get_type_of_string(&lhs) {
            Some(t) => t,
            None => return vec![],
        }
    };

    // check if rhs is a number
    let rhs_type = {
        match get_type_of_string(&rhs) {
            Some(t) => t,
            None => return vec![],
        }
    };

    // check if lhs and rhs are of the same type, but count Zint and Int to be the same
    if lhs_type != rhs_type && lhs_type != StTypes::ZInt && rhs_type != StTypes::ZInt {
        return vec![];
    }

    // OK, we have something.  It's either a sequence of integers, ascending
    // or descending, or a sequence or letters, ditto.  Generate the sequence,
    // put it into a string vector, and return it.
    if lhs_type == StTypes::Char {
        return mkseq(
            lhs.chars().next().unwrap() as isize,
            rhs.chars().next().unwrap() as isize,
            incr,
            lhs_type,
            1,
        );
    }

    mkseq(
        lhs.parse().unwrap(),
        rhs.parse().unwrap(),
        incr,
        {
            if rhs_type == StTypes::ZInt {
                StTypes::ZInt
            } else {
                lhs_type
            }
        },
        {
            if lhs_type == StTypes::ZInt || rhs_type == StTypes::ZInt {
                std::cmp::max(lhs.chars().count(), rhs.chars().count())
            } else {
                0
            }
        },
    )
}

fn array_concat<'a>(arr1: &'a Vec<String>, arr2: &'a Vec<String>) -> Cow<'a, Vec<String>> {
    if arr1.is_empty() {
        return Cow::Borrowed(arr2);
    }
    if arr2.is_empty() {
        return Cow::Borrowed(arr1);
    }

    let mut result = Vec::with_capacity(arr1.len() + arr2.len());

    for s1 in arr1 {
        for s2 in arr2 {
            result.push(s1.to_string() + s2);
        }
    }

    Cow::Owned(result)
}

/* Return an array of strings; the brace expansion of TEXT. */
pub fn brace_expand<S: Into<String>>(text: S) -> Vec<String> {
    let text = text.into();

    if text.is_empty() {
        return vec![];
    }

    let text_chars = text.chars();

    let mut c: Option<char>;
    /* Find the text of the preamble. */
    let mut i = 0;

    /* Make sure that when we exit this loop, c == None or text[i] begins a
    valid brace expansion sequence. */
    loop {
        c = brace_gobbler(&text, &mut i, '{');
        if c.is_none() {
            // println!(
            //     "brace_expand: text[{}] broken at first gobble with i {}",
            //     text, i
            // );
            break;
        }
        let c1 = c;
        // Verify that c begins a valid brace expansion word.  If it doesn't, we
        // go on.  Loop stops when there are no more open braces in the word.
        let mut j = i + 1;
        c = brace_gobbler(&text, &mut j, '}');
        if c.is_none() {
            // it's not
            i += 1;
            // println!(
            //     "brace_expand: text[{}] continueing at second gobble i {}",
            //     text, i
            // );
            continue;
        } else {
            // it is
            c = c1;
            // println!(
            //     "brace_expand: text[{}] broken at second gobble with i {} c {:?}",
            //     text, i, c
            // );
            break;
        }
    }
    let result = vec![text_chars.clone().take(i).collect::<String>()];
    // Special case.  If we never found an exciting character, then
    // the preamble is all of the text, so just return that.
    if c != Some('{') {
        // println!(
        //     "text: {} retuns here with c {:?} with preable {} with i {}",
        //     text, c, result[0], i
        // );
        return result;
    }
    // Find the amble.  This is the stuff inside this set of braces.
    i += 1;
    let start = i;
    c = brace_gobbler(&text, &mut i, '}');

    if c.is_none() {
        return vec![text.to_string()];
    }

    let amble = text_chars
        .clone()
        .skip(start)
        .take(i - start)
        .collect::<String>();

    // SHELL SPECIFIC: expand_seqterm
    let mut j;
    let mut amble_chars = amble.chars();
    loop {
        j = amble_chars.next();
        if j.is_none() {
            break;
        }
        if j == Some('\\') {
            amble_chars.next();
            continue;
        }
        if j == Some(BRACE_ARG_SEPARATOR) {
            break;
        }
    }

    // println!("text: {} amble: {}, j {:?}", text, amble, j);

    fn add_tack(postamble: &str, tack: Vec<String>, result: Vec<String>) -> Vec<String> {
        let res = array_concat(&result, &tack).to_owned();
        if postamble.is_empty() {
            return res.into_owned();
        }
        array_concat(&res, &brace_expand(postamble.to_string()))
            .to_owned()
            .into_owned()
    }
    let postamble = text.chars().skip(i + 1).collect::<String>();

    if j.is_some() {
        return add_tack(&postamble, expand_amble(&amble), result);
    }

    let tack = expand_seqterm(&amble);
    if !tack.is_empty() {
        return add_tack(&postamble, tack, result);
    }
    // If the sequence expansion fails (e.g., because the integers
    // overflow), but there is more in the string, try and process
    // the rest of the string, which may contain additional brace
    // expansions.  Treat the unexpanded sequence term as a simple
    // string (including the braces). */
    if text_chars.clone().nth(i + 1).is_some() {
        return add_tack(&postamble, vec![amble], result);
    }
    vec![text.to_string()]
    // END SHELL SPECIFIC
}

// does space, tab, newline, and backslash expansion on a string and returns a vector of strings expanded
pub fn shell_brace_expansion<S: Into<String>>(text: S) -> Vec<String> {
    let text = text.into();
    text.replace("\\\r\n", " ")
        .replace("\\\n", " ")
        .replace("\\\r", " ")
        .split_whitespace()
        .map(|s| brace_expand(s.to_string()))
        .fold(vec![], |mut acc, mut v| {
            acc.append(&mut v);
            acc
        })
}

// tests are based on braces.tests and the output from the main function of braces.c rather than bash itself
// due to different escape rules and subsitutions that aren't easily reproduced without bash
#[cfg(test)]
mod brace_expand_tests {
    #[test]
    fn basic_expansion() {
        // ff{c,b,a}
        assert_eq!(super::brace_expand("ff{c,b,a}"), vec!["ffc", "ffb", "ffa"]);
        // f{d,e,f}g
        assert_eq!(super::brace_expand("f{d,e,f}g"), vec!["fdg", "feg", "ffg"]);
        // {l,n,m}xyz
        assert_eq!(
            super::brace_expand("{l,n,m}xyz"),
            vec!["lxyz", "nxyz", "mxyz"]
        );
        // {abc}
        assert_eq!(super::brace_expand("{abc}"), vec!["{abc}"]);
    }
    #[test]
    fn escaped_expansion() {
        // {x,y,\{a,b,c}} (actually returns "x} y} {a} b} c}" in a shell, but the `\` is interpreted as an escape)
        assert_eq!(
            super::brace_expand("{x,y,\\{a,b,c}}"),
            vec!["x}", "y}", "\\{a}", "b}", "c}"]
        );
        // {x\,y,\{abc\},trie}
        assert_eq!(
            super::brace_expand("{x\\,y,\\{abc\\},trie}"),
            vec!["x\\,y", "\\{abc\\}", "trie"]
        );
    }
    #[test]
    fn file_path_expansion() {
        // /usr/{ucb/{ex,edit},lib/{ex,how_ex}}
        assert_eq!(
            super::brace_expand("/usr/{ucb/{ex,edit},lib/{ex,how_ex}}"),
            vec![
                "/usr/ucb/ex",
                "/usr/ucb/edit",
                "/usr/lib/ex",
                "/usr/lib/how_ex"
            ]
        );
    }
    #[test]
    fn subsitution_expansion() {
        // doesn't actually do any expansions here due to lack of substituion, both from this and from brace_expand
        // XXXX\{`echo a b c | tr ' ' ','`\}
        assert_eq!(
            super::brace_expand("XXXX\\{`echo a b c | tr ' ' ','`\\}"),
            vec!["XXXX\\{`echo a b c | tr ' ' ','`\\}"]
        );
    }

    #[test]
    fn expansion_with_spaces() {
        // foo {1,2} bar (actually prints foo 1 2 bar in shell because of IFS and the shell separating the arguments)
        assert_eq!(
            super::brace_expand("foo {1,2} bar"),
            vec!["foo 1 bar", "foo 2 bar"]
        );
    }

    #[test]
    fn sequence_expansion() {
        // {1..10}
        assert_eq!(
            super::brace_expand("{1..10}"),
            vec!["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
        );
        // {0..10,braces}
        assert_eq!(
            super::brace_expand("{0..10,braces}"),
            vec!["0..10", "braces"]
        );
        // {{0..10},braces}
        assert_eq!(
            super::brace_expand("{{0..10},braces}"),
            vec!["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "braces"]
        );
        // x{{0..10},braces}y
        assert_eq!(
            super::brace_expand("x{{0..10},braces}y"),
            vec![
                "x0y", "x1y", "x2y", "x3y", "x4y", "x5y", "x6y", "x7y", "x8y", "x9y", "x10y",
                "xbracesy"
            ]
        );
        // {3..3}
        assert_eq!(super::brace_expand("{3..3}"), vec!["3"]);
        // x{3..3}y
        assert_eq!(super::brace_expand("x{3..3}y"), vec!["x3y"]);
        // {10..1}
        assert_eq!(
            super::brace_expand("{10..1}"),
            vec!["10", "9", "8", "7", "6", "5", "4", "3", "2", "1"]
        );
        // {10..1}y
        assert_eq!(
            super::brace_expand("{10..1}y"),
            vec!["10y", "9y", "8y", "7y", "6y", "5y", "4y", "3y", "2y", "1y"]
        );
        // x{10..1}y
        assert_eq!(
            super::brace_expand("x{10..1}y"),
            vec!["x10y", "x9y", "x8y", "x7y", "x6y", "x5y", "x4y", "x3y", "x2y", "x1y"]
        );

        // {a..f}
        assert_eq!(
            super::brace_expand("{a..f}"),
            vec!["a", "b", "c", "d", "e", "f"]
        );
        // {f..a}
        assert_eq!(
            super::brace_expand("{f..a}"),
            vec!["f", "e", "d", "c", "b", "a"]
        );
        // {a..A}
        assert_eq!(
            super::brace_expand("{a..A}"),
            vec![
                "a", "`", "_", "^", "]", "\\", "[", "Z", "Y", "X", "W", "V", "U", "T", "S", "R",
                "Q", "P", "O", "N", "M", "L", "K", "J", "I", "H", "G", "F", "E", "D", "C", "B",
                "A"
            ]
        );
        // {A..a}
        assert_eq!(
            super::brace_expand("{A..a}"),
            vec![
                "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
                "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]", "^", "_", "`",
                "a"
            ]
        );
        // {f..f}
        assert_eq!(super::brace_expand("{f..f}"), vec!["f"]);
        // {-1..-10}
        assert_eq!(
            super::brace_expand("{-1..-10}"),
            vec!["-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8", "-9", "-10"]
        );
        // {-20..0}
        assert_eq!(
            super::brace_expand("{-20..0}"),
            vec![
                "-20", "-19", "-18", "-17", "-16", "-15", "-14", "-13", "-12", "-11", "-10", "-9",
                "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0"
            ]
        );
        // {a..z}
        assert_eq!(
            super::brace_expand("{a..z}"),
            vec![
                "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p",
                "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
            ]
        );
        // {2147483645..2147483649}
        assert_eq!(
            super::brace_expand("{2147483645..2147483649}"),
            vec![
                "2147483645",
                "2147483646",
                "2147483647",
                "2147483648",
                "2147483649"
            ]
        );
    }
    #[test]
    fn sequence_expansion_with_increments() {
        // {1..10..2}
        assert_eq!(
            super::brace_expand("{1..10..2}"),
            vec!["1", "3", "5", "7", "9"]
        );
        // {-1..-10..2}
        assert_eq!(
            super::brace_expand("{-1..-10..2}"),
            vec!["-1", "-3", "-5", "-7", "-9"]
        );
        // {-1..-10..-2}
        assert_eq!(
            super::brace_expand("{-1..-10..-2}"),
            vec!["-1", "-3", "-5", "-7", "-9"]
        );
        // {10..1..-2}
        assert_eq!(
            super::brace_expand("{10..1..-2}"),
            vec!["10", "8", "6", "4", "2"]
        );
        // {10..1..2}
        assert_eq!(
            super::brace_expand("{10..1..2}"),
            vec!["10", "8", "6", "4", "2"]
        );
        // {1..20..2}
        assert_eq!(
            super::brace_expand("{1..20..2}"),
            vec!["1", "3", "5", "7", "9", "11", "13", "15", "17", "19"]
        );
        // {1..20..20}
        assert_eq!(super::brace_expand("{1..20..20}"), vec!["1"]);
        // {100..0..5}
        assert_eq!(
            super::brace_expand("{100..0..5}"),
            vec![
                "100", "95", "90", "85", "80", "75", "70", "65", "60", "55", "50", "45", "40",
                "35", "30", "25", "20", "15", "10", "5", "0"
            ]
        );
        // {100..0..-5}
        assert_eq!(
            super::brace_expand("{100..0..-5}"),
            vec![
                "100", "95", "90", "85", "80", "75", "70", "65", "60", "55", "50", "45", "40",
                "35", "30", "25", "20", "15", "10", "5", "0"
            ]
        );
        // {a..z..2}
        assert_eq!(
            super::brace_expand("{a..z..2}"),
            vec!["a", "c", "e", "g", "i", "k", "m", "o", "q", "s", "u", "w", "y"]
        );
        // {z..a..-2}
        assert_eq!(
            super::brace_expand("{z..a..-2}"),
            vec!["z", "x", "v", "t", "r", "p", "n", "l", "j", "h", "f", "d", "b"]
        );
    }
    #[test]
    fn zero_padded_expansion() {
        // {10..0..2}
        assert_eq!(
            super::brace_expand("{10..0..2}"),
            vec!["10", "8", "6", "4", "2", "0"]
        );
        // {10..0..-2}
        assert_eq!(
            super::brace_expand("{10..0..-2}"),
            vec!["10", "8", "6", "4", "2", "0"]
        );
        // {-50..-0..5}
        assert_eq!(
            super::brace_expand("{-50..-0..5}"),
            vec!["-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5", "0"]
        );
    }

    #[test]
    fn weird_expansions() {
        // a-{b{d,e}}-c
        assert_eq!(
            super::brace_expand("a-{b{d,e}}-c"),
            vec!["a-{bd}-c", "a-{be}-c"]
        );
        // a-{bdef-{g,i}-c
        assert_eq!(
            super::brace_expand("a-{bdef-{g,i}-c"),
            vec!["a-{bdef-g-c", "a-{bdef-i-c"]
        );
        // {"klklkl"}{1,2,3}
        assert_eq!(
            super::brace_expand("{\"klklkl\"}{1,2,3}"),
            vec!["{\"klklkl\"}1", "{\"klklkl\"}2", "{\"klklkl\"}3"]
        );
    }

    #[test]
    fn invalid_expansions() {
        // {1..10.f}
        assert_eq!(super::brace_expand("{1..10.f}"), vec!["{1..10.f}"]);
        // {1..ff}
        assert_eq!(super::brace_expand("{1..ff}"), vec!["{1..ff}"]);
        // {1..10..ff}
        assert_eq!(super::brace_expand("{1..10..ff}"), vec!["{1..10..ff}"]);
        // {1.20..2}
        assert_eq!(super::brace_expand("{1.20..2}"), vec!["{1.20..2}"]);
        // {1..20..f2}
        assert_eq!(super::brace_expand("{1..20..f2}"), vec!["{1..20..f2}"]);
        // {1..20..2f}
        assert_eq!(super::brace_expand("{1..20..2f}"), vec!["{1..20..2f}"]);
        // {1..2f..2}
        assert_eq!(super::brace_expand("{1..2f..2}"), vec!["{1..2f..2}"]);
        // {1..ff..2}
        assert_eq!(super::brace_expand("{1..ff..2}"), vec!["{1..ff..2}"]);
        // {1..ff}
        assert_eq!(super::brace_expand("{1..ff}"), vec!["{1..ff}"]);
        // {1..f}
        assert_eq!(super::brace_expand("{1..f}"), vec!["{1..f}"]);
        // {1..0f}
        assert_eq!(super::brace_expand("{1..0f}"), vec!["{1..0f}"]);
        // {1..10f}
        assert_eq!(super::brace_expand("{1..10f}"), vec!["{1..10f}"]);
        // {1..10.f}
        assert_eq!(super::brace_expand("{1..10.f}"), vec!["{1..10.f}"]);
        // {1..10.f}
        assert_eq!(super::brace_expand("{1..10.f}"), vec!["{1..10.f}"]);
        // {}
        assert_eq!(super::brace_expand("{}"), vec!["{}"]);
        // { }
        assert_eq!(super::brace_expand("{ }"), vec!["{ }"]);
        // }
        assert_eq!(super::brace_expand("}"), vec!["}"]);
        // {
        assert_eq!(super::brace_expand("{"), vec!["{"]);
        // abcd{efgh
        assert_eq!(super::brace_expand("abcd{efgh"), vec!["abcd{efgh"]);
        // {"x,x"}
        assert_eq!(super::brace_expand("{\"x,x\"}"), vec!["{\"x,x\"}"]);
        // {f..1}
        assert_eq!(super::brace_expand("{f..1}"), vec!["{f..1}"]);
        // {abc\,def} (actually returns {abc,def} in a shell, but that is because the `,` is escaped)
        assert_eq!(super::brace_expand("{abc\\,def}"), vec!["{abc\\,def}"]);
        // \{a,b,c,d,e}
        assert_eq!(super::brace_expand("\\{a,b,c,d,e}"), vec!["\\{a,b,c,d,e}"]);
    }
}

#[cfg(test)]
mod mksq_tests {
    #[test]
    fn single_int() {
        assert_eq!(super::mkseq(3, 3, 1, super::StTypes::Int, 0), vec!["3"]);
    }

    #[test]
    fn padded_single_int() {
        assert_eq!(super::mkseq(3, 3, 1, super::StTypes::ZInt, 2), vec!["03"]);
    }

    #[test]
    fn incremented_int() {
        assert_eq!(
            super::mkseq(1, 5, 3, super::StTypes::Int, 0),
            vec!["1", "4"]
        );
    }

    #[test]
    fn padded_incremented_int() {
        assert_eq!(
            super::mkseq(1, 5, 3, super::StTypes::ZInt, 2),
            vec!["01", "04"]
        );
    }

    #[test]
    fn incremented_int_with_negative_increment() {
        assert_eq!(
            super::mkseq(1, 5, -3, super::StTypes::Int, 0),
            vec!["1", "4"]
        );
    }

    #[test]
    fn alphabet() {
        assert_eq!(
            super::mkseq('A' as isize, 'D' as isize, 1, super::StTypes::Char, 0),
            vec!["A", "B", "C", "D"]
        );
    }

    #[test]
    fn large_int() {
        assert_eq!(
            super::mkseq(2147483645, 2147483649, 1, super::StTypes::Int, 0),
            vec![
                "2147483645",
                "2147483646",
                "2147483647",
                "2147483648",
                "2147483649"
            ]
        );
    }

    #[test]
    fn descending_incremented_int() {
        assert_eq!(
            super::mkseq(10, 0, 2, super::StTypes::Int, 0),
            vec!["10", "8", "6", "4", "2", "0"]
        );
    }
}

#[cfg(test)]
mod expand_seqterm_tests {
    #[test]
    fn normal_expansion() {
        assert_eq!(
            super::expand_seqterm("1..10"),
            vec!["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
        );
        assert_eq!(
            super::expand_seqterm("10..1"),
            vec!["10", "9", "8", "7", "6", "5", "4", "3", "2", "1"]
        );
    }

    #[test]
    fn empty_expansion() {
        assert_eq!(super::expand_seqterm(""), vec![] as Vec<String>);
    }

    #[test]
    fn normal_with_incr_expansion() {
        assert_eq!(
            super::expand_seqterm("1..10..2"),
            vec!["1", "3", "5", "7", "9"]
        );
        assert_eq!(
            super::expand_seqterm("-1..-10..2"),
            vec!["-1", "-3", "-5", "-7", "-9"]
        );
        assert_eq!(
            super::expand_seqterm("-1..-10..-2"),
            vec!["-1", "-3", "-5", "-7", "-9"]
        );
        assert_eq!(
            super::expand_seqterm("10..1..-2"),
            vec!["10", "8", "6", "4", "2"]
        );
        assert_eq!(
            super::expand_seqterm("10..1..2"),
            vec!["10", "8", "6", "4", "2"]
        );
        assert_eq!(
            super::expand_seqterm("1..20..2"),
            vec!["1", "3", "5", "7", "9", "11", "13", "15", "17", "19"]
        );
        assert_eq!(super::expand_seqterm("1..20..20"), vec!["1"]);
        assert_eq!(
            super::expand_seqterm("a..z..2"),
            vec!["a", "c", "e", "g", "i", "k", "m", "o", "q", "s", "u", "w", "y"]
        );
        assert_eq!(
            super::expand_seqterm("-50..-0..5"),
            vec!["-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5", "0"]
        );
    }

    #[test]
    fn invalid_expansion() {
        assert_eq!(super::expand_seqterm("0..10,braces"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("0..10,10"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("f..1"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..10.f"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..ff"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..10..ff"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1.20..2"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..20..f2"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..20..2f"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..2f..2"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..ff..2"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..ff"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..f"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..0f"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..10f"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..10.f"), vec![] as Vec<String>);
        assert_eq!(super::expand_seqterm("1..10.f"), vec![] as Vec<String>);
    }

    #[test]
    fn singular_expansion() {
        assert_eq!(super::expand_seqterm("3..3"), vec!["3"]);
    }

    #[test]
    fn alphabetic_expansion() {
        assert_eq!(
            super::expand_seqterm("a..f"),
            vec!["a", "b", "c", "d", "e", "f"]
        );
        assert_eq!(
            super::expand_seqterm("f..a"),
            vec!["f", "e", "d", "c", "b", "a"]
        );
        assert_eq!(
            super::expand_seqterm("a..A"),
            vec![
                "a", "`", "_", "^", "]", "\\", "[", "Z", "Y", "X", "W", "V", "U", "T", "S", "R",
                "Q", "P", "O", "N", "M", "L", "K", "J", "I", "H", "G", "F", "E", "D", "C", "B",
                "A"
            ]
        );
    }

    #[test]
    fn negative_numbers() {
        assert_eq!(
            super::expand_seqterm("-1..-10"),
            vec!["-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8", "-9", "-10"]
        );
        assert_eq!(
            super::expand_seqterm("-20..0"),
            vec![
                "-20", "-19", "-18", "-17", "-16", "-15", "-14", "-13", "-12", "-11", "-10", "-9",
                "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0"
            ]
        );
    }
}

#[cfg(test)]
mod general_tests {
    #[test]
    fn brace_gobbler_basic() {
        let input = "{a,b}";
        let mut i = 0;
        assert_eq!(super::brace_gobbler(&input, &mut i, '{'), Some('{'));
        assert_eq!(i, 0);

        assert_eq!(super::brace_gobbler(&input, &mut i, '}'), None);
        assert_eq!(i, 5);
    }
}
