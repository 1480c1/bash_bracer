/* braces.c -- code for doing word expansion in curly braces. */

/* Copyright (C) 1987-2020 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Bash is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Bash.  If not, see <http://www.gnu.org/licenses/>.
*/

/* Stuff in curly braces gets expanded before all other shell expansions. */
#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <iomanip>
#include <algorithm>

#include "config.h"

#if defined(BRACE_EXPANSION)

#if defined(HAVE_UNISTD_H)
#ifdef _MINIX
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include <errno.h>

#include "bashansi.h"
#include "bashintl.h"

#if defined(SHELL)
#include "shell.h"
#else
#if defined(TEST)
typedef char  *WORD_DESC;
typedef char **WORD_LIST;
#endif /* TEST */
#endif /* SHELL */

#include "typemax.h" /* INTMAX_MIN, INTMAX_MAX */
#include "general.h"
#include "shmbutil.h"
#include "chartypes.h"

#ifndef errno
extern int errno;
#endif

#define brace_whitespace(c) (!(c) || (c) == ' ' || (c) == '\t' || (c) == '\n')

const std::string BRACE_SEQ_SPECIFIER = "..";

/* Integer to string conversion.  This conses the string; the
   caller should free it. */
static std::string itos(intmax_t i) {
    std::stringstream ss;
    ss << i;
    return ss.str();
}

#ifndef whitespace
#define whitespace(c) (((c) == ' ') || ((c) == '\t'))
#endif

bool legal_number(const std::string &string, intmax_t *result) {
    if (result)
        *result = 0;

    errno = 0;
    char          *ep;
    const intmax_t value = strtoimax(string.c_str(), &ep, 10);
    if (errno || ep == string.c_str())
        return false; /* errno is set on overflow or underflow */

    /* Skip any trailing whitespace, since strtoimax does not. */
    while (whitespace(*ep)) ep++;

    /* If *string is not '\0' but *ep is '\0' on return, the entire string
     is valid. */
    if (*ep == '\0') {
        if (result)
            *result = value;
        /* The SunOS4 implementation of strtol() will happily ignore
	 overflow conditions, so this cannot do overflow correctly
	 on those systems. */
        return true;
    }

    return false;
}

/* Basic idea:

   Segregate the text into 3 sections: preamble (stuff before an open brace),
   postamble (stuff after the matching close brace) and amble (stuff after
   preamble, and before postamble).  Expand amble, and then tack on the
   expansions to preamble.  Expand postamble, and tack on the expansions to
   the result so far.
 */

/* The character which is used to separate arguments. */
static const char brace_arg_separator = ',';

std::vector<std::string> brace_expand(const std::string &text);

/* Start at INDEX, and skip characters in TEXT. Set INDEX to the
   index of the character matching SATISFY.  This understands about
   quoting.  Return the character that caused us to stop searching;
   this is either the same as SATISFY, or 0. */
/* If SATISFY is `}', we are looking for a brace expression, so we
   should enforce the rules that govern valid brace expansions:
	1) to count as an arg separator, a comma or `..' has to be outside
	   an inner set of braces.	 
*/
static char brace_gobbler(const std::string &text, size_t *indx, char satisfy) {
    char   c;
    char   quoted    = '\0';
    int    level     = 0;
    bool   pass_next = false;
    bool   commas    = (satisfy != '}');
    size_t i         = *indx;
    while (true) {
        c = text[i];
        if (c == '\0') {
            break;
        }
        ++i;
        if (pass_next) {
            pass_next = false;
            continue;
        }

        /* A backslash escapes the next character.  This allows backslash to
	 escape the quote character in a double-quoted string. */
        switch (quoted) {
        case '\0':
        case '"':
        case '`':
            if (c == '\\') {
                pass_next = true;

                continue;
            }
        }

        /* If compiling for the shell, treat ${...} like \{...} */
        if (c == '$' && text[i + 1] == '{' && quoted != '\'') /* } */
        {
            pass_next = true;

            if (quoted == '\0')
                level++;
            continue;
        }

        if (quoted) {
            if (c == quoted)
                quoted = '\0';
#if defined(SHELL)
            /* The shell allows quoted command substitutions */
            if (quoted == '"' && c == '$' && text[i + 1] == '(') /*)*/
                goto comsub;
#endif

            continue;
        }

        switch (c) {
        case '"':
        case '\'':
        case '`': quoted = c; continue;
        }

#if defined(SHELL)
        /* Pass new-style command and process substitutions through unchanged. */
        if ((c == '$' || c == '<' || c == '>') && text[i + 1] == '(') /* ) */
        {
        comsub:
            int si = i + 2;
            free(extract_command_subst(text, &si, 0));
            i = si;

            continue;
        }
#endif

        if (c == satisfy && level == 0 && commas) {
            /* We ignore an open brace surrounded by whitespace, and also
	     an open brace followed immediately by a close brace preceded
	     by whitespace.  */
            if (c == '{' &&
                ((i == 0 || isspace(text[i - 1])) &&
                 (isspace(text[i + 1]) || text[i + 1] == '}'))) {
                continue;
            }
            i -= 1;
            break;
        }

        if (c == '{')
            level++;
        else if (c == '}' && level)
            level--;
        else if (satisfy == '}' && c == brace_arg_separator && level == 0)
            commas = true;
        else if (satisfy == '}' && text.substr(i, 2) == BRACE_SEQ_SPECIFIER &&
                 text[i + 2] != satisfy && level == 0)
            commas = true;
    }

    *indx = i;
    return (c);
}

/* Expand the text found inside of braces.  We simply try to split the
   text at BRACE_ARG_SEPARATORs into separate strings.  We then brace
   expand each slot which needs it, until there are no more slots which
   need it. */
static std::vector<std::string> expand_amble(const std::string &text) {
    std::vector<std::string> result;
    size_t                   i = 0, start = 0;

    for (;;) {
        int c = brace_gobbler(text, &i, brace_arg_separator);

        const std::vector<std::string> partial = brace_expand(text.substr(start, i - start));

        std::copy(partial.begin(), partial.end(), std::back_inserter(result));
        start = ++i;
        if (c == '\0')
            break;
    }
    return (result);
}

// invalid number
#define ST_BAD 0
// integer
#define ST_INT 1
// character
#define ST_CHAR 2
// zero-padded integer
#define ST_ZINT 3

/* Handle signed arithmetic overflow and underflow.  Have to do it this way
   to avoid compilers optimizing out simpler overflow checks. */

/* Make sure that a+b does not exceed MAXV or is smaller than MINV (if b < 0).
   Assumes that b > 0 if a > 0 and b < 0 if a < 0 */
#define ADDOVERFLOW(a, b, minv, maxv) \
    ((((a) > 0) && ((b) > ((maxv) - (a)))) || (((a) < 0) && ((b) < ((minv) - (a)))))

/* Make sure that a-b is not smaller than MINV or exceeds MAXV (if b < 0).
   Assumes that b > 0 if a > 0 and b < 0 if a < 0 */
#define SUBOVERFLOW(a, b, minv, maxv) \
    ((((b) > 0) && ((a) < ((minv) + (b)))) || (((b) < 0) && ((a) > ((maxv) + (b)))))

#ifndef sh_imaxabs
#define sh_imaxabs(x) (((x) >= 0) ? (x) : -(x))
#endif

static std::vector<std::string> mkseq(intmax_t start, intmax_t end, intmax_t incr, int type,
                                      size_t width) {
    if (incr == 0)
        incr = 1;

    if (start > end && incr > 0)
        incr = -incr;
    else if (start < end && incr < 0) {
        if (incr == INTMAX_MIN) /* Don't use -INTMAX_MIN */
            return {};
        incr = -incr;
    }

    /* Check that end-start will not overflow INTMAX_MIN, INTMAX_MAX.  The +3
     and -2, not strictly necessary, are there because of the way the number
     of elements and value passed to strvec_create() are calculated below. */
    if (SUBOVERFLOW(end, start, INTMAX_MIN + 3, INTMAX_MAX - 2))
        return {};

    const intmax_t prevn = sh_imaxabs(end - start);
    /* Need to check this way in case INT_MAX == INTMAX_MAX */
    if (INT_MAX == INTMAX_MAX && (ADDOVERFLOW(prevn, 2, INT_MIN, INT_MAX)))
        return {};
    /* Make sure the assignment to nelem below doesn't end up <= 0 due to
     intmax_t overflow */
    else if (ADDOVERFLOW((prevn / sh_imaxabs(incr)), 1, INTMAX_MIN, INTMAX_MAX))
        return {};

    /* XXX - TOFIX: potentially allocating a lot of extra memory if
     imaxabs(incr) != 1 */
    /* Instead of a simple nelem = prevn + 1, something like:
  	nelem = (prevn / imaxabs(incr)) + 1;
     would work */
    if ((prevn / sh_imaxabs(incr)) > INT_MAX - 3) /* check int overflow */
        return {};
    std::vector<std::string> result((prevn / sh_imaxabs(incr)) + 1 + 1);

    /* Make sure we go through the loop at least once, so {3..3} prints `3' */
    intmax_t n = start;
    for (size_t i = 0;; ++i) {
        switch (type) {
        case ST_INT: result[i] = itos(n); break;
        case ST_ZINT:
            result[i] = (std::stringstream() << std::setfill('0') << std::setw(width) << n).str();
            break;
        default: result[i] = std::string(1, (char)n); break;
        }

        /* Handle overflow and underflow of n+incr */
        if (ADDOVERFLOW(n, incr, INTMAX_MIN, INTMAX_MAX))
            break;

        n += incr;

        if ((incr < 0 && n < end) || (incr > 0 && n > end))
            break;
    }

    return (result);
}

static bool is_whole_string_digits(const std::string &str) {
    if (str.empty())
        return false;
    if (!isdigit(str[0]) && str[0] != '-' && str[0] != '+')
        return false;
    for (auto &&c : str.substr(1)) {
        if (!isdigit(c))
            return false;
    }
    return true;
}

static std::vector<std::string> expand_seqterm(std::string &text, size_t tlen) {
    const size_t lhs_l = text.find(BRACE_SEQ_SPECIFIER); /* index of start of BRACE_SEQ_SPECIFIER */
    if (lhs_l == std::string::npos)
        return {};

    const std::string lhs = text.substr(0, lhs_l);
    std::string       rhs = text.substr(lhs_l + BRACE_SEQ_SPECIFIER.size());

    /* index of end of BRACE_SEQ_SPECIFIER */
    const size_t rhs_l = rhs.find(BRACE_SEQ_SPECIFIER);

    std::string inc = "";
    if (rhs_l != std::string::npos) {
        inc = rhs.substr(rhs_l + BRACE_SEQ_SPECIFIER.size());
        rhs.erase(rhs_l);
    }

    /* Now figure out whether LHS and RHS are integers or letters.  Both
     sides have to match. */
    int lhs_type = ST_BAD, rhs_type = ST_BAD;

    intmax_t tl, tr;
    int      lhs_t = legal_number(lhs, &tl) && is_whole_string_digits(lhs) ? ST_INT
             : isalpha(lhs[0]) && lhs.size() == 1                          ? ST_CHAR
                                                                           : ST_BAD;
    int      rhs_t = legal_number(rhs, &tr) && is_whole_string_digits(rhs) ? ST_INT
             : isalpha(rhs[0]) && rhs.size() == 1                          ? ST_CHAR
                                                                           : ST_BAD;
    if (lhs_t == ST_BAD || rhs_t == ST_BAD || lhs_t != rhs_t)
        return {};

    intmax_t incr = 1;

    /* Decide if the incr is valid or not, only integers are allowed here */
    if (inc.size() > 0) {
        if (!legal_number(inc, &incr))
            return {};
    }

    /* OK, we have something.  It's either a sequence of integers, ascending
     or descending, or a sequence or letters, ditto.  Generate the sequence,
     put it into a string vector, and return it. */
    if (lhs_t == ST_CHAR)
        return mkseq((unsigned char)lhs[0], (unsigned char)rhs[0], incr, lhs_t, 1);

    /* Decide whether or not the terms need zero-padding */
    size_t width = 0;
    if ((lhs.size() > 1 && lhs[0] == '0') || (lhs.size() > 2 && lhs[0] == '-' && lhs[1] == '0')) {
        width = lhs.size();
        lhs_t = ST_ZINT;
    }

    if ((width < rhs.size() && rhs.size() > 1 && rhs[0] == '0') ||
        (width < rhs.size() && rhs.size() > 2 && rhs[0] == '-' && rhs[1] == '0')) {
        width = rhs.size();
        lhs_t = ST_ZINT;
    }

    if (lhs_t == ST_ZINT)
        width = std::max(width, std::max(lhs.size(), rhs.size()));

    return mkseq(tl, tr, incr, lhs_t, width);
}

/* Return a new array of strings which is the result of appending each
   string in ARR2 to each string in ARR1.  The resultant array is
   len (arr1) * len (arr2) long.  For convenience, ARR1 (and its contents)
   are free ()'ed.  ARR1 can be NULL, in that case, a new version of ARR2
   is returned. */
static std::vector<std::string> array_concat(const std::vector<std::string> &arr1,
                                             const std::vector<std::string> &arr2) {
    if (arr1.empty())
        return (arr2);

    if (arr2.empty())
        return (arr1);

    std::vector<std::string> result;

    for (auto &&s : arr1) {
        std::transform(
            arr2.begin(), arr2.end(), std::back_inserter(result), [&s](const std::string &rhs) {
                return (s + rhs);
            });
    }

    return (result);
}

/* Return an array of strings; the brace expansion of TEXT. */
std::vector<std::string> brace_expand(const std::string &text) {
    int c;

    DECLARE_MBSTATE;

    /* Find the text of the preamble. */
    size_t i = 0;
    /* Make sure that when we exit this loop, c == 0 or text[i] begins a
     valid brace expansion sequence. */
    do {
        c      = brace_gobbler(text, &i, '{'); /* } */
        int c1 = c;
        /* Verify that c begins a valid brace expansion word.  If it doesn't, we
	 go on.  Loop stops when there are no more open braces in the word. */
        if (c == '\0')
            break;

        size_t j = i + 1; /* { */
        c        = brace_gobbler(text, &j, '}');
        /* it's not */
        if (c == '\0') {
            ++i;
            c = c1;
            continue;
        }
        /* it is */
        c = c1;
        break;
    } while (c);

    std::string preamble;
    if (i > 0)
        preamble = text.substr(0, i);

    std::vector<std::string> result;
    result.push_back(preamble);

    /* Special case.  If we never found an exciting character, then
     the preamble is all of the text, so just return that. */
    if (c != '{')
        return (result);

    /* Find the amble.  This is the stuff inside this set of braces. */
    size_t start = ++i;
    c            = brace_gobbler(text, &i, '}');

    /* What if there isn't a matching close brace? */
    if (c == '\0') {
        result[0] = text;
        return (result);
    }

    std::string amble = text.substr(start, i - start);

    std::vector<std::string> tack;

    /* If the amble does not contain an unquoted BRACE_ARG_SEPARATOR, then
     just return without doing any expansion.  */
    size_t j = 0;
    while (amble[j]) {
        if (amble[j] == '\\') {
            j += 2;
            continue;
        }

        if (amble[j] == brace_arg_separator)
            break;

        ++j;
    }

    if (amble[j] == '\0') {
        tack = expand_seqterm(amble, amble.length());
        if (tack.size())
            goto add_tack;
        if (text[i + 1]) {
            /* If the sequence expansion fails (e.g., because the integers
	     overflow), but there is more in the string, try and process
	     the rest of the string, which may contain additional brace
	     expansions.  Treat the unexpanded sequence term as a simple
	     string (including the braces). */
            tack = std::vector<std::string>(1, text.substr(start - 1));
            goto add_tack;
        }
        result[0] = text;
        return (result);
    }

    tack = expand_amble(amble);
add_tack:
    result = array_concat(result, tack);

    const std::string postamble = text.substr(i + 1);

    if (postamble.size()) {
        result = array_concat(result, brace_expand(postamble));
    }

    return (result);
}

#if defined(TEST)
#include <stdio.h>

int main(void) {
    std::string example;

    for (;;) {
        std::cerr << "brace_expand> ";
        std::getline(std::cin, example);
        if (example.empty() || example == "quit")
            break;

        std::vector<std::string> result = brace_expand(example);

        for (size_t i = 0; i < result.size(); i++) { std::cout << result[i] << "\n"; }
    }
}

/*
 * Local variables:
 * compile-command: "gcc -g -Bstatic -DTEST -o brace_expand braces.c general.o"
 * end:
 */

#endif /* TEST */
#endif /* BRACE_EXPANSION */
