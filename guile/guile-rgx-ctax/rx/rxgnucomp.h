/* classes: h_files */

#ifndef RXGNUCOMPH
#define RXGNUCOMPH
/*	Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330, 
 * Boston, MA 02111-1307, USA. 
 */



#include "rxcset.h"
#include "rxnode.h"




/* This is an array of error messages corresponding to the error codes.
 */
extern const char *rx_error_msg[];



/* If any error codes are removed, changed, or added, update the
 * `rx_error_msg' table.
 */
typedef enum
{
  REG_NOERROR = 0,	/* Success.  */
  REG_NOMATCH,		/* Didn't find a match (for regexec).  */

  /* POSIX regcomp return error codes.  (In the order listed in the
     standard.)  */
  REG_BADPAT,		/* Invalid pattern.  */
  REG_ECOLLATE,		/* Not implemented.  */
  REG_ECTYPE,		/* Invalid character class name.  */
  REG_EESCAPE,		/* Trailing backslash.  */
  REG_ESUBREG,		/* Invalid back reference.  */
  REG_EBRACK,		/* Unmatched left bracket.  */
  REG_EPAREN,		/* Parenthesis imbalance.  */ 
  REG_EBRACE,		/* Unmatched \{.  */
  REG_BADBR,		/* Invalid contents of \{\}.  */
  REG_ERANGE,		/* Invalid range end.  */
  REG_ESPACE,		/* Ran out of memory.  */
  REG_BADRPT,		/* No preceding re for repetition op.  */

  /* Error codes we've added.  */
  REG_EEND,		/* Premature end.  */
  REG_ESIZE,		/* Compiled pattern bigger than 2^16 bytes.  */
  REG_ERPAREN		/* Unmatched ) or \); not returned from regcomp.  */
} reg_errcode_t;


/* {Syntax Bits}
 */

/* The following bits are used to determine the regexp syntax we
   recognize.  The set/not-set meanings are chosen so that Emacs syntax
   remains the value 0.  The bits are given in alphabetical order, and
   the definitions shifted by one from the previous bit; thus, when we
   add or remove a bit, only one other definition need change.  */

/* If this bit is not set, then \ inside a bracket expression is literal.
   If set, then such a \ quotes the following character.  */
#define RE_BACKSLASH_ESCAPE_IN_LISTS (1)

/* If this bit is not set, then + and ? are operators, and \+ and \? are
     literals. 
   If set, then \+ and \? are operators and + and ? are literals.  */
#define RE_BK_PLUS_QM (RE_BACKSLASH_ESCAPE_IN_LISTS << 1)

/* If this bit is set, then character classes are supported.  They are:
     [:alpha:], [:upper:], [:lower:],  [:digit:], [:alnum:], [:xdigit:],
     [:space:], [:print:], [:punct:], [:graph:], and [:cntrl:].
   If not set, then character classes are not supported.  */
#define RE_CHAR_CLASSES (RE_BK_PLUS_QM << 1)

/* If this bit is set, then ^ and $ are always anchors (outside bracket
     expressions, of course).
   If this bit is not set, then it depends:
        ^  is an anchor if it is at the beginning of a regular
           expression or after an open-group or an alternation operator;
        $  is an anchor if it is at the end of a regular expression, or
           before a close-group or an alternation operator.  

   This bit could be (re)combined with RE_CONTEXT_INDEP_OPS, because
   POSIX draft 11.2 says that * etc. in leading positions is undefined.
   We already implemented a previous draft which made those constructs
   invalid, though, so we haven't changed the code back.  */
#define RE_CONTEXT_INDEP_ANCHORS (RE_CHAR_CLASSES << 1)

/* If this bit is set, then special characters are always special
     regardless of where they are in the pattern.
   If this bit is not set, then special characters are special only in
     some contexts; otherwise they are ordinary.  Specifically, 
     * + ? and intervals are only special when not after the beginning,
     open-group, or alternation operator.  */
#define RE_CONTEXT_INDEP_OPS (RE_CONTEXT_INDEP_ANCHORS << 1)

/* If this bit is set, then *, +, ?, and { cannot be first in an re or
     immediately after an alternation or begin-group operator.  */
#define RE_CONTEXT_INVALID_OPS (RE_CONTEXT_INDEP_OPS << 1)

/* If this bit is set, then . matches newline.
   If not set, then it doesn't.  */
#define RE_DOT_NEWLINE (RE_CONTEXT_INVALID_OPS << 1)

/* If this bit is set, then . doesn't match NUL.
   If not set, then it does.  */
#define RE_DOT_NOT_NULL (RE_DOT_NEWLINE << 1)

/* If this bit is set, nonmatching lists [^...] do not match newline.
   If not set, they do.  */
#define RE_HAT_LISTS_NOT_NEWLINE (RE_DOT_NOT_NULL << 1)

/* If this bit is set, either \{...\} or {...} defines an
     interval, depending on RE_NO_BK_BRACES. 
   If not set, \{, \}, {, and } are literals.  */
#define RE_INTERVALS (RE_HAT_LISTS_NOT_NEWLINE << 1)

/* If this bit is set, +, ? and | aren't recognized as operators.
   If not set, they are.  */
#define RE_LIMITED_OPS (RE_INTERVALS << 1)

/* If this bit is set, newline is an alternation operator.
   If not set, newline is literal.  */
#define RE_NEWLINE_ALT (RE_LIMITED_OPS << 1)

/* If this bit is set, then `{...}' defines an interval, and \{ and \}
     are literals.
  If not set, then `\{...\}' defines an interval.  */
#define RE_NO_BK_BRACES (RE_NEWLINE_ALT << 1)

/* If this bit is set, (...) defines a group, and \( and \) are literals.
   If not set, \(...\) defines a group, and ( and ) are literals.  */
#define RE_NO_BK_PARENS (RE_NO_BK_BRACES << 1)

/* If this bit is set, then \<digit> matches <digit>.
   If not set, then \<digit> is a back-reference.  */
#define RE_NO_BK_REFS (RE_NO_BK_PARENS << 1)

/* If this bit is set, then | is an alternation operator, and \| is literal. 
   If not set, then \| is an alternation operator, and | is literal.  */
#define RE_NO_BK_VBAR (RE_NO_BK_REFS << 1)

/* If this bit is set, then an ending range point collating higher
     than the starting range point, as in [z-a], is invalid.
   If not set, then when ending range point collates higher than the
     starting range point, the range is ignored.  */
#define RE_NO_EMPTY_RANGES (RE_NO_BK_VBAR << 1)

/* If this bit is set, then an unmatched ) is ordinary.
   If not set, then an unmatched ) is invalid.  */
#define RE_UNMATCHED_RIGHT_PAREN_ORD (RE_NO_EMPTY_RANGES << 1)

/* Define combinations of the above bits for the standard possibilities.
   (The [[[ comments delimit what gets put into the Texinfo file, so
   don't delete them!)  */ 
/* [[[begin syntaxes]]] */
#define RE_SYNTAX_EMACS 0

#define RE_SYNTAX_AWK							\
  (RE_BACKSLASH_ESCAPE_IN_LISTS | RE_DOT_NOT_NULL			\
   | RE_NO_BK_PARENS            | RE_NO_BK_REFS				\
   | RE_NO_BK_VBAR               | RE_NO_EMPTY_RANGES			\
   | RE_UNMATCHED_RIGHT_PAREN_ORD)

#define RE_SYNTAX_POSIX_AWK 						\
  (RE_SYNTAX_POSIX_EXTENDED | RE_BACKSLASH_ESCAPE_IN_LISTS)

#define RE_SYNTAX_GREP							\
  (RE_BK_PLUS_QM              | RE_CHAR_CLASSES				\
   | RE_HAT_LISTS_NOT_NEWLINE | RE_INTERVALS				\
   | RE_NEWLINE_ALT)

#define RE_SYNTAX_EGREP							\
  (RE_CHAR_CLASSES        | RE_CONTEXT_INDEP_ANCHORS			\
   | RE_CONTEXT_INDEP_OPS | RE_HAT_LISTS_NOT_NEWLINE			\
   | RE_NEWLINE_ALT       | RE_NO_BK_PARENS				\
   | RE_NO_BK_VBAR)

#define RE_SYNTAX_POSIX_EGREP						\
  (RE_SYNTAX_EGREP | RE_INTERVALS | RE_NO_BK_BRACES)

#define RE_SYNTAX_SED RE_SYNTAX_POSIX_BASIC

/* Syntax bits common to both basic and extended POSIX regex syntax.  */
#define _RE_SYNTAX_POSIX_COMMON						\
  (RE_CHAR_CLASSES | RE_DOT_NEWLINE      | RE_DOT_NOT_NULL		\
   | RE_INTERVALS  | RE_NO_EMPTY_RANGES)

#define RE_SYNTAX_POSIX_BASIC						\
  (_RE_SYNTAX_POSIX_COMMON | RE_BK_PLUS_QM)

/* Differs from ..._POSIX_BASIC only in that RE_BK_PLUS_QM becomes
   RE_LIMITED_OPS, i.e., \? \+ \| are not recognized.  Actually, this
   isn't minimal, since other operators, such as \`, aren't disabled.  */
#define RE_SYNTAX_POSIX_MINIMAL_BASIC					\
  (_RE_SYNTAX_POSIX_COMMON | RE_LIMITED_OPS)

#define RE_SYNTAX_POSIX_EXTENDED					\
  (_RE_SYNTAX_POSIX_COMMON | RE_CONTEXT_INDEP_ANCHORS			\
   | RE_CONTEXT_INDEP_OPS  | RE_NO_BK_BRACES				\
   | RE_NO_BK_PARENS       | RE_NO_BK_VBAR				\
   | RE_UNMATCHED_RIGHT_PAREN_ORD)

/* Differs from ..._POSIX_EXTENDED in that RE_CONTEXT_INVALID_OPS
   replaces RE_CONTEXT_INDEP_OPS and RE_NO_BK_REFS is added.  */
#define RE_SYNTAX_POSIX_MINIMAL_EXTENDED				\
  (_RE_SYNTAX_POSIX_COMMON  | RE_CONTEXT_INDEP_ANCHORS			\
   | RE_CONTEXT_INVALID_OPS | RE_NO_BK_BRACES				\
   | RE_NO_BK_PARENS        | RE_NO_BK_REFS				\
   | RE_NO_BK_VBAR	    | RE_UNMATCHED_RIGHT_PAREN_ORD)
/* [[[end syntaxes]]] */



/* Maximum number of duplicates an interval can allow.  Some systems
   (erroneously) define this in other header files, but we want our
   value, so remove any previous define.  */
#undef RE_DUP_MAX
#define RE_DUP_MAX ((1 << 15) - 1) 



#ifdef __STDC__
extern reg_errcode_t rx_parse (struct rexp_node ** rexp_p,
			       const char *pattern,
			       int size,
			       unsigned long syntax,
			       int cset_size,
			       unsigned char *translate);

#else /* STDC */
extern reg_errcode_t rx_parse ();

#endif /* STDC */


#endif  /* RXGNUCOMPH */
