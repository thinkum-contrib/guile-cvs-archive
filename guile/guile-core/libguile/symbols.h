/* classes: h_files */

#ifndef SYMBOLSH
#define SYMBOLSH
/*	Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


#include "libguile/__scm.h"


extern int scm_symhash_dim;

/* SCM_LENGTH(SYM) is the length of SYM's name in characters, and
   SCM_CHARS(SYM) is the address of the first character of SYM's name.
   Every symbol occupies a position in scm_weak_symhash and possibly
   in one of guile's environments.
   The macros SCM_SYMBOL_HASH and SCM_HASHCODE both return the symbol's
   position in the obarray.

   Symbols aren't very useful without environments.  You can create a
   symbol with scm_intern() but you can't bind values to it until
   you create an environment in which you can bind a symbol to a location
   and store a value in it.

   For historical reasons all symbols are called `msymbols'.  But note
   that in guile msymbols do not carry any values.
*/
#define SCM_SYMBOLP(x) (SCM_TYP7(x)==scm_tc7_msymbol)
#define SCM_LENGTH(x) (((unsigned long)SCM_CAR(x))>>8)
#define SCM_LENGTH_MAX (0xffffffL)
#define SCM_SETLENGTH(x, v, t) SCM_SETCAR((x), ((v)<<8)+(t))
#define SCM_SETCHARS SCM_SETCDR
#define SCM_CHARS(x) ((char *)(SCM_CDR(x)))
#define SCM_UCHARS(x) ((unsigned char *)(SCM_CDR(x)))
#define SCM_SLOTS(x) ((SCM *) (* ((SCM *)SCM_CHARS(x) - 1)))

#define SCM_SYMBOL_SLOTS 2
#define SCM_SYMBOL_HASH(X) (*(unsigned long*)(&SCM_SLOTS(X)[0]))
#define SCM_HASHCODE SCM_SYMBOL_HASH

#define SCM_ROSTRINGP(x) ((SCM_TYP7S(x)==scm_tc7_string) \
			  || (SCM_TYP7(x) == scm_tc7_msymbol))
#define SCM_ROCHARS(x) ((SCM_TYP7(x) == scm_tc7_substring) \
			? SCM_INUM (SCM_CADR (x)) + SCM_CHARS (SCM_CDDR (x))  \
			: SCM_CHARS (x))
#define SCM_ROUCHARS(x) ((SCM_TYP7(x) == scm_tc7_substring) \
			 ? SCM_INUM (SCM_CADR (x)) + SCM_UCHARS (SCM_CDDR (x))\
			 : SCM_UCHARS (x))
#define SCM_ROLENGTH(x) SCM_LENGTH (x)
#define SCM_SUBSTRP(x) ((SCM_TYP7(x) == scm_tc7_substring))
#define SCM_SUBSTR_STR(x) (SCM_CDDR (x))
#define SCM_SUBSTR_OFFSET(x) (SCM_CADR (x))

#define SCM_COERCE_SUBSTR(x) { if (SCM_SUBSTRP (x)) \
				 x = scm_makfromstr (SCM_ROCHARS (x), \
						     SCM_ROLENGTH (x), 0); }



extern unsigned long scm_strhash SCM_P ((unsigned char *str, scm_sizet len, unsigned long n));
extern SCM scm_intern SCM_P ((char *name));

extern SCM scm_symbol_p(SCM x);
extern SCM scm_string_to_symbol(SCM s);
extern SCM scm_symbol_to_string(SCM s);
SCM scm_gensym (SCM name);

extern SCM scm_init_symbols SCM_P ((SCM env));

#endif  /* SYMBOLSH */
