/*	Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.
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


#include <stdio.h>
#include <assert.h>

#include "_scm.h"
#include "chars.h"
#include "eval.h"
#include "alist.h"
#include "weaks.h"
#include "unif.h"

#include "symbols.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif




/* NUM_HASH_BUCKETS is the number of symbol hash table buckets. 
 */
#define NUM_HASH_BUCKETS 69 /* 137 */
int scm_symhash_dim = NUM_HASH_BUCKETS;


/* {Symbols}
 */

unsigned long 
scm_strhash (str, len, n)
     unsigned char *str;
     scm_sizet len;
     unsigned long n;
{
  if (len > 5)
    {
      scm_sizet i = 5;
      unsigned long h = (NUM_HASH_BUCKETS * 2)% n;
      while (i--)
	h = ((h << 8) + ((unsigned) (scm_downcase (str[h % len])))) % n;
      return h;
    }
  else
    {
      scm_sizet i = len;
      unsigned long h = 0;
      while (i)
	h = ((h << 8) + ((unsigned) (scm_downcase (str[--i])))) % n;
      return h;
    }
}



/*
 * look up the symbol name in obarray and return its vcell or SCM_BOOL_F
 */
static SCM
scm_symbolname_get_handle (obarray, name, len, scm_hash)
     char *name;
     scm_sizet len;
     SCM obarray;
     scm_sizet scm_hash;
{
  register SCM lsym;
  register SCM z;

  SCM_REDEFER_INTS;

  for (lsym = SCM_VELTS (obarray)[scm_hash]; 
       SCM_NIMP (lsym); 
       lsym = SCM_CDR (lsym))
    {
      z = SCM_CAAR (lsym);
      if ((SCM_LENGTH (z) == len) && !strncmp (SCM_UCHARS (z), name, len))
	{
	  SCM_REALLOW_INTS;
	  return SCM_CAR(lsym);
	}
    }
  
  SCM_REALLOW_INTS;
  return SCM_BOOL_F;
}


/*
 * Intern the symbol `name' in obarray. obarray may be either scm_weak_symhash
 * or SCM_BOOL_F in which case a symbol is created which is listed in
 * no obarray
 *
 */
static SCM
scm_symbolname_create_handle (obarray, name, len, scm_hash)
     char *name;
     scm_sizet len;
     SCM obarray;
     scm_sizet scm_hash;
{
  SCM lsym;
  SCM a;
  SCM b;
  
  SCM_REDEFER_INTS;

  lsym = scm_makfromstr (name, len, SCM_SYMBOL_SLOTS);

  SCM_SETLENGTH (lsym, (long) len, scm_tc7_msymbol);
  SCM_SYMBOL_HASH (lsym) = scm_hash;

  SCM_NEWCELL (a);
  SCM_NEWCELL (b);
  SCM_SETCAR (a, lsym);
  SCM_SETCDR (a, SCM_UNDEFINED);
  SCM_SETCAR (b, a);
  SCM_SETCDR (b, SCM_VELTS(obarray)[scm_hash]);
  SCM_VELTS(obarray)[scm_hash] = b;
  SCM_REALLOW_INTS;
  return SCM_CAR (b);
}



/*
 * define a symbol
 *
 * look up a symbol with the name "name" in weak_symhash or create a
 * new one and return its vcell 
 */
SCM 
scm_intern(name)
     char *name;
{
  SCM scm_hash;
  SCM vcell;
  scm_sizet len;

  len = strlen(name);
  scm_hash = scm_strhash (name, len, scm_symhash_dim);

  vcell = scm_symbolname_get_handle (scm_weak_symhash, name, len, scm_hash);

  if (SCM_IMP (vcell))	
    {
				/* Not in weak: intern it */
      vcell = scm_symbolname_create_handle (scm_weak_symhash, name, len, scm_hash);
    }

  return vcell;
}




SCM_PROC(s_symbol_p, "symbol?", 1, 0, 0, scm_symbol_p);

SCM
scm_symbol_p(x)
     SCM x;
{
  if SCM_IMP(x) return SCM_BOOL_F;
  return SCM_SYMBOLP(x) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_PROC(s_symbol_to_string, "symbol->string", 1, 0, 0, scm_symbol_to_string);

SCM
scm_symbol_to_string(s)
     SCM s;
{
  SCM_ASSERT(SCM_NIMP(s) && SCM_SYMBOLP(s), s, SCM_ARG1, s_symbol_to_string);
  
  return scm_makfromstr(SCM_CHARS(s), (scm_sizet)SCM_LENGTH(s), 0);
}

SCM_PROC(s_string_to_symbol, "string->symbol", 1, 0, 0, scm_string_to_symbol);

SCM
scm_string_to_symbol(s)
     SCM s;
{
  SCM vcell;
  char *name;
  scm_sizet len;
  scm_sizet scm_hash;

  SCM_ASSERT(SCM_NIMP(s) && SCM_ROSTRINGP(s), s, SCM_ARG1, s_string_to_symbol);

  name = SCM_ROCHARS(s);
  len = (scm_sizet)SCM_LENGTH(s);

  scm_hash = scm_strhash (name, len, scm_symhash_dim);

  vcell = scm_symbolname_get_handle (scm_weak_symhash, name, len, scm_hash);

  if (SCM_IMP (vcell))	
    {
				/* Not in weak: intern it */
      vcell = scm_symbolname_create_handle (scm_weak_symhash, name, len, scm_hash);
    }

  return SCM_CAR(vcell);
}

static int gensym_counter;
static SCM gensym_prefix;

SCM_PROC (s_gensym, "gensym", 0, 1, 0, scm_gensym);

SCM
scm_gensym (name)
     SCM name;
{
  unsigned char *new;
  SCM obarray = scm_weak_symhash;
  SCM sym;
  scm_sizet len;
  scm_sizet scm_hash;

  if (SCM_UNBNDP (name))
    {
      name = gensym_prefix;
    }
  else
    {
      SCM_ASSERT (SCM_NIMP(name) && SCM_ROSTRINGP (name), name, SCM_ARG1, s_gensym);
    }

  do
    {
      new = SCM_UCHARS (scm_string_append
			(scm_cons2 (name,
				    scm_number_to_string (SCM_MAKINUM (gensym_counter++),
							  SCM_UNDEFINED),
				    SCM_EOL)));
      
      len = strlen (new);
      scm_hash = scm_strhash (new, len, scm_symhash_dim);
      sym = scm_symbolname_get_handle (obarray, new, len, scm_hash);
    }
  while (sym != SCM_BOOL_F);

  return SCM_CAR(scm_symbolname_create_handle(obarray, new, len, scm_hash));
}

SCM
scm_init_symbols (env)
     SCM env;
{
  gensym_counter = 0;
  gensym_prefix = scm_permanent_object (scm_makfrom0str ("%%gensym"));

#include "symbols.x"

  return SCM_UNSPECIFIED;
}

