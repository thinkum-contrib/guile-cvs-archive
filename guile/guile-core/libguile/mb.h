#ifndef SCM_MB_H
#define SCM_MB_H

/*	Copyright (C) 1999 Free Software Foundation, Inc.
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

/* Here are macros and functions for working with Guile's multibyte
   text representation.  At present, Guile uses the same encoding as
   GNU Emacs 20.4, but Guile and Emacs will hopefully switch to UTF-8
   sometime soon; you should use these macros to insulate your code
   from the details of the encoding, so when the switch occurs, your
   code won't break.  All knowledge of Guile's character set should be
   in mb.h (here) or mb.c.

   These are all documented in ref/mbapi.texi, which is part of the
   guile-doc CVS module.

   Actually, a lot of these definitions only rely on the ``Promised
   Properties of the Guile Multibyte Encoding'', as described in
   mbapi.texi --- mostly the promise that ASCII characters are encoded
   as themselves.  */

typedef int scm_char_t;


/* Retrieve the character whose encoding is at P.  */
#define scm_mb_get(p) \
  (*(p) < 128 ? *(p) : scm_mb_get_func (p))
extern scm_char_t scm_mb_get_func (const unsigned char *p);

/* Store the encoding of the character C at P, and return the
   encoding's length in bytes.  */
#define scm_mb_put(c, p) \
  ((c) < 128 ? (*(p) = c, 1) : scm_mb_put_func ((c), (p)))
extern int scm_mb_put_func (scm_char_t c, unsigned char *p);

/* The length of the longest character encoding, in bytes.  */
#define scm_mb_max_len (4)

/* Given an encoding's first byte, return its length.  */
#define scm_mb_len(b)				\
  ((b) < 0x80 ? 1				\
   : (b) < 0x90 ? 2				\
   : (b) < 0x9C ? 3				\
   : (b) < 0x9E ? 4				\
   : 1)
extern int scm_mb_len_func (unsigned char b);

/* Given a Guile character, return the length of its encoding.  */
#define scm_mb_len_char(c) (scm_mb_len_char_func(c))
extern int scm_mb_len_char_func (scm_char_t c);



/* Finding character encoding boundaries.  */

/* Return true if P points at the first byte of an encoding.  */
#define scm_mb_boundary_p(p) (*(p) < 0xA0)

/* Round P to the previous/next character boundary.  */
extern const unsigned char *scm_mb_floor (const unsigned char *p);
extern const unsigned char *scm_mb_ceiling (const unsigned char *p);


/* Multibyte string functions.  */

/* Return the number of characters encoded by the LEN bytes at P.  */
extern int scm_mb_count (const unsigned char *p, int len);

/* Return the character at *PP, and advance *PP to the next character.  */
extern scm_char_t scm_mb_walk (const unsigned char **pp);

/* Return the address of the character before P.  */
extern const unsigned char *scm_mb_prev (const unsigned char *p);

/* Return the address of the character after P.  */
extern const unsigned char *scm_mb_next (const unsigned char *p);

/* Return the location of the I'th character in LEN bytes at P.  */
extern const unsigned char *scm_mb_index (const unsigned char *p, int len,
					  int i);

/* A cache of information about the positions of characters in
   strings.  Initialize all elements to zero before using.  */
struct scm_mb_cache {
  int character;		/* a character index */
  int byte;			/* its byte offset in the string */
};

/* Return the location of the I'th character in LEN bytes at P.
   Use and update CACHE, if possible.  */
#define scm_mb_index_cached(p, len, i, cache)				\
  ((i) <= (cache)->character && (cache)->character == (cache)->byte	\
   ? &(p)[(i)]								\
   : scm_mb_index_cached_func ((p), (len), (i), (cache)))
extern const unsigned char *scm_mb_index_cached_func (const unsigned char *p,
						      int len, int i, 
						      struct scm_mb_cache *cache);

/* Convert a multibyte string to an array of scm_char_t's.
   The caller is responsible for freeing the result.  */
extern scm_char_t *scm_mb_multibyte_to_fixed (const unsigned char *p, int len,
					      int *result_len);

/* Convert an array of scm_char_t's to a multibyte string.
   The caller is responsible for freeing the result.  */
extern unsigned char *scm_mb_fixed_to_multibyte (const scm_char_t *fixed,
						 int len, int *result_len);

/* Initialize the multibyte stuff.  */
extern void scm_init_mb (void);

#endif  /* SCM_MB_H */
