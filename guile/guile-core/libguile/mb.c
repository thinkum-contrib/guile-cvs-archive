/* mb.c --- functions for dealing with multibyte text
   Jim Blandy <jimb@red-bean.com> --- August 1999
  
 	 Copyright (C) 1999 Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this software; see the file COPYING.  If not, write to
   the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
   Boston, MA 02111-1307 USA
  
   As a special exception, the Free Software Foundation gives permission
   for additional uses of the text contained in its release of GUILE.
  
   The exception is that, if you link the GUILE library with other files
   to produce an executable, this does not by itself cause the
   resulting executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of
   linking the GUILE library code into it.
  
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.
  
   This exception applies only to the code released by the
   Free Software Foundation under the name GUILE.  If you copy
   code from other Free Software Foundation releases into a copy of
   GUILE, as the General Public License permits, the exception does
   not apply to the code that you add in this way.  To avoid misleading
   anyone as to the status of such modified files, you must delete
   this exception notice from them.
  
   If you write modifications of your own for GUILE, it is your choice
   whether to permit this exception to apply to your modifications.
   If you do not wish that, delete this exception notice.  */


/* Headers.  */

#include <stdlib.h>

#include "_scm.h"
#include "mb.h"
#include "mbemacs.h"


/* Exceptions.  */

SCM_GLOBAL_SYMBOL (scm_text_not_char_boundary, "text:not-char-boundary");
SCM_GLOBAL_SYMBOL (scm_text_bad_encoding,      "text:bad-encoding");
static const char text_bad_encoding_msg[] =
  "string contains byte sequence which is not a valid character encoding";
SCM_GLOBAL_SYMBOL (scm_text_not_guile_char,    "text:not-guile-char");



/* Basic multibyte character processing.  */

scm_char_t
scm_mb_get_func (const unsigned char *p)
{
  unsigned char lead = *p;
  
  if (lead < 0x80)
    return lead;
  else if (lead == 0x80)
    /* Guile does *not* support composite characters, thank goodness.  */
    return -1;
  else if (lead < 0x90)
    {
      unsigned char set = lead;
      unsigned char pos = p[1] & 0x7f;

      return BUILD_CHAR1 (set, pos);
    }
  else if (lead < 0x9A)
    {
      unsigned char set = lead;
      unsigned char pos_hi = p[1] & 0x7f;
      unsigned char pos_lo = p[2] & 0x7f;

      return BUILD_CHAR2 (set, 0x8F, pos_hi, pos_lo);
    }
  else if (lead < 0x9C)
    {
      unsigned char set = p[1];
      unsigned char pos = p[2] & 0x7f;

      return BUILD_CHAR1 (set, pos);
    }
  else if (lead < 0x9E)
    {
      unsigned char set = p[1];
      unsigned char pos_hi = p[2] & 0x7f;
      unsigned char pos_lo = p[3] & 0x7f;

      return BUILD_CHAR2 (set, 0xE0, pos_hi, pos_lo);
    }
  else
    return -1;
}

int
scm_mb_put_func (unsigned char *p, scm_char_t c)
{
  if (c < 0)
    return 0;
  else if (IS_ASCII_CHAR (c))
    {
      *p = c;
      return 1;
    }
  else if (c < FIRST_CHAR1O)
    return 0;
  else if (c <= LAST_CHAR1O)
    {
      /* encoding is SET; POS+0x80 */
      *p++ = CHAR1_SET (c);
      *p++ = CHAR1_POS (c) | 0x80;
      return 2;
    }
  else if (c < FIRST_CHAR1P)
    return 0;
  else if (c <= LAST_CHAR1P)
    {
      /* encoding is: 0x9A or 0x9B; SET; POS+0x80  */
      unsigned char set = CHAR1_SET (c);
      *p++ = (set < 0xE0) ? 0x9A : 0x9B;
      *p++ = set;
      *p++ = CHAR1_POS (c) | 0x80;
      return 3;
    }
  else if (c < FIRST_CHAR2O)
    return 0;
  else if (c <= LAST_CHAR2O)
    {
      /* encoding is: SET; POS1+0x80; POS2+0x80 */
      *p++ = CHAR2_SET (c, 0x8f);
      *p++ = CHAR2_POS1 (c) | 0x80;
      *p++ = CHAR2_POS2 (c) | 0x80;
      return 3;
    }
  else if (c < FIRST_CHAR2P)
    return 0;
  else if (c <= LAST_CHAR2P)
    {
      /* encoding is: 0x9C or 0x9D; SET; POS1+0x80; POS2+0x80  */
      unsigned char set = CHAR2_SET (c, 0xE0);
      *p++ = (set < 0xF5) ? 0x9C : 0x9D;
      *p++ = set;
      *p++ = CHAR2_POS1 (c) | 0x80;
      *p++ = CHAR2_POS2 (c) | 0x80;
      return 4;
    }
  else
    return 0;
}

int
scm_mb_len_func (unsigned char b)
{
  return scm_mb_len (b);
}

int
scm_mb_char_len_func (scm_char_t c)
{
  return (IS_ASCII_CHAR (c) ? 1
	  : c < FIRST_CHAR1O ? 0 : c <= LAST_CHAR1O ? 2
	  : c < FIRST_CHAR1P ? 0 : c <= LAST_CHAR1P ? 3
	  : c < FIRST_CHAR2O ? 0 : c <= LAST_CHAR2O ? 3
	  : c < FIRST_CHAR2P ? 0 : c <= LAST_CHAR2P ? 4
	  : 0);
}


/* Finding character encoding boundaries.  */

const unsigned char *
scm_mb_floor (const unsigned char *p)
{
  while (! scm_mb_boundary_p (p))
    p--;

  return p;
}

const unsigned char *
scm_mb_ceiling (const unsigned char *p)
{
  while (! scm_mb_boundary_p (p))
    p++;

  return p;
}


/* Multibyte string functions.  */

/* Return the number of characters encoded by the LEN bytes at P.  */
int
scm_mb_count (const unsigned char *p, int len)
{
  int count = 0;
  const unsigned char *end = p + len;
  
  /* If this turns out to be a big bottleneck, then we'll make it not
     check every byte.  But for now I think I want the sanity checking.  */
  while (p < end)
    {
      if (*p < 0x80)
	count++, p++;
      else if (! scm_mb_boundary_p (p))
	/* At the top of the loop, p must always be pointing at the
	   beginning of a character encoding.  */
	goto error;
      else
	{
	  int n = scm_mb_len (*p);

	  /* Make sure this character's encoding fits within the string.  */
	  if (p + n > end)
	    goto error;

	  p++, n--;
	  while (n > 0)
	    {
	      /* No character start bytes should occur within the
                 encoding.  */
	      if (scm_mb_boundary_p (p))
		goto error;
	      p++, n--;
	    }

	  count++;
	}
    }
  
  return count;

 error:
  scm_error (scm_text_bad_encoding, "scm_mb_count",
	     text_bad_encoding_msg, SCM_EOL, SCM_EOL);
}


/* Return the character at *PP, and advance *PP to the next character.  */
scm_char_t
scm_mb_walk (const unsigned char **pp)
{
  const unsigned char *p = *pp;
  scm_char_t c = scm_mb_get (p);
  *pp = p + scm_mb_len (*p);
  return c;
}


/* Return the address of the character before P.  */
const unsigned char *
scm_mb_prev (const unsigned char *p)
{
  p--;
  while (! scm_mb_boundary_p (p))
    p--;

  return p;
}


/* Return the address of the character after P.  */
const unsigned char *
scm_mb_next (const unsigned char *p)
{
  p++;
  while (! scm_mb_boundary_p (p))
    p++;

  return p;
}


/* Return the location of the I'th character in LEN bytes at P.  */
const unsigned char *
scm_mb_index (const unsigned char *p, int len, int i)
{
  struct scm_mb_cache cache;

  cache.character = 0;
  cache.byte = 0;

  return scm_mb_index_cached_func (p, len, i, &cache);
}


const unsigned char *
scm_mb_index_cached_func (const unsigned char *p, int len, int i,
			  struct scm_mb_cache *cache)
{
  int character = cache->character;
  int byte = cache->byte;

  SCM_ASSERT (i >= 0, i, SCM_OUTOFRANGE, "scm_mb_index");

  /* If cache's character and byte offsets are the same, then that
     means that all characters up to that position are a single byte
     long, so that prefix of the string can be indexed normally.  */
  if (i <= character
      && character == byte)
    return &p[i];
    
  /* We start from the beginning of the string or the cache position,
     whichever is closer.  */
  if (i <= character / 2)
    character = byte = 0;

  if (i < character)
    {
      /* Scanning backwards!  */

      while (byte > 0 && i < character)
	{
	  byte--;
	  if (scm_mb_boundary_p (&p[byte]))
	    character--;
	}

      /* We never got there!  The cache and the string must have been
         out of sync.  */
      if (i < character)
	scm_misc_error ("scm_mb_index",
			"multibyte position cache was inaccurate",
			SCM_EOL);
    }
  else if (i > character)
    {
      /* Scanning forwards!  */
      while (byte < len && i > character)
	{
	  if (! scm_mb_boundary_p (&p[byte]))
	    scm_error (scm_text_bad_encoding, "scm_mb_index",
		       text_bad_encoding_msg, SCM_EOL, SCM_EOL);
	  byte += scm_mb_len (p[byte]);
	  character++;
	}

      /* We never got there!  This could mean that 1) i was off the
         end of the string, or 2) the cache and string were out of
         sync.  Assume the former.  */
      if (i > character)
	SCM_ASSERT (0, i, SCM_OUTOFRANGE, "scm_mb_index");
    }
  else
    /* Perfect cache hit!  */
    return &p[byte];

  cache->character = character;
  cache->byte = byte;
  return &p[byte];
}


/* Convert a multibyte string to an array of scm_char_t's.
   The caller is responsible for freeing the result.  */
scm_char_t *
scm_mb_multibyte_to_fixed (const unsigned char *p, int len, int *result_len)
{
  const unsigned char *end = p + len;
  scm_char_t *buf;
  int buf_len;

  buf = scm_must_malloc (len * sizeof (*buf), "scm_mb_multibyte_to_fixed");
  buf_len = 0;

  while (p < end)
    {
      scm_char_t c = scm_mb_get (p);
      if (c < 0)
	scm_error (scm_text_bad_encoding, "scm_mb_multibyte_to_fixed",
		   text_bad_encoding_msg, SCM_EOL, SCM_EOL);
      buf[buf_len++] = c;
      p += scm_mb_len (*p);
    }

  buf = scm_must_realloc (buf, len * sizeof (*buf), buf_len * sizeof (*buf),
			  "scm_mb_multibyte_to_fixed");
  *result_len = buf_len;
  return buf;
}

/* Convert an array of scm_char_t's to a multibyte string.
   The caller is responsible for freeing the result.  */
unsigned char *
scm_mb_fixed_to_multibyte (const scm_char_t *fixed, int len, int *result_len)
{
  int i;
  int buf_size;
  unsigned char *buf, *p;

  /* Compute the buffer size.  I think it's faster to make two passes
     over the string like this than to possibly recopy it.  */
  buf_size = 0;
  for (i = 0; i < len; i++)
    buf_size += scm_mb_char_len (fixed[i]);

  buf = scm_must_malloc (buf_size + 1, "scm_mb_fixed_to_multibyte");
  p = buf;
  for (i = 0; i < len; i++)
    p += scm_mb_put (p, fixed[i]);

  /* Was the size we computed actually correct?  */
  if (p != buf + buf_size)
    abort ();

  /* Null-terminate the string.  */
  *p = '\0';

  *result_len = buf_size;
  return buf;
}


/* Initialization.  */

void
scm_init_mb ()
{
#include "mb.x"
}
