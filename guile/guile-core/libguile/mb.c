/*	Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.
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

/* Headers.  */

#include "_scm.h"
#include "mb.h"


/* Here is a description of the encoding.

   ** THIS WILL CHANGE IN FUTURE VERSIONS OF GUILE --- IT IS NOT
   CORRECT TO ASSUME THE USE OF THIS ENCODING OUTSIDE OF mb.c and
   mb.h. **

   If you can't accomplish what you want without this info, then the
   multibyte API is flawed, and we need to extend it.  If you spread
   this knowledge around into other code, then it will break when we
   change encodings.

   You have been warned.


   For all ASCII characters, the Guile scm_char_t code is equal to the
   ASCII code.  The Guile multi-byte encoding is a single byte whose
   value is the character's ASCII code.  Note that ASCII doesn't
   contain any characters whose numbers are above 127.

   For non-ASCII characters:

   Each character is assigned a character set number from 0x81 to 0xFE
   (except for 0x9A .. 0x9F), and a position within that character
   set.  A "position" within a character set is one or two bytes from
   0x20 to 0x7F.

   For example:
   - The Latin-1 character set is 0x81.
   - The Japanese JISX0208.1983/1990 Kanji set is 0x92.
   - The character á (a lower-case a with an acute accent) is part of
     the Latin-1 character set.  Its position is the byte 0x61.
   - The Japanese Katakana character "ka" is part of the JISX0208
     character set.  Its position is the pair of bytes 0x25 0x2B.

   Once we know a character's character set, we can determine whether
   its position is one or two bytes, and the form of its encoding.

   Character set number     positions 	 encoding byte sequence
   ===========================================================================
   from 0x81 to 0x8f        1 byte    	 SET   POS +0x80
   from 0x90 to 0x99        2 bytes   	 SET   POS1+0x80  POS2+0x80
   from 0xA0 to 0xDF        1 byte    	 0x9A  SET        POS +0x80
   from 0xE0 to 0xEF        1 byte    	 0x9B  SET        POS +0x80
   from 0xF0 to 0xF4        2 bytes   	 0x9C  SET        POS1+0x80  POS2+0x80
   from 0xF5 to 0xFE        2 bytes   	 0x9D  SET        POS1+0x80  POS2+0x80

   "SET" is the character set number;
   "POS" is a one-byte position; and
   "POS1" and "POS2" are a two-byte position, 

   Some examples:
   - For the character á, SET is 0x81, and POS is 0x61, so it would be
     encoded by the byte sequence 0x81 0xE1.
   - For the Japanese Katakana character "ka", SET is 0x92, and POS1
     and POS2 are 0x25 and 0x2B, so it would be encoded by the byte
     sequence 0x92 0xA5 0xAB.

   So the longest encoding is four bytes long.

   It's easy to verify that this encoding meets the conditions
   promised by mbapi.texi:

   - Every ASCII character is encoded as a single byte from 0 to 127,
     in the obvious way.
   - The encodings of non-ASCII characters use only bytes between 0x80
     and 0xFF.
   - No character encoding is a subsequence of any other character
     encoding, since bytes from 0x00 to 0x9f occur only at the
     beginning of a sequence.
   - You can always determine the full length of a character's
     encoding from its first byte.
   - Given an arbitrary byte position in a Guile string, you can
     always find the beginning and end of the character containing
     that byte without scanning too far in either direction, assuming
     the string is null-terminated or followed by another valid
     character (as substrings are).


   How does Guile choose scm_char_t values for non-ASCII characters?

   We divide a character value up into three fields:
   FIELD1: bits 18 -- 14  (most significant bits)
   FIELD2: bits 13 --  7
   FIELD3: bits  6 --  0  (least significant bits)

   If the character's position is one byte, then:
     FIELD1 is zero.
     FIELD2 is the character set number, minus 0x70.
     FIELD3 is the character position.
   
   If the character's position is two bytes, then:
     FIELD2 is the first byte of the character's position.
     FIELD3 is the second byte of the character's position.
     If the character set number is from 0x90 to 0x99, then:
       FIELD1 is the character set number, minus 0x8f.
       (Thus, a number from 0x01 to 0x0A.)
     If the character set number is from 0xF0 to 0xFE, then:
       FIELD1 is the character set number, minus 0xE0.
       (Thus, a number from 0x10 to 0x1E.)

   For example:
   - For the character á, FIELD1 would be zero, FIELD2 would be 0x11,
     and FIELD3 would be 0x61.  Thus, the full character code would be
     (0x11 << 7) | 0x61, or 2273.
   - For the Japanese Katakana character "ka", FIELD1 would be 0x3,
     FIELD2 would be 0x25 and FIELD3 would be 0x2B.  Thus, the full
     character code would be (0x3 << 14) | (0x25 << 7) | 0x2B, or 53931.

   Thus, character codes fall into the following ranges:

         0 ..    127    ASCII
      2208 ..   4095    "official"   one-byte position character sets
      6176 ..  16383    "unofficial" one-byte position character sets
     20512 .. 180223    "official"   two-byte position character sets
    266272 .. 507903    "unofficial" two-byte position character sets

   It's hairy, but at the time this was designed, Unicode didn't exist
   --- this encoding allowed Emacs to incorporate characters from all
   kinds of character sets unchanged.  It also allows Emacs to
   distinguish between Japanese and Chinese character sets, which is
   important to some users.

   Even when we make the transition to Unicode, we will probably
   retain some way of distinguishing Japanese and Chinese characters.
   This is a highly controvertial issue.  However, I think that the
   opinions of people who do not use Chinese or Japanese regularly
   should be discounted; once this is done, there is a substantial
   body of users who say they need this distinction in the encoding
   itself.  So Guile will support it.  */



/* Exceptions.  */

SCM_SYMBOL (text_not_char_boundary, "text:not-char-boundary");
SCM_SYMBOL (text_bad_encoding,      "text:bad-encoding");
static const char text_bad_encoding_msg[] =
  "string contains byte sequence which is not a valid character encoding";
SCM_SYMBOL (text_not_guile_char,    "text:not-guile-char");



/* Basic multibyte character processing.  */

/* Assembling and disassembling character codes.
   A `CHAR1' is a character whose position is one byte.
   A `CHAR2' is a character whose position is two bytes.
   The suffix `O' refers to an "official" character set --- one
       whose character set number is in the range 0x81 -- 0x99.
   The suffix `P' refers to a "private" character set --- one
       whose character set number is in the range 0xA0 -- 0xFE.
*/

#define BUILD_CHAR1(set, pos) ((((set) - 0x70) << 7) | (pos))
#define BUILD_CHAR2(set, offset, pos1, pos2)	\
  ((((set) - (offset)) << 14) 			\
   | ((pos1) << 7)				\
   | (pos2))

#define IS_ASCII_CHAR(c) ((c) < 0x80)

#define FIRST_CHAR1O (BUILD_CHAR1 (0x81, 0x20))
#define LAST_CHAR1O  (BUILD_CHAR1 (0x8f, 0x7F))

#define FIRST_CHAR1P (BUILD_CHAR1 (0xA0, 0x20))
#define LAST_CHAR1P  (BUILD_CHAR1 (0xEF, 0x7F))

#define FIRST_CHAR2O (BUILD_CHAR2 (0x90, 0x8F, 0x20, 0x20))
#define LAST_CHAR2O  (BUILD_CHAR2 (0x99, 0x8F, 0x7F, 0x7F))

#define FIRST_CHAR2P (BUILD_CHAR2 (0xF0, 0xE0, 0x20, 0x20))
#define LAST_CHAR2P  (BUILD_CHAR2 (0xFE, 0xE0, 0x7F, 0x7F))

#define CHAR1_SET(c) (((c) >> 7) + 0x70)
#define CHAR1_POS(c) ((c) & 0x7F)

#define CHAR2_SET(c, offset) (((c) >> 14) + (offset))
#define CHAR2_POS1(c) (((c) >> 7) & 0x7f)
#define CHAR2_POS2(c) ((c) & 0x7f)

scm_char_t
scm_mb_get_func (const unsigned char *p)
{
  unsigned char lead = *p;
  
  if (IS_ASCII_CHAR (lead))
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
      unsigned char set = load;
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
scm_mb_put_func (scm_char_t c, unsigned char *p)
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
scm_mb_len_char_func (scm_char_t c)
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
  scm_error (text_bad_encoding, "scm_mb_count",
	     text_bad_encoding_msg, SCM_EOL, SCM_EOL);
}


/* Return the character at *PP, and advance *PP to the next character.  */
scm_char_t
scm_mb_walk (const unsigned char **pp)
{
  const unsigned char *p = *p;
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
	    scm_error (text_bad_encoding, "scm_mb_index",
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

  while (p < len)
    {
      scm_char_t c = scm_mb_get (p);
      if (c < 0)
	scm_error (text_bad_encoding, "scm_mb_multibyte_to_fixed",
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
    buf_size += scm_mb_len_char (fixed[i]);

  buf = scm_must_malloc (buf_size + 1, "scm_mb_fixed_to_multibyte");
  p = buf;
  for (i = 0; i < len; i++)
    {
      scm_mb_put (fixed[i], p);
      p += scm_mb_len (*p);
    }

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
