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
 * If you do not wish that, delete this exception notice.
 */



#include <stdio.h>
#include <stdlib.h>
#include <gh.h>

struct pair {
  scm_char_t character;
  unsigned char encoding[10];
  int encoding_length; 
};

struct pair pairs[] = {

  /* Check some ASCII characters.  */
  { 0,   { 0 },   1 },
  { 127, { 127 }, 1 },
  { 31,  { 31 },  1 },
  { 32,  { 32 },  1 },
  { 42,  { 42 },  1 },

  /* Sometimes we mark something as an "end of range", when it's not
     actually the last character that would use that encoding form.
     This is because not all character set numbers are assigned, and we
     can't use unassigned character set numbers.  So the value given is
     the last value which actually corresponds to something in a real
     character set.  */

  /* Check some characters encoded in two bytes.  */
  { 2208, { 0x81, 0xA0 }, 2 }, /* beginning of range */
  { 3839, { 0x8d, 0xFF }, 2 }, /* end of range */
  { 2273, { 0x81, 0xE1 }, 2 },

  /* Check some big characters encoded in three bytes.  */
  { 20512,  { 0x90, 0xA0, 0xA0 }, 3 }, /* beginning of range */
  { 180223, { 0x99, 0xFF, 0xFF }, 3 }, /* end of range */
  { 53931,  { 0x92, 0xA5, 0xAB }, 3 },

  /* Check some small characters encoded in three bytes --- some from
     the 0x9A prefix range, and some from the 0x9B prefix range.  */
  { 6176,   { 0x9A, 0xA0, 0xA0 }, 3 },  /* start of the #9A prefix range */
  { 7167,   { 0x9A, 0xA7, 0xFF }, 3 },  /* end   of the #9A prefix range */
  { 14368,  { 0x9B, 0xE0, 0xA0 }, 3 },  /* start of the #9B prefix range */
  { 14591,  { 0x9B, 0xE1, 0xFF }, 3 },  /* end   of the #9B prefix range */

  /* Check some characters encoded in four bytes.  */
  { 266272, { 0x9C, 0xF0, 0xA0, 0xA0 }, 4 },/* start of the #9C prefix range */
  { 294911, { 0x9C, 0xF1, 0xFF, 0xFF }, 4 },/* end   of the #9C prefix range */
  { 348192, { 0x9D, 0xF5, 0xA0, 0xA0 }, 4 },/* start of the #9D prefix range */
  { 475135, { 0x9D, 0xFC, 0xFF, 0xFF }, 4 },/* start of the #9D prefix range */

  { 0, { 0 }, 0 }
};

#define NUM_PAIRS ((sizeof (pairs) / sizeof (pairs[0])) - 1)


static void
test_one_char_encodings ()
{
  struct pair *p;

  for (p = pairs; p->encoding_length > 0; p++)
    {
      unsigned char buffer[scm_mb_max_len + 10];
      int i;
      int len;

      /* scm_mb_len_char should return a sane value.  */
      len = scm_mb_len_char (p->character);
      if (len <= 0 || len > scm_mb_max_len)
	exit (1);
      if (len != p->encoding_length)
	exit (1);

      /* scm_mb_put should return the same length, and write that many
         characters, but no more.  */
      memset (buffer, 1, sizeof (buffer));
      if (scm_mb_put (p->character, buffer) != len)
	exit (1);
      for (i = 0; i < len; i++)
	if (buffer[i] == 1)
	  exit (1);
      for (; i < sizeof (buffer); i++)
	if (buffer[i] != 1)
	  exit (1);

      /* Compare the encoding against the one in our table.  */
      if (memcmp (buffer, p->encoding, len))
	exit (1);

      /* Try to get the character, and see if it works.  */
      if (scm_mb_get (buffer) != p->character)
	exit (1);

      /* Check against value returned by scm_mb_len.  */
      if (scm_mb_len (buffer[0]) != len)
	exit (1);

      /* Test ceiling and floor functions.  */
      for (i = 0; i < len; i++)
	if (scm_mb_floor (buffer + i) != buffer)
	  exit (1);
      for (i = 1; i < len; i++)
	if (scm_mb_ceiling (buffer + i) != buffer + len)
	  exit (1);
    }
}


/* Fill BUFFER with LEN random numbers in the range 0 to MODULUS - 1.
   Use SEED to choose the random numbers.  */
static void
make_permutation (int modulus, int len, int *buffer, unsigned int seed)
{
  int i;

  srandom (seed);
  for (i = 0; i < len; i++)
    buffer[i] = (unsigned long) random () % modulus;

  if (len > 0)
    buffer[(unsigned long) random % len] = seed % modulus;
}

#define MAX_STRING_LEN 20
static void
test_string_encodings ()
{
  int perm[MAX_STRING_LEN];
  unsigned char buffer[scm_mb_max_len * MAX_STRING_LEN];
  int start[MAX_STRING_LEN];
  unsigned int seed;
  int len;

  for (seed = 0; seed < 100; seed++)
    for (len = 0; len < MAX_STRING_LEN; len++)
      {
	int i;
	int bytes;
	struct pair *p;
	unsigned char *t;

	/* Choose a random sequence of characters to try out.  */
	make_permutation (NUM_PAIRS, len, perm, seed);
	
	/* Render those characters into the buffer.  */
 	t = buffer;
	bytes = 0;
	for (i = 0; i < len; i++)
	  {
	    start[i] = t - buffer;
	    p = &pairs[perm[i]];
	    t += scm_mb_put (p->character, t);
	    bytes += p->encoding_length;
	  }
	if (t != buffer + bytes)
	  exit (1);

	/* Check its length.  */
	if (scm_mb_count (buffer, bytes) != len)
	  exit (1);

	/* Read it back using the offsets we recorded.  */
	for (i = 0; i < len; i++)
	  {
	    if (! scm_mb_boundary_p (buffer + start[i]))
	      exit (1);
	    if (scm_mb_get (buffer + start[i]) != pairs[perm[i]].character)
	      exit (1);
	  }

	/* Read it back using scm_mb_walk.  */
	t = buffer;
	for (i = 0; i < len; i++)
	  {
	    if (scm_mb_walk ((const unsigned char **)&t) != pairs[perm[i]].character)
	      exit (1);
	    if (t > buffer + bytes)
	      exit (1);
	  }
	if (t != buffer + bytes)
	  exit (1);

	/* Try going forward and backward for each character.  */
	for (i = 0; i < len - 1; i++)
	  {
	    if (scm_mb_prev (buffer + start[i + 1]) != buffer + start[i])
	      exit (1);
	    if (scm_mb_next (buffer + start[i]) != buffer + start[i + 1])
	      exit (1);
	  }

	/* Try fetching characters out of the middle, without caching.  */
	for (i = 0; i < len; i++)
	  if (scm_mb_index (buffer, bytes, i) != buffer + start[i])
	    exit (1);

	/* Try fetching characters out of the middle, with caching.  */
	{
	  struct scm_mb_cache cache;

	  cache.character = 0;
	  cache.byte = 0;

	  /* scan forwards */
	  for (i = 0; i < len; i++)
	    if (scm_mb_index_cached (buffer, bytes, i, &cache)
		!= buffer + start[i])
	      exit (1);

	  /* scan backwards */
	  for (i = len - 1; i >= 0; i--)
	    if (scm_mb_index_cached (buffer, bytes, i, &cache)
		!= buffer + start[i])
	      exit (1);

	  /* Try fetching characters in random order, with caching.  */
	  {
	    int index[MAX_STRING_LEN];

	    for (i = 0; i < len; i++)
	      index[i] = i;

	    srandom (seed + len + 1);
	    for (i = 0; i < len; i++)
	      {
		int j, temp;
		j = (unsigned long) random () % len;
		temp = index[i];
		index[i] = index[j];
		index[j] = temp;
	      }

	    for (i = 0; i < len; i++)
	      if (scm_mb_index_cached (buffer, bytes, index[i], &cache)
		  != buffer + start[index[i]])
		exit (1);
	  }
	}

	/* Try converting whole strings back and forth.  */
	{
	  int result_len;
	  scm_char_t *copy = scm_mb_multibyte_to_fixed (buffer, bytes,
							&result_len);
	  unsigned char *copy2;

	  if (result_len != len)
	    exit (1);
	  for (i = 0; i < len; i++)
	    if (copy[i] != pairs[perm[i]].character)
	      exit (1);

	  copy2 = scm_mb_fixed_to_multibyte (copy, len, &result_len);
	  if (result_len != bytes)
	    exit (1);
	  if (memcmp (copy2, buffer, bytes))
	    exit (1);
	  if (copy2[bytes] != '\0')
	    exit (1);

	  free (copy);
	  free (copy2);
	}
      }
}

static void 
main_prog (int argc, char *argv[])
{
  test_one_char_encodings ();
  test_string_encodings ();
}

int 
main (int argc, char *argv[])
{
  gh_enter (argc, argv, main_prog);
  return 0;
}

