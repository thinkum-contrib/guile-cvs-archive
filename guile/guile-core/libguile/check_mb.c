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



/* Testing multibyte text handling functions.  */

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


/* A very simple random number generator.  */

/* It's important that the test suite run consistently each time.
   Changes to Guile's random number generator shouldn't change the set
   of permutations tried.  */

static unsigned long test_rand_seed;

static unsigned long test_rand ()
{
  test_rand_seed = ((test_rand_seed * 1103515245) + 12345) & 0x7fffffff;
  return test_rand_seed >> 4;
}



/* Fill BUFFER with LEN random numbers in the range 0 to MODULUS - 1.
   Use SEED to choose the random numbers.  */

static void
make_permutation (int modulus, int len, int *buffer, unsigned int seed)
{
  int i;

  test_rand_seed = seed;

  for (i = 0; i < len; i++)
    buffer[i] = test_rand () % modulus;

  if (len > 0)
    buffer[test_rand () % len] = seed % modulus;
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

	    test_rand_seed = seed + len + 1;
	    for (i = 0; i < len; i++)
	      {
		int j, temp;
		j = test_rand () % len;
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


/* Testing conversion functions.  */

char *all_encodings[] = {
  "Emacs-Mule", "Guile", 
  "ISO-8859-1", "Latin-1",
  "ISO-8859-2", "Latin-2",
  "ISO-8859-3", "Latin-3",
  "ISO-8859-4", "Latin-4",
  "ISO-8859-5", "Latin-5",
  "ISO-8859-6", "Latin-6",
  "ISO-8859-7", "Latin-7",
  "ISO-8859-8", "Latin-8",
  "ISO-8859-9", "Latin-9",
  "US-ASCII", "ASCII",
  0
};

struct text {
  char *encoding;		/* an encoding name */
  char *original;		/* some text in that encoding */
  char *guile;			/* the same text in Guile's encoding */
};

struct text texts[] = {
  { "Emacs-Mule", "hi\x81\xE1\x92\xA5\xAB", "hi\x81\xE1\x92\xA5\xAB" },
  { "ISO-8859-1",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x81\xA0\x81\xFE\x81\xFF" },
  { "ISO-8859-1",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f*\xA0*\xFE*\xFF" },
  { "ISO-8859-1",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x81\xA0\x81\xFE\x81\xFF" },
  { "ISO-8859-2",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x82\xA0\x82\xFE\x82\xFF" },
  { "ISO-8859-3",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x83\xA0\x83\xFE\x83\xFF" },
  { "ISO-8859-4",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x84\xA0\x84\xFE\x84\xFF" },
  { "ISO-8859-5",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x8C\xA0\x8C\xFE\x8C\xFF" },
  { "ISO-8859-6",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x87\xA0\x87\xFE\x87\xFF" },
  { "ISO-8859-7",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x86\xA0\x86\xFE\x86\xFF" },
  { "ISO-8859-8",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x88\xA0\x88\xFE\x88\xFF" },
  { "ISO-8859-9",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF", 
    "\x00hello\x7f\x8D\xA0\x8D\xFE\x8D\xFF" },
  { "US-ASCII",
    "\x00hello\x7f\x80\x9F\xA0\xFE\xFF",
    "\x00hello\x7f" },
  { 0, 0, 0 }
};


static void
one_conversion (const char *code1, const char *code2,
		char *text1, char *text2)
{
  int split;
  int text1_len = strlen (text1);
  int text2_len = strlen (text2);
  char *buf = malloc (text2_len + text1_len);
  const char *inptr;
  char *outptr;
  size_t inbytesleft, outbytesleft;
  struct scm_mb_iconv *context;

  /* Convert from code1 to code2.  */
  context = scm_mb_iconv_open (code2, code1);
  inptr = text1;
  inbytesleft = text1_len;
  outptr = buf;
  outbytesleft = text2_len;
  if (scm_mb_iconv (context, &inptr, &inbytesleft, &outptr, &outbytesleft)
      != 0)
    exit (1);
  if (outptr - buf != text2_len
      || outbytesleft != 0
      || inptr - text1 != text1_len
      || inbytesleft != 0
      || memcmp (buf, text2, text2_len))
    exit (1);
  scm_mb_iconv_close (context);

  /* Split the input string at various points, and convert in two
     steps.  */
  for (split = 0; split <= text1_len; split++)
    {
      int result;

      context = scm_mb_iconv_open (code2, code1);
      inptr = text1;
      inbytesleft = split;
      outptr = buf;
      outbytesleft = text2_len;
      result = scm_mb_iconv (context,
			     &inptr, &inbytesleft,
			     &outptr, &outbytesleft);
      if (result != 0
	  && result != scm_mb_iconv_incomplete_encoding)
	exit (1);
      inbytesleft += text1_len - split;
      result = scm_mb_iconv (context,
			     &inptr, &inbytesleft, &outptr, &outbytesleft);
      if (result != 0)
	exit (1);
      if (outptr - buf != text2_len
	  || outbytesleft != 0
	  || inptr - text1 != text1_len
	  || inbytesleft != 0
	  || memcmp (buf, text2, text2_len))
	exit (1);
      scm_mb_iconv_close (context);
    }

  /* Split the output buffer at various points, and convert in two
     steps.  */
  for (split = 0; split <= text2_len; split++)
    {
      int result;

      context = scm_mb_iconv_open (code2, code1);
      inptr = text1;
      inbytesleft = text1_len;
      outptr = buf;
      outbytesleft = split;
      result = scm_mb_iconv (context,
			     &inptr, &inbytesleft,
			     &outptr, &outbytesleft);
      if (split < text2_len)
	{
	  if (result != scm_mb_iconv_more_room)
	    exit (1);
	}
      else
	{
	  if (result != 0)
	    exit (1);
	}
      outbytesleft += text2_len - split;
      if (scm_mb_iconv (context, &inptr, &inbytesleft, &outptr, &outbytesleft)
	  != 0)
	exit (1);
      if (outptr - buf != text2_len
	  || outbytesleft != 0
	  || inptr - text1 != text1_len
	  || inbytesleft != 0
	  || memcmp (buf, text2, text2_len))
	exit (1);
      scm_mb_iconv_close (context);
    }
}


static void
test_conversions ()
{
  int i;
  struct scm_mb_iconv *context;

  /* Just try opening each conversion, each way.  */
  for (i = 0; all_encodings[i]; i++)
    {
      context = scm_mb_iconv_open ("guile", all_encodings[i]);
      scm_mb_iconv_close (context);

      context = scm_mb_iconv_open (all_encodings[i], "guile");
      scm_mb_iconv_close (context);
    }

  /* Convert some text back and forth.  */
  for (i = 0; texts[i].encoding; i++)
    {
      struct text *text= &texts[i];

      /* Go in both directions.  */
      one_conversion ("guile", text->encoding,
		      text->guile, text->original);
      one_conversion (text->encoding, "guile",
		      text->original, text->guile);
    }
}


/* Main function.  */

static void 
main_prog (int argc, char *argv[])
{
  test_one_char_encodings ();
  test_string_encodings ();
  test_conversions ();
}

int 
main (int argc, char *argv[])
{
  gh_enter (argc, argv, main_prog);
  return 0;
}

