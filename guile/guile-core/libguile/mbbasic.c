/* mbbasic.c --- conversions to and from very common encodings.

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

#include <stddef.h>
#include <stdlib.h>

#include "_scm.h"
#include "mb.h"
#include "mbconv.h"
#include "mbbasic.h"

#include "mbemacs.h"


/* The Emacs-Mule encoding.  */

static struct scm_mb_encoding emacs_mule_encoding;

static enum scm_mb_read_result
emacs_mule_read (void *cookie,
		 const char **inbuf,  size_t *inbytesleft,
		 scm_char_t **outbuf, size_t *outcharsleft)
{
  const char *in = *inbuf;
  const char *in_end = in + *inbytesleft;
  scm_char_t *out = *outbuf;
  scm_char_t *out_end = out + *outcharsleft;

  while (in < in_end && out < out_end)
    {
      const char *next = in + scm_mb_len (*in);
      if (next > in_end)
	{
	  *inbuf = in;
	  *inbytesleft = in_end - in;
	  *outbuf = out;
	  *outcharsleft = out_end - out;

	  return scm_mb_read_incomplete;
	}
      *out++ = scm_mb_get (in);
      in = next;
    }

  *inbuf = in;
  *inbytesleft = in_end - in;
  *outbuf = out;
  *outcharsleft = out_end - out;

  return scm_mb_read_ok;
}

static enum scm_mb_write_result
emacs_mule_write (void *cookie,
		  const scm_char_t **inbuf,  size_t *incharsleft,
		  char             **outbuf, size_t *outbytesleft)
{
  const scm_char_t *in = *inbuf;
  const scm_char_t *in_end = in + *incharsleft;
  char *out = *outbuf;
  char *out_end = out + *outbytesleft;
  
  while (in < in_end)
    {
      const char *next = out + scm_mb_char_len (*in);
      if (next > out_end)
	{
	  *inbuf = in;
	  *incharsleft = in_end - in;
	  *outbuf = out;
	  *outbytesleft = out_end - out;

	  return scm_mb_write_too_big;
	}

      out += scm_mb_put (*out, in);
      in++;
    }

  *inbuf = in;
  *incharsleft = in_end - in;
  *outbuf = out;
  *outbytesleft = out_end - out;

  return scm_mb_write_ok;
}



/* The ISO 8859 encodings.  */

/* Conversions to and from these encodings are trivial.  The lower 128
   characters are identical to ASCII, and can be passed through
   directly.  The upper 128 characters map directly onto the
   corresponding CHAR1O character codes.  No tables needed.

   All these encoding share the same read and write functions; the
   only thing which varies from one to the next is the init function,
   which stores the character set number in the context's cookie for
   this conversion.

   Perhaps we can cram CHARSET_KATAKANA_JISX0201 and
   CHARSET_LATIN_JISX0201 into this system, too.  */

static enum scm_mb_read_result
iso8859_read (void *priv,
	      const char **inbuf,  size_t *inbytesleft,
	      scm_char_t **outbuf, size_t *outcharsleft)
{
  unsigned char charset = * (unsigned char *) priv;
  const unsigned char *in = *inbuf;
  const unsigned char *in_end = in + *inbytesleft;
  scm_char_t *out = *outbuf;
  scm_char_t *out_end = out + *outcharsleft;

  while (in < in_end && out < out_end)
    {
      if (*in < 0x80)
	*out++ = *in;
      else if (*in >= 0xA0)
	*out++ = BUILD_CHAR1 (charset, (*in | 0x80));
      /* We just drop characters in the range 0x80 .. 0xA0.  */
    }

  *inbuf = in;
  *inbytesleft = in_end - in;
  *outbuf = out;
  *outcharsleft = out_end - out;

  return scm_mb_read_ok;
}

static enum scm_mb_write_result
iso8859_write (void *priv,
	       const scm_char_t **inbuf,  size_t *incharsleft,
	       char             **outbuf, size_t *outbytesleft)
{
  unsigned char charset = * (unsigned char *) priv;
  const scm_char_t *in = *inbuf;
  const scm_char_t *in_end = in + *incharsleft;
  char *out = *outbuf;
  char *out_end = out + *outbytesleft;
  
  while (in < in_end)
    {
      scm_char_t c;

      if (out >= out_end)
	{
	  *inbuf = in;
	  *incharsleft = in_end - in;
	  *outbuf = out;
	  *outbytesleft = out_end - out;

	  return scm_mb_write_too_big;
	}
      
      c = *in++;
      if (IS_ASCII_CHAR (c))
	*out++ = c;
      else if (FIRST_CHAR1O <= c && c <= LAST_CHAR1O
	       && CHAR1_SET (c) == charset)
	*out++ = CHAR1_POS (c) | 0x80;
      else
	/* We just eat characters not present in our character set.  I
	   hope this doesn't screw anyone.  */
	;
    }

  *inbuf = in;
  *incharsleft = in_end - in;
  *outbuf = out;
  *outbytesleft = out_end - out;

  return scm_mb_write_ok;
}

#define ISO8859_INIT(charset, number)			\
static int						\
iso8859_ ## number ## _init (void **privp)		\
{							\
  static unsigned char charset_num = number;		\
  *privp = (void *) &charset_num;			\
  return 1;						\
}							\
static char *iso8859_ ## number ##_names[] = {		\
  "ISO-8859-" ## # number, "Latin-" ## #number, 0	\
};

ISO8859_INIT(CHARSET_LATIN_ISO8859_1, 1)
ISO8859_INIT(CHARSET_LATIN_ISO8859_2, 2)
ISO8859_INIT(CHARSET_LATIN_ISO8859_3, 3)
ISO8859_INIT(CHARSET_LATIN_ISO8859_4, 4)
ISO8859_INIT(CHARSET_CYRILLIC_ISO8859_5, 5)
ISO8859_INIT(CHARSET_ARABIC_ISO8859_6, 6)
ISO8859_INIT(CHARSET_GREEK_ISO8859_7, 7)
ISO8859_INIT(CHARSET_HEBREW_ISO8859_8, 8)
ISO8859_INIT(CHARSET_LATIN_ISO8859_9, 9)

#define ISO8859_ENTRY(number)				\
  {							\
    iso8859_ ## number ## _names,			\
    iso8859_ ## number ## _init,			\
    0, 0, iso8859_read, iso8859_write, 0		\
  }

static struct scm_mb_encoding iso8859_encodings[] =
{
  ISO8859_ENTRY(1),
  ISO8859_ENTRY(2),
  ISO8859_ENTRY(3),
  ISO8859_ENTRY(4),
  ISO8859_ENTRY(5),
  ISO8859_ENTRY(6),
  ISO8859_ENTRY(7),
  ISO8859_ENTRY(8),
  ISO8859_ENTRY(9)
};


/* The US-ASCII encoding.  */

static struct scm_mb_encoding us_ascii_encoding;

static enum scm_mb_read_result
us_ascii_read (void *priv,
	       const char **inbuf,  size_t *inbytesleft,
	       scm_char_t **outbuf, size_t *outcharsleft)
{
  const unsigned char *in = *inbuf;
  const unsigned char *in_end = in + *inbytesleft;
  scm_char_t *out = *outbuf;
  scm_char_t *out_end = out + *outcharsleft;

  while (in < in_end && out < out_end)
    {
      if (*in < 0x80)
	*out++ = *in;
      /* We just drop characters outside the range 0 .. 0x7F.  */
    }

  *inbuf = in;
  *inbytesleft = in_end - in;
  *outbuf = out;
  *outcharsleft = out_end - out;

  return scm_mb_read_ok;
}


static enum scm_mb_write_result
us_ascii_write (void *priv,
		const scm_char_t **inbuf,  size_t *incharsleft,
		char             **outbuf, size_t *outbytesleft)
{
  const scm_char_t *in = *inbuf;
  const scm_char_t *in_end = in + *incharsleft;
  char *out = *outbuf;
  char *out_end = out + *outbytesleft;
  
  while (in < in_end)
    {
      scm_char_t c;

      if (out >= out_end)
	{
	  *inbuf = in;
	  *incharsleft = in_end - in;
	  *outbuf = out;
	  *outbytesleft = out_end - out;

	  return scm_mb_write_too_big;
	}
      
      c = *in++;
      if (IS_ASCII_CHAR (c))
	*out++ = c;
      /* We just eat non-ASCII characters.  */
    }

  *inbuf = in;
  *incharsleft = in_end - in;
  *outbuf = out;
  *outbytesleft = out_end - out;

  return scm_mb_write_ok;
}


/* Initialization.  */

void
scm_init_mbbasic ()
{
  /* The Emacs-Mule encoding.
     When Guile switches to a UTF-8-like encoding, we'll move the name
     "Guile" to that encoding.  */
  {
    static char *names[] = { "Emacs-Mule", "Guile", 0 };
    memset (&emacs_mule_encoding, 0, sizeof (emacs_mule_encoding));
    emacs_mule_encoding.names = names;
    emacs_mule_encoding.read = emacs_mule_read;
    emacs_mule_encoding.write = emacs_mule_write;

    scm_mb_register_encoding (&emacs_mule_encoding);
  }

  /* ISO 8859 character sets.  */
  {
    int i;

    for (i = 0;
	 i < (sizeof (iso8859_encodings) / sizeof (iso8859_encodings[0]));
	 i++)
      scm_mb_register_encoding (&iso8859_encodings[i]);
  }

  /* The US-ASCII encoding.  */
  {
    static char *names[] = { "US-ASCII", "ASCII", 0 };

    memset (&us_ascii_encoding, 0, sizeof (us_ascii_encoding));
    us_ascii_encoding.names = names;
    us_ascii_encoding.read = us_ascii_read;
    us_ascii_encoding.write = us_ascii_write;

    scm_mb_register_encoding (&us_ascii_encoding);
  }
}
