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

/* Much code here is borrowed from Tom Tromey's libunicode --- thanks,
   Tom!  Unfortunately, I can't simply use libunicode itself at the
   moment, since our canonical internal encoding is different.  When
   Emacs and Guile switch to UTF-8, hopefully that will change.  */


/* Headers.  */

#include "_scm.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "mbconv.h"

#ifdef HAVE_ICONV
#  include <iconv.h>
#else
/* This is harmless as it is completely ignored by the code when the
   above condition is false.  */
typedef void *iconv_t;
#endif


/* Exceptions.  */

SCM_GLOBAL_SYMBOL (scm_text_unknown_encoding, "text:unknown-encoding");
static const char text_unknown_encoding_msg[] =
  "unable to convert from encoding `%s' to encoding `%s'";


/* The implementation of the conversion context object.  */

struct scm_mb_iconv_ours
{
  /* Converter from source character set to Guile character set.  */
  struct scm_mb_encoding *from;

  /* Converter private data.  */
  void *from_data;

  /* Converter from Guile character set to destination character set.  */
  struct scm_mb_encoding *to;

  /* Converter private data.  */
  void *to_data;

  /* Buffer used to store intermediate results.  */
  scm_char_t *buffer;

  /* Number of valid characters in buffer.  */
  size_t valid;

  /* Total number of character slots in buffer.  */
  size_t size;
};

struct scm_mb_iconv
{
  /* Type of converter.  */
  enum {
    scm_mb_iconv_type_native,
    scm_mb_iconv_type_ours
  } type;

  union
  {
    iconv_t native;		   /* Used in `native' case.  */
    struct scm_mb_iconv_ours ours; /* Used in `our' case.  */
  } u;
};


/* The registry of all encodings.  */

/* Linked list of all character sets.  */
static struct scm_mb_encoding *encodings;

void
scm_mb_register_encoding (struct scm_mb_encoding *vec)
{
  vec->next = encodings;
  encodings = vec;
}

static struct scm_mb_encoding *
find_encoding (const char *name)
{
  struct scm_mb_encoding *cs;

  for (cs = encodings; cs; cs = cs->next)
    {
      int i;
      for (i = 0; cs->names[i]; ++i)
	{
	  if (! strcasecmp (cs->names[i], name))
	    return cs;
	}
    }

  return cs;
}


/* Creating and destroying conversion contexts.  */

static void
no_memory_for_open (struct scm_mb_iconv *r)
{
  if (r)
    {
      if (r->u.ours.buffer) free (r->u.ours.buffer);
      free (r);
    }

  scm_memory_error ("scm_mb_iconv_open");
}

struct scm_mb_iconv *
scm_mb_iconv_open (const char *tocode, const char *fromcode)
{
  struct scm_mb_iconv *r = (struct scm_mb_iconv *) malloc (sizeof (*r));

  if (! r)
    no_memory_for_open (r);

  r->u.ours.buffer = 0;

  /* Try our encodings first.  */
  r->u.ours.from = find_encoding (fromcode);
  if (r->u.ours.from)
    r->u.ours.to = find_encoding (tocode);
  if (r->u.ours.from && r->u.ours.to)
    {
      r->type = scm_mb_iconv_type_ours;

      /* FIXME: how to pick the size?  */
      r->u.ours.size = 1024;
      r->u.ours.buffer = (scm_char_t *) malloc (r->u.ours.size
						* sizeof (scm_char_t));
      if (! r->u.ours.buffer)
	no_memory_for_open (r);

      r->u.ours.valid = 0;

      if (r->u.ours.from->init
	  && ! r->u.ours.from->init (&r->u.ours.from_data))
	no_memory_for_open (r);

      if (r->u.ours.to->init && ! r->u.ours.to->init (&r->u.ours.to_data))
	{
	  if (r->u.ours.from->destroy)
	    r->u.ours.from->destroy (&r->u.ours.from_data);
	  no_memory_for_open (r);
	}

      return r;
    }

#ifdef HAVE_ICONV
  /* We don't have the conversions the user requested.  Try the
     system's iconv library.  */
  r->u.native = iconv_open (tocode, fromcode);
  if (r->u.native != (iconv_t) -1)
    {
      r->type = scm_mb_iconv_type_native;
      return r;
    }
#endif

  free (r);

  {
    SCM args = SCM_LIST2 (scm_makfrom0str (fromcode),
			  scm_makfrom0str (tocode));

    scm_error (scm_text_unknown_encoding, "scm_mb_iconv_open",
	       text_unknown_encoding_msg, args, args);
  }

  return 0;
}

void
scm_mb_iconv_close (struct scm_mb_iconv *cd)
{
  switch (cd->type)
    {
    case scm_mb_iconv_type_native:
#ifdef HAVE_ICONV
      if (iconv_close (cd->u.native) < 0)
	scm_syserror ("scm_mb_iconv_close");
      break;
#else
      abort ();
#endif

    case scm_mb_iconv_type_ours:
      if (cd->u.ours.to->destroy)
	cd->u.ours.to->destroy (&cd->u.ours.to_data);
      if (cd->u.ours.from->destroy)
	cd->u.ours.from->destroy (&cd->u.ours.from_data);
      free (cd->u.ours.buffer);
      break;
    }

  free (cd);
}


/* Actually converting text.  */

size_t
scm_mb_iconv (struct scm_mb_iconv *context,
	      const char **inbuf,  size_t *inbytesleft,
	      char **outbuf,       size_t *outbytesleft)
{
  struct scm_mb_iconv_ours *ours;

#ifdef HAVE_ICONV
  if (context->type == scm_mb_iconv_type_native)
    {
      size_t result = iconv (context->u.native,
			     inbuf, inbytesleft,
			     outbuf, outbytesleft);
      if (result == (size_t) -1)
	switch (errno)
	  {
	  case EILSEQ: return scm_mb_iconv_bad_encoding;
	  case E2BIG:  return scm_mb_iconv_more_room;
	  case EINVAL: return scm_mb_iconv_incomplete_encoding;
	  default:
	    scm_syserror ("scm_mb_iconv");
	  }
      else
	return result;
    }
#endif /* HAVE_ICONV */

  ours = &context->u.ours;
  
  /* If inbuf or *inbuf is zero, then we need to reset the conversion
     states.  */
  if (! inbuf || ! *inbuf)
    {
      int result;

      /* Try to reset the output conversion first; if it fails, we need to
	 leave the input conversion state untouched.  */
      result = ours->to->reset (ours->to_data, outbuf, outbytesleft);
      if (result < 0)
	return result;

      /* Reset the input context.  */
      return ours->from->reset (ours->from_data, 0, 0);
    }

  if (! outbuf || *outbytesleft <= 0)
    return scm_mb_iconv_more_room;

  /* Oh!  We actually have some *TEXT* to convert!  Not bureaucracy!!!  */
  while (*inbytesleft > 0)
    {
      /* Convert as many characters as possible from the input buffer
	 into the intermediate scm_char_t buffer.  */
      {
	scm_char_t *buf = ours->buffer + ours->valid;
	size_t buf_left = ours->size - ours->valid;
	enum scm_mb_read_result read_result
	  = ours->from->read (ours->from_data, inbuf, inbytesleft,
			      &buf, &buf_left);
	ours->valid = ours->size - buf_left;

	if (*inbytesleft < 0)
	  abort ();

	switch (read_result)
	  {
	  case scm_mb_read_ok:
	    break;
	  case scm_mb_read_incomplete:
	    return scm_mb_iconv_incomplete_encoding;
	  case scm_mb_read_error:
	    return scm_mb_iconv_bad_encoding;
	  default:
	    abort ();
	  }
      }

      /* Convert as many characters as possible from the intermediate
         scm_char_t buffer to the output buffer.  */
      {
	scm_char_t *buf = ours->buffer;
	size_t buf_left = ours->valid;
	enum scm_mb_write_result write_result
	  = ours->to->write (ours->to_data, &buf, &buf_left,
			     outbuf, outbytesleft);
	if (buf_left > 0)
	  memmove (ours->buffer, buf, buf_left * sizeof (scm_char_t));
	ours->valid = buf_left;
	switch (write_result)
	  {
	  case scm_mb_write_ok:
	    break;
	  case scm_mb_write_more_room:
	    return scm_mb_iconv_more_room;
	  default:
	    abort ();
	  }
      }
    }
    
  return 0;
}

#if 0

/* Input conversion ports.  */

/* Given that PORT is an input port containing data in the encoding
   named ENCODING, return a new input port carrying the same data as
   PORT, converted into Guile's internal encoding.  The resulting input
   port is not seekable.  */
SCM_PROC (s_convert_input_port, "convert-input-port", 2, 0, 0, scm_convert_input_port);
SCM
scm_convert_input_port (SCM port, SCM encoding)
{
  
}


/* Output conversion ports.  */
/* Return a new port which accepts data in Guile's internal encoding,
   and sends it to PORT in ENCODING.  PORT must be an output port, and
   ENCODING must be the name of an encoding.  */
SCM
scm_convert_output_port (SCM port, SCM encoding)
{
  
}
#endif


/* Initialization.  */

void
scm_init_mbconv ()
{
#include "mbconv.x"
}
