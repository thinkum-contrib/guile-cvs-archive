#ifndef SCM_MBCONV_H
#define SCM_MBCONV_H

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
#include "mb.h"

/* Here are functions for converting between various textual
   encodings.  These are all documented in ref/mbapi.texi, which is
   part of the guile-doc CVS module.  */


/* Exceptions.  */

extern SCM scm_text_unknown_encoding;


/* Converting text.  */

/* The type for conversion context objects.  */
struct scm_mb_iconv;

/* Create a conversion context object for converting text from
   FROMCODE to TOCODE.  */
extern struct scm_mb_iconv *scm_mb_iconv_open (const char *tocode,
					       const char *fromcode);

/* Error return values for scm_mb_iconv.  */
enum scm_mb_iconv_result {
  scm_mb_iconv_more_room = -1,
  scm_mb_iconv_bad_encoding = -2,
  scm_mb_iconv_incomplete_encoding = -3
};

/* Convert the *INBYTESLEFT bytes at *INBUF according to CONTEXT, and
   place the output in the buffer at *OUTBUF, which is *OUTBYTESLEFT
   bytes long.  Adjust the buffers to indicate input consumed and
   output produced.  Return a count of the number of characters which
   were converted questionably, or one of the scm_mb_iconv values
   above.  */
extern size_t scm_mb_iconv (struct scm_mb_iconv *context,
			    const char **inbuf,  size_t *inbytesleft,
			    char **outbuf,       size_t *outbytesleft);

/* Free a conversion context.  */			    
extern void scm_mb_iconv_close (struct scm_mb_iconv *context);


/* Adding new textual conversions.  */

/* These are the values a read function might return.  */
enum scm_mb_read_result
{
  /* Read was ok, consumed at least one byte or produced at least one
     character.  */
  scm_mb_read_ok,

  /* Input is incomplete, nothing was consumed or produced.
     This could be okay, if you have more text to pass through, but it's
     an error if you've provided all the input text you've got.  */
  scm_mb_read_incomplete,

  /* Invalid sequence.  */
  scm_mb_read_error
};

/* These are the values a write might return.  */
enum scm_mb_write_result
{
  /* Write was ok.  */
  scm_mb_write_ok,

  /* Write needs more room.  */
  scm_mb_write_more_room
};


/* This is the type representing an encoding.  */
struct scm_mb_encoding
{
  /* NULL terminated array of names of elements.  */
  char **names;

  /* Initialize private data.  NULL means no init necessary.  Return 0
     on error.  */
  int (*init) (void **privp);

  /* Destroy private data.  NULL means no destroy necessary.  */
  void (*destroy) (void **privp);

  /* Put output buffer into initial shift state.  If NULL, this
     encoding doesn't use a shift state.  Returns error indicator like
     iconv(); see man page.  */
  int (*reset) (void *priv, char **outbuf, size_t *outbytesleft);

  /* Read some bytes and convert into an array of scm_char_t characters.

     On entry, there are *INBYTESLEFT bytes of text at *INBUF to
     be converted, and *OUTCHARSLEFT characters available at *OUTBUF
     to hold the results.
  
     On exit, *INBYTESLEFT and *INBUF indicate the input bytes still not
     consumed.  *OUTCHARSLEFT and *OUTBUF indicate the output buffer
     space still not filled.  (By exclusion, these indicate which input
     bytes were consumed, and which output characters were produced.)

     Return one of the scm_mb_read_ values.  */
  enum scm_mb_read_result
    (*read) (void *priv,
             const char **inbuf,  size_t *inbytesleft,
             scm_char_t **outbuf, size_t *outcharsleft);

  /* Convert an array of scm_char_t characters to output bytes.

     On entry, there are *INCHARSLEFT Guile characters available at
     *INBUF, and *OUTBYTESLEFT bytes available to store output at
     *OUTBUF.

     On exit, *INCHARSLEFT and *INBUF indicate the number of Guile
     characters left unconverted (because there was insufficient room in
     the output buffer to hold their converted forms), and *OUTBYTESLEFT
     and *OUTBUF indicate the unused portion of the output buffer.

     Return one of the scm_mb_write_ values.  */
  enum scm_mb_write_result
    (*write) (void *priv,
              const scm_char_t **inbuf,  size_t *incharsleft,
              char             **outbuf, size_t *outbytesleft);

  /* Link.  */
  struct scm_mb_encoding *next;
};

/* Register a new encoding.  */
extern void scm_mb_register_encoding (struct scm_mb_encoding *encoding);



/* Conversion ports.  */

extern SCM scm_convert_input_port (SCM port, SCM encoding);
extern SCM scm_convert_output_port (SCM port, SCM encoding);


extern void scm_init_mbconv (void);

#endif  /* SCM_MBCONV_H */
