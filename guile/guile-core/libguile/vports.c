/*	Copyright (C) 1995,1996,1998,1999 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include "_scm.h"
#include "eval.h"
#include "chars.h"
#include "fports.h"

#include "scm_validate.h"
#include "vports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/* {Ports - soft ports}
 * 
 */


static void
sf_flush (SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  SCM stream = pt->stream;

  if (pt->write_pos > pt->write_buf)
    {
      /* write the byte. */
      scm_apply (SCM_VELTS (stream)[0], SCM_MAKICHR (*pt->write_buf),
		 scm_listofnull);
      pt->write_pos = pt->write_buf;
  
      /* flush the output.  */
      {
	SCM f = SCM_VELTS (stream)[2];

	if (f != SCM_BOOL_F)
	  scm_apply (f, SCM_EOL, SCM_EOL);
      }
    }
}

static void
sf_write (SCM port, void *data, size_t size)
{
  SCM p = SCM_STREAM (port);

  scm_apply (SCM_VELTS (p)[1], 
	     scm_cons (scm_makfromstr ((char *) data, size, 0), SCM_EOL),
	     SCM_EOL);
}

/* calling the flush proc (element 2) is in case old code needs it,
   but perhaps softports could the use port buffer in the same way as
   fports.  */

/* places a single char in the input buffer.  */
static int 
sf_fill_input (SCM port)
{
  SCM p = SCM_STREAM (port);
  SCM ans;

  ans = scm_apply (SCM_VELTS (p)[3], SCM_EOL, SCM_EOL); /* get char.  */
  if (SCM_FALSEP (ans) || SCM_EOF_OBJECT_P (ans))
    return EOF;
  SCM_ASSERT (SCM_ICHRP (ans), ans, SCM_ARG1, "sf_fill_input");
  {
    scm_port *pt = SCM_PTAB_ENTRY (port);    

    *pt->read_buf = SCM_ICHR (ans);
    pt->read_pos = pt->read_buf;
    pt->read_end = pt->read_buf + 1;
    return *pt->read_buf;
  }
}


static int 
sf_close (SCM port)
{
  SCM p = SCM_STREAM (port);
  SCM f = SCM_VELTS (p)[4];
  if (SCM_BOOL_F == f)
    return 0;
  f = scm_apply (f, SCM_EOL, SCM_EOL);
  errno = 0;
  return SCM_BOOL_F == f ? EOF : 0;
}



GUILE_PROC(scm_make_soft_port, "make-soft-port", 2, 0, 0,
           (SCM pv, SCM modes),
"")
#define FUNC_NAME s_scm_make_soft_port
{
  scm_port *pt;
  SCM z;
  SCM_VALIDATE_VECTOR_LEN(1,pv,5);
  SCM_VALIDATE_ROSTRING(2,modes);
  SCM_COERCE_SUBSTR (modes);
  SCM_NEWCELL (z);
  SCM_DEFER_INTS;
  pt = scm_add_to_port_table (z);
  SCM_SETCAR (z, scm_tc16_sfport | scm_mode_bits (SCM_ROCHARS (modes)));
  SCM_SETPTAB_ENTRY (z, pt);
  SCM_SETSTREAM (z, pv);
  pt->read_pos = pt->read_buf = pt->read_end = &pt->shortbuf;
  pt->write_buf = pt->write_pos = &pt->shortbuf;
  pt->read_buf_size = pt->write_buf_size = 1;
  pt->write_end = pt->write_buf + pt->write_buf_size;
  pt->rw_random = 0;
  SCM_ALLOW_INTS;
  return z;
}
#undef FUNC_NAME


void scm_make_sfptob (void); /* Called from ports.c */

void
scm_make_sfptob ()
{
  long tc = scm_make_port_type ("soft", sf_fill_input, sf_write);
  scm_set_port_mark (tc, scm_markstream);
  scm_set_port_flush (tc, sf_flush);
  scm_set_port_close (tc, sf_close);
}

void
scm_init_vports ()
{
#include "vports.x"
}
