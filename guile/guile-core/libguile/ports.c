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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


/* Headers.  */

#include <stdio.h>
#include "_scm.h"
#include "objects.h"
#include "smob.h"
#include "chars.h"

#include "keywords.h"

#include "scm_validate.h"
#include "ports.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif


/* The port kind table --- a dynamically resized array of port types.  */


/* scm_ptobs scm_numptob
 * implement a dynamicly resized array of ptob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */
scm_ptob_descriptor *scm_ptobs;
int scm_numptob;

/* GC marker for a port with stream of SCM type.  */
SCM 
scm_markstream (SCM ptr)
{
  int openp;
  openp = SCM_CAR (ptr) & SCM_OPN;
  if (openp)
    return SCM_STREAM  (ptr);
  else
    return SCM_BOOL_F;
}

/*
 * We choose to use an interface similar to the smob interface with
 * fill_input and write as standard fields, passed to the port
 * type constructor, and optional fields set by setters.
 */

static void flush_void_port (SCM port);
static void end_input_void_port (SCM port, int offset);
static void write_void_port (SCM port, void *data, size_t size);

long 
scm_make_port_type (char *name,
		    int (*fill_input) (SCM port),
		    void (*write) (SCM port, void *data, size_t size))
{
  char *tmp;
  if (255 <= scm_numptob)
    goto ptoberr;
  SCM_DEFER_INTS;
  SCM_SYSCALL (tmp = (char *) realloc ((char *) scm_ptobs,
				       (1 + scm_numptob)
				       * sizeof (scm_ptob_descriptor)));
  if (tmp)
    {
      scm_ptobs = (scm_ptob_descriptor *) tmp;

      scm_ptobs[scm_numptob].name = name;
      scm_ptobs[scm_numptob].mark = 0;
      scm_ptobs[scm_numptob].free = scm_free0;
      scm_ptobs[scm_numptob].print = scm_port_print;
      scm_ptobs[scm_numptob].equalp = 0;
      scm_ptobs[scm_numptob].close = 0;

      scm_ptobs[scm_numptob].write = write;
      scm_ptobs[scm_numptob].flush = flush_void_port;

      scm_ptobs[scm_numptob].end_input = end_input_void_port;
      scm_ptobs[scm_numptob].fill_input = fill_input;
      scm_ptobs[scm_numptob].input_waiting = 0;

      scm_ptobs[scm_numptob].seek = 0;
      scm_ptobs[scm_numptob].truncate = 0;

      scm_numptob++;
    }
  SCM_ALLOW_INTS;
  if (!tmp)
  ptoberr:scm_wta (SCM_MAKINUM ((long) scm_numptob),
		   (char *) SCM_NALLOC, "scm_make_port_type");
  /* Make a class object if Goops is present */
  if (scm_port_class)
    scm_make_port_classes (scm_numptob - 1, SCM_PTOBNAME (scm_numptob - 1));
  return scm_tc7_port + (scm_numptob - 1) * 256;
}

void
scm_set_port_mark (long tc, SCM (*mark) (SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].mark = mark;
}

void
scm_set_port_free (long tc, scm_sizet (*free) (SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].free = free;
}

void
scm_set_port_print (long tc, int (*print) (SCM exp, SCM port,
					   scm_print_state *pstate))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].print = print;
}

void
scm_set_port_equalp (long tc, SCM (*equalp) (SCM, SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].equalp = equalp;
}

void
scm_set_port_flush (long tc, void (*flush) (SCM port))
{
   scm_ptobs[SCM_TC2PTOBNUM (tc)].flush = flush;
}

void
scm_set_port_end_input (long tc, void (*end_input) (SCM port, int offset))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].end_input = end_input;
}

void
scm_set_port_close (long tc, int (*close) (SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].close = close;
}

void
scm_set_port_seek (long tc, off_t (*seek) (SCM port,
					   off_t OFFSET,
					   int WHENCE))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].seek = seek;
}

void
scm_set_port_truncate (long tc, void (*truncate) (SCM port, off_t length))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].truncate = truncate;
}

void
scm_set_port_input_waiting (long tc, int (*input_waiting) (SCM))
{
  scm_ptobs[SCM_TC2PTOBNUM (tc)].input_waiting = input_waiting;
}



GUILE_PROC(scm_char_ready_p, "char-ready?", 0, 1, 0, 
           (SCM port),
"")
#define FUNC_NAME s_scm_char_ready_p
{
  scm_port *pt;

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_VALIDATE_OPINPORT(1,port);

  pt = SCM_PTAB_ENTRY (port);

  /* if the current read buffer is filled, or the
     last pushed-back char has been read and the saved buffer is
     filled, result is true.  */
  if (pt->read_pos < pt->read_end 
      || (pt->read_buf == pt->putback_buf
	  && pt->saved_read_pos < pt->saved_read_end))
    return SCM_BOOL_T;
  else
    {
      scm_ptob_descriptor *ptob = &scm_ptobs[SCM_PTOBNUM (port)];
      
      if (ptob->input_waiting)
	return SCM_BOOL(ptob->input_waiting (port));
      else
	return SCM_BOOL_T;
    }
}
#undef FUNC_NAME

/* Clear a port's read buffers, returning the contents.  */
GUILE_PROC (scm_drain_input, "drain-input", 1, 0, 0, 
            (SCM port),
"")
#define FUNC_NAME s_scm_drain_input
{
  SCM result;
  scm_port *pt = SCM_PTAB_ENTRY (port);
  int count;
  char *dst;

  SCM_VALIDATE_OPINPORT(1,port);

  count = pt->read_end - pt->read_pos;
  if (pt->read_buf == pt->putback_buf)
    count += pt->saved_read_end - pt->saved_read_pos;

  result = scm_makstr (count, 0);
  dst = SCM_CHARS (result);

  while (pt->read_pos < pt->read_end)
    *dst++ = *(pt->read_pos++);
  
  if (pt->read_buf == pt->putback_buf)
    {
      while (pt->saved_read_pos < pt->saved_read_end)
	*dst++ = *(pt->saved_read_pos++);
    }

  return result;
}
#undef FUNC_NAME


/* Standard ports --- current input, output, error, and more(!).  */

GUILE_PROC(scm_current_input_port, "current-input-port", 0, 0, 0,
           (),
"")
#define FUNC_NAME s_scm_current_input_port
{
  return scm_cur_inp;
}
#undef FUNC_NAME

GUILE_PROC(scm_current_output_port, "current-output-port", 0, 0, 0,
           (),
"")
#define FUNC_NAME s_scm_current_output_port
{
  return scm_cur_outp;
}
#undef FUNC_NAME

GUILE_PROC(scm_current_error_port, "current-error-port", 0, 0, 0,
           (),
"")
#define FUNC_NAME s_scm_current_error_port
{
  return scm_cur_errp;
}
#undef FUNC_NAME

GUILE_PROC(scm_current_load_port, "current-load-port", 0, 0, 0,
           (),
"")
#define FUNC_NAME s_scm_current_load_port
{
  return scm_cur_loadp;
}
#undef FUNC_NAME

GUILE_PROC(scm_set_current_input_port, "set-current-input-port", 1, 0, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_set_current_input_port
{
  SCM oinp = scm_cur_inp;
  SCM_VALIDATE_OPINPORT(1,port);
  scm_cur_inp = port;
  return oinp;
}
#undef FUNC_NAME


GUILE_PROC(scm_set_current_output_port, "set-current-output-port", 1, 0, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_set_current_output_port
{
  SCM ooutp = scm_cur_outp;
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPOUTPORT(1,port);
  scm_cur_outp = port;
  return ooutp;
}
#undef FUNC_NAME


GUILE_PROC(scm_set_current_error_port, "set-current-error-port", 1, 0, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_set_current_error_port
{
  SCM oerrp = scm_cur_errp;
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPOUTPORT(1,port);
  scm_cur_errp = port;
  return oerrp;
}
#undef FUNC_NAME


/* The port table --- an array of pointers to ports.  */

scm_port **scm_port_table;

int scm_port_table_size = 0;	/* Number of ports in scm_port_table.  */
int scm_port_table_room = 20;	/* Size of the array.  */

/* Add a port to the table.  */

scm_port *
scm_add_to_port_table (SCM port)
{
  scm_port *entry;

  if (scm_port_table_size == scm_port_table_room)
    {
      void *newt = realloc ((char *) scm_port_table,
			    (scm_sizet) (sizeof (scm_port *)
					 * scm_port_table_room * 2));
      if (newt == NULL)
	scm_memory_error ("scm_add_to_port_table");
      scm_port_table = (scm_port **) newt;
      scm_port_table_room *= 2;
    }
  entry = (scm_port *) malloc (sizeof (scm_port));
  if (entry == NULL)
    scm_memory_error ("scm_add_to_port_table");

  entry->port = port;
  entry->entry = scm_port_table_size;
  entry->revealed = 0;
  entry->stream = 0;
  entry->file_name = SCM_BOOL_F;
  entry->line_number = 0;
  entry->column_number = 0;
  entry->putback_buf = 0;
  entry->putback_buf_size = 0;
  entry->rw_active = SCM_PORT_NEITHER;
  entry->rw_random = 0;

  scm_port_table[scm_port_table_size] = entry;
  scm_port_table_size++;

  return entry;
}

/* Remove a port from the table and destroy it.  */

void
scm_remove_from_port_table (SCM port)
{
  scm_port *p = SCM_PTAB_ENTRY (port);
  int i = p->entry;

  if (i >= scm_port_table_size)
    scm_wta (port, "Port not in table", "scm_remove_from_port_table");
  if (p->putback_buf)
    free (p->putback_buf);
  free (p);
  /* Since we have just freed slot i we can shrink the table by moving
     the last entry to that slot... */
  if (i < scm_port_table_size - 1)
    {
      scm_port_table[i] = scm_port_table[scm_port_table_size - 1];
      scm_port_table[i]->entry = i;
    }
  SCM_SETPTAB_ENTRY (port, 0);
  scm_port_table_size--;
}

#ifdef GUILE_DEBUG
/* Undocumented functions for debugging.  */
/* Return the number of ports in the table.  */

GUILE_PROC(scm_pt_size, "pt-size", 0, 0, 0,
           (),
"")
#define FUNC_NAME s_scm_pt_size
{
  return SCM_MAKINUM (scm_port_table_size);
}
#undef FUNC_NAME

/* Return the ith member of the port table.  */
GUILE_PROC(scm_pt_member, "pt-member", 1, 0, 0,
           (SCM member),
"")
#define FUNC_NAME s_scm_pt_member
{
  int i;
  SCM_VALIDATE_INT_copy(1,member,i);
  if (i < 0 || i >= scm_port_table_size)
    return SCM_BOOL_F;
  else
    return scm_port_table[i]->port;
}
#undef FUNC_NAME
#endif



/* Revealed counts --- an oddity inherited from SCSH.  */

/* Find a port in the table and return its revealed count.
   Also used by the garbage collector.
 */

int
scm_revealed_count (SCM port)
{
  return SCM_REVEALED(port);
}



/* Return the revealed count for a port.  */

GUILE_PROC(scm_port_revealed, "port-revealed", 1, 0, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_port_revealed
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_PORT(1,port);
  return SCM_MAKINUM (scm_revealed_count (port));
}
#undef FUNC_NAME

/* Set the revealed count for a port.  */
GUILE_PROC(scm_set_port_revealed_x, "set-port-revealed!", 2, 0, 0,
           (SCM port, SCM rcount),
"")
#define FUNC_NAME s_scm_set_port_revealed_x
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_PORT(1,port);
  SCM_VALIDATE_INT(2,rcount);
  SCM_REVEALED (port) = SCM_INUM (rcount);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Retrieving a port's mode.  */

/* Return the flags that characterize a port based on the mode
 * string used to open a file for that port.
 *
 * See PORT FLAGS in scm.h
 */

long
scm_mode_bits (char *modes)
{
  return (SCM_OPN
	  | (strchr (modes, 'r') || strchr (modes, '+') ? SCM_RDNG : 0)
	  | (   strchr (modes, 'w')
	     || strchr (modes, 'a')
	     || strchr (modes, '+') ? SCM_WRTNG : 0)
	  | (strchr (modes, '0') ? SCM_BUF0 : 0)
	  | (strchr (modes, 'l') ? SCM_BUFLINE : 0));
}


/* Return the mode flags from an open port.
 * Some modes such as "append" are only used when opening
 * a file and are not returned here.  */

GUILE_PROC(scm_port_mode, "port-mode", 1, 0, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_port_mode
{
  char modes[3];
  modes[0] = '\0';

  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPPORT(1,port);
  if (SCM_CAR (port) & SCM_RDNG) {
    if (SCM_CAR (port) & SCM_WRTNG)
      strcpy (modes, "r+");
    else
      strcpy (modes, "r");
  }
  else if (SCM_CAR (port) & SCM_WRTNG)
    strcpy (modes, "w");
  if (SCM_CAR (port) & SCM_BUF0)
    strcat (modes, "0");
  return scm_makfromstr (modes, strlen (modes), 0);
}
#undef FUNC_NAME



/* Closing ports.  */

/* scm_close_port
 * Call the close operation on a port object. 
 * see also scm_close.
 */
GUILE_PROC(scm_close_port, "close-port", 1, 0, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_close_port
{
  scm_sizet i;
  int rv;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_PORT(1,port);
  if (SCM_CLOSEDP (port))
    return SCM_BOOL_F;
  i = SCM_PTOBNUM (port);
  if (scm_ptobs[i].close)
    rv = (scm_ptobs[i].close) (port);
  else
    rv = 0;
  scm_remove_from_port_table (port);
  SCM_SETAND_CAR (port, ~SCM_OPN);
  return SCM_NEGATE_BOOL(rv < 0);
}
#undef FUNC_NAME

GUILE_PROC(scm_close_all_ports_except, "close-all-ports-except", 0, 0, 1,
           (SCM ports),
"")
#define FUNC_NAME s_scm_close_all_ports_except
{
  int i = 0;
  SCM_VALIDATE_NIMCONS(1,ports);
  while (i < scm_port_table_size)
    {
      SCM thisport = scm_port_table[i]->port;
      int found = 0;
      SCM ports_ptr = ports;

      while (SCM_NNULLP (ports_ptr))
	{
	  SCM port = SCM_COERCE_OUTPORT (SCM_CAR (ports_ptr));
	  if (i == 0)
            SCM_VALIDATE_OPPORT(1,port);
	  if (port == thisport)
	    found = 1;
	  ports_ptr = SCM_CDR (ports_ptr);
	}
      if (found)
	i++;
      else
	/* i is not to be incremented here.  */
	scm_close_port (thisport);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Utter miscellany.  Gosh, we should clean this up some time.  */

GUILE_PROC(scm_input_port_p, "input-port?", 1, 0, 0,
           (SCM x),
"")
#define FUNC_NAME s_scm_input_port_p
{
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  return SCM_BOOL(SCM_INPORTP (x));
}
#undef FUNC_NAME

GUILE_PROC(scm_output_port_p, "output-port?", 1, 0, 0,
           (SCM x),
"")
#define FUNC_NAME s_scm_output_port_p
{
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  if (SCM_PORT_WITH_PS_P (x))
    x = SCM_PORT_WITH_PS_PORT (x);
  return SCM_BOOL(SCM_OUTPORTP (x));
}
#undef FUNC_NAME

GUILE_PROC(scm_port_closed_p, "port-closed?", 1, 0, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_port_closed_p
{
  SCM_VALIDATE_OPPORT(1,port);
  return SCM_NEGATE_BOOL(SCM_OPPORTP (port));
}
#undef FUNC_NAME

GUILE_PROC(scm_eof_object_p, "eof-object?", 1, 0, 0,
           (SCM x),
"")
#define FUNC_NAME s_scm_eof_object_p
{
  return SCM_BOOL(SCM_EOF_OBJECT_P (x));
}
#undef FUNC_NAME

GUILE_PROC(scm_force_output, "force-output", 0, 1, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_force_output
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    {
      port = SCM_COERCE_OUTPORT (port);
      SCM_VALIDATE_OPOUTPORT(1,port);
    }
  scm_flush (port);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

GUILE_PROC (scm_flush_all_ports, "flush-all-ports", 0, 0, 0,
            (),
"")
#define FUNC_NAME s_scm_flush_all_ports
{
  int i;

  for (i = 0; i < scm_port_table_size; i++)
    {
      if (SCM_OPOUTPORTP (scm_port_table[i]->port))
	scm_flush (scm_port_table[i]->port);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

GUILE_PROC(scm_read_char, "read-char", 0, 1, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_read_char
{
  int c;
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  SCM_VALIDATE_OPINPORT(1,port);
  c = scm_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  return SCM_MAKICHR (c);
}
#undef FUNC_NAME

/* this should only be called when the read buffer is empty.  it
   tries to refill the read buffer.  it returns the first char from
   the port, which is either EOF or *(pt->read_pos).  */
int
scm_fill_input (SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->read_buf == pt->putback_buf)
    {
      /* finished reading put-back chars.  */
      pt->read_buf = pt->saved_read_buf;
      pt->read_pos = pt->saved_read_pos;
      pt->read_end = pt->saved_read_end;
      pt->read_buf_size = pt->saved_read_buf_size;
      if (pt->read_pos < pt->read_end)
	return *(pt->read_pos);
    }
  return scm_ptobs[SCM_PTOBNUM (port)].fill_input (port);
}

int 
scm_getc (SCM port)
{
  int c;
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    {
      /* may be marginally faster than calling scm_flush.  */
      scm_ptobs[SCM_PTOBNUM (port)].flush (port);
    }
  
  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (pt->read_pos >= pt->read_end)
    {
      if (scm_fill_input (port) == EOF)
	return EOF;
    }

  c = *(pt->read_pos++);

  if (c == '\n')
    {
      SCM_INCLINE (port);
    }
  else if (c == '\t')
    {
      SCM_TABCOL (port);
    }
  else
    {
      SCM_INCCOL (port);
    }

  return c;
}

void 
scm_putc (char c, SCM port)
{
  scm_lfwrite (&c, 1, port);
}

void 
scm_puts (char *s, SCM port)
{
  scm_lfwrite (s, strlen (s), port);
}

void 
scm_lfwrite (char *ptr, scm_sizet size, SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  scm_ptob_descriptor *ptob = &scm_ptobs[SCM_PTOBNUM (port)];

  if (pt->rw_active == SCM_PORT_READ)
    scm_end_input (port);

  ptob->write (port, ptr, size);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_WRITE;
}


void 
scm_flush (SCM port)
{
  scm_sizet i = SCM_PTOBNUM (port);
  (scm_ptobs[i].flush) (port);
}

void
scm_end_input (SCM port)
{
  int offset;
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->read_buf == pt->putback_buf)
    {
      offset = pt->read_end - pt->read_pos;
      pt->read_buf = pt->saved_read_buf;
      pt->read_pos = pt->saved_read_pos;
      pt->read_end = pt->saved_read_end;
      pt->read_buf_size = pt->saved_read_buf_size;
    }
  else
    offset = 0;

  scm_ptobs[SCM_PTOBNUM (port)].end_input (port, offset);
}




void 
scm_ungetc (int c, SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->read_buf == pt->putback_buf)
    /* already using the put-back buffer.  */
    {
      /* enlarge putback_buf if necessary.  */
      if (pt->read_end == pt->read_buf + pt->read_buf_size
	  && pt->read_buf == pt->read_pos)
	{
	  int new_size = pt->read_buf_size * 2;
	  unsigned char *tmp = 
	    (unsigned char *) realloc (pt->putback_buf, new_size);

	  if (tmp == NULL)
	    scm_memory_error ("scm_ungetc");
	  pt->read_pos = pt->read_buf = pt->putback_buf = tmp;
	  pt->read_end = pt->read_buf + pt->read_buf_size;
	  pt->read_buf_size = pt->putback_buf_size = new_size;
	}

      /* shift any existing bytes to buffer + 1.  */
      if (pt->read_pos == pt->read_end)
	pt->read_end = pt->read_buf + 1;
      else if (pt->read_pos != pt->read_buf + 1)
	{
	  int count = pt->read_end - pt->read_pos;

	  memmove (pt->read_buf + 1, pt->read_pos, count);
	  pt->read_end = pt->read_buf + 1 + count;
	}

      pt->read_pos = pt->read_buf;
    }
  else
    /* switch to the put-back buffer.  */
    {
      if (pt->putback_buf == NULL)
	{
	  pt->putback_buf = (char *) malloc (SCM_INITIAL_PUTBACK_BUF_SIZE);
	  if (pt->putback_buf == NULL)
	    scm_memory_error ("scm_ungetc");
	  pt->putback_buf_size = SCM_INITIAL_PUTBACK_BUF_SIZE;
	}

      pt->saved_read_buf = pt->read_buf;
      pt->saved_read_pos = pt->read_pos;
      pt->saved_read_end = pt->read_end;
      pt->saved_read_buf_size = pt->read_buf_size;

      pt->read_pos = pt->read_buf = pt->putback_buf;
      pt->read_end = pt->read_buf + 1;
      pt->read_buf_size = pt->putback_buf_size;
    }

  *pt->read_buf = c;

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (c == '\n')
    {
      /* What should col be in this case?
       * We'll leave it at -1.
       */
      SCM_LINUM (port) -= 1;
    }
  else
    SCM_COL(port) -= 1;
}


void 
scm_ungets (char *s, int n, SCM port)
{
  /* This is simple minded and inefficient, but unreading strings is
   * probably not a common operation, and remember that line and
   * column numbers have to be handled...
   *
   * Please feel free to write an optimized version!
   */
  while (n--)
    scm_ungetc (s[n], port);
}


GUILE_PROC(scm_peek_char, "peek-char", 0, 1, 0,
           (SCM port),
"")
#define FUNC_NAME s_scm_peek_char
{
  int c;
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_VALIDATE_OPINPORT(1,port);
  c = scm_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  scm_ungetc (c, port);
  return SCM_MAKICHR (c);
}
#undef FUNC_NAME

GUILE_PROC (scm_unread_char, "unread-char", 2, 0, 0,
            (SCM cobj, SCM port),
"")
#define FUNC_NAME s_scm_unread_char
{
  int c;

  SCM_VALIDATE_CHAR(1,cobj);
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_VALIDATE_OPINPORT(1,port);

  c = SCM_ICHR (cobj);

  scm_ungetc (c, port);
  return cobj;
}
#undef FUNC_NAME

GUILE_PROC (scm_unread_string, "unread-string", 2, 0, 0,
            (SCM str, SCM port),
"")
#define FUNC_NAME s_scm_unread_string
{
  SCM_VALIDATE_STRING(1,str);
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_VALIDATE_OPINPORT(1,port);

  scm_ungets (SCM_ROUCHARS (str), SCM_LENGTH (str), port);
  
  return str;
}
#undef FUNC_NAME

GUILE_PROC (scm_seek, "seek", 3, 0, 0,
            (SCM object, SCM offset, SCM whence),
"")
#define FUNC_NAME s_scm_seek
{
  off_t off;
  off_t rv;
  int how;

  object = SCM_COERCE_OUTPORT (object);

  off = SCM_NUM2LONG (2,offset);
  SCM_VALIDATE_INT_COPY(3,whence,how);
  if (how != SEEK_SET && how != SEEK_CUR && how != SEEK_END)
    SCM_OUT_OF_RANGE (3, whence);
  if (SCM_NIMP (object) && SCM_OPPORTP (object))
    {
      scm_ptob_descriptor *ptob = scm_ptobs + SCM_PTOBNUM (object);

      if (!ptob->seek)
	SCM_MISC_ERROR ("port is not seekable", 
                        scm_cons (object, SCM_EOL));
      else
	rv = ptob->seek (object, off, how);
    }
  else /* file descriptor?.  */
    {
      SCM_VALIDATE_INT(1,object);
      rv = lseek (SCM_INUM (object), off, how);
      if (rv == -1)
	SCM_SYSERROR;
    }
  return scm_long2num (rv);
}
#undef FUNC_NAME

GUILE_PROC (scm_truncate_file, "truncate-file", 1, 1, 0,
            (SCM object, SCM length),
"")
#define FUNC_NAME s_scm_truncate_file
{
  int rv;
  off_t c_length;

  /* object can be a port, fdes or filename.  */

  if (SCM_UNBNDP (length))
    {
      /* must supply length if object is a filename.  */
      if (SCM_NIMP (object) && SCM_ROSTRINGP (object))
	scm_wrong_num_args (SCM_FUNC_NAME);
      
      length = scm_seek (object, SCM_INUM0, SCM_MAKINUM (SEEK_CUR));
    }
  c_length = SCM_NUM2LONG (2,length);
  if (c_length < 0)
    SCM_MISC_ERROR ("negative offset", SCM_EOL);

  object = SCM_COERCE_OUTPORT (object);
  if (SCM_INUMP (object))
    {
      SCM_SYSCALL (rv = ftruncate (SCM_INUM (object), c_length));
    }
  else if (SCM_NIMP (object) && SCM_OPOUTPORTP (object))
    {
      scm_port *pt = SCM_PTAB_ENTRY (object);
      scm_ptob_descriptor *ptob = scm_ptobs + SCM_PTOBNUM (object);
      
      if (!ptob->truncate)
	SCM_MISC_ERROR ("port is not truncatable", SCM_EOL);
      if (pt->rw_active == SCM_PORT_READ)
	scm_end_input (object);
      else if (pt->rw_active == SCM_PORT_WRITE)
	ptob->flush (object);
      
      ptob->truncate (object, c_length);
      rv = 0;
    }
  else
    {
      SCM_VALIDATE_ROSTRING(1,object);
      SCM_COERCE_SUBSTR (object);
      SCM_SYSCALL (rv = truncate (SCM_ROCHARS (object), c_length));
    }
  if (rv == -1)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

GUILE_PROC (scm_port_line, "port-line", 1, 0, 0,
            (SCM port),
"")
#define FUNC_NAME s_scm_port_line
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT(1,port);
  return SCM_MAKINUM (SCM_LINUM (port));
}
#undef FUNC_NAME

GUILE_PROC (scm_set_port_line_x, "set-port-line!", 2, 0, 0,
            (SCM port, SCM line),
"")
#define FUNC_NAME s_scm_set_port_line_x
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT(1,port);
  SCM_VALIDATE_INT(2,line);
  return SCM_PTAB_ENTRY (port)->line_number = SCM_INUM (line);
}
#undef FUNC_NAME

GUILE_PROC (scm_port_column, "port-column", 1, 0, 0,
            (SCM port),
"")
#define FUNC_NAME s_scm_port_column
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT(1,port);
  return SCM_MAKINUM (SCM_COL (port));
}
#undef FUNC_NAME

GUILE_PROC (scm_set_port_column_x, "set-port-column!", 2, 0, 0,
            (SCM port, SCM column),
"")
#define FUNC_NAME s_scm_set_port_column_x
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT(1,port);
  SCM_VALIDATE_INT(2,column);
  return SCM_PTAB_ENTRY (port)->column_number = SCM_INUM (column);
}
#undef FUNC_NAME

GUILE_PROC (scm_port_filename, "port-filename", 1, 0, 0,
            (SCM port),
"")
#define FUNC_NAME s_scm_port_filename
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT(1,port);
  return SCM_PTAB_ENTRY (port)->file_name;
}
#undef FUNC_NAME

GUILE_PROC (scm_set_port_filename_x, "set-port-filename!", 2, 0, 0,
            (SCM port, SCM filename),
"")
#define FUNC_NAME s_scm_set_port_filename_x
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_VALIDATE_OPENPORT(1,port);
  /* We allow the user to set the filename to whatever he likes.  */
  return SCM_PTAB_ENTRY (port)->file_name = filename;
}
#undef FUNC_NAME

#ifndef ttyname
extern char * ttyname();
#endif

void
scm_print_port_mode (SCM exp, SCM port)
{
  scm_puts (SCM_CLOSEDP (exp)
	    ? "closed: "
	    : (SCM_RDNG & SCM_CAR (exp)
	       ? (SCM_WRTNG & SCM_CAR (exp)
		  ? "input-output: "
		  : "input: ")
	       : (SCM_WRTNG & SCM_CAR (exp)
		  ? "output: "
		  : "bogus: ")),
	    port);
}

int
scm_port_print (SCM exp, SCM port, scm_print_state *pstate)
{
  char *type = SCM_PTOBNAME (SCM_PTOBNUM (exp));
  if (!type)
    type = "port";
  scm_puts ("#<", port);
  scm_print_port_mode (exp, port);
  scm_puts (type, port);
  scm_putc (' ', port);
  scm_intprint (SCM_CDR (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}

extern void scm_make_fptob ();
extern void scm_make_stptob ();
extern void scm_make_sfptob ();

void
scm_ports_prehistory ()
{
  scm_numptob = 0;
  scm_ptobs = (scm_ptob_descriptor *) malloc (sizeof (scm_ptob_descriptor));
  
  /* WARNING: These scm_newptob calls must be done in this order.
   * They must agree with the port declarations in tags.h.
   */
  /* scm_tc16_fport = */ scm_make_fptob ();
  /* scm_tc16_pipe was here */ scm_make_fptob (); /* dummy.  */
  /* scm_tc16_strport = */ scm_make_stptob ();
  /* scm_tc16_sfport = */ scm_make_sfptob ();
}



/* Void ports.   */

long scm_tc16_void_port = 0;

static void
flush_void_port (SCM port)
{
}

static void
end_input_void_port (SCM port, int offset)
{
}

static void
write_void_port (SCM port, void *data, size_t size)
{
}

SCM
scm_void_port (char *mode_str)
{
  int mode_bits;
  SCM answer;
  scm_port * pt;

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  mode_bits = scm_mode_bits (mode_str);
  pt = scm_add_to_port_table (answer);
  SCM_SETPTAB_ENTRY (answer, pt);
  SCM_SETSTREAM (answer, 0);
  SCM_SETCAR (answer, scm_tc16_void_port | mode_bits);
  SCM_ALLOW_INTS;
  return answer;
}


GUILE_PROC (scm_sys_make_void_port, "%make-void-port", 1, 0, 0,
            (SCM mode),
"")
#define FUNC_NAME s_scm_sys_make_void_port
{
  SCM_VALIDATE_ROSTRING(1,mode);
  SCM_COERCE_SUBSTR (mode);
  return scm_void_port (SCM_ROCHARS (mode));
}
#undef FUNC_NAME


/* Initialization.  */

void
scm_init_ports ()
{
  /* lseek() symbols.  */
  scm_sysintern ("SEEK_SET", SCM_MAKINUM (SEEK_SET));
  scm_sysintern ("SEEK_CUR", SCM_MAKINUM (SEEK_CUR));
  scm_sysintern ("SEEK_END", SCM_MAKINUM (SEEK_END));

  scm_tc16_void_port = scm_make_port_type ("void", 0, write_void_port);
#include "ports.x"
}
