/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer inline functions (common part)
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1, or (at your option)
 * any later version.
 * 
 * GNU lightning is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 ***********************************************************************/

#ifndef __lightning_funcs_common_h
#define __lightning_funcs_common_h

#include <stdio.h>

static int jit_fail() JIT_UNUSED;

int
jit_fail(msg, file, line, function)
     char *msg;
     char *file;
     int line;
     char *function;
{
  fprintf(stderr, "%s: In function `%s':\n", file, function);
  fprintf(stderr, "%s:%d: %s\n", file, line, msg);
  abort();
  return 0;
}

#endif /* __lightning_funcs_common_h */
