/* Copyright (C) 1999,2000,2001,2003 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

/* some bits originally by Jim Blandy <jimb@red-bean.com> */

#include "libguile.h"
#include "libguile/gh.h"

#include <assert.h>
#include <string.h>

static int
string_equal (SCM str, char *lit)
{
  int len = strlen (lit);
  int result;
 
  result = ((SCM_STRING_LENGTH (str) == len)
            && (!memcmp (SCM_STRING_CHARS (str), lit, len)));
  scm_remember_upto_here_1 (str);
  return result;
}

static void
test_gh_set_substr ()
{
  SCM string;

  string = gh_str02scm ("Free, darnit!");
  assert (gh_string_p (string));

  gh_set_substr ("dammit", string, 6, 6);
  assert (string_equal (string, "Free, dammit!"));
  
  /* Make sure that we can use the string itself as a source.

     I guess this behavior isn't really visible, since the GH API
     doesn't provide any direct access to the string contents.  But I
     think it should, eventually.  You can't write efficient string
     code if you have to copy the string just to look at it.  */

  /* Copy a substring to an overlapping region to its right.  */
  gh_set_substr (SCM_STRING_CHARS (string), string, 4, 6);
  assert (string_equal (string, "FreeFree, it!"));
  
  string = gh_str02scm ("Free, darnit!");
  assert (gh_string_p (string));

  /* Copy a substring to an overlapping region to its left.  */
  gh_set_substr (SCM_STRING_CHARS (string) + 6, string, 2, 6);
  assert (string_equal (string, "Frdarnitrnit!"));
}

int 
main (int argc, char *argv[])
{
  scm_init_guile ();
  test_gh_set_substr ();
  return 0;
}