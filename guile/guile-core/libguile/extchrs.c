/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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


#include "extchrs.h"



#ifdef SCM_FAKE_EXT_CHARS


int
xmblen (str, size)
     const char * str;
     size_t size;
{
  if (!str)
    return 0;

  if (*(unsigned char *)str > 127)
    return ((size < 4)
	    ? -1
	    : 4);
  else if (!*str)
    return 0;
  else
    return 1;
}


int
xwctomb (_str, c)
     char * _str;
     int c;
{
  unsigned char * str;
  str = (unsigned char *)_str;
  if (!str)
    return 0;

  if (!c)
    {
      *str = 0;
      return 0;
    }


  if (c < 127)
    {
      *str = c;
      return 1;
    }

  str[0] = 255;
  str[1] = 0x80 | ((c >> 10) & 0x3f);
  str[2] = 0x80 | ((c >> 4) & 0x3f);
  str[3] = 0x80 | (c & 0xf);
  return 4;
}


int
xmbtowc (result, _str, size)
     xwchar_t * result;
     const char * _str;
     size_t size;
{
  const unsigned char * str;
  str = (const unsigned char *)_str;
  if (!str)
    return 0;

  if ((size == 0) || !*str)
    {
      *result = 0;
      return 0;
    }

  if (*str < 128)
    {
      *result = *str;
      return 1;
    }

  if (   (*str != 255)
      || (size < 4))
    return -1;

  *result = (  ((str[1] & 0x3f) << 10)
	     | ((str[2] & 0x3f) << 4)
	     | (str[3] & 0xf));
  return 4;
}

#endif /* SCM_FAKE_EXT_CHARS */

