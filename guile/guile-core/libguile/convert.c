/* Copyright (C) 2002 Free Software Foundation, Inc.
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



#include "libguile/_scm.h"
#include "libguile/validate.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/pairs.h"
#if HAVE_ARRAYS
# include "libguile/unif.h"
#endif

#include "libguile/convert.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#define CTYPE char
#define SCM2CTYPES_FN "scm_c_scm2chars"
#define SCM2CTYPES scm_c_scm2chars
#define CTYPES2SCM_FN "scm_c_chars2scm"
#define CTYPES2SCM scm_c_chars2scm
#define CTYPEFIXABLE
#define CTYPES2UVECT_FN "scm_c_chars2byvect"
#define CTYPES2UVECT scm_c_chars2byvect
#define UVECTTYPE scm_tc7_byvect
#define CTYPEMIN -128
#define CTYPEMAX +255
#define ARRAYTYPE1 scm_tc7_byvect
#define STRINGTYPE
#include "convert.i.c"

#define CTYPE short
#define SCM2CTYPES_FN "scm_c_scm2shorts"
#define SCM2CTYPES scm_c_scm2shorts
#define CTYPES2SCM_FN "scm_c_shorts2scm"
#define CTYPES2SCM scm_c_shorts2scm
#define CTYPEFIXABLE
#define CTYPES2UVECT_FN "scm_c_shorts2svect"
#define CTYPES2UVECT scm_c_shorts2svect
#define UVECTTYPE scm_tc7_svect
#define CTYPEMIN -32768
#define CTYPEMAX +65535
#define ARRAYTYPE1 scm_tc7_svect
#include "convert.i.c"

#define CTYPE int
#define SCM2CTYPES_FN "scm_c_scm2ints"
#define SCM2CTYPES scm_c_scm2ints
#define CTYPES2SCM_FN "scm_c_ints2scm"
#define CTYPES2SCM scm_c_ints2scm
#define CTYPES2UVECT_FN "scm_c_ints2ivect"
#define CTYPES2UVECT scm_c_ints2ivect
#define UVECTTYPE scm_tc7_ivect
#define CTYPES2UVECT_FN2 "scm_c_uints2uvect"
#define CTYPES2UVECT2 scm_c_uints2uvect
#define UVECTTYPE2 scm_tc7_uvect
#define ARRAYTYPE1 scm_tc7_ivect
#define ARRAYTYPE2 scm_tc7_uvect
#include "convert.i.c"

#define CTYPE long
#define SCM2CTYPES_FN "scm_c_scm2longs"
#define SCM2CTYPES scm_c_scm2longs
#define CTYPES2SCM_FN "scm_c_longs2scm"
#define CTYPES2SCM scm_c_longs2scm
#define CTYPES2UVECT_FN "scm_c_longs2ivect"
#define CTYPES2UVECT scm_c_longs2ivect
#define UVECTTYPE scm_tc7_ivect
#define CTYPES2UVECT_FN2 "scm_c_ulongs2uvect"
#define CTYPES2UVECT2 scm_c_ulongs2uvect
#define UVECTTYPE2 scm_tc7_uvect
#define ARRAYTYPE1 scm_tc7_ivect
#define ARRAYTYPE2 scm_tc7_uvect
#include "convert.i.c"

#define CTYPE float
#define SCM2CTYPES_FN "scm_c_scm2floats"
#define SCM2CTYPES scm_c_scm2floats
#define CTYPES2SCM_FN "scm_c_floats2scm"
#define CTYPES2SCM scm_c_floats2scm
#define CTYPES2UVECT_FN "scm_c_floats2fvect"
#define CTYPES2UVECT scm_c_floats2fvect
#define UVECTTYPE scm_tc7_fvect
#define ARRAYTYPE1 scm_tc7_fvect
#define ARRAYTYPE2 scm_tc7_dvect
#define FLOATTYPE1 float
#define FLOATTYPE2 double
#include "convert.i.c"

#define CTYPE double
#define SCM2CTYPES_FN "scm_c_scm2doubles"
#define SCM2CTYPES scm_c_scm2doubles
#define CTYPES2SCM_FN "scm_c_doubles2scm"
#define CTYPES2SCM scm_c_doubles2scm
#define CTYPES2UVECT_FN "scm_c_doubles2dvect"
#define CTYPES2UVECT scm_c_doubles2dvect
#define UVECTTYPE scm_tc7_dvect
#define ARRAYTYPE1 scm_tc7_dvect
#define ARRAYTYPE2 scm_tc7_fvect
#define FLOATTYPE1 double
#define FLOATTYPE2 float
#include "convert.i.c"

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
