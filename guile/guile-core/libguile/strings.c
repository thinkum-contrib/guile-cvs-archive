/* Copyright (C) 1995,1996,1998,2000,2001 Free Software Foundation, Inc.
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




#include <string.h>
#include <stdio.h>

#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/deprecation.h"
#include "libguile/validate.h"
#include "libguile/dynwind.h"



/* {Strings}
 */

/* Stringbufs 
 */

#define STRINGBUF_TAG           scm_tc7_stringbuf
#define STRINGBUF_REFCOUNT(buf) ((size_t)(SCM_CELL_WORD_3 (buf)))
#define STRINGBUF_CHARS(buf)    ((char *)SCM_CELL_WORD_1(buf))
#define STRINGBUF_LENGTH(buf)   (SCM_CELL_WORD_2(buf))

#define SET_STRINGBUF_REFCOUNT(buf,rc)\
         (SCM_SET_CELL_WORD_3 ((buf), (scm_t_bits)rc))

static SCM
make_stringbuf (size_t len)
{
  /* XXX - for the benefit of SCM_STRING_CHARS, all stringbufs are
     null-terminated.  Once SCM_STRING_CHARS is removed, this
     null-termination can be dropped.
  */

  char *mem = scm_gc_malloc (len+1, "string");
  mem[len] = '\0';
  return scm_double_cell (STRINGBUF_TAG, (scm_t_bits) mem,
			  (scm_t_bits) len, (scm_t_bits) 1);
}

SCM
scm_i_stringbuf_mark (SCM buf)
{
  return SCM_BOOL_F;
}

void
scm_i_stringbuf_free (SCM buf)
{
  //  fprintf (stderr, "freeing buf %p\n", buf);
  scm_gc_free (STRINGBUF_CHARS (buf), STRINGBUF_LENGTH (buf) + 1, "string");
}

SCM_MUTEX (stringbuf_refcount_mutex);

static void
stringbuf_ref (SCM buf)
{
  scm_mutex_lock (&stringbuf_refcount_mutex);
  SET_STRINGBUF_REFCOUNT (buf, STRINGBUF_REFCOUNT (buf) + 1);
  scm_mutex_unlock (&stringbuf_refcount_mutex);
}

/* Copy-on-write strings.
 */

#define STRING_TAG            scm_tc7_string

#define STRING_STRINGBUF(str) (SCM_CELL_OBJECT_1(str))
#define STRING_START(str)     ((size_t)SCM_CELL_WORD_2(str))
#define STRING_LENGTH(str)    ((size_t)SCM_CELL_WORD_3(str))

#define SET_STRING_STRINGBUF(str,buf) (SCM_SET_CELL_OBJECT_1(str,buf))
#define SET_STRING_START(str,start) (SCM_SET_CELL_WORD_2(str,start))

#define IS_STRING(str)        (SCM_NIMP(str) && SCM_TYP7(str) == STRING_TAG)

SCM
scm_c_make_string (size_t len, char **charsp)
{
  SCM buf = make_stringbuf (len);
  if (charsp)
    *charsp = STRINGBUF_CHARS (buf);
  return scm_double_cell (STRING_TAG, SCM_UNPACK(buf),
			  (scm_t_bits)0, (scm_t_bits) len);
}

SCM
scm_c_substring (SCM str, size_t start, size_t end)
{
  size_t len = end - start;
  SCM buf = STRING_STRINGBUF (str);
  stringbuf_ref (buf);
  return scm_double_cell (STRING_TAG, SCM_UNPACK(buf),
			  (scm_t_bits)start, (scm_t_bits) len);
}

SCM
scm_c_substring_copy (SCM str, size_t start, size_t end)
{
  size_t len = end - start;
  SCM buf = STRING_STRINGBUF (str);
  SCM my_buf = make_stringbuf (len);
  memcpy (STRINGBUF_CHARS (my_buf), STRINGBUF_CHARS (buf) + start, len);
  return scm_double_cell (STRING_TAG, SCM_UNPACK(my_buf),
			  (scm_t_bits)0, (scm_t_bits) len);
}

/* Mutation-sharing substrings
 */

#define SH_STRING_TAG       (scm_tc7_string + 0x100)

#define SH_STRING_STRING(sh) (SCM_CELL_OBJECT_1(sh))
/* START and LENGTH as for STRINGs. */

#define IS_SH_STRING(str)   (SCM_CELL_TYPE(str)==SH_STRING_TAG)

SCM
scm_c_substring_shared (SCM str, size_t start, size_t end)
{
  size_t len = end - start;
  return scm_double_cell (SH_STRING_TAG, SCM_UNPACK(str),
			  (scm_t_bits)start, (scm_t_bits) len);
}

SCM
scm_i_string_mark (SCM str)
{
  if (IS_SH_STRING (str))
    return SH_STRING_STRING (str);
  else
    return STRING_STRINGBUF (str);
}

void
scm_i_string_free (SCM str)
{
  /* The refcount of a stringbuf is stored in its fourth word, so even
     if the stringbuf of this string has already been swept, we can
     safely decrement its refcount.
  */
  if (!IS_SH_STRING (str))
    {
#if 0
      SCM buf = STRING_STRINGBUF (str);
      SET_STRINGBUF_REFCOUNT (buf, STRINGBUF_REFCOUNT (buf) - 1);
#endif
    }
}


/* Internal accessors
 */

size_t
scm_i_string_length (SCM str)
{
  return STRING_LENGTH (str);
}

const char *
scm_i_string_chars (SCM str)
{
  size_t start = STRING_START(str);
  if (IS_SH_STRING (str))
    {
      str = SH_STRING_STRING (str);
      start += STRING_START (str);
    }
  return STRINGBUF_CHARS (STRING_STRINGBUF (str)) + start;
}

char *
scm_i_string_writable_chars (SCM str)
{
  SCM buf;
  size_t start = STRING_START(str);
  if (IS_SH_STRING (str))
    {
      str = SH_STRING_STRING (str);
      start += STRING_START (str);
    }
  buf = STRING_STRINGBUF (str);
  scm_mutex_lock (&stringbuf_refcount_mutex);
  if (STRINGBUF_REFCOUNT (buf) > 1)
    {
      /* Clone stringbuf.  For this, we put all threads to sleep.
       */
      size_t len = STRING_LENGTH (str);
      SCM new_buf;

      SET_STRINGBUF_REFCOUNT (buf, STRINGBUF_REFCOUNT (buf) - 1);
      scm_mutex_unlock (&stringbuf_refcount_mutex);
      new_buf = make_stringbuf (len);
      memcpy (STRINGBUF_CHARS (new_buf),
	      STRINGBUF_CHARS (buf) + STRING_START (str), len);

      fprintf (stderr, "cloned %p to %p\n", buf, new_buf);
      buf = new_buf;

      scm_i_thread_put_to_sleep ();
      SET_STRING_STRINGBUF (str, buf);
      start -= STRING_START (str);
      SET_STRING_START (str, 0);
      scm_i_thread_wake_up ();
    }
  else
    scm_mutex_unlock (&stringbuf_refcount_mutex);

  return STRINGBUF_CHARS (buf) + start;
}



SCM_DEFINE (scm_string_p, "string?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a string, else @code{#f}.")
#define FUNC_NAME s_scm_string_p
{
  return scm_from_bool (IS_STRING (obj));
}
#undef FUNC_NAME


SCM_REGISTER_PROC (s_scm_list_to_string, "list->string", 1, 0, 0, scm_string);

SCM_DEFINE (scm_string, "string", 0, 0, 1, 
            (SCM chrs),
	    "@deffnx {Scheme Procedure} list->string chrs\n"
	    "Return a newly allocated string composed of the arguments,\n"
	    "@var{chrs}.")
#define FUNC_NAME s_scm_string
{
  SCM result;
  size_t len;
  char *data;

  {
    long i = scm_ilength (chrs);

    SCM_ASSERT (i >= 0, chrs, SCM_ARG1, FUNC_NAME);
    len = i;
  }

  result = scm_c_make_string (len, &data);
  while (len > 0 && SCM_CONSP (chrs))
    {
      SCM elt = SCM_CAR (chrs);

      SCM_VALIDATE_CHAR (SCM_ARGn, elt);
      *data++ = SCM_CHAR (elt);
      chrs = SCM_CDR (chrs);
      len--;
    }
  if (len > 0)
    scm_misc_error (NULL, "list changed while constructing string", SCM_EOL);
  if (!SCM_NULLP (chrs))
    scm_wrong_type_arg_msg (NULL, 0, chrs, "proper list");

  return result;
}
#undef FUNC_NAME


/* converts C scm_array of strings to SCM scm_list of strings. */
/* If argc < 0, a null terminated scm_array is assumed. */
SCM 
scm_makfromstrs (int argc, char **argv)
{
  int i = argc;
  SCM lst = SCM_EOL;
  if (0 > i)
    for (i = 0; argv[i]; i++);
  while (i--)
    lst = scm_cons (scm_from_locale_string (argv[i]), lst);
  return lst;
}


/* This function must only be applied to memory obtained via malloc,
   since the GC is going to apply `free' to it when the string is
   dropped.

   Also, s[len] must be `\0', since we promise that strings are
   null-terminated.  Perhaps we could handle non-null-terminated
   strings by claiming they're shared substrings of a string we just
   made up.  */
SCM
scm_take_str (char *s, size_t len)
#define FUNC_NAME "scm_take_str"
{
  SCM answer = scm_from_locale_stringn (s, len);
  free (s);
  return answer;
}
#undef FUNC_NAME


/* `s' must be a malloc'd string.  See scm_take_str.  */
SCM
scm_take0str (char *s)
{
  return scm_take_locale_string (s);
}


SCM 
scm_mem2string (const char *src, size_t len)
{
  return scm_from_locale_stringn (src, len);
}


SCM
scm_str2string (const char *src)
{
  return scm_from_locale_string (src);
}


SCM 
scm_makfrom0str (const char *src)
{
  if (!src) return SCM_BOOL_F;
  return scm_from_locale_string (src);
}


SCM 
scm_makfrom0str_opt (const char *src)
{
  return scm_makfrom0str (src);
}


SCM
scm_allocate_string (size_t len)
#define FUNC_NAME "scm_allocate_string"
{
  return scm_c_make_string (len, NULL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_string, "make-string", 1, 1, 0,
            (SCM k, SCM chr),
	    "Return a newly allocated string of\n"
            "length @var{k}.  If @var{chr} is given, then all elements of\n"
	    "the string are initialized to @var{chr}, otherwise the contents\n"
	    "of the @var{string} are unspecified.")
#define FUNC_NAME s_scm_make_string
{
  size_t i = scm_to_size_t (k);
  char *dst;
  SCM res = scm_c_make_string (i, &dst);

  if (!SCM_UNBNDP (chr))
    {
      SCM_VALIDATE_CHAR (2, chr);
      memset (dst, SCM_CHAR (chr), i);
    }

  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_string_length, "string-length", 1, 0, 0, 
	    (SCM string),
	    "Return the number of characters in @var{string}.")
#define FUNC_NAME s_scm_string_length
{
  SCM_VALIDATE_STRING (1, string);
  return scm_from_size_t (STRING_LENGTH (string));
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_ref, "string-ref", 2, 0, 0,
            (SCM str, SCM k),
	    "Return character @var{k} of @var{str} using zero-origin\n"
	    "indexing. @var{k} must be a valid index of @var{str}.")
#define FUNC_NAME s_scm_string_ref
{
  unsigned long idx;

  SCM_VALIDATE_STRING (1, str);
  idx = scm_to_unsigned_integer (k, 0, scm_i_string_length (str)-1);
  return SCM_MAKE_CHAR (scm_i_string_chars (str)[idx]);
}
#undef FUNC_NAME

SCM
scm_c_string_ref (SCM str, size_t p)
{
  if (p >= scm_i_string_length (str))
    scm_out_of_range (NULL, scm_from_size_t (p));
  return SCM_MAKE_CHAR (scm_i_string_chars (str)[p]);
}

SCM_DEFINE (scm_string_set_x, "string-set!", 3, 0, 0,
            (SCM str, SCM k, SCM chr),
	    "Store @var{chr} in element @var{k} of @var{str} and return\n"
	    "an unspecified value. @var{k} must be a valid index of\n"
	    "@var{str}.")
#define FUNC_NAME s_scm_string_set_x
{
  unsigned long idx;

  SCM_VALIDATE_STRING (1, str);
  idx = scm_to_unsigned_integer (k, 0, scm_i_string_length(str)-1);
  SCM_VALIDATE_CHAR (3, chr);
  {
    char *dst = scm_i_string_writable_chars (str);
    dst[idx] = SCM_CHAR (chr);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_c_string_set_x (SCM str, size_t p, SCM chr)
{
  if (p >= scm_i_string_length (str))
    scm_out_of_range (NULL, scm_from_size_t (p));
  {
    char *dst = scm_i_string_writable_chars (str);
    dst[p] = SCM_CHAR (chr);
  }
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_substring, "substring", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a newly allocated string formed from the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring
{
  size_t len, from, to;

  SCM_VALIDATE_STRING (1, str);
  len = scm_i_string_length (str);
  from = scm_to_unsigned_integer (start, 0, len);
  if (SCM_UNBNDP (end))
    to = len;
  else
    to = scm_to_unsigned_integer (end, from, len);
  return scm_c_substring (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_copy, "substring/copy", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return a newly allocated string formed from the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring_copy
{
  size_t len, from, to;

  SCM_VALIDATE_STRING (1, str);
  len = scm_i_string_length (str);
  from = scm_to_unsigned_integer (start, 0, len);
  if (SCM_UNBNDP (end))
    to = len;
  else
    to = scm_to_unsigned_integer (end, from, len);
  return scm_c_substring_copy (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_substring_shared, "substring/shared", 2, 1, 0,
	    (SCM str, SCM start, SCM end),
	    "Return string that indirectly refers to the characters\n"
            "of @var{str} beginning with index @var{start} (inclusive) and\n"
	    "ending with index @var{end} (exclusive).\n"
            "@var{str} must be a string, @var{start} and @var{end} must be\n"
	    "exact integers satisfying:\n\n"
            "0 <= @var{start} <= @var{end} <= (string-length @var{str}).")
#define FUNC_NAME s_scm_substring_shared
{
  size_t len, from, to;

  SCM_VALIDATE_STRING (1, str);
  len = scm_i_string_length (str);
  from = scm_to_unsigned_integer (start, 0, len);
  if (SCM_UNBNDP (end))
    to = len;
  else
    to = scm_to_unsigned_integer (end, from, len);
  return scm_c_substring_shared (str, from, to);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_append, "string-append", 0, 0, 1, 
            (SCM args),
	    "Return a newly allocated string whose characters form the\n"
            "concatenation of the given strings, @var{args}.")
#define FUNC_NAME s_scm_string_append
{
  SCM res;
  size_t i = 0;
  SCM l, s;
  char *data;

  SCM_VALIDATE_REST_ARGUMENT (args);
  for (l = args; !SCM_NULLP (l); l = SCM_CDR (l)) 
    {
      s = SCM_CAR (l);
      SCM_VALIDATE_STRING (SCM_ARGn, s);
      i += scm_i_string_length (s);
    }
  res = scm_c_make_string (i, &data);
  for (l = args; !SCM_NULLP (l); l = SCM_CDR (l)) 
    {
      s = SCM_CAR (l);
      SCM_VALIDATE_STRING (SCM_ARGn, s);
      size_t len = scm_i_string_length (s);
      memcpy (data, scm_i_string_chars (s), len);
      data += len;
      scm_remember_upto_here_1 (s);
    }
  return res;
}
#undef FUNC_NAME

int
scm_is_string (SCM obj)
{
  return IS_STRING (obj);
}

SCM
scm_from_locale_stringn (const char *str, size_t len)
{
  SCM res;
  char *dst;

  if (len == (size_t)-1)
    len = strlen (str);
  res = scm_c_make_string (len, &dst);
  memcpy (dst, str, len);
  return res;
}

SCM
scm_from_locale_string (const char *str)
{
  return scm_from_locale_stringn (str, -1);
}

SCM
scm_take_locale_stringn (char *str, size_t len)
{
  if (len == (size_t)-1)
    return scm_take_locale_string (str);
  else
    {
      /* STR might not be zero terminated and we are not allowed to
	 look at str[len], so we have to make a new one...
      */
      SCM res = scm_from_locale_stringn (str, len);
      free (str);
      return res;
    }
}

SCM
scm_take_locale_string (char *str)
{
  size_t len = strlen (str);
  SCM buf, res;

  buf = scm_double_cell (STRINGBUF_TAG, (scm_t_bits) str,
			 (scm_t_bits) len, 0);
  res = scm_double_cell (STRING_TAG,
			 SCM_UNPACK (buf),
			 (scm_t_bits) 0, (scm_t_bits) len);
  scm_gc_register_collectable_memory (str, len+1, "string");

  return res;
}

char *
scm_to_locale_stringn (SCM str, size_t *lenp)
{
  char *res;
  size_t len;

  if (!scm_is_string (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  len = scm_i_string_length (str);
  res = scm_malloc (len + ((lenp==NULL)? 1 : 0));
  memcpy (res, scm_i_string_chars (str), len);
  if (lenp == NULL)
    {
      res[len] = '\0';
      if (strlen (res) != len)
	{
	  free (res);
	  scm_misc_error (NULL,
			  "string contains #\\nul character: ~S",
			  scm_list_1 (str));
	}
    }
  else
    *lenp = len;

  scm_remember_upto_here_1 (str);
  return res;
}

char *
scm_to_locale_string (SCM str)
{
  return scm_to_locale_stringn (str, NULL);
}

size_t
scm_to_locale_stringbuf (SCM str, char *buf, size_t max_len)
{
  size_t len;
  
  if (!scm_is_string (str))
    scm_wrong_type_arg_msg (NULL, 0, str, "string");
  len = scm_i_string_length (str);
  memcpy (buf, scm_i_string_chars (str), (len > max_len)? max_len : len);
  scm_remember_upto_here_1 (str);
  return len;
}

/* Return a newly allocated array of char pointers to each of the strings
   in args, with a terminating NULL pointer.  */

char **
scm_i_allocate_string_pointers (SCM list)
{
  char **result;
  int len = scm_ilength (list);
  int i;

  if (len < 0)
    scm_wrong_type_arg_msg (NULL, 0, list, "proper list");

  scm_frame_begin (0);

  result = (char **) scm_malloc ((len + 1) * sizeof (char *));
  result[len] = NULL;
  scm_frame_unwind_handler (free, result, 0);

  /* The list might be have been modified in another thread, so
     we check LIST before each access.
   */
  for (i = 0; i < len && SCM_CONSP (list); i++)
    {
      result[i] = scm_to_locale_string (SCM_CAR (list));
      list = SCM_CDR (list);
    }

  scm_frame_end ();
  return result;
}

void
scm_i_free_string_pointers (char **pointers)
{
  int i;
  
  for (i = 0; pointers[i]; i++)
    free (pointers[i]);
  free (pointers);
}

void
scm_i_get_substring_spec (size_t len,
			  SCM start, size_t *cstart,
			  SCM end, size_t *cend)
{
  if (SCM_UNBNDP (start))
    *cstart = 0;
  else
    *cstart = scm_to_unsigned_integer (start, 0, len);

  if (SCM_UNBNDP (end))
    *cend = len;
  else
    *cend = scm_to_unsigned_integer (end, *cstart, len);
}
		  
void
scm_init_strings ()
{
  scm_nullstr = scm_c_make_string (0, NULL);

#include "libguile/strings.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
