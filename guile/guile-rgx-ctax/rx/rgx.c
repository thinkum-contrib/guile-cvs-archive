/*	Copyright (C) 1995 Free Software Foundation, Inc.
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



#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/smob.h"
#include "libguile/mallocs.h"
#include "libguile/chars.h"
#include "libguile/feature.h"

#include "rgx.h"

#ifdef STDC_HEADERS
#include <string.h>
#endif

#include "inst-rxposix.h"
#include "rxgnucomp.h"
#include "rxanal.h"
#include "rxunfa.h"
#include "rxbasic.h"


long scm_tc16_regex_t;
#define RGX(X)	((regex_t *)SCM_CDR(X))
#define RGXP(X)	(SCM_CAR(X) == (SCM)scm_tc16_regex_t)

size_t
free_regex_t (obj)
     SCM obj;
{
  regex_t *r;
  r = RGX(obj);
  free ((char *)(r->owner_data));
  regfree (r);
  return 0;
}

int
print_regex_t (obj, port, pstate)
     SCM obj;
     SCM port;
     scm_print_state *pstate;
{
  regex_t *r;
  r = RGX (obj);
  scm_gen_puts (scm_regular_string, "#<rgx ", port);
  scm_gen_puts (scm_regular_string, (char *)(r->owner_data), port);
  scm_gen_puts (scm_regular_string, ">", port);
  return 1;
}


static scm_smobfuns regex_t_smob =
{ scm_mark0, free_regex_t, print_regex_t, 0 };

SCM scm_regex_error_key;

static void
scm_regex_error (subr, code)
     char *subr;
     int code;
{
  lgh_error (scm_regex_error_key,
	     subr,
	     "%s",
	     scm_listify (scm_makfrom0str (rx_error_msg [code]),
			  SCM_UNDEFINED),
	     scm_listify (SCM_MAKINUM (code), SCM_UNDEFINED));
}

SCM_PROC (s_compiled_regexp_p, "compiled-regexp?", 1, 0, 0, scm_compiled_regexp_p);
SCM
scm_compiled_regexp_p (obj)
     SCM obj;
{
  return ((SCM_NIMP (obj) && RGXP (obj))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM_PROC(s_regcomp, "regcomp", 1, 1, 0, scm_regcomp);
SCM
scm_regcomp (pat, cfl)
     SCM pat;
     SCM cfl;
{
  SCM answer;
  SCM_ASSERT (SCM_NIMP (pat) && SCM_ROSTRINGP (pat), pat, SCM_ARG1, s_regcomp);
  if (cfl == SCM_UNDEFINED)
    cfl = SCM_INUM0;
  SCM_ASSERT (SCM_INUMP (cfl), cfl, SCM_ARG2, s_regcomp);

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  {
    regex_t * it;
    int status;
    it = malloc (sizeof (*it));
    if (!it)
      {
      allocation:
	scm_memory_error (s_regcomp);
      }
    status = regncomp (it, SCM_ROLENGTH (pat), SCM_ROCHARS (pat), SCM_INUM (cfl));
    if (status)
      {
	free (it);
	scm_regex_error (s_regcomp, status);
      }
    else
      {
	it->owner_data = (void *)malloc (SCM_ROLENGTH (pat) + 1);
	if (!it->owner_data)
	  {
	    regfree (it);
	    free (it);
	    goto allocation;
	  }
	memcpy (it->owner_data, SCM_ROCHARS (pat), SCM_ROLENGTH (pat));
	((char *)it->owner_data)[SCM_ROLENGTH (pat)] = 0;
	SCM_SETCAR (answer, scm_tc16_regex_t);
	SCM_SETCDR (answer, (SCM)it);
      }
  }
  SCM_ALLOW_INTS;
  return answer;
}

#if 0
SCM_PROC(s_regexec, "regexec", 2, 1, 0, scm_regexec);
SCM
scm_regexec (rgx, str, eflags)
     SCM rgx;
     SCM str;
     SCM eflags;
{
  SCM answer;
  SCM malloc_protect;
  regmatch_t *pmatch = 0;
  int status;

  SCM_ASSERT (SCM_NIMP (rgx) && RGXP (rgx), rgx, SCM_ARG1, s_regexec);
  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG2, s_regexec);
  if (eflags == SCM_UNDEFINED)
    eflags = SCM_INUM0;
  SCM_ASSERT (SCM_INUMP (eflags), eflags, SCM_ARG3, s_regexec);

  malloc_protect = scm_malloc_obj (0);
  SCM_DEFER_INTS;

  status = regnexec (RGX(rgx), SCM_ROLENGTH (str), SCM_ROCHARS (str),
		     0, &pmatch, eflags | REG_ALLOC_REGS);
  if (status)
    {
      SCM_ALLOW_INTS;
      if (status == REG_NOMATCH)
	return SCM_BOOL_F;
      else
	scm_regex_error (s_regexec, status);
    }

  SCM_SETMALLOCDATA (malloc_protect, pmatch);
  SCM_ALLOW_INTS;

  {
    int i;
    size_t bound;
    int vlen;

    bound = RGX(rgx)->n_subexps;
    answer = scm_make_vector (SCM_MAKINUM (bound), SCM_UNDEFINED, SCM_BOOL_F);
    for (i = 0; i < bound; ++i)
      if (pmatch[i].rm_so >= 0)
	SCM_VELTS (answer)[i] = scm_cons (SCM_MAKINUM (pmatch[i].rm_so),
					  SCM_MAKINUM (pmatch[i].rm_eo));
      else
	SCM_VELTS (answer)[i] = SCM_BOOL_F;
    }

  return answer;
}
#endif

SCM_PROC(s_regexec, "regexec", 2, 2, 0, scm_regexec);
SCM
scm_regexec (rgx, str, match_pick, eflags)
     SCM rgx;
     SCM str;
     SCM match_pick;
     SCM eflags;
{
  SCM answer;
  SCM malloc_protect;
  regmatch_t * pmatch;

  SCM_ASSERT (SCM_NIMP (rgx) && RGXP (rgx), rgx, SCM_ARG1, s_regexec);
  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG2, s_regexec);
  if (eflags == SCM_UNDEFINED)
    eflags = SCM_INUM0;
  SCM_ASSERT (SCM_INUMP (eflags), eflags, SCM_ARG4, s_regexec);

  malloc_protect = scm_malloc_obj (0);
  SCM_DEFER_INTS;
  {
    int status;
    pmatch = 0;
    status = regnexec (RGX(rgx), SCM_ROLENGTH (str), SCM_ROCHARS (str),
		       0, &pmatch, SCM_INUM (eflags) | REG_ALLOC_REGS);
    if (status)
      {
	SCM_ALLOW_INTS;
	if (status == REG_NOMATCH)
	  return SCM_BOOL_F;
	else
	  scm_regex_error (s_regexec, status);
      }
    if (match_pick == SCM_BOOL_F)
      free (pmatch);
    else
      SCM_SETMALLOCDATA (malloc_protect, pmatch);
  }
  SCM_ALLOW_INTS;

  if (match_pick == SCM_BOOL_F)
    return SCM_BOOL_T;
  else if ((match_pick == SCM_BOOL_T) || (match_pick == SCM_UNDEFINED))
    {
      int i;

      answer = scm_make_vector (SCM_MAKINUM (RGX (rgx)->n_subexps),
				SCM_BOOL_F, SCM_BOOL_F);
      for (i = 0; i < RGX (rgx)->n_subexps; i++)
	{
	  if (pmatch[i].rm_so >= 0)
	    {
	      SCM_VELTS (answer)[i] = scm_cons (SCM_MAKINUM (pmatch[i].rm_so),
						SCM_MAKINUM (pmatch[i].rm_eo));
	    }
	}
      return answer;
    }
  else if ((SCM_NIMP (match_pick) && SCM_ROSTRINGP (match_pick)))
    {
      int i;

      answer = scm_listify (SCM_UNDEFINED);
      
      for (i = RGX (rgx)->n_subexps - 1; i >= 1; i--)
	{
	  if (pmatch[i].rm_so >= 0)
	    {
	      answer = scm_cons (scm_make_shared_substring
				 (str, SCM_MAKINUM (pmatch[i].rm_so), 
				  SCM_MAKINUM (pmatch[i].rm_eo)),
				 answer);
	    }
	  else
	    answer = scm_cons (SCM_BOOL_F, answer);
	}
      answer = scm_cons (scm_listify (scm_make_shared_substring
				      (str, SCM_MAKINUM (0),
				       SCM_MAKINUM (pmatch[0].rm_so)),
				      scm_make_shared_substring
				      (str, SCM_MAKINUM (pmatch[0].rm_so),
				       SCM_MAKINUM (pmatch[0].rm_eo)),
				      scm_make_shared_substring
				      (str, SCM_MAKINUM (pmatch[0].rm_eo),
				       SCM_MAKINUM (SCM_LENGTH (str))),
				      SCM_UNDEFINED),
			 answer);

      return answer;
    }
  else if (SCM_NIMP (match_pick) && SCM_VECTORP (match_pick))
    {
      int i;
      size_t bound;
      int vlen;

      bound = RGX(rgx)->n_subexps;
      vlen = SCM_LENGTH (match_pick);
      if (vlen < bound)
	bound = vlen;
      for (i = 0; i < bound; ++i)
	if (pmatch[i].rm_so >= 0)
	  SCM_VELTS (match_pick)[i] = scm_cons (SCM_MAKINUM (pmatch[i].rm_so),
					    SCM_MAKINUM (pmatch[i].rm_eo));
	else
	  SCM_VELTS (match_pick)[i] = SCM_BOOL_F;
      while (i < vlen)
	{
	  SCM_VELTS (match_pick)[i] = SCM_BOOL_F;
	  ++i;
	}
      return match_pick;
    }
  else
    {
      SCM spec;
      size_t bound;
      SCM ans_pos;

      answer = scm_cons (SCM_BOOL_F, SCM_BOOL_F);
      ans_pos = answer;
      bound = RGX(rgx)->n_subexps;

      for (spec = match_pick; spec != SCM_EOL; spec = SCM_CDR (spec))
	{
	  SCM item;
	  SCM frm;
	  SCM to;

	  SCM_ASSERT (SCM_NIMP (spec) && SCM_CONSP (spec), spec, SCM_ARG3, s_regexec);
	  item = SCM_CAR (spec);

	  if (SCM_ICHRP (item))
	    {
	      if (SCM_ICHR (item) == '<')
		{
		  frm = SCM_INUM0;
		  to = SCM_MAKINUM (pmatch[0].rm_so);
		  SCM_SETCDR (ans_pos,
			      scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL));
		}
	      else if (SCM_ICHR (item) == '>')
		{
		  frm = SCM_MAKINUM (pmatch[0].rm_eo);
		  to = SCM_UNDEFINED;
		  SCM_SETCDR (ans_pos,
			      scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL));
		}
	      else if (SCM_ICHR (item) == 'c')
		{
		  SCM_SETCDR (ans_pos,
			      scm_cons (SCM_MAKINUM (pmatch[0].final_tag), SCM_EOL));
		}
	      else
		SCM_ASSERT (0, spec, SCM_ARG3, s_regexec);
	      ans_pos = SCM_CDR (ans_pos);
	    }
	  else if (SCM_NIMP (item) && SCM_CONSP (item))
	    {
	      SCM ipos;
	      int solved;

	      solved = 0;
	      for (ipos = item; SCM_NIMP (ipos) && SCM_CONSP (ipos); ipos = SCM_CDR (ipos))
		{
		  SCM iitem;
		  int ival;
		  iitem = SCM_CAR (ipos);
		  SCM_ASSERT (SCM_INUMP (iitem), spec, SCM_ARG3, s_regexec);
		  ival = SCM_INUM (iitem);
		  SCM_ASSERT ((ival >= 0) && (ival < bound), spec, SCM_OUTOFRANGE, s_regexec);
		  if (pmatch[ival].rm_so >= 0)
		    {
 		      SCM_SETCDR (ans_pos, scm_cons (SCM_MAKINUM (ival), SCM_EOL));
		      ans_pos = SCM_CDR (ans_pos);
		      solved = 1;
		      break;
		    }
		}
	      if (!solved)
		{
		  SCM_SETCDR (ans_pos, scm_cons (SCM_BOOL_F, SCM_EOL));
		  ans_pos = SCM_CDR (ans_pos);
		}
	    }
	  else
	    {
	      int n;
	      SCM_ASSERT (SCM_INUMP (item), spec, SCM_ARG3, s_regexec);
	      n = SCM_INUM (item);
	      SCM_ASSERT ((n >= 0) && (n < bound), spec, SCM_OUTOFRANGE, s_regexec);
	      if (pmatch[n].rm_so < 0)
		{
		  SCM_SETCDR (ans_pos, scm_cons (SCM_BOOL_F, SCM_EOL));
		  ans_pos = SCM_CDR (ans_pos);
		  continue;
		}
	      frm = SCM_MAKINUM (pmatch[n].rm_so);
	      to = SCM_MAKINUM (pmatch[n].rm_eo);
	      SCM_SETCDR (ans_pos,
			  scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL));
	      ans_pos = SCM_CDR (ans_pos);
	    }
	}
      return SCM_CDR (answer);
    }
}



struct rx_dfa_state
{
  struct rx_classical_system frame;
  struct rx_unfa * unfa;
};

long scm_tc16_dfa_t;
#define DFA(X)	((struct rx_dfa_state *)SCM_CDR(X))
#define DFAP(X)	(SCM_CAR(X) == (SCM)scm_tc16_dfa_t)

size_t
free_dfa_t (obj)
     SCM obj;
{
  struct rx_dfa_state *r;
  r = DFA(obj);
  rx_terminate_system (&r->frame);
  rx_free_unfa (r->unfa);
  scm_must_free ((char *)r);
  return sizeof (struct rx_dfa_state);
}

int
print_dfa_t (obj, port, pstate)
     SCM obj;
     SCM port;
     scm_print_state *pstate;
{
  struct rx_dfa_state *r;
  r = DFA (obj);
  scm_gen_puts (scm_regular_string, "#<dfa ", port);
  scm_intprint (r->frame.rx->rx_id, 10, port);
  scm_gen_puts (scm_regular_string, ">", port);
  return 1;
}

static scm_smobfuns dfa_t_smob =
{ scm_mark0, free_dfa_t, print_dfa_t, 0 };


SCM_PROC (s_regexp_to_dfa, "regexp->dfa", 1, 1, 0, scm_regexp_to_dfa);
SCM 
scm_regexp_to_dfa (regexp, cfl)
     SCM regexp;
     SCM cfl;
{
  reg_errcode_t ret;
  unsigned int syntax;
  struct rx_dfa_state *r;
  struct rexp_node * parsed;
  int cflags;
  char * pattern;
  int len;
  SCM answer;

  SCM_ASSERT (SCM_NIMP (regexp) && SCM_ROSTRINGP (regexp),
	      regexp, SCM_ARG1, s_regexp_to_dfa);
  if (cfl == SCM_UNDEFINED)
    cfl = SCM_INUM0;
  SCM_ASSERT (SCM_INUMP (cfl), cfl, SCM_ARG2, s_regexp_to_dfa);

  pattern = SCM_ROCHARS (regexp);
  len = SCM_ROLENGTH (regexp);
  cflags = SCM_INUM (cfl);


  SCM_NEWCELL (answer);

  SCM_DEFER_INTS;
  syntax = ((cflags & REG_EXTENDED)
	    ? RE_SYNTAX_POSIX_EXTENDED
	    : RE_SYNTAX_POSIX_BASIC);

  if (cflags & REG_NEWLINE)
    {
      syntax &= ~RE_DOT_NEWLINE;
      syntax |= RE_HAT_LISTS_NOT_NEWLINE;
    }


  ret = rx_parse (&parsed, pattern, len, syntax, 256, 0);

  if (ret)
    scm_regex_error (s_regexp_to_dfa, ret);

  r = (struct rx_dfa_state *)scm_must_malloc (sizeof (struct rx_dfa_state), "dfa");
  r->unfa = rx_unfa (rx_basic_unfaniverse (), parsed, 256);
  rx_free_rexp (parsed);
  if (!r->unfa)
    {
      scm_mallocated -= sizeof (*r);
      scm_must_free ((char *)r);
      SCM_ALLOW_INTS;
      SCM_ASSERT (0, regexp, "internal error constructing rx_unfa", s_regexp_to_dfa);
    }

  rx_init_system (&r->frame, r->unfa->nfa);
  SCM_SETCAR (answer, scm_tc16_dfa_t);
  SCM_SETCDR (answer, (SCM)r);
  SCM_ALLOW_INTS;
  return answer;
}


SCM_PROC (s_dfa_fork, "dfa-fork", 1, 0, 0, scm_dfa_fork);
SCM
scm_dfa_fork (dfa)
     SCM dfa;
{
  struct rx_dfa_state *r;
  SCM answer;

  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_dfa_fork);

  SCM_NEWCELL (answer);

  SCM_DEFER_INTS;
  r = (struct rx_dfa_state *)scm_must_malloc (sizeof (struct rx_dfa_state), "dfa");
  rx_save_unfa (DFA (dfa)->unfa);
  r->unfa = DFA (dfa)->unfa;
  rx_init_system (&r->frame, r->unfa->nfa);
  r->frame.state = DFA(dfa)->frame.state;
  if (r->frame.state)
    rx_lock_superstate (r->frame.rx, r->frame.state);
  SCM_SETCAR (answer, scm_tc16_dfa_t);
  SCM_SETCDR (answer, (SCM)r);
  SCM_ALLOW_INTS;
  return answer;
}


SCM_PROC (s_reset_dfa_x, "reset-dfa!", 1, 0, 0, scm_reset_dfa_x);
SCM
scm_reset_dfa_x (dfa)
     SCM dfa;
{
  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_reset_dfa_x);
  
  SCM_DEFER_INTS;
  if (rx_yes != rx_start_superstate (&DFA(dfa)->frame))
    {
      SCM_ALLOW_INTS;
      SCM_ASSERT (0, dfa, "internal error constructing rx starting superstate", s_reset_dfa_x);
    }
  SCM_ALLOW_INTS;
  return dfa;
}



SCM_PROC (s_dfa_final_tag, "dfa-final-tag", 1, 0, 0, scm_dfa_final_tag);
SCM
scm_dfa_final_tag (dfa)
     SCM dfa;
{
  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_dfa_final_tag);

  if (DFA (dfa)->frame.state)
    return scm_long2num ((long)DFA (dfa)->frame.state->contents->is_final);
  else
    return SCM_INUM0;
}



SCM_PROC (s_dfa_continuable_p, "dfa-continuable?", 1, 0, 0, scm_dfa_continuable_p);
SCM
scm_dfa_continuable_p (dfa)
     SCM dfa;
{
  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_dfa_continuable_p);

  return ((DFA (dfa)->frame.state && DFA (dfa)->frame.state->contents->has_cset_edges)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_PROC (s_advance_dfa_x, "advance-dfa!", 2, 0, 0, scm_advance_dfa_x);
SCM
scm_advance_dfa_x (dfa, s)
     SCM dfa;
     SCM s;
{
  struct rx_dfa_state * d;
  char * str;
  int len;
  int matched;
  
  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_advance_dfa_x);
  SCM_ASSERT (SCM_NIMP (s) && SCM_ROSTRINGP (s),
	      s, SCM_ARG2, s_advance_dfa_x);

  str = SCM_ROCHARS (s);
  len = SCM_ROLENGTH (s);
  d = DFA (dfa);

  SCM_DEFER_INTS;
  matched = rx_advance_to_final (&d->frame, (unsigned char *)str, len);
  SCM_ALLOW_INTS;

  if (matched >= 0)
    scm_return_first (SCM_MAKINUM (matched), dfa, s);
  else
    SCM_ASSERT (0, dfa, "internal error in rx_advance_to_final", s_advance_dfa_x);
}






void
scm_init_rgx ()
{
  scm_add_feature ("regex");
  scm_tc16_regex_t = scm_newsmob (&regex_t_smob);
  scm_tc16_dfa_t = scm_newsmob (&dfa_t_smob);
  scm_regex_error_key
    = scm_permanent_object (SCM_CAR (scm_intern0 ("regex-error")));

  /* from inst-rxposix.h  */
  scm_sysintern ("REG_EXTENDED", SCM_MAKINUM (REG_EXTENDED));
  scm_sysintern ("REG_ICASE", SCM_MAKINUM (REG_ICASE));
  scm_sysintern ("REG_NEWLINE", SCM_MAKINUM (REG_NEWLINE));
  scm_sysintern ("REG_NOTBOL", SCM_MAKINUM (REG_NOTBOL));
  scm_sysintern ("REG_NOTEOL", SCM_MAKINUM (REG_NOTEOL));

  /* from rxgnucomp.h, can be used by regexp->dfa.  */
  scm_sysintern ("RE_BACKSLASH_ESCAPE_IN_LISTS", SCM_MAKINUM (RE_BACKSLASH_ESCAPE_IN_LISTS));
  scm_sysintern ("RE_BK_PLUS_QM", SCM_MAKINUM (RE_BK_PLUS_QM));
  scm_sysintern ("RE_CHAR_CLASSES", SCM_MAKINUM (RE_CHAR_CLASSES));
  scm_sysintern ("RE_CONTEXT_INDEP_ANCHORS", SCM_MAKINUM (RE_CONTEXT_INDEP_ANCHORS));
  scm_sysintern ("RE_CONTEXT_INDEP_OPS", SCM_MAKINUM (RE_CONTEXT_INDEP_OPS));
  scm_sysintern ("RE_CONTEXT_INVALID_OPS", SCM_MAKINUM (RE_CONTEXT_INVALID_OPS));
  scm_sysintern ("RE_DOT_NEWLINE", SCM_MAKINUM (RE_DOT_NEWLINE));
  scm_sysintern ("RE_DOT_NOT_NULL", SCM_MAKINUM (RE_DOT_NOT_NULL));
  scm_sysintern ("RE_HAT_LISTS_NOT_NEWLINE", SCM_MAKINUM (RE_HAT_LISTS_NOT_NEWLINE));
  scm_sysintern ("RE_INTERVALS", SCM_MAKINUM (RE_INTERVALS));
  scm_sysintern ("RE_LIMITED_OPS", SCM_MAKINUM (RE_LIMITED_OPS));
  scm_sysintern ("RE_NEWLINE_ALT", SCM_MAKINUM (RE_NEWLINE_ALT));
  scm_sysintern ("RE_NO_BK_BRACES", SCM_MAKINUM (RE_NO_BK_BRACES));
  scm_sysintern ("RE_NO_BK_PARENS", SCM_MAKINUM (RE_NO_BK_PARENS));
  scm_sysintern ("RE_NO_BK_REFS", SCM_MAKINUM (RE_NO_BK_REFS));
  scm_sysintern ("RE_NO_BK_VBAR", SCM_MAKINUM (RE_NO_BK_VBAR));
  scm_sysintern ("RE_NO_EMPTY_RANGES", SCM_MAKINUM (RE_NO_EMPTY_RANGES));
  scm_sysintern ("RE_UNMATCHED_RIGHT_PAREN_ORD", SCM_MAKINUM (RE_UNMATCHED_RIGHT_PAREN_ORD));
  scm_sysintern ("RE_SYNTAX_EMACS", SCM_MAKINUM (RE_SYNTAX_EMACS));
  scm_sysintern ("RE_SYNTAX_AWK", SCM_MAKINUM (RE_SYNTAX_AWK));
  scm_sysintern ("RE_SYNTAX_POSIX_AWK", SCM_MAKINUM (RE_SYNTAX_POSIX_AWK));
  scm_sysintern ("RE_SYNTAX_GREP", SCM_MAKINUM (RE_SYNTAX_GREP));
  scm_sysintern ("RE_SYNTAX_EGREP", SCM_MAKINUM (RE_SYNTAX_EGREP));
  scm_sysintern ("RE_SYNTAX_POSIX_EGREP", SCM_MAKINUM (RE_SYNTAX_POSIX_EGREP));
  scm_sysintern ("RE_SYNTAX_SED", SCM_MAKINUM (RE_SYNTAX_SED));
  scm_sysintern ("RE_SYNTAX_POSIX_BASIC", SCM_MAKINUM (RE_SYNTAX_POSIX_BASIC));
  scm_sysintern ("RE_SYNTAX_POSIX_MINIMAL_BASIC", SCM_MAKINUM (RE_SYNTAX_POSIX_MINIMAL_BASIC));
  scm_sysintern ("RE_SYNTAX_POSIX_EXTENDED", SCM_MAKINUM (RE_SYNTAX_POSIX_EXTENDED));
  scm_sysintern ("RE_SYNTAX_POSIX_MINIMAL_EXTENDED", SCM_MAKINUM (RE_SYNTAX_POSIX_MINIMAL_EXTENDED));

#include "rgx.x"
}

