/* Copyright (C) 1994, 1995, 1996, 1997, 1998, 2000, 2001 Free Software Foundation, Inc.
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

/* "script.c" argv tricks for `#!' scripts.
   Authors: Aubrey Jaffer and Jim Blandy */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */

#include <stdio.h>
#include <errno.h>
#include <ctype.h>

#include "libguile/_scm.h"
#include "libguile/gh.h"
#include "libguile/load.h"
#include "libguile/version.h"

#include "libguile/validate.h"
#include "libguile/script.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>		/* for X_OK define */
#endif

/* Concatentate str2 onto str1 at position n and return concatenated
   string if file exists; 0 otherwise. */

static char *
scm_cat_path (char *str1, const char *str2, long n)
{
  if (!n)
    n = strlen (str2);
  if (str1)
    {
      long len = strlen (str1);
      str1 = (char *) realloc (str1, (scm_sizet) (len + n + 1));
      if (!str1)
	return 0L;
      strncat (str1 + len, str2, n);
      return str1;
    }
  str1 = (char *) malloc ((scm_sizet) (n + 1));
  if (!str1)
    return 0L;
  str1[0] = 0;
  strncat (str1, str2, n);
  return str1;
}

#if 0 
static char *
scm_try_path (char *path)
{
  FILE *f;
  /* fprintf(stderr, "Trying %s\n", path);fflush(stderr); */
  if (!path)
    return 0L;
  SCM_SYSCALL (f = fopen (path, "r");
    );
  if (f)
    {
      fclose (f);
      return path;
    }
  free (path);
  return 0L;
}

static char *
scm_sep_init_try (char *path, const char *sep, const char *initname)
{
  if (path)
    path = scm_cat_path (path, sep, 0L);
  if (path)
    path = scm_cat_path (path, initname, 0L);
  return scm_try_path (path);
}
#endif 

#ifndef LINE_INCREMENTORS
#define LINE_INCREMENTORS  '\n'
#ifdef MSDOS
#define WHITE_SPACES  ' ':case '\t':case '\r':case '\f':case 26
#else
#define WHITE_SPACES  ' ':case '\t':case '\r':case '\f'
#endif /* def MSDOS */
#endif /* ndef LINE_INCREMENTORS */

#ifndef MAXPATHLEN
#define MAXPATHLEN 80
#endif /* ndef MAXPATHLEN */
#ifndef X_OK
#define X_OK 1
#endif /* ndef X_OK */

char *
scm_find_executable (const char *name)
{
  char tbuf[MAXPATHLEN];
  int i = 0;
  FILE *f;

  /* fprintf(stderr, "s_f_e checking access %s ->%d\n", name, access(name, X_OK)); fflush(stderr); */
  if (access (name, X_OK))
    return 0L;
  f = fopen (name, "r");
  if (!f)
    return 0L;
  if ((fgetc (f) == '#') && (fgetc (f) == '!'))
    {
      while (1)
	switch (tbuf[i++] = fgetc (f))
	  {
	  case /*WHITE_SPACES */ ' ':
	  case '\t':
	  case '\r':
	  case '\f':
	  case EOF:
	    tbuf[--i] = 0;
	    fclose (f);
	    return scm_cat_path (0L, tbuf, 0L);
	  }
    }
  fclose (f);
  return scm_cat_path (0L, name, 0L);
}


/* Read a \nnn-style escape.  We've just read the backslash.  */
static int
script_get_octal (FILE *f)
#define FUNC_NAME "script_get_octal"
{
  int i;
  int value = 0;

  for (i = 0; i < 3; i++)
    {
      int c = getc (f);
      if ('0' <= c && c <= '7')
	value = (value * 8) + (c - '0');
      else
	SCM_MISC_ERROR ("malformed script: bad octal backslash escape",
			SCM_EOL);
    }
  return value;
}
#undef FUNC_NAME


static int
script_get_backslash (FILE *f)
#define FUNC_NAME "script_get_backslash"
{
  int c = getc (f);

  switch (c)
    {
    case 'a': return '\a';
    case 'b': return '\b';
    case 'f': return '\f';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    case 'v': return '\v';

    case '\\':
    case ' ':
    case '\t':
    case '\n':
      return c;

    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
      ungetc (c, f);
      return script_get_octal (f);

    case EOF:
      SCM_MISC_ERROR ("malformed script: backslash followed by EOF", SCM_EOL);
      return 0; /* not reached? */

    default:
      SCM_MISC_ERROR ("malformed script: bad backslash sequence", SCM_EOL);
      return 0; /* not reached? */
    }
}
#undef FUNC_NAME


static char *
script_read_arg (FILE *f)
#define FUNC_NAME "script_read_arg"
{
  int size = 7;
  char *buf = malloc (size + 1);
  int len = 0;

  if (! buf)
    return 0;

  for (;;)
    {
      int c = getc (f);
      switch (c)
	{
	case '\\':
	  c = script_get_backslash (f);
	  /* The above produces a new character to add to the argument.
             Fall through.  */
	default:
	  if (len >= size)
	    {
	      size = (size + 1) * 2;
	      buf = realloc (buf, size);
	      if (! buf)
		return 0;
	    }
	  buf[len++] = c;
	  break;

	case '\n':
	  /* This may terminate an arg now, but it will terminate the
             entire list next time through.  */
	  ungetc ('\n', f);
	case EOF:
	  if (len == 0)
	    {
	      free (buf);
	      return 0;
	    }
	  /* Otherwise, those characters terminate the argument; fall
             through.  */
	case ' ':
	  buf[len] = '\0';
	  return buf;

	case '\t':
	  free (buf);
	  SCM_MISC_ERROR ("malformed script: TAB in meta-arguments", SCM_EOL);
	  return 0; /* not reached? */
	}
    }
}
#undef FUNC_NAME


static int
script_meta_arg_P (char *arg)
{
  if ('\\' != arg[0])
    return 0L;
#ifdef MSDOS
  return !arg[1];
#else
  switch (arg[1])
    {
    case 0:
    case '%':
    case WHITE_SPACES:
      return !0;
    default:
      return 0L;
    }
#endif
}

char **
scm_get_meta_args (int argc, char **argv)
{
  int nargc = argc, argi = 1, nargi = 1;
  char *narg, **nargv;
  if (!(argc > 2 && script_meta_arg_P (argv[1])))
    return 0L;
  if (!(nargv = (char **) malloc ((1 + nargc) * sizeof (char *))))
      return 0L;
  nargv[0] = argv[0];
  while (((argi + 1) < argc) && (script_meta_arg_P (argv[argi])))
    {
      FILE *f = fopen (argv[++argi], "r");
      if (f)
	{
	  nargc--;		/* to compensate for replacement of '\\' */
	  while (1)
	    switch (getc (f))
	      {
	      case EOF:
		return 0L;
	      default:
		continue;
	      case '\n':
		goto found_args;
	      }
	found_args:
	  while ((narg = script_read_arg (f)))
	    if (!(nargv = (char **) realloc (nargv,
					     (1 + ++nargc) * sizeof (char *))))
	        return 0L;
	    else
	      nargv[nargi++] = narg;
	  fclose (f);
	  nargv[nargi++] = argv[argi++];
	}
    }
  while (argi <= argc)
    nargv[nargi++] = argv[argi++];
  return nargv;
}

int
scm_count_argv (char **argv)
{
  int argc = 0;
  while (argv[argc])
    argc++;
  return argc;
}


/* For use in error messages.  */
char *scm_usage_name = 0;

void
scm_shell_usage (int fatal, char *message)
{
  if (message)
    fprintf (stderr, "%s\n", message);

  fprintf (stderr, 
           "Usage: %s OPTION ...\n"
           "Evaluate Scheme code, interactively or from a script.\n"
           "\n"
           "  -s SCRIPT      load Scheme source code from FILE, and exit\n"
           "  -c EXPR        evalute Scheme expression EXPR, and exit\n"
           "  --             stop scanning arguments; run interactively\n"
           "The above switches stop argument processing, and pass all\n"
           "remaining arguments as the value of (command-line).\n"
           "\n"
           "  -l FILE        load Scheme source code from FILE\n"
           "  -e FUNCTION    after reading script, apply FUNCTION to\n"
           "                 command line arguments\n"
           "  -ds            do -s script at this point\n"
           "  --debug        start with debugging evaluator and backtraces\n"
	   "  -q             inhibit loading of user init file\n"
           "  --emacs        enable Emacs protocol (experimental)\n"
           "  -h, --help     display this help and exit\n"
           "  -v, --version  display version information and exit\n"
	   "  \\              read arguments from following script lines\n",
           scm_usage_name);

  if (fatal)
    exit (fatal);
}


/* Some symbols used by the command-line compiler.  */
SCM_SYMBOL (sym_load, "load");
SCM_SYMBOL (sym_eval_string, "eval-string");
SCM_SYMBOL (sym_command_line, "command-line");
SCM_SYMBOL (sym_begin, "begin");
SCM_SYMBOL (sym_load_user_init, "load-user-init");
SCM_SYMBOL (sym_top_repl, "top-repl");
SCM_SYMBOL (sym_quit, "quit");


/* Given an array of command-line switches, return a Scheme expression
   to carry out the actions specified by the switches.

   If you told me this should have been written in Scheme, I'd
   probably agree.  I'd say I didn't feel comfortable doing that in
   the present system.  You'd say, well, fix the system so you are
   comfortable doing that.  I'd agree again.  *shrug*
 */

static char guile[] = "guile";

SCM
scm_compile_shell_switches (int argc, char **argv)
{
  SCM tail = SCM_EOL;		/* We accumulate the list backwards,
				   and then reverse! it before we
				   return it.  */
  SCM do_script = SCM_EOL;	/* The element of the list containing
				   the "load" command, in case we get
				   the "-ds" switch.  */
  SCM entry_point = SCM_EOL;	/* for -e switch */
  int interactive = 1;		/* Should we go interactive when done? */
  int inhibit_user_init = 0;	/* Don't load user init file */
  int use_emacs_interface = 0;
  int i;
  char *argv0 = guile;

  if (argc > 0)
    {
      argv0 = argv[0];
      scm_usage_name = strrchr (argv[0], '/');
      if (! scm_usage_name)
	scm_usage_name = argv[0];
      else
	scm_usage_name++;
    }
  if (! scm_usage_name)
    scm_usage_name = guile;
  
  for (i = 1; i < argc; i++)
    {
      if (! strcmp (argv[i], "-s")) /* load script */
	{
	  if (++i >= argc)
	    scm_shell_usage (1, "missing argument to `-s' switch");

	  /* If we specified the -ds option, do_script points to the
	     cdr of an expression like (load #f); we replace the car
	     (i.e., the #f) with the script name.  */
	  if (!SCM_NULLP (do_script))
	    {
	      SCM_SETCAR (do_script, scm_makfrom0str (argv[i]));
	      do_script = SCM_EOL;
	    }
	  else
	    /* Construct an application of LOAD to the script name.  */
	    tail = scm_cons (scm_cons2 (sym_load,
					scm_makfrom0str (argv[i]),
					SCM_EOL),
			       tail);
	  argv0 = argv[i];
	  i++;
	  interactive = 0;
	  break;
	}

      else if (! strcmp (argv[i], "-c")) /* evaluate expr */
	{
	  if (++i >= argc)
	    scm_shell_usage (1, "missing argument to `-c' switch");
	  tail = scm_cons (scm_cons2 (sym_eval_string,
				      scm_makfrom0str (argv[i]),
				      SCM_EOL),
			   tail);
	  i++;
	  interactive = 0;
	  break;
	}

      else if (! strcmp (argv[i], "--")) /* end args; go interactive */
	{
	  i++;
	  break;
	}

      else if (! strcmp (argv[i], "-l")) /* load a file */
	{
	  if (++i < argc)
	    tail = scm_cons (scm_cons2 (sym_load,
					scm_makfrom0str (argv[i]),
					SCM_EOL),
			     tail);
	  else
	    scm_shell_usage (1, "missing argument to `-l' switch");
	}	  

      else if (! strcmp (argv[i], "-e")) /* entry point */
	{
	  if (++i < argc)
	    entry_point = gh_symbol2scm (argv[i]);
	  else
	    scm_shell_usage (1, "missing argument to `-e' switch");
	}

      else if (! strcmp (argv[i], "-ds")) /* do script here */
	{
	  /* We put a dummy "load" expression, and let the -s put the
             filename in.  */
	  if (!SCM_NULLP (do_script))
	    scm_shell_usage (1, "the -ds switch may only be specified once");
	  do_script = scm_cons (SCM_BOOL_F, SCM_EOL);
	  tail = scm_cons (scm_cons (sym_load, do_script),
			   tail);
	}

      else if (! strcmp (argv[i], "--debug")) /* debug eval + backtraces */
	{
	  SCM_DEVAL_P = 1;
	  SCM_BACKTRACE_P = 1;
	  SCM_RECORD_POSITIONS_P = 1;
	  SCM_RESET_DEBUG_MODE;
	}

      else if (! strcmp (argv[i], "--emacs")) /* use emacs protocol */ 
	use_emacs_interface = 1;

      else if (! strcmp (argv[i], "-q")) /* don't load user init */ 
	inhibit_user_init = 1;

      else if (! strcmp (argv[i], "-h")
	       || ! strcmp (argv[i], "--help"))
	{
	  scm_shell_usage (0, 0);
	  exit (0);
	}

      else if (! strcmp (argv[i], "-v")
	       || ! strcmp (argv[i], "--version"))
	{
	  /* Print version number.  */
	  printf ("Guile %s\n"
		  "Copyright (c) 1995, 1996, 1997, 2000 Free Software Foundation\n"
		  "Guile may be distributed under the terms of the GNU General Public Licence;\n"
		  "certain other uses are permitted as well.  For details, see the file\n"
		  "`COPYING', which is included in the Guile distribution.\n"
		  "There is no warranty, to the extent permitted by law.\n",
		  SCM_STRING_CHARS (scm_version ()));
	  exit (0);
	}

      else
	{
	  fprintf (stderr, "%s: Unrecognized switch `%s'\n",
		   scm_usage_name, argv[i]);
	  scm_shell_usage (1, 0);
	}
    }

  /* Check to make sure the -ds got a -s. */
  if (!SCM_NULLP (do_script))
    scm_shell_usage (1, "the `-ds' switch requires the use of `-s' as well");

  /* Make any remaining arguments available to the
     script/command/whatever.  */
  scm_set_program_arguments (argc ? argc - i : 0, argv + i, argv0);
  
  /* If the --emacs switch was set, now is when we process it.  */
  scm_define ("use-emacs-interface", SCM_BOOL (use_emacs_interface));

  /* Handle the `-e' switch, if it was specified.  */
  if (!SCM_NULLP (entry_point))
    tail = scm_cons (scm_cons2 (entry_point,
				scm_cons (sym_command_line, SCM_EOL),
				SCM_EOL),
		       tail);

  /* If we didn't end with a -c or a -s, start the repl.  */
  if (interactive)
    {
      tail = scm_cons (scm_cons (sym_top_repl, SCM_EOL), tail);
    }
  else
    {
      /* After doing all the other actions prescribed by the command line,
	 quit.  */
      tail = scm_cons (scm_cons (sym_quit, SCM_EOL),
		       tail);
      /* Allow asyncs (signal handlers etc.) to be run.  */
      scm_mask_ints = 0;
    }

  /* After the following line, actions will be added to the front. */
  tail = scm_reverse_x (tail, SCM_UNDEFINED);
  
  /* If we didn't end with a -c or a -s and didn't supply a -q, load
     the user's customization file.  */
  if (interactive && !inhibit_user_init)
    {
      tail = scm_cons (scm_cons (sym_load_user_init, SCM_EOL), tail);
    }

  {
    SCM val = scm_cons (sym_begin, tail);

#if 0
    scm_write (val, SCM_UNDEFINED);
    scm_newline (SCM_UNDEFINED);
#endif
    
    return val;
  }
}


void
scm_shell (int argc, char **argv)
{
  /* If present, add SCSH-style meta-arguments from the top of the
     script file to the argument vector.  See the SCSH manual: "The
     meta argument" for more details.  */
  {
    char **new_argv = scm_get_meta_args (argc, argv);

    if (new_argv)
      {
	argv = new_argv;
	argc = scm_count_argv (new_argv);
      }
  }

  exit (scm_exit_status (scm_eval_x (scm_compile_shell_switches (argc, argv),
				     scm_the_root_module ())));
}


void
scm_init_script ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/script.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
