/*
 *
 * t k - g l u e . c 		- Glue function between the scheme and Tk worlds
 *
 * Copyright © 1993-1996 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
 * 
 *
 * Permission to use, copy, and/or distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that both the above copyright notice and this permission notice appear in
 * all copies and derived works.  Fees for distribution or use of this
 * software or derived works may only be charged with express written
 * permission of the copyright holder.  
 * This software is provided ``as is'' without express or implied warranty.
 *
 * This software is a derivative work of other copyrighted softwares; the
 * copyright notices of these softwares are placed in the file COPYRIGHTS
 *
 *
 *            Author: Erick Gallesio [eg@unice.fr]
 *    Creation date: 19-Feb-1993 22:15
 * Last file update: 13-Jun-1996 19:10
 *
 *
 */

#include <libguile.h>

/* #include "libguile/ports.h" */
/* #include "libguile/throw.h" */
/* #include "libguile/snarf.h" */

#include "gstk.h"
#include "tkcmd.h"
#include "tk-glue.h"
/* #include "gc.h" */
#include "tkInt.h"

#define MAXARG 64	/* Max args on stack. Use malloc if greater */

SCM_SYMBOL (scm_tk_error_key, "tk-error");
void
scm_tk_error (subr, message, args)
     char *subr;
     char *message;
     SCM args;
{
  scm_error (scm_tk_error_key, subr, message, args, SCM_BOOL_F);
}

/* Scheme objects used to represent the "." pseudo widget and its name */
SCM STk_root_window;
/* SCM STk_root_window_name; */

/* Last result of Tcl_GlobalEval (as a SCM object rather than a string) */
SCM STk_last_Tk_result; /*fixme* Need to get this thread safe. */

SCM_SYMBOL (scm_sym_dot, ".");

struct sr_data {
  SCM port;
  int *errp;
};

static SCM
sr_body (struct sr_data *data, SCM jmpbuf)
{
  SCM result = scm_read (data->port);
  *data->errp = 0;
  return result;
}

static SCM
sr_handler (struct sr_data *data, SCM tag, SCM args)
{
  *data->errp = 1;
  return SCM_BOOL_F;
}

static SCM
scm_safe_read (SCM port, int *errp)
{
  struct sr_data data = { port, errp };
  return scm_internal_catch (SCM_BOOL_T,
			     (scm_catch_body_t) sr_body, &data,
			     (scm_catch_handler_t) sr_handler, &data);
}

/*fixme* need to handle #pxxxx tokens */
static SCM TkResult2Scheme(Tcl_Interp *interp)
{
  register char *s = interp->result;
  register SCM tmp1, tmp2, z, port;
  SCM result = SCM_EOL;
  int errp;

  if (*s) {
    port = scm_mkstrport (SCM_MAKINUM (0),
			  scm_makfrom0str (s),
			  SCM_OPN | SCM_RDNG,
			  "TkResult2Scheme");    
    result = scm_read (port);
    if (result == scm_sym_dot) result = STk_root_window;

    if (!SCM_EOF_OBJECT_P (result)) {
      /*  Result was a list of value, build a proper Scheme list */
      tmp1 = result = SCM_LIST1 (result);
      for ( ; ; ) {
	z = scm_safe_read (port, &errp);
	if (errp || SCM_EOF_OBJECT_P (z)) break;
	if (z == scm_sym_dot) z = STk_root_window;
	SCM_NEWCELL (tmp2);
	SCM_SETCAR (tmp2, z); 
	SCM_SETCDR (tmp1, tmp2);
	tmp1      = tmp2;
      }
      SCM_SETCDR (tmp1, SCM_EOL);
    }
    /* close_string_port(port); */
  }

  Tcl_ResetResult(interp); 
  return errp ? SCM_UNSPECIFIED : result;
}

/*fixme* need to output ports as #filexxxx */
char *
STk_convert_for_Tk (SCM obj, SCM *res)
{
  if (IMP (obj))
    {
      if (SCM_INUMP (obj))
	{
	  *res = scm_number_to_string (obj, SCM_MAKINUM (10L));
	  return SCM_CHARS (*res);
	}
      else if (obj == SCM_BOOL_T)
	return "#t";
      else if (obj == SCM_BOOL_F)
	return "#f";
      goto dflt;
    }
  else
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_symbols:
      case scm_tc7_string:
	*res = obj;
	return SCM_ROCHARS (obj);
      case scm_tc7_smob:
	if (SCM_NUMP (obj))
	  {
	    *res = scm_number_to_string (obj, SCM_MAKINUM (10L));
	    return SCM_ROCHARS (*res);
	  }
	if (SCM_KEYWORDP (obj))
	  {
	    *res = obj;
	    return SCM_CHARS (SCM_CDR (obj));
	  }
	goto dflt;
      case scm_tcs_cons_gloc:
	if (SCM_TKCMDP (obj))
	  {
	    *res = obj;
	    return SCM_TKCMD (obj)->Id;
	  }
      default:          /* Ok, take the big hammer (i.e. use a string port for 
			 * type coercion) Here, use write (and not display) 
			 * since it handles complex data structures containing
			 * eventually special chars which must be escaped
			 * Ex: (bind .w "<Enter>" '(display "<Enter>"))
			 *     First <Enter> is unquotted and second is not
			 */
      dflt:
	{
	  SCM port;
	  
	  port = scm_mkstrport (SCM_INUM0,
				scm_make_string(SCM_MAKINUM(30),
						SCM_UNDEFINED),
				SCM_OPN | SCM_WRTNG,
				"STk_convert_for_Tk");
	  scm_write (obj, port); 
	  *res = scm_makfromstr (SCM_CHARS (SCM_CDR (SCM_STREAM (port))),
				 SCM_INUM (SCM_CAR (SCM_STREAM (port))),
				 0);
	  return SCM_CHARS (*res);
	}
      }
}

 
/******************************************************************************
 *
 * Callback management
 *
 ******************************************************************************/

static Tcl_HashTable Tk_callbacks;


int STk_valid_callback(char *s, void **closure)
{
  /* A callback is valid iff it is of the form "#pxxxx" where xxxx is composed
   * only of hexadecimal digit.
   * Furthermore, the given address must  be a valid adress
   */
  int l = strlen(s);
  char *p;

  *closure = NULL;
  if (l > 2) {
    if (s[0] == '#' && s[1] == 'p') {
      /* Verify that the rest of the string only contains hexadecimal digits */
      for (p = s + 2; *p; p++)
	if (!isxdigit(*p)) return 0;

      sscanf(s+2, "%lx", (unsigned long) closure);
      if (!scm_cellp((SCM) *closure)) return 0;
    }
  }
  return 1;
}

void STk_add_callback(char *key1, char *key2, char *key3, SCM closure)
{
  Tcl_HashEntry *entry;
  Tcl_HashTable *secondary_hash_table;
  int new;
  char key[200]; /* Largely sufficient */

  if (*key2) {
    /* We have two keys. Use a secondary hash table */
    if (entry=Tcl_FindHashEntry(&Tk_callbacks, key1))
      /* Key already in hash table */
      secondary_hash_table = Tcl_GetHashValue(entry);
    else {
      secondary_hash_table = ((Tcl_HashTable *)
			      scm_must_malloc (sizeof(Tcl_HashTable),
					       "STk_add_callback"));
      Tcl_InitHashTable(secondary_hash_table, TCL_STRING_KEYS);
      entry = Tcl_CreateHashEntry(&Tk_callbacks, (char *) key1, &new);
      Tcl_SetHashValue(entry, secondary_hash_table);
    }
    
    /* Enter a new key (obtained from key2 and key3) in the hash table.
     * Don't worry about old value: since it is no more pointed by the 
     * hash table, it will be garbaged at next GC run
     */
    sprintf(key, "%s#%s", key2, key3);/* Create a new key from key2 and key3 */
    entry = Tcl_CreateHashEntry(secondary_hash_table, key, &new);
    Tcl_SetHashValue(entry, closure); 
  }
  else {
    /* Only one key. No need for a secondary hash table */
    entry =Tcl_CreateHashEntry(&Tk_callbacks, key1, &new);
    Tcl_SetHashValue(entry, closure);
  }
}


void STk_delete_callback(char *key)
{
  /*
   * key is destroyed. We only need to free the entry associated to it in the 
   * Tk_callback hash table (if it exists).
   */
  Tcl_HashEntry *entry;
  Tcl_HashTable *secondary_hash_table;

  if (entry=Tcl_FindHashEntry(&Tk_callbacks, key)) {
    if (*key != 'a' && strncmp(key, "after#", 6) != 0) {
      /* Delete the secondary hash table associated to this entry */
      secondary_hash_table = Tcl_GetHashValue(entry);
      Tcl_DeleteHashTable(secondary_hash_table);
      scm_must_free ((char*) secondary_hash_table);
    }
    /* Delete the entry itself */
    Tcl_DeleteHashEntry(entry);
  }
}

void STk_mark_callbacks(void)
{
  Tcl_HashEntry *entry1, *entry2;
  Tcl_HashSearch search1, search2;
  Tcl_HashTable *secondary;
  char *key;

  for (entry1 = Tcl_FirstHashEntry(&Tk_callbacks, &search1);
       entry1;
       entry1 = Tcl_NextHashEntry(&search1)) {
    
    key = Tcl_GetHashKey(&Tk_callbacks, entry1);
    if (*key == 'a' && strncmp(key, "after#", 6) == 0) {
      /* No secondary hash table */
      scm_gc_mark((SCM) Tcl_GetHashValue(entry1));
    }
    else {
      /* We have a secondary hash table. Scan it  */
      secondary = Tcl_GetHashValue(entry1);
      for (entry2 = Tcl_FirstHashEntry(secondary, &search2);
	   entry2;
	   entry2 = Tcl_NextHashEntry(&search2)) {
	
	scm_gc_mark((SCM) Tcl_GetHashValue(entry2));
      }
    }
  }
}

/* 
 * Return the parameters associated to the callback contained (as a string)
 * in the value parameter. If an error occurs, this function returns NULL
 *
 */
static char s_STk_append_callback_parameters[] = "STk_append_callback_parameters";
char *
STk_append_callback_parameters (SCM proc)
{
  SCM param, port;

  if (!(SCM_NIMP (proc) && SCM_CLOSUREP(proc)))
    return NULL;
  param = SCM_CAR (SCM_CODE (proc));

  if (SCM_NULLP(param) || (SCM_NIMP (param) && SCM_CONSP(param)))
    {
      char *new;
      int len;
      SCM pstate;
      port = scm_mkstrport (SCM_INUM0,
			    scm_make_string(SCM_MAKINUM(30),
					    SCM_UNDEFINED),
			    SCM_OPN | SCM_WRTNG,
			    s_STk_append_callback_parameters);
      scm_gen_puts (scm_regular_string, "(#p", port);
      scm_intprint (proc, 16, port);
      pstate = scm_make_print_state ();
      scm_iprlist (" ", param, ')', port, SCM_PRINT_STATE (pstate));
      scm_free_print_state (pstate);
      len = SCM_INUM (SCM_CAR (SCM_STREAM (port)));
      /*fixme* Code should be reorganized so that the following isn't
               necessary */
      new = ckalloc (len + 1);
      strncpy (new, SCM_CHARS (SCM_CDR (SCM_STREAM (port))), len);
      new[len] = '\0';
      return new;
    }
  return NULL;
}


/******************************************************************************
 *
 * Tcl result manipulation functions
 *
 ******************************************************************************/

void STk_sharp_dot_result(Tcl_Interp *interp, char *value)
{
  /* Transform Tcl result in #.result so that it is evaluated when read */
  int len = strlen(value);
  char *s;

  s = (char *) scm_must_malloc (len + 3, "STk_sharp_dot_result");
  s[0] = '#';
  s[1] = '.';
  strcpy(s+2, value);
  
  Tcl_SetResult(interp, s, TCL_VOLATILE);
}

void STk_stringify_result(Tcl_Interp *interp, char *value)
{
  /* Transform Tcl result in "result" with " and \ escaped */
  Tcl_SetResult(interp,  STk_stringify(value, 0), TCL_VOLATILE);
}

SCM STk_last_Tk_as_SCM(void)
{
  return STk_last_Tk_result;
}

SCM STk_get_NIL_value(void)
{
  return SCM_EOL;
}


/*
 * STk_stringify permits to transform the string "s" in a valid STk string.
 * Original string is deallocated if free_original is 1 
 */

char *STk_stringify(char *s, int free_original)
{
  char *res, *d;
  
  if (s == NULL)
    s = "";
  /* worst overestimation */
  res = d = scm_must_malloc(2 * strlen(s) + 3, "STk_stringify");
  
  for ( *d++ = '"'; *s; s++, d++) {
    if (*s == '"' || *s == '\\') *d++ = '\\';
    *d = *s;
  }
  *d++ = '"';
  *d   = '\0';
  
  if (free_original)
    scm_must_free (s);
  return res;
}


/******************************************************************************
 *
 * Motif simulation
 *
 * Tk 4.0 uses a field in the Tk_Window structure to tell the library if it
 * must be conform to Motif look. This field is Tcl_LinkVar'ed.
 *
 ******************************************************************************/

static SCM get_Motif(char *s)
{
  TkWindow *p = (TkWindow *) Tk_MainWindow(STk_main_interp);
  return (p->mainPtr->strictMotif) ? SCM_BOOL_T: SCM_BOOL_F;
}

static void set_Motif(char *s, SCM value)
{ 
  TkWindow *p = (TkWindow *) Tk_MainWindow(STk_main_interp);
  p->mainPtr->strictMotif = !(value == SCM_BOOL_F);
}


void
scm_init_tk_glue (void)
{
  /* 
   * Take into account the fact that Tk main window  name (i.e. ``.'') 
   * cannot be used in list since it leads to erroneous evaluation 
   * (e.g. [focus .] would produce an error since read will find a malformed
   * pair).
   *
   */
  STk_root_window     = SCM_CDR (scm_sysintern (ROOT_WINDOW,
						scm_eval (scm_sym_dot)));
  
  /* Init the callback table */
  Tcl_InitHashTable(&Tk_callbacks, TCL_STRING_KEYS);
  
  /* Associate a getter and a setter for the global variable *Tk-strict-Motif*  */
  /* STk_define_C_variable("*tk-strict-motif*", get_Motif, set_Motif); */
}
