/* Copyright (C) 2001 Free Software Foundation, Inc.
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

#include <libguile.h>
#include <libguile/values.h>
#include <lightning.h>
#include <dlfcn.h>
#include <stdarg.h>

#include "disassemble.h"

static SCM scm_tc16_codevector;

struct codevector {
  size_t size;
  SCM protects;
  jit_insn *start;
  jit_insn *end;
  jit_insn buf[0];
};

#define CODEVECTOR_P(x)    (SCM_NIMP(x) && SCM_CAR(x) == scm_tc16_codevector)
#define CODEVECTOR_DATA(x) ((struct codevector *)SCM_CDR(x))

static SCM
codevector_mark (SCM obj)
{
  return CODEVECTOR_DATA(obj)->protects;
}

static int
codevector_print (SCM obj, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<codevector ", port);
  scm_intprint ((long)CODEVECTOR_DATA(obj)->start, 16, port);
  scm_puts (">", port);
  return 1;
}

static scm_sizet
codevector_free (SCM obj)
{
  struct codevector *c = CODEVECTOR_DATA(obj);
  size_t sz = c->size;
  scm_must_free (c);
  return sz;
}

static SCM
make_codevector (struct codevector *c)
{
  SCM z;

  SCM_DEFER_INTS;
  SCM_NEWCELL (z);
  SCM_SETCAR (z, scm_tc16_codevector);
  SCM_SETCDR (z, (SCM) c);
  SCM_ALLOW_INTS;

  return z;
}

static SCM scm_tc16_code;

#define CODE_P(x)    (SCM_NIMP(x) && SCM_CELL_WORD_0(x) == scm_tc16_code)
#define CODE_INSN(x) ((jit_insn *)SCM_CELL_WORD_1(x))
#define CODE_ENV(x)  (SCM_CELL_OBJECT_2(x))
#define CODE_VEC(x)  (SCM_CELL_OBJECT_3(x))

static SCM
code_mark (SCM obj)
{
  scm_gc_mark (CODE_VEC(obj));
  return CODE_ENV(obj);
}

static int
code_print (SCM obj, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<compiled-procedure ", port);
  scm_intprint ((long)obj, 16, port);
  scm_puts (">", port);
  return 1;
}

static scm_sizet
code_free (SCM obj)
{
  return 0;
}

static SCM
make_code (SCM codevector, SCM env)
{
  SCM z;

  SCM_DEFER_INTS;
  SCM_NEWCELL2 (z);
  SCM_SET_CELL_WORD_0 (z, scm_tc16_code);
  SCM_SET_CELL_WORD_1 (z, CODEVECTOR_DATA(codevector)->start);
  SCM_SET_CELL_OBJECT_2 (z, env);
  SCM_SET_CELL_OBJECT_3 (z, codevector);
  SCM_ALLOW_INTS;

  return z;
}

// args has already been validated to be a proper list
static SCM (*call_tc) (jit_insn *proc, SCM args, SCM env);

#ifndef offsetof
#define offsetof(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)
#endif

static void
create_call_tc ()
{
  jit_insn *buf = scm_must_malloc (sizeof(jit_insn)*500, "call_tc");
  int arg_proc, arg_args, arg_env;

  jit_insn *l0, *ref0, *ref1, *l2, *ref2, *ref3, *ref4;

  call_tc = (SCM (*)()) jit_set_ip(buf).ptr;
  jit_prolog (2);
  arg_proc = jit_arg_l ();
  arg_args = jit_arg_l ();
  arg_env = jit_arg_l ();
  jit_movi_l (JIT_R1, 0);
  jit_getarg_l (JIT_R0, arg_args);
 l0 = jit_get_label ();
  ref0 = jit_beqi_l (jit_forward (), JIT_R0, SCM_EOL);
  jit_ldxi_l (JIT_R2, JIT_R0, offsetof (scm_cell, word_0));
  jit_pushr_l (JIT_R2);
  jit_addi_l (JIT_R1, JIT_R1, sizeof(SCM));
  jit_ldxi_l (JIT_R0, JIT_R0, offsetof (scm_cell, word_1));
  jit_jmpi (l0);
 jit_patch (ref0);
  jit_getarg_l (JIT_R0, arg_env);
  jit_getarg_l (JIT_R2, arg_proc);
  jit_callr (JIT_R2);
  ref1 = jit_bnei_l (jit_forward (), JIT_R1, sizeof(SCM));
  jit_movr_l (JIT_RET, JIT_R0);
  jit_ret ();
 jit_patch (ref1);
  jit_movi_l (JIT_V0, SCM_EOL);
  jit_movr_l (JIT_V1, JIT_SP);
  jit_movr_l (JIT_V2, JIT_SP);
  ref2 = jit_beqi_l (jit_forward (), JIT_R1, 0);
  jit_subr_l (JIT_SP, JIT_SP, JIT_R1);
  jit_str_l (JIT_SP, JIT_R0);
 l2 = jit_get_label ();
  ref3 = jit_bler_l (jit_forward (), JIT_V1, JIT_SP);
  jit_prepare (2);
  jit_pusharg_l (JIT_V0);
  jit_ldxi_l (JIT_R0, JIT_V1, -sizeof(SCM));
  jit_pusharg_l (JIT_R0);
  jit_finish (scm_cons);
  jit_retval (JIT_V0);
  jit_subi_l (JIT_V1, JIT_V1, sizeof(SCM));
  jit_jmpi (l2);
 jit_patch (ref2);
 jit_patch (ref3);
  jit_prepare (1);
  jit_pusharg_l (JIT_V0);
  jit_finish (scm_values);
  jit_retval (JIT_RET);
  jit_movr_l (JIT_SP, JIT_V2);
  jit_ret ();

  jit_flush_code (buf, jit_get_ip().ptr);
}
  
static SCM
code_apply (SCM smob, SCM args)
{
#define FUNC_NAME "code_apply"
  SCM_VALIDATE_LIST (SCM_ARG1, args);
  return call_tc (CODE_INSN(smob),
		  scm_reverse_x (args, SCM_EOL),
		  CODE_ENV (smob));
#undef FUNC_NAME
}

static SCM
nlistify (int n, va_list ap)
{
  SCM l = SCM_EOL;
  SCM *t = &l;
  while (--n >= 0)
    {
      *t = scm_cons (va_arg (ap, SCM), SCM_EOL);
      t = SCM_CDRLOC (*t);
    }
  return l;
}

static SCM
nlistify2 (int n, SCM e1, SCM e2, va_list ap)
{
  SCM l = nlistify (n-2, ap);
  if (n >= 2)
    l = scm_cons (e2, l);
  if (n >= 1)
    l = scm_cons (e1, l);
  return l;
}

SCM 
scm_invoke (SCM proc, int _n, void *retaddress,
	    SCM arg1, SCM arg2, ...)
{
  // We MUST not change `n' and `retaddress', they are used by the
  // caller.

  int n;

  SCM_ASRTGO (SCM_NIMP (proc), badproc);

  n = _n / sizeof(SCM);

 tail:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_2o:
      return (SCM_SUBRF (proc) (arg1, (n > 1)? arg2 : SCM_UNDEFINED));
    case scm_tc7_subr_2:
      SCM_ASRTGO (n == 2, wrongnumargs);
      return (SCM_SUBRF (proc) (arg1, arg2));
    case scm_tc7_subr_0:
      SCM_ASRTGO (n == 0, wrongnumargs);
      return (SCM_SUBRF (proc) ());
    case scm_tc7_subr_1:
      SCM_ASRTGO (n == 1, wrongnumargs);
      return (SCM_SUBRF (proc) (arg1));
    case scm_tc7_subr_1o:
      SCM_ASRTGO (n <= 1, wrongnumargs);
      return (SCM_SUBRF (proc) ((n < 1)? SCM_UNDEFINED : arg1));
    case scm_tc7_cxr:
      SCM_ASRTGO (n == 1, wrongnumargs);
      if (SCM_SUBRF (proc))
	{
	  if (SCM_INUMP (arg1))
	    {
	      return 
		(scm_make_real (SCM_DSUBRF (proc) ((double) SCM_INUM (arg1))));
	    }
	  SCM_ASRTGO (SCM_NIMP (arg1), floerr);
	  if (SCM_REALP (arg1))
	    {
	      return 
		(scm_make_real (SCM_DSUBRF (proc) (SCM_REAL_VALUE (arg1))));
	    }
#ifdef SCM_BIGDIG
	  if (SCM_BIGP (arg1))
	    return (scm_make_real (SCM_DSUBRF (proc) (scm_big2dbl (arg1))));
#endif
	floerr:
	  SCM_WTA_DISPATCH_1 (*SCM_SUBR_GENERIC (proc), arg1,
			      SCM_ARG1, SCM_SYMBOL_CHARS (SCM_SNAME (proc)));
	}
      proc = SCM_SNAME (proc);
      {
	char *chrs = SCM_SYMBOL_CHARS (proc) + SCM_SYMBOL_LENGTH (proc) - 1;
	while ('c' != *--chrs)
	  {
	    SCM_ASSERT (SCM_CONSP (arg1),
		    arg1, SCM_ARG1, SCM_SYMBOL_CHARS (proc));
	    arg1 = ('a' == *chrs) ? SCM_CAR (arg1) : SCM_CDR (arg1);
	  }
	return (arg1);
      }
    case scm_tc7_subr_3:
      {
	va_list ap;
	SCM arg3;
	SCM_ASRTGO (n == 3, wrongnumargs);
	va_start (ap, arg2);
	arg3 = va_arg (ap, SCM);
	va_end (ap);
	return (SCM_SUBRF (proc) (arg1, arg2, arg3));
      }
    case scm_tc7_lsubr:
      {
	va_list ap;
	SCM x;
	va_start (ap, arg2);
	x = nlistify2 (n, arg1, arg2, ap);
	va_end (ap);
	return (SCM_SUBRF (proc) (x));
      }
    case scm_tc7_lsubr_2:
      {
	va_list ap;
	SCM x;
	SCM_ASRTGO (n >= 2, wrongnumargs);
	va_start (ap, arg2);
	x = nlistify (n-2, ap);
	va_end (ap);
	return (SCM_SUBRF (proc) (arg1, arg2, x));
      }
    case scm_tc7_asubr:
      {
	va_list ap;
	SCM x;
	if (n == 0)
	  return (SCM_SUBRF (proc) (SCM_UNDEFINED, SCM_UNDEFINED));
	if (n == 1)
	  return (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
	x = SCM_SUBRF (proc) (arg1, arg2);
	va_start (ap, arg2);
	while (n > 2)
	  {
	    x = SCM_SUBRF (proc) (x, va_arg (ap, SCM));
	    n--;
	  }
	va_end (ap);
	return x;
      }
    case scm_tc7_rpsubr:
      {
	va_list ap;
	SCM x;
	if (n <= 1)
	  return (SCM_BOOL_T);
	if (SCM_FALSEP (SCM_SUBRF (proc) (arg1, arg2)))
	  return SCM_BOOL_F;
	if (n == 2)
	  return SCM_BOOL_T;
	va_start (ap, arg2);
	x = arg2;
	while (n > 2)
	  {
	    SCM y = va_arg (ap, SCM);
	    if (SCM_FALSEP (SCM_SUBRF (proc) (x, y)))
	      {
		va_end (ap);
		return (SCM_BOOL_F);
	      }
	    x = y;
	    n--;
	  }
	va_end (ap);
	return (SCM_BOOL_T);
      }
    case scm_tcs_closures:
      {
	scm_misc_error ("invoke", 
			"can't invoke interpreted code"
			" from compiled code yet.", SCM_EOL);
      }
    case scm_tc7_smob:
      {
	if (!SCM_SMOB_APPLICABLE_P (proc))
	  goto badproc;
	if (n == 0)
	  return (SCM_SMOB_APPLY_0 (proc));
	else if (n == 1)
	  return (SCM_SMOB_APPLY_1 (proc, arg1));
	else if (n == 2)
	  return (SCM_SMOB_APPLY_2 (proc, arg1, arg2));
	else
	  {
	    va_list ap;
	    SCM x;
	    va_start (ap, arg2);
	    x = nlistify (n-2, ap);
	    va_end (ap);
	    return (SCM_SMOB_APPLY_3 (proc, arg1, arg2, x));
	  }
      }
    case scm_tc7_cclo:
      {
	/* XXX - this chickens out to scm_apply */
	va_list ap;
	SCM args;
	va_start (ap, arg2);
	args = nlistify2 (n, arg1, arg2, ap);
	va_end (ap);
	args = scm_cons (proc, args);
	return scm_apply (SCM_CCLO_SUBR (proc), args, SCM_EOL);
      }
    case scm_tc7_pws:
      {
	proc = SCM_PROCEDURE (proc);
	goto tail;
      }
    case scm_tcs_cons_gloc:
      {
	if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	  {
	    va_list ap;
	    SCM x;
	    va_start (ap, arg2);
	    x = nlistify2 (n, arg1, arg2, ap);
	    va_end (ap);
	    return (scm_apply_generic (proc, x));
	  }
	else if (!SCM_I_OPERATORP (proc))
	  goto badproc;
	else
	  {
	    /* XXX - this chickens out to scm_apply */
	    va_list ap;
	    SCM args;
	    va_start (ap, arg2);
	    args = nlistify2 (n, arg1, arg2, ap);
	    va_end (ap);
	    args = scm_cons (proc, args);
	    proc = (SCM_I_ENTITYP (proc)
		    ? SCM_ENTITY_PROCEDURE (proc)
		    : SCM_OPERATOR_PROCEDURE (proc));
	    if (SCM_NIMP (proc))
	      return scm_apply (proc, args, SCM_EOL);
	    else
	      goto badproc;
	  }
      }
    wrongnumargs:
      scm_wrong_num_args (proc);
    default:
    badproc:
      scm_wrong_type_arg ("invoke", SCM_ARG1, proc);
      return (arg1);
    }
}

static void
do_label_def (SCM label_hash, SCM label)
{
  SCM cell = scm_hashq_ref (label_hash, label, SCM_BOOL_F);

  if (cell == SCM_BOOL_F)
    {
      /* Label has not been seen yet.  Define it. */
      SCM loc = scm_ulong2num ((unsigned long)jit_get_label());
      cell = scm_cons (loc, SCM_EOL);
      scm_hashq_set_x (label_hash, label, cell);
    }
  else if (SCM_CAR (cell) != SCM_BOOL_F)
    {
      /* Label has already been defined.  Complain. */
      scm_misc_error ("assemble", "label ~S defined twice",
		      SCM_LIST1 (label));
    }
  else
    {
      /* Label must have been referenced.  Define and Patch. */
      SCM refs;
      SCM loc = scm_ulong2num ((unsigned long)jit_get_ip().ptr);
      SCM_SETCAR (cell, loc);
      
      for (refs = SCM_CDR (cell); SCM_CONSP (refs); refs = SCM_CDR (refs))
	{
	  jit_insn *ref = (jit_insn *)scm_num2ulong (SCM_CAR (refs),
						     (char *)SCM_ARG1,
						     "assemble");
	  jit_patch (ref);
	}
    }
}

static jit_insn *
get_label_def (SCM label_hash, SCM label)
{
  SCM cell = scm_hashq_ref (label_hash, label, SCM_BOOL_F);

  if (cell == SCM_BOOL_F || SCM_CAR(cell) == SCM_BOOL_F)
    return NULL;
  return (jit_insn *)scm_num2ulong (SCM_CAR (cell), (char *)SCM_ARG1,
				    "assemble");
}

static void
add_label_ref (SCM label_hash, SCM label, jit_insn *ref)
{
  SCM cell = scm_hashq_ref (label_hash, label, SCM_BOOL_F);
  SCM sref = scm_ulong2num ((unsigned long)ref);

  if (cell == SCM_BOOL_F)
    {
      cell = scm_cons (SCM_BOOL_F, SCM_EOL);
      scm_hashq_set_x (label_hash, label, cell);
    }
  
  if (SCM_CAR (cell) == SCM_BOOL_F)
    {
      /* Only add a reference when it needs to be patched. */
      SCM_SETCDR (cell, scm_cons (sref, SCM_CDR (cell)));
    }
}
        
static SCM
check_label (void *closure, SCM label, SCM defrefs, SCM ununsed)
{
  if (SCM_CAR (defrefs) == SCM_BOOL_F)
    scm_misc_error ("assemble", "label ~S is undefined", SCM_LIST1(label));
  return SCM_BOOL_F;
}

static void
check_labels (SCM label_hash)
{
  scm_internal_hash_fold (check_label, NULL, SCM_BOOL_F, label_hash);
}

static void
def_arg (SCM arg_hash, SCM sym, int id)
{
  scm_hashq_set_x (arg_hash, sym, SCM_MAKINUM (id));
}

static int
get_arg (SCM arg_hash, SCM sym)
{
  SCM id = scm_hashq_ref (arg_hash, sym, SCM_BOOL_F);
  if (id == SCM_BOOL_F)
    scm_misc_error ("assemble", "undefined argument: ~S", SCM_LIST1 (sym));
  return SCM_INUM (id);
}

SCM_SYMBOL (sym_scm, "scm");
SCM_SYMBOL (sym_subr, "subr");
SCM_SYMBOL (sym_label, "label");
SCM_SYMBOL (sym_code, "code");
SCM_SYMBOL (sym_var, "var");
SCM_SYMBOL (sym_codetag, "codetag");

static unsigned long
imm2int (SCM imm, SCM label_hash, struct codevector *c)
{
  if (scm_ilength (imm) == 2)
    {
      if (SCM_CAR (imm) == sym_scm)
	{
	  SCM x = SCM_CADR (imm);
	  if (SCM_NIMP (x))
	    c->protects = scm_cons (x, c->protects);
	  return x;
	}
      else if (SCM_CAR (imm) == sym_subr && SCM_STRINGP (SCM_CADR (imm)))
	{
	  void *addr;
	  addr = dlsym (NULL, SCM_CHARS (SCM_CADR (imm)));
	  if (addr == NULL)
	    scm_misc_error ("assemble", "undefined subr: ~S", SCM_LIST1 (imm));
	  return (unsigned long)addr;
	}
      else if (SCM_CAR (imm) == sym_label && SCM_SYMBOLP (SCM_CADR (imm)))
	{
	  jit_insn *lab = get_label_def (label_hash, SCM_CADR (imm));
	  if (lab == NULL)
	    scm_misc_error ("assemble", "undefined label: ~S",
			    SCM_LIST1 (imm));
	  return (unsigned long)lab;
	}
      else if (SCM_CAR (imm) == sym_code)
	{
	  #define FUNC_NAME "assemble"
	  SCM x = SCM_CADR (imm);
	  SCM_VALIDATE_SMOB (SCM_ARG1, x, codevector);
	  c->protects = scm_cons (x, c->protects);
	  return (unsigned long)CODEVECTOR_DATA(x)->start;
	  #undef FUNC_NAME
	}
      else if (SCM_CAR (imm) == sym_var)
	{
	  #define FUNC_NAME "assemble"
	  SCM x = SCM_CADR (imm);
	  SCM_VALIDATE_VARIABLE (SCM_ARG1, x);
	  x = SCM_VARVCELL (x);
	  c->protects = scm_cons (x, c->protects);
	  return (unsigned long)SCM_CDRLOC(x);
          #undef FUNC_NAME
	}
    }
  else if (SCM_CONSP (imm) && SCM_CAR (imm) == sym_codetag)
    {
      return scm_tc16_code;
    }
  else if (SCM_STRINGP (imm))
    {
      c->protects = scm_cons (imm, c->protects);
      return (unsigned long)SCM_STRING_CHARS (imm);
    }
  else if (SCM_NUMBERP (imm))
    return scm_num2ulong (imm, (char *)SCM_ARG1, "assemble");
  
  scm_misc_error ("assemble", "unrecognized immediate: ~S", SCM_LIST1 (imm));
}

static int
sym2reg (SCM sym)
{
  static struct {
    char *sym;
    int reg;
  } table[] = {
    "r0", JIT_R0,
    "r1", JIT_R1,
    "r2", JIT_R2,
    "v0", JIT_V0,
    "v1", JIT_V1,
    "v2", JIT_V2,
    "sp", JIT_SP,
    "ret", JIT_RET,
    NULL
  };

  int i;

  if (!SCM_SYMBOLP (sym))
    scm_misc_error ("assemble", "not a register: ~S", SCM_LIST1 (sym));

  for (i = 0; table[i].sym; i++)
    if (!strcmp (table[i].sym, SCM_CHARS(sym)))
      return table[i].reg;
  scm_misc_error ("assemble", "unrecognized register: ~S", SCM_LIST1 (sym));
}

/* Assemble one instruction.  The guts is generated by `rod.scm'
*/

/* XXX - sort this out. */
#define jit_pop_l jit_popr_l
#define jit_pop_i jit_popr_i
#define jit_pop_ul jit_popr_ul
#define jit_pop_ui jit_popr_ui
#define jit_push_l jit_pushr_l
#define jit_push_i jit_pushr_i
#define jit_push_ul jit_pushr_ul
#define jit_push_ui jit_pushr_ui

static void
assemble1 (SCM insn, SCM label_hash, SCM arg_hash, struct codevector *c)
{
  if (SCM_SYMBOLP (insn))
    do_label_def (label_hash, insn);
  else 
    {
      char *insn_op;
      SCM insn_1 = SCM_BOOL_F, insn_2 = SCM_BOOL_F, insn_3 = SCM_BOOL_F;
      int insn_len = scm_ilength (insn);

      if (insn_len < 1 || !SCM_SYMBOLP (SCM_CAR (insn)))
	scm_misc_error ("assemble", "invalid instruction: ~S",
			SCM_LIST1 (insn));
      insn_op = SCM_SYMBOL_CHARS (SCM_CAR (insn));
      switch (insn_len)
	{
	default:
	case 4:
	  insn_3 = SCM_CADDDR (insn);
	case 3:
	  insn_2 = SCM_CADDR (insn);
	case 2:
	  insn_1 = SCM_CADR (insn);
	case 1:
	}

#define ASSERT_LEN(n) if (insn_len-1 != (n)) \
                       scm_misc_error ("assemble", \
                                       "wrong number of operands: ~S", \
                                       SCM_LIST1 (insn));
#define ASSERT_SYM(s) if (!SCM_SYMBOLP ((s))) \
                       scm_misc_error ("assemble", \
                                       "in ~S, not a symbol: ~S", \
                                       SCM_LIST2 (insn, s));

#define AS_INT(x)     (imm2int ((x), label_hash, c))
#define AS_REG(x)     (sym2reg ((x)))
#define IS_REG(x)     (SCM_SYMBOLP ((x)))

#include "assemble-ops.c"

    }
}

/* Try to assemble the code into C.  This function either throws an
   error when something is wrong with ASM_CODE, or returns -1 when C
   is too small, or return the number of jit_insn required by
   ASM_CODE.
*/

#define JIT_MAX_INSNS 10              // the longest possible jit opcode

static int
try_assemble (SCM asm_code, struct codevector *c)
{
  SCM label_hash = scm_c_make_hash_table (63);
  SCM arg_hash = scm_c_make_hash_table (13);

  c->start = jit_set_ip(c->buf).ptr;

  while (SCM_CONSP (asm_code))
    {
      assemble1 (SCM_CAR (asm_code), label_hash, arg_hash, c);
      asm_code = SCM_CDR (asm_code);

      if (((jit_insn *)jit_get_ip().ptr) >= c->buf+c->size-JIT_MAX_INSNS)
	return -1;
    }
  check_labels (label_hash);
  c->end = (jit_insn *)jit_get_ip().ptr;
  jit_flush_code (c->buf, c->end);

  // c->proc = scm_make_gsubr ("", 1, 0, 0, (SCM (*)())c->start);

  return ((jit_insn *)jit_get_ip().ptr) - c->buf;
}

SCM_DEFINE(scm_assemble, "assemble", 1, 0, 0,
	   (SCM asm_code),
	   "Assembles a Lightning program into machine code.")
#define FUNC_NAME s_scm_assemble
{
  size_t sz;
  struct codevector *c;
  SCM z;
  int asm_len;

  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG1, asm_code, asm_len);

  sz = sizeof(struct codevector) + sizeof(jit_insn)*JIT_MAX_INSNS*asm_len;
  c  = scm_must_malloc (sz, "code");
  c->size = sz;
  c->protects = SCM_EOL;

  z = make_codevector (c);

  if (try_assemble (asm_code, c) < 0)
    scm_misc_error (FUNC_NAME, "machine code too long", SCM_EOL);

  return z;
}
#undef FUNC_NAME

SCM_DEFINE(scm_disassemble, "disassemble", 1, 0, 0,
	   (SCM codevector),
	   "Disassembles a codevector.")
#define FUNC_NAME s_scm_disassemble
{
  struct codevector *c;

  SCM_VALIDATE_SMOB (SCM_ARG1, codevector, codevector);
  c = CODEVECTOR_DATA (codevector);

  disassemble (stderr, (bfd_byte *)c->start, (bfd_byte *)c->end);
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(scm_make_closure, "make-closure", 2, 0, 0,
	   (SCM codevector, SCM env),
	   "Create a clsoure from a codevector and an environment.")
#define FUNC_NAME s_scm_make_closure
{
  SCM_VALIDATE_SMOB (SCM_ARG1, codevector, codevector);
  return make_code (codevector, env);
}
#undef FUNC_NAME

void
scm_init_lightning ()
{
  create_call_tc ();
  scm_tc16_codevector = scm_make_smob_type ("codevector", 0);
  scm_set_smob_mark (scm_tc16_codevector, codevector_mark);
  scm_set_smob_free (scm_tc16_codevector, codevector_free);
  scm_set_smob_print (scm_tc16_codevector, codevector_print);

  scm_tc16_code = scm_make_smob_type ("code", 0);
  scm_set_smob_mark (scm_tc16_code, code_mark);
  scm_set_smob_free (scm_tc16_code, code_free);
  scm_set_smob_print (scm_tc16_code, code_print);
  scm_set_smob_apply (scm_tc16_code, code_apply, 0, 0, 1);

#ifndef SCM_MAGIC_SNARFER
#ifndef MKDEP
#include "lightning.x"
#endif /* MKDEP */
#endif /* SCM_MAGIC_SNARFER */
}
