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
#include <lightning.h>

#include "disassemble.h"

static SCM scm_tc16_code;

struct code {
  size_t size;
  SCM proc;
  jit_insn *end;
  jit_insn buf[0];
};

#define CODE_P(x)       (SCM_NIMP(x) && SCM_CAR(x) == tc16_code)
#define CODE_CODE(x)    ((struct code *)SCM_CDR(x))

static SCM
code_mark (SCM obj)
{
  return CODE_CODE(obj)->proc;
}

static int
code_print (SCM obj, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<code ", port);
  scm_intprint ((long)CODE_CODE(obj), 16, port);
  scm_puts (">", port);
  return 1;
}

static scm_sizet
code_free (SCM obj)
{
  struct code *c = CODE_CODE(obj);
  size_t sz = c->size;
  scm_must_free (c);
  return sz;
}

static SCM
make_code (struct code *c)
{
  SCM z;

  SCM_DEFER_INTS;
  SCM_NEWCELL (z);
  SCM_SETCAR (z, scm_tc16_code);
  SCM_SETCDR (z, (SCM) c);
  SCM_ALLOW_INTS;

  return z;
}

static SCM
code_apply (SCM smob, SCM args)
{
  struct code *c = CODE_CODE (smob);
  return scm_apply (c->proc, args, SCM_EOL);
}

static void
do_label_def (SCM label_hash, SCM label)
{
  SCM cell = scm_hashq_ref (label_hash, label, SCM_BOOL_F);

  if (cell == SCM_BOOL_F)
    {
      /* Label has not been seen yet.  Define it. */
      SCM loc = scm_ulong2num ((unsigned long)jit_get_ip().ptr);
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
    scm_misc_error ("assemble", "undefined argument: ~A", sym);
  return SCM_INUM (id);
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
    scm_misc_error ("assemble", "not a register: ~A", SCM_LIST1 (sym));

  for (i = 0; table[i].sym; i++)
    if (!strcmp (table[i].sym, SCM_CHARS(sym)))
      return table[i].reg;
  scm_misc_error ("assemble", "unrecognized register: ~A", SCM_LIST1 (sym));
}

/* Assemble one instruction.  The guts is generated by `rod.scm'
*/

static void
assemble1 (SCM insn, SCM label_hash, SCM arg_hash)
{
  if (SCM_SYMBOLP (insn))
    do_label_def (label_hash, insn);
  else 
    {
      char *insn_op;
      SCM insn_1 = SCM_BOOL_F, insn_2 = SCM_BOOL_F, insn_3 = SCM_BOOL_F;
      int insn_len = scm_ilength (insn);

      if (insn_len < 1 || !SCM_SYMBOLP (SCM_CAR (insn)))
	scm_misc_error ("assemble", "invalid instruction: ~A",
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
                                       "wrong number of operands: ~A", \
                                       SCM_LIST1 (insn));
#define ASSERT_SYM(s) if (!SCM_SYMBOLP ((s))) \
                       scm_misc_error ("assemble", \
                                       "in ~A, not a symbol: ~A", \
                                       SCM_LIST2 (insn, s));

#define AS_INT(x)     (scm_num2ulong ((x), (char *)SCM_ARG1, "assemble"))
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
try_assemble (SCM asm_code, struct code *c)
{
  SCM label_hash = scm_c_make_hash_table (63);
  SCM arg_hash = scm_c_make_hash_table (13);

  jit_insn *start_pc;

  void *scm_sum_ptr;
  int arg1, arg2;
  
  start_pc = jit_set_ip(c->buf).ptr;

  while (SCM_CONSP (asm_code))
    {
      assemble1 (SCM_CAR (asm_code), label_hash, arg_hash);
      asm_code = SCM_CDR (asm_code);

      if (((jit_insn *)jit_get_ip().ptr) >= c->buf+c->size-JIT_MAX_INSNS)
	return -1;
    }
  check_labels (label_hash);
  c->end = (jit_insn *)jit_get_ip().ptr;
  jit_flush_code (c->buf, c->end);

  c->proc = scm_make_gsubr ("", 1, 0, 0, (SCM (*)())start_pc);

  return ((jit_insn *)jit_get_ip().ptr) - c->buf;
}

SCM_DEFINE(scm_assemble, "assemble", 1, 0, 0,
	   (SCM asm_code),
	   "Assembles a Lightning program into machine code.")
#define FUNC_NAME s_scm_assemble
{
  size_t sz;
  struct code *c;
  SCM z;

  SCM_VALIDATE_LIST (SCM_ARG1, asm_code);

  sz = sizeof(struct code) + sizeof(jit_insn)*200;
  c  = scm_must_malloc (sz, "code");
  c->size = sz;
  c->proc = SCM_BOOL_F;

  z = make_code (c);

  if (try_assemble (asm_code, c) < 0)
    scm_misc_error (FUNC_NAME, "machine code too long", SCM_EOL);

  return z;
}
#undef FUNC_NAME

SCM_DEFINE(scm_disassemble, "disassemble", 1, 0, 0,
	   (SCM code),
	   "Disassembles a code vector.")
#define FUNC_NAME s_scm_disassemble
{
  struct code *c;

  SCM_VALIDATE_SMOB (SCM_ARG1, code, code);
  c = CODE_CODE (code);

  disassemble (stderr, (bfd_byte *)c->buf, (bfd_byte *)c->end);
  
  return SCM_UNSPECIFIED;
}

void
scm_init_lightning ()
{
  scm_tc16_code = scm_make_smob_type ("code", 0);
  scm_set_smob_free (scm_tc16_code, code_free);
  scm_set_smob_print (scm_tc16_code, code_print);
  scm_set_smob_apply (scm_tc16_code, code_apply, 0, 0, 1);
#ifndef SCM_MAGIC_SNARFER
#ifndef MKDEP
#include "lightning.x"
#endif /* MKDEP */
#endif /* SCM_MAGIC_SNARFER */
}
