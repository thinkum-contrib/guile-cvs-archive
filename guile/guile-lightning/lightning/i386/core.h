/******************************** -*- C -*- ****************************
 *
 *	Platform-independent layer (i386 version)
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



#ifndef __lightning_core_h
#define __lightning_core_h

#define JIT_R0			_EAX
#define JIT_R1			_ECX
#define JIT_R2			_EDX
#define JIT_V0			_EBX
#define JIT_V1			_ESI
#define JIT_V2			_EDI
#define JIT_FP			_EBP
#define JIT_SP			_ESP
#define JIT_RET			_EAX

struct jit_local_state {
  int	framesize;
  int	numargs;
};

/* 3-parameter operation */
#define jit_opr_(d, s1, s2, op1d, op2d)				\
	( (s2 == d) ? op1d :					\
	  (  ((s1 == d) ? (void)0 : MOVLrr(s1, d)), op2d )	\
	)

/* 3-parameter operation, with immediate */
#define jit_op_(d, s1, op2d)				\
	((s1 == d) ? op2d : (MOVLrr(s1, d), op2d))	\

/* 3-parameter operation, optimizable */
#define jit_opo_(d, s1, s2, op1d, op2d, op12d)		\
	((s2 == d) ? op2d : 				\
	((s1 == d) ? op1d : op12d))

/* 3-parameter operation, optimizable, with immediate */
#define jit_opi_(d, rs, opdi, opdri)			\
	((rs == d) ? opdi : opdri)

/* An operand is forced into a register */
#define jit_replace(rd, rs, forced, op)					\
	((rd == forced) ? JITFAIL("Register conflict for " # op) :	\
	 (rs == forced) ? op : (PUSHLr(forced), MOVLrr(rs, forced), op, POPLr(forced)))

/* For LT, LE, ... */
#define jit_bool_r(d, s1, s2, op)					\
	(CMPLrr(s2, s1), op, ANDLir(1, d))

#define jit_bool_i(d, rs, is, op)					\
	(CMPLir(is, rs), op, ANDLir(1, d))

/* When CMP with 0 can be replaced with TEST */
#define jit_bool_i0(d, rs, is, op, op0)					  \
	( ((is) != 0 ? (CMPLir(is, rs), op) : (TESTLrr(rs, rs), op0)),    \
	  ANDLir(1, d))

/* For BLT, BLE, ... */
#define jit_bra_r(s1, s2, op)		(CMPLrr(s2, s1), op, _jit.pc)
#define jit_bra_i(rs, is, op)		(CMPLir(is, rs), op, _jit.pc)

/* When CMP with 0 can be replaced with TEST */
#define jit_bra_i0(rs, is, op, op0)					\
	( (is) == 0 ? (TESTLrr(rs, rs), op0, _jit.pc) : (CMPLir(is, rs), op, _jit.pc))

/* Used to implement ldc, stc, ... */
#define jit_check8(rs)		( (rs) <= _EBX )
#define jit_reg8(rs)		( ((rs) & _BH) | _AL )
#define jit_reg16(rs)		( ((rs) & _BH) | _AX )

#define jit_movbmr(sd, sb, si, ss, d, ext)			\
	((jit_check8(d)						\
		? MOVBmr(sd, sb, si, ss, jit_reg8(d))		\
		: MOVWmr(sd, sb, si, ss, jit_reg16(d))), ext)

/* In jit_replace below, _EBX is dummy */
#define jit_movbrm(rs, dd, db, di, ds)						\
	(jit_check8(rs)								\
		? MOVBrm(jit_reg8(rs), dd, db, di, ds)				\
		: jit_replace(_EBX, rs, _EAX, MOVBrm(_AL, dd, db, di, ds)))

/* Reduce arguments of XOR/OR/TEST */
#define jit_reduce_(op)	op
#define jit_reduce(op, is, rs)							\
	(_u8P(is) && jit_check8(rs) ? jit_reduce_(op##Bir(is, jit_reg8(rs))) :	\
	(_u16P(is) ? jit_reduce_(op##Wir(is, jit_reg16(rs))) :			\
	jit_reduce_(op##Lir(is, rs)) ))

/* Helper macros for MUL/DIV/IDIV */
#define jit_might(d, s1, op)					\
	((s1 == d) ? (void)0 : op)

#define jit_mulr_ui_(s1, s2)	jit_opr_(_EAX, s1, s2, MULLr(s1),  MULLr(s2))
#define jit_mulr_i_(s1, s2)	jit_opr_(_EAX, s1, s2, IMULLr(s1), IMULLr(s2))


#define jit_muli_i_(is, rs)				\
	(MOVLir(is, rs == _EAX ? _EDX : _EAX),		\
	 IMULLr(rs == _EAX ? _EDX : rs))

#define jit_muli_ui_(is, rs)				\
	(MOVLir(is, rs == _EAX ? _EDX : _EAX),		\
	 IMULLr(rs == _EAX ? _EDX : rs))

#define jit_divi_i_(result, d, rs, is)			\
	(jit_might (d,    _EAX, PUSHLr(_EAX)),		\
	jit_might (d,    _ECX, PUSHLr(_ECX)),		\
	jit_might (d,    _EDX, PUSHLr(_EDX)),		\
	jit_might (rs,   _EAX, MOVLrr(rs, _EAX)),	\
	jit_might (rs,   _EDX, MOVLrr(rs, _EDX)),	\
	MOVLir(is, _ECX),				\
	SARLir(31, _EDX),				\
	IDIVLr(_ECX),					\
	jit_might(d,    result, MOVLrr(result, d)),	\
	jit_might(d,     _EDX,  POPLr(_EDX)),		\
	jit_might(d,     _ECX,  POPLr(_ECX)),		\
	jit_might(d,     _EAX,  POPLr(_EAX)))

#define jit_divr_i_(result, d, s1, s2)			\
	(jit_might (d,    _EAX, PUSHLr(_EAX)),		\
	jit_might (d,    _ECX, PUSHLr(_ECX)),		\
	jit_might (d,    _EDX, PUSHLr(_EDX)),		\
	((s1 == _ECX) ? PUSHLr(_ECX) : 0),		\
	jit_might (s2,   _ECX, MOVLrr(s2, _ECX)),	\
	((s1 == _ECX) ? POPLr(_EDX) :			\
	jit_might (s1,   _EDX, MOVLrr(s1, _EDX))),	\
	MOVLrr(_EDX, _EAX),				\
	SARLir(31, _EDX),				\
	IDIVLr(_ECX),					\
	jit_might(d,    result, MOVLrr(result, d)),	\
	jit_might(d,     _EDX,  POPLr(_EDX)),		\
	jit_might(d,     _ECX,  POPLr(_ECX)),		\
	jit_might(d,     _EAX,  POPLr(_EAX)))

#define jit_divi_ui_(result, d, rs, is)			\
	(jit_might (d,    _EAX, PUSHLr(_EAX)),		\
	jit_might (d,    _ECX, PUSHLr(_ECX)),		\
	jit_might (d,    _EDX, PUSHLr(_EDX)),		\
	jit_might (rs,   _EAX, MOVLrr(rs, _EAX)),	\
	MOVLir(is, _ECX),				\
	XORLrr(_EDX, _EDX),				\
	DIVLr(_ECX),					\
	jit_might(d,    result, MOVLrr(result, d)),	\
	jit_might(d,     _EDX,  POPLr(_EDX)),		\
	jit_might(d,     _ECX,  POPLr(_ECX)),		\
	jit_might(d,     _EAX,  POPLr(_EAX)))

#define jit_divr_ui_(result, d, s1, s2)			\
	(jit_might (d,    _EAX, PUSHLr(_EAX)),		\
	jit_might (d,    _ECX, PUSHLr(_ECX)),		\
	jit_might (d,    _EDX, PUSHLr(_EDX)),		\
	((s1 == _ECX) ? PUSHLr(_ECX) : 0),		\
	jit_might (s2,   _ECX, MOVLrr(s2, _ECX)),	\
	((s1 == _ECX) ? POPLr(_EAX) :			\
	jit_might (s1,   _EAX, MOVLrr(s1, _EAX))),	\
	XORLrr(_EDX, _EDX),				\
	DIVLr(_ECX),					\
	jit_might(d,    result, MOVLrr(result, d)),	\
	jit_might(d,     _EDX,  POPLr(_EDX)),		\
	jit_might(d,     _ECX,  POPLr(_ECX)),		\
	jit_might(d,     _EAX,  POPLr(_EAX)))


/* ALU */
#define jit_addi_i(d, rs, is)	jit_opi_((d), (rs),       ADDLir((is), (d)), 			LEALmr((is), (rs), 0, 0, (d))  )
#define jit_addr_i(d, s1, s2)	jit_opo_((d), (s1), (s2), ADDLrr((s2), (d)), ADDLrr((s1), (d)), LEALmr(0, (s1), (s2), 1, (d))  )
#define jit_addci_i(d, rs, is)	jit_op_ ((d), (rs),       ADDLir((is), (d)) 		       )
#define jit_addcr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), ADDLrr((s1), (d)), ADDLrr((s2), (d)) )
#define jit_addxi_i(d, rs, is)	jit_op_ ((d), (rs),       ADCLir((is), (d)) 		       )
#define jit_addxr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), ADCLrr((s1), (d)), ADCLrr((s2), (d)) )
#define jit_andi_i(d, rs, is)	jit_op_ ((d), (rs),       ANDLir((is), (d)) 		       )
#define jit_andr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), ANDLrr((s1), (d)), ANDLrr((s2), (d)) )
#define jit_orr_i(d, s1, s2)	jit_opr_((d), (s1), (s2),  ORLrr((s1), (d)),  ORLrr((s2), (d)) )
#define jit_subr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), (SUBLrr((s1), (d)), NEGLr(d)),	SUBLrr((s2), (d))	       )
#define jit_subcr_i(d, s1, s2)	jit_subr_i((d), (s1), (s2))
#define jit_subxr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), SBBLrr((s1), (d)), SBBLrr((s2), (d)) )
#define jit_subxi_i(d, rs, is)	jit_op_ ((d), (rs),       SBBLir((is), (d)) 		       )
#define jit_xorr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), XORLrr((s1), (d)), XORLrr((s2), (d)) )

/* These can sometimes use byte or word versions! */
#define jit_ori_i(d, rs, is)	jit_op_ ((d), (rs),        jit_reduce(OR, (is), (d))	       )
#define jit_xori_i(d, rs, is)	jit_op_ ((d), (rs),        jit_reduce(XOR, (is), (d))	       )

#define jit_muli_i(d, rs, is)	jit_op_ ((d), (rs),       IMULLir((is), (d)) 		       )
#define jit_mulr_i(d, s1, s2)	jit_opr_((d), (s1), (s2), IMULLrr((s1), (d)), IMULLrr((s2), (d)) )
#define jit_muli_ui(d, rs, is)	jit_op_ ((d), (rs),       IMULLir((is), (d)) 		       )
#define jit_mulr_ui(d, s1, s2)	jit_opr_((d), (s1), (s2), IMULLrr((s1), (d)), IMULLrr((s2), (d)) )

/* As far as low bits are concerned, signed and unsigned multiplies are
 * exactly the same. */
#ifdef not_needed
#undef jit_muli_i
#undef jit_muli_ui

#define jit_muli_ui(d, rs, is)															\
	((d) == _EDX ? (	      PUSHLr(_EAX), MOVLir((is), _EAX), MULLr(rs), MOVLrr(_EAX, _EDX), POPLr(_EAX)		) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    MOVLir((is), _EAX), MULLr(rs),				    POPLr(_EDX) ) :	\
	               (PUSHLr(_EDX), PUSHLr(_EAX), MOVLir((is), _EAX), MULLr(rs), MOVLrr(_EAX, (d)),  POPLr(_EAX), POPLr(_EDX) )))

#define jit_mulr_ui(d, s1, s2)													\
	((d) == _EDX ? (	      PUSHLr(_EAX), jit_mulr_ui_((s1), (s2)), MOVLrr(_EAX, _EDX), POPLr(_EAX)		    ) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    jit_mulr_ui_((s1), (s2)),  				       POPLr(_EDX)  ) :	\
	 	       (PUSHLr(_EDX), PUSHLr(_EAX), jit_mulr_ui_((s1), (s2)), MOVLrr(_EAX, (d)),  POPLr(_EAX), POPLr(_EDX)  )))
#endif /* not_needed */

#define jit_hmuli_i(d, rs, is)														\
	((d) == _EDX ? (	      PUSHLr(_EAX), jit_muli_i_((is), (rs)), 				     POPLr(_EAX)		) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    jit_muli_i_((is), (rs)), MOVLrr(_EDX, _EAX),	     POPLr(_EDX) ) :	\
	               (PUSHLr(_EDX), PUSHLr(_EAX), jit_muli_i_((is), (rs)), MOVLrr(_EDX, (d)), POPLr(_EAX), POPLr(_EDX) )))

#define jit_hmulr_i(d, s1, s2)													\
	((d) == _EDX ? (	      PUSHLr(_EAX), jit_mulr_i_((s1), (s2)), 			  POPLr(_EAX)		    ) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    jit_mulr_i_((s1), (s2)), MOVLrr(_EDX, _EAX), 	       POPLr(_EDX)  ) :	\
	 	       (PUSHLr(_EDX), PUSHLr(_EAX), jit_mulr_i_((s1), (s2)), MOVLrr(_EDX, (d)),   POPLr(_EAX), POPLr(_EDX)  )))

#define jit_hmuli_ui(d, rs, is)														\
	((d) == _EDX ? (	      PUSHLr(_EAX), jit_muli_ui_((is), (rs)), 				      POPLr(_EAX)		) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    jit_muli_ui_((is), (rs)), MOVLrr(_EDX, _EAX),	      POPLr(_EDX) ) :	\
	               (PUSHLr(_EDX), PUSHLr(_EAX), jit_muli_ui_((is), (rs)), MOVLrr(_EDX, (d)), POPLr(_EAX), POPLr(_EDX) )))

#define jit_hmulr_ui(d, s1, s2)													\
	((d) == _EDX ? (	      PUSHLr(_EAX), jit_mulr_ui_((s1), (s2)), 			  POPLr(_EAX)		    ) :	\
	((d) == _EAX ? (PUSHLr(_EDX),		    jit_mulr_ui_((s1), (s2)), MOVLrr(_EDX, _EAX), 	       POPLr(_EDX)  ) :	\
	 	       (PUSHLr(_EDX), PUSHLr(_EAX), jit_mulr_ui_((s1), (s2)), MOVLrr(_EDX, (d)),  POPLr(_EAX), POPLr(_EDX)  )))

#define jit_divi_i(d, rs, is)	jit_divi_i_(_EAX, (d), (rs), (is))
#define jit_divi_ui(d, rs, is)	jit_divi_ui_(_EAX, (d), (rs), (is))
#define jit_modi_i(d, rs, is)	jit_divi_i_(_EDX, (d), (rs), (is))
#define jit_modi_ui(d, rs, is)	jit_divi_ui_(_EDX, (d), (rs), (is))
#define jit_divr_i(d, s1, s2)	jit_divr_i_(_EAX, (d), (s1), (s2))
#define jit_divr_ui(d, s1, s2)	jit_divr_ui_(_EAX, (d), (s1), (s2))
#define jit_modr_i(d, s1, s2)	jit_divr_i_(_EDX, (d), (s1), (s2))
#define jit_modr_ui(d, s1, s2)	jit_divr_ui_(_EDX, (d), (s1), (s2))


/* Shifts */
#define jit_lshi_i(d, rs, is)	((is) <= 3 ?   LEALmr(0, 0, (rs), 1 << (is), (d))   :   jit_op_ ((d), (rs), SHLLir((is), (d)) ))
#define jit_rshi_i(d, rs, is)								jit_op_ ((d), (rs), SARLir((is), (d))  )
#define jit_rshi_ui(d, rs, is)								jit_op_ ((d), (rs), SHRLir((is), (d))  )
#define jit_lshr_i(d, r1, r2)	jit_replace((r1), (r2), _ECX, 				jit_op_ ((d), (r1), SHLLrr(_CL,  (d)) ))
#define jit_rshr_i(d, r1, r2)	jit_replace((r1), (r2), _ECX, 				jit_op_ ((d), (r1), SARLrr(_CL,  (d)) ))
#define jit_rshr_ui(d, r1, r2)	jit_replace((r1), (r2), _ECX, 				jit_op_ ((d), (r1), SHRLrr(_CL,  (d)) ))

/* Stack */
#define jit_pushr_i(rs)		PUSHLr(rs)
#define jit_popr_i(rs)		POPLr(rs)
#define jit_prolog(n)		(_jitl.framesize = 8, PUSHLr(_EBP), MOVLrr(_ESP, _EBP), PUSHLr(_EBX), PUSHLr(_ESI), PUSHLr(_EDI))

/* The += allows for stack pollution */
#define jit_prepare(n)		((void) (_jitl.numargs += (n)) )
#define jit_pusharg_i(rs)	PUSHLr(rs)
#define jit_finish(sub)		(jit_calli((sub)), ADDLir(4 * _jitl.numargs, JIT_SP), _jitl.numargs = 0)
#define jit_retval(rd)		((void) ( (rd) == _EAX ? 0 : MOVLrr(_EAX, (rd)) ))

#define	jit_arg_c()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_uc()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_s()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_us()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_i()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_ui()		((_jitl.framesize += sizeof(int)) - sizeof(int))
#define	jit_arg_l()		((_jitl.framesize += sizeof(long)) - sizeof(long))
#define	jit_arg_ul()		((_jitl.framesize += sizeof(long)) - sizeof(long))
#define	jit_arg_p()		((_jitl.framesize += sizeof(long)) - sizeof(long))

/* Unary */
#define jit_negr_i(d, rs)	jit_opi_((d), (rs), NEGLr(d), (XORLrr((d), (d)), SUBLrr((rs), (d))) )
#define jit_negr_l(d, rs)	jit_opi_((d), (rs), NEGLr(d), (XORLrr((d), (d)), SUBLrr((rs), (d))) )

#define jit_movr_i(d, rs)		MOVLrr((rs), (d))
#define jit_movi_i(d, is)	((is) ? MOVLir((is), (d)) : XORLrr ((d), (d)) )

#define jit_ntoh_ui(d, rs)	jit_op_((d), (rs), BSWAPLr(d))
#define jit_ntoh_us(d, rs)	jit_op_((d), (rs), RORLir(16, d))

/* Boolean */
#define jit_ltr_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETLr(jit_reg8(d))  )
#define jit_ler_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETLEr(jit_reg8(d)) )
#define jit_gtr_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETGr(jit_reg8(d))  )
#define jit_ger_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETGEr(jit_reg8(d)) )
#define jit_eqr_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETEr(jit_reg8(d))  )
#define jit_ner_i(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETNEr(jit_reg8(d)) )
#define jit_ltr_ui(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETBr(jit_reg8(d))  )
#define jit_ler_ui(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETBEr(jit_reg8(d)) )
#define jit_gtr_ui(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETAr(jit_reg8(d))  )
#define jit_ger_ui(d, s1, s2)	jit_bool_r((d), (s1), (s2), SETAEr(jit_reg8(d)) )

#define jit_lti_i(d, rs, is)	jit_bool_i0((d), (rs), (is), SETLr(jit_reg8(d)),  SETSr(jit_reg8(d))  )
#define jit_lei_i(d, rs, is)	jit_bool_i ((d), (rs), (is), SETLEr(jit_reg8(d))		      )
#define jit_gti_i(d, rs, is)	jit_bool_i ((d), (rs), (is), SETGr(jit_reg8(d)) 		      )
#define jit_gei_i(d, rs, is)	jit_bool_i0((d), (rs), (is), SETGEr(jit_reg8(d)), SETNSr(jit_reg8(d)) )
#define jit_eqi_i(d, rs, is)	jit_bool_i0((d), (rs), (is), SETEr(jit_reg8(d)),  SETEr(jit_reg8(d))  )
#define jit_nei_i(d, rs, is)	jit_bool_i0((d), (rs), (is), SETNEr(jit_reg8(d)), SETNEr(jit_reg8(d)) )
#define jit_lti_ui(d, rs, is)	jit_bool_i0((d), (rs), (is), SETBr(jit_reg8(d)),  XORLrr((d), (d))    )
#define jit_lei_ui(d, rs, is)	jit_bool_i0((d), (rs), (is), SETBEr(jit_reg8(d)), SETEr(jit_reg8(d))  )
#define jit_gti_ui(d, rs, is)	jit_bool_i0((d), (rs), (is), SETAr(jit_reg8(d)),  SETNEr(jit_reg8(d)) )
#define jit_gei_ui(d, rs, is)	jit_bool_i0((d), (rs), (is), SETAEr(jit_reg8(d)), INCLr((d))	      )

/* Jump */
#define jit_bltr_i(label, s1, s2)	jit_bra_r((s1), (s2), JLm(label, 0,0,0) )
#define jit_bler_i(label, s1, s2)	jit_bra_r((s1), (s2), JLEm(label,0,0,0) )
#define jit_bgtr_i(label, s1, s2)	jit_bra_r((s1), (s2), JGm(label, 0,0,0) )
#define jit_bger_i(label, s1, s2)	jit_bra_r((s1), (s2), JGEm(label,0,0,0) )
#define jit_beqr_i(label, s1, s2)	jit_bra_r((s1), (s2), JEm(label, 0,0,0) )
#define jit_bner_i(label, s1, s2)	jit_bra_r((s1), (s2), JNEm(label,0,0,0) )
#define jit_bltr_ui(label, s1, s2)	jit_bra_r((s1), (s2), JBm(label, 0,0,0) )
#define jit_bler_ui(label, s1, s2)	jit_bra_r((s1), (s2), JBEm(label,0,0,0) )
#define jit_bgtr_ui(label, s1, s2)	jit_bra_r((s1), (s2), JAm(label, 0,0,0) )
#define jit_bger_ui(label, s1, s2)	jit_bra_r((s1), (s2), JAEm(label,0,0,0) )
#define jit_bmsr_i(label, s1, s2)	(TESTLrr((s1), (s2)), JNZm(label,0,0,0), _jit.pc)
#define jit_bmcr_i(label, s1, s2)	(TESTLrr((s1), (s2)), JZm(label,0,0,0),  _jit.pc)
#define jit_boaddr_i(label, s1, s2)	(ADDLrr((s2), (s1)), JOm(label,0,0,0), _jit.pc)
#define jit_bosubr_i(label, s1, s2)	(SUBLrr((s2), (s1)), JOm(label,0,0,0), _jit.pc)
#define jit_boaddr_ui(label, s1, s2)	(ADDLrr((s2), (s1)), JCm(label,0,0,0), _jit.pc)
#define jit_bosubr_ui(label, s1, s2)	(SUBLrr((s2), (s1)), JCm(label,0,0,0), _jit.pc)

#define jit_blti_i(label, rs, is)	jit_bra_i0((rs), (is), JLm(label, 0,0,0), JSm(label, 0,0,0) )
#define jit_blei_i(label, rs, is)	jit_bra_i ((rs), (is), JLEm(label,0,0,0)		    )
#define jit_bgti_i(label, rs, is)	jit_bra_i ((rs), (is), JGm(label, 0,0,0)		    )
#define jit_bgei_i(label, rs, is)	jit_bra_i0((rs), (is), JGEm(label,0,0,0), JNSm(label,0,0,0) )
#define jit_beqi_i(label, rs, is)	jit_bra_i0((rs), (is), JEm(label, 0,0,0), JEm(label, 0,0,0) )
#define jit_bnei_i(label, rs, is)	jit_bra_i0((rs), (is), JNEm(label,0,0,0), JNEm(label,0,0,0) )
#define jit_blti_ui(label, rs, is)	jit_bra_i ((rs), (is), JLm(label, 0,0,0)		    )
#define jit_blei_ui(label, rs, is)	jit_bra_i0((rs), (is), JLEm(label,0,0,0), JEm(label, 0,0,0) )
#define jit_bgti_ui(label, rs, is)	jit_bra_i0((rs), (is), JGm(label, 0,0,0), JNEm(label,0,0,0) )
#define jit_bgei_ui(label, rs, is)	jit_bra_i ((rs), (is), JGEm(label,0,0,0)		    )
#define jit_boaddi_i(label, rs, is)	(ADDLir((is), (rs)), JOm(label,0,0,0), _jit.pc)
#define jit_bosubi_i(label, rs, is)	(SUBLir((is), (rs)), JOm(label,0,0,0), _jit.pc)
#define jit_boaddi_ui(label, rs, is)	(ADDLir((is), (rs)), JCm(label,0,0,0), _jit.pc)
#define jit_bosubi_ui(label, rs, is)	(SUBLir((is), (rs)), JCm(label,0,0,0), _jit.pc)

#define jit_bmsi_i(label, rs, is)	(jit_reduce(TEST, (is), (rs)), JNZm(label,0,0,0), _jit.pc)
#define jit_bmci_i(label, rs, is)	(jit_reduce(TEST, (is), (rs)), JZm(label,0,0,0),  _jit.pc)

#define jit_jmpi(label)		(JMPm( ((unsigned long) (label)),	0, 0, 0), _jit.pc)
#define jit_calli(label)	(CALLm( ((unsigned long) (label)),	0, 0, 0), _jit.pc)
#define jit_jmpr(reg)		JMPsr(reg)
#define jit_patch(jump_pc)	(*_PSL((jump_pc) - 4) = _jit.pc - (jump_pc))
#define jit_ret()		(POPLr(_EDI), POPLr(_ESI), POPLr(_EBX), POPLr(_EBP), RET())

/* Memory */
#define jit_ldi_c(d, is)		jit_movbmr((is), 0,    0,    0, (d), jit_extr_c_i((d), (d)))
#define jit_ldr_c(d, rs)		jit_movbmr(0,    (rs), 0,    0, (d), jit_extr_c_i((d), (d)))
#define jit_ldxr_c(d, s1, s2)		jit_movbmr(0,    (s1), (s2), 1, (d), jit_extr_c_i((d), (d)))
#define jit_ldxi_c(d, rs, is)		jit_movbmr((is), (rs), 0,    0, (d), jit_extr_c_i((d), (d)))

#define jit_ldi_uc(d, is)		jit_movbmr((is), 0,    0,    0, (d), jit_extr_uc_ui((d), (d)))
#define jit_ldr_uc(d, rs)		jit_movbmr(0,    (rs), 0,    0, (d), jit_extr_uc_ui((d), (d)))
#define jit_ldxr_uc(d, s1, s2)		jit_movbmr(0,    (s1), (s2), 1, (d), jit_extr_uc_ui((d), (d)))
#define jit_ldxi_uc(d, rs, is)		jit_movbmr((is), (rs), 0,    0, (d), jit_extr_uc_ui((d), (d)))

#define jit_sti_c(id, rs)		jit_movbrm((rs), (id), 0,    0,    0)
#define jit_str_c(rd, rs)		jit_movbrm((rs), 0,    (rd), 0,    0)
#define jit_stxr_c(d1, d2, rs)		jit_movbrm((rs), 0,    (d1), (d2), 1)
#define jit_stxi_c(id, rd, rs)		jit_movbrm((rs), (id), (rd), 0,    0)

#define jit_ldi_s(d, is)		(MOVWmr((is), 0,    0,    0,  jit_reg16(d)), jit_extr_s_i((d), (d)))
#define jit_ldr_s(d, rs)		(MOVWmr(0,    (rs), 0,    0,  jit_reg16(d)), jit_extr_s_i((d), (d)))
#define jit_ldxr_s(d, s1, s2)		(MOVWmr(0,    (s1), (s2), 1,  jit_reg16(d)), jit_extr_s_i((d), (d)))
#define jit_ldxi_s(d, rs, is)		(MOVWmr((is), (rs), 0,    0,  jit_reg16(d)), jit_extr_s_i((d), (d)))

#define jit_ldi_us(d, is)		(XORrr((d), (d)), MOVWmr((is), 0,    0,    0,  jit_reg16(d)))
#define jit_ldr_us(d, rs)		(XORrr((d), (d)), MOVWmr(0,    (rs), 0,    0,  jit_reg16(d)))
#define jit_ldxr_us(d, s1, s2)		(XORrr((d), (d)), MOVWmr(0,    (s1), (s2), 1,  jit_reg16(d)))
#define jit_ldxi_us(d, rs, is)		(XORrr((d), (d)), MOVWmr((is), (rs), 0,    0,  jit_reg16(d)))

#define jit_sti_s(id, rs)		MOVWrm(jit_reg16(rs), (id), 0,    0,    0)
#define jit_str_s(rd, rs)		MOVWrm(jit_reg16(rs), 0,    (rd), 0,    0)
#define jit_stxr_s(d1, d2, rs)		MOVWrm(jit_reg16(rs), 0,    (d1), (d2), 1)
#define jit_stxi_s(id, rd, rs)		MOVWrm(jit_reg16(rs), (id), (rd), 0,    0)

#define jit_ldi_i(d, is)		MOVLmr((is), 0,    0,    0,  (d))
#define jit_ldr_i(d, rs)		MOVLmr(0,    (rs), 0,    0,  (d))
#define jit_ldxr_i(d, s1, s2)		MOVLmr(0,    (s1), (s2), 1,  (d))
#define jit_ldxi_i(d, rs, is)		MOVLmr((is), (rs), 0,    0,  (d))

#define jit_sti_i(id, rs)		MOVLrm((rs), (id), 0,    0,    0)
#define jit_str_i(rd, rs)		MOVLrm((rs), 0,    (rd), 0,    0)
#define jit_stxr_i(d1, d2, rs)		MOVLrm((rs), 0,    (d1), (d2), 1)
#define jit_stxi_i(id, rd, rs)		MOVLrm((rs), (id), (rd), 0,    0)

/* Extra */
#define jit_nop()			NOP()

#define _jit_alignment(pc, n)		(((pc ^ _MASK(4)) + 1) & _MASK(n))
#define jit_align(n) 			_NOPi(_jit_alignment(_UL(_jit.pc), (n)))

#endif /* __lightning_core_h */
