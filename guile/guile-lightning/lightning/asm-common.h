/******************************** -*- C -*- ****************************
 *
 *	Dynamic assembler support
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


#ifndef __lightning_asm_common_h
#define __lightning_asm_common_h_


#ifndef _ASM_SAFETY
#define JITFAIL(MSG) 0
#else
#define JITFAIL(MSG) jit_fail(MSG, __FILE__, __LINE__, __FUNCTION__)
#endif

#ifdef __GNUC__
#define JIT_UNUSED		__attribute__((unused))
#else
#define JIT_UNUSED
#endif


/* NextStep 2.0 cc is really gcc 1.93 but it defines __GNUC__ = 2 and
   does not implement __extension__.  But that compiler doesn't define
   __GNUC_MINOR__.  */
#ifdef __GNUC__
#if __GNUC__ < 2 || (defined(__NeXT__) && !__GNUC_MINOR__)
#define __extension__
#endif

#define _TEMPD(type, var) 	

#define _TEMP(type, var, val, body) __extension__ ({	\
  register struct { type var } _jitl; _jitl.var = val;	\
  body;							\
})

#else

/* Between loading a global and calling a subroutine, we choose the lesser
 * evil. */
#define _TEMPD(type, var) 	static type var;
#define _TEMP(type, var, val, body) ((var = val), body)

#endif

typedef char		_sc;
typedef unsigned char	_uc;
typedef unsigned short	_us;
typedef unsigned int	_ui;
typedef long		_sl;
typedef unsigned long	_ul;

#define _UC(X)		((_uc  )(X))
#define _US(X)		((_us  )(X))
#define _UI(X)		((_ui  )(X))
#define _SL(X)		((_sl  )(X))
#define _UL(X)		((_ul  )(X))

#ifdef __cplusplus
# define _PUC(X)	((_uc *&)(X))
# define _PUS(X)	((_us *&)(X))
# define _PUI(X)	((_ui *&)(X))
# define _PSL(X)	((_sl *&)(X))
# define _PUL(X)	((_ul *&)(X))
#else
# define _PUC(X)	((_uc *)(X))
# define _PUS(X)	((_us *)(X))
# define _PUI(X)	((_ui *)(X))
# define _PSL(X)	((_sl *)(X))
# define _PUL(X)	((_ul *)(X))
#endif

#define _B(B)		((*_PUC(_jit.pc)++)= _UC((B)&  0xff))
#define _W(W)		((*_PUS(_jit.pc)++)= _US((W)&0xffff))
#define _I(I)		((*_PUI(_jit.pc)++)= _UI((I)       ))
#define _L(L)		((*_PUL(_jit.pc)++)= _UL((L)       ))

#define _MASK(N)	((unsigned)((1<<(N)))-1)
#define _siP(N,I)	(!((((unsigned)(I))^(((unsigned)(I))<<1))&~_MASK(N)))
#define _uiP(N,I)	(!(((unsigned)(I))&~_MASK(N)))
#define _suiP(N,I)	(_siP(N,I) | uiP(N,I))

#ifndef _ASM_SAFETY
#define _ck_s(W,I)	(_UL(I) & _MASK(W))
#define _ck_u(W,I)    	(_UL(I) & _MASK(W))
#define _ck_su(W,I)    	(_UL(I) & _MASK(W))
#define _ck_d(W,I)    	(_UL(I) & _MASK(W))
#else
#define _ck_s(W,I)	(_siP(W,I) ? (_UL(I) & _MASK(W)) : JITFAIL(  "signed integer `"#I"' too large for "#W"-bit field"))
#define _ck_u(W,I)    	(_uiP(W,I) ? (_UL(I) & _MASK(W)) : JITFAIL("unsigned integer `"#I"' too large for "#W"-bit field"))
#define _ck_su(W,I)    	(_suiP(W,I) ? (_UL(I) & _MASK(W)) : JITFAIL(        "integer `"#I"' too large for "#W"-bit field"))
#define _ck_d(W,I)    	(_siP(W,I) ? (_UL(I) & _MASK(W)) : JITFAIL(    "displacement `"#I"' too large for "#W"-bit field"))
#endif

#define _s0P(I)		((I)==0)
#define _s8P(I)		_siP(8,I)
#define _s16P(I)	_siP(16,I)
#define _u8P(I)		_uiP(8,I)
#define _u16P(I)	_uiP(16,I)

#define _su16(I)	_ck_su(16,I)

#define _s1(I)          _ck_s( 1,I)
#define _s2(I)          _ck_s( 2,I)
#define _s3(I)          _ck_s( 3,I)
#define _s4(I)          _ck_s( 4,I)
#define _s5(I)          _ck_s( 5,I)
#define _s6(I)          _ck_s( 6,I)
#define _s7(I)          _ck_s( 7,I)
#define _s8(I)          _ck_s( 8,I)
#define _s9(I)          _ck_s( 9,I)
#define _s10(I)         _ck_s(10,I)
#define _s11(I)         _ck_s(11,I)
#define _s12(I)         _ck_s(12,I)
#define _s13(I)         _ck_s(13,I)
#define _s14(I)         _ck_s(14,I)
#define _s15(I)         _ck_s(15,I)
#define _s16(I)         _ck_s(16,I)
#define _s17(I)         _ck_s(17,I)
#define _s18(I)         _ck_s(18,I)
#define _s19(I)         _ck_s(19,I)
#define _s20(I)         _ck_s(20,I)
#define _s21(I)         _ck_s(21,I)
#define _s22(I)         _ck_s(22,I)
#define _s23(I)         _ck_s(23,I)
#define _s24(I)         _ck_s(24,I)
#define _s25(I)         _ck_s(25,I)
#define _s26(I)         _ck_s(26,I)
#define _s27(I)         _ck_s(27,I)
#define _s28(I)         _ck_s(28,I)
#define _s29(I)         _ck_s(29,I)
#define _s30(I)         _ck_s(30,I)
#define _s31(I)         _ck_s(31,I)
#define _u1(I)          _ck_u( 1,I)
#define _u2(I)          _ck_u( 2,I)
#define _u3(I)          _ck_u( 3,I)
#define _u4(I)          _ck_u( 4,I)
#define _u5(I)          _ck_u( 5,I)
#define _u6(I)          _ck_u( 6,I)
#define _u7(I)          _ck_u( 7,I)
#define _u8(I)          _ck_u( 8,I)
#define _u9(I)          _ck_u( 9,I)
#define _u10(I)         _ck_u(10,I)
#define _u11(I)         _ck_u(11,I)
#define _u12(I)         _ck_u(12,I)
#define _u13(I)         _ck_u(13,I)
#define _u14(I)         _ck_u(14,I)
#define _u15(I)         _ck_u(15,I)
#define _u16(I)         _ck_u(16,I)
#define _u17(I)         _ck_u(17,I)
#define _u18(I)         _ck_u(18,I)
#define _u19(I)         _ck_u(19,I)
#define _u20(I)         _ck_u(20,I)
#define _u21(I)         _ck_u(21,I)
#define _u22(I)         _ck_u(22,I)
#define _u23(I)         _ck_u(23,I)
#define _u24(I)         _ck_u(24,I)
#define _u25(I)         _ck_u(25,I)
#define _u26(I)         _ck_u(26,I)
#define _u27(I)         _ck_u(27,I)
#define _u28(I)         _ck_u(28,I)
#define _u29(I)         _ck_u(29,I)
#define _u30(I)         _ck_u(30,I)
#define _u31(I)         _ck_u(31,I)

#endif /* __lightning_asm_common_h */
