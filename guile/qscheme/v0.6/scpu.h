/* -*- tab-width:8; -*- */
/*
 * Virtual cpu for scheme
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <limits.h>    /* for PAGESIZE */

#ifndef PAGESIZE
#define PAGESIZE 4096
#endif

#ifdef __i386__
#define INSTR_ALIGN	1

/*-- pushq is mapped to movl $MAGIC,(SP) encoding OP OP MAGIC */
#define SCPU_LOADQ_LIT_OFS	2

#define SCPU_TEST_LIT_H(addr) (memcmp(addr,&magic,sizeof(magic))==0)
#define SCPU_TEST_LIT_L(addr) 1

#define SCPU_STORE_LIT_H(addr,n) memcpy(addr,&(n),sizeof((n)))
#define SCPU_STORE_LIT_L(addr,n) 

#endif

#ifdef SPARC
#define INSTR_ALIGN	4

#define SCPU_LIT_MASK_H		((1 << 22) - 1)
#define SCPU_LIT_MASK_L 	((1 << 10) - 1)

#define SCPU_TEST_LIT_H(addr) \
  (((*((long*)(addr)) ^ (MAGIC>>10)) & SCPU_LIT_MASK_H) == 0)

#define SCPU_TEST_LIT_L(addr) \
  (((*((long*)(addr)) ^ MAGIC) & SCPU_LIT_MASK_L) == 0)

#define SCPU_STORE_LIT_H(addr, n) \
  *((long*)(addr)) = (*((long*)(addr)) & ~(SCPU_LIT_MASK_H)) | ((n) >> 10)

#define SCPU_STORE_LIT_L(addr, n) \
  *((long*)(addr)) = (*((long*)(addr)) & ~(SCPU_LIT_MASK_L)) | ((n) & SCPU_LIT_MASK_L)

#endif

#ifdef RS6000
#define INSTR_ALIGN	4

#define SCPU_LIT_MASK_H		0x0000ffff
#define SCPU_LIT_MASK_L 	0x0000ffff

#define SCPU_TEST_LIT_H(addr) \
  (((*((long*)(addr)) ^ (MAGIC>>16)) & SCPU_LIT_MASK_H) == 0)

#define SCPU_TEST_LIT_L(addr) \
  (((*((long*)(addr)) ^ MAGIC) & SCPU_LIT_MASK_L) == 0)

#define SCPU_STORE_LIT_H(addr, n) \
  *((long*)(addr)) = (*((long*)(addr)) & ~(SCPU_LIT_MASK_H)) | ((n) >> 16)

#define SCPU_STORE_LIT_L(addr, n) \
  *((long*)(addr)) = (*((long*)(addr)) & ~(SCPU_LIT_MASK_L)) | ((n) & SCPU_LIT_MASK_L)

#endif /* RS6000 */


#define SCPU_WORD		long

typedef unsigned char uchar;

typedef struct {
  char 		*name;
  void 		*start;
  void 		*end;
  uchar		haslit;		/* has lit */

  /* data filled during initialization*/
  int		litofs;
} SCPU_OPCODE;

typedef struct {
  SCPU_WORD	t;		/* top register */
  SCPU_WORD	*l;		/* lit register */
  SCPU_WORD	*sp, *ip, *cont, *env;

  SCPU_WORD *stack;
  long stack_size;

  long   	code_size;
  uchar 	*code_alloc;	/* allocated code area */
  uchar 	*code;		/* code area, aligned to PAGESIZE */
  uchar 	*cp;

  long		lit_max;	/* max number of lits */
  long		lit_cnt;	/* current number of lits */
  SCPU_WORD 	*lit;		/* the lit array */

  /* input param */
  int in_opc;			/* opcode number */
  char *in_asm_str;		/* string for assembler */

  /* output */
  SCPU_OPCODE *out_opc;		/* ptr to opcode entry */

  void *out_asm_code;		/* assembler output: code and lit*/
  SCPU_WORD *out_asm_lit;
} SCPU;

enum MODES {
  SCPU_INITIALIZE,
  SCPU_EXECUTE,
  SCPU_ASSEMBLE,
  SCPU_GETOPC
};

#include "scpu-op-enum.h"

#define MAGIC	0x12345678l

#define INSTR(name, code) s_##name: code; e_##name: goto *(ip++)
#define DECL(name,l) { #name, &&s_##name, &&e_##name, l }

#define SPUSH(x)	*sp++=(x)
#define SPOP(x)		(x) = *(--sp)

