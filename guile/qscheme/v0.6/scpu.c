/* -*- tab-width: 8; -*- */
#include "scpu.h"

/*** Temporaries to assemble some code */
char code[1024];
char *cp;
SCPU_WORD stack[128];

/* Force call to mprotect to work */
void * allocate ( size_t size )
{
  void* result ;
  int fd ;
	
  fd = open("/dev/zero", O_RDONLY ) ;
  result = mmap ( 0, size,
		  PROT_READ|PROT_WRITE|PROT_EXEC,MAP_PRIVATE,fd,0 ) ;
  close(fd) ;
  return result ;
}

/*-- lookup for instruction in scpu */
SCPU_OPCODE *scpu_lookup(SCPU_OPCODE *t, char *name)
{
  while(t->name) {
    if (strcmp(t->name, name) == 0) return(t);
    t++;
  }
  return(NULL);
}

void scpu_initialize(SCPU *cpu, SCPU_OPCODE *t)
{
  int size, i, found;
  long magic = MAGIC;
  char *p;

  fprintf(stderr, "SCPU_INIT: initialize code buffer\n");
  cpu->code_size = 8192;
  p = malloc(cpu->code_size + PAGESIZE - 1);
  if (p == NULL) {
    perror("cannot alloc");
    exit(1);
  }
  fprintf(stderr, "SCPU_INIT: code_alloc=%p\n", p);
  cpu->code_alloc = p;
  p = (char *)(((int) p + PAGESIZE-1) & ~(PAGESIZE-1));
  memset(p, 0x90, cpu->code_size);

  if (mprotect(p, cpu->code_size, PROT_READ|PROT_WRITE|PROT_EXEC)) {
    perror("mprotect failed");
    exit(1);
  }
  fprintf(stderr, "SCPU_INIT: code=%p\n", p);
  cpu->code = p;
  cpu->cp = p;

  fprintf(stderr, "SCPU_INIT: initialize lit[]\n");
  cpu->lit_max = 256;
  cpu->lit_cnt = 0;
  cpu->lit = malloc(cpu->lit_max * sizeof(SCPU_WORD));
  fprintf(stderr, "SCPU_INIT: lit[]=%p\n", cpu->lit);
    
  fprintf(stderr, "SCPU_INIT: initialize stack\n");
  cpu->stack_size = 8192;
  cpu->stack = malloc(cpu->stack_size * sizeof(cpu->stack[0]));
  cpu->sp = cpu->stack;
  
  fprintf(stderr, "SCPU_INIT: initialize code buffer\n");
  cp = code;

#ifdef OLD
  fprintf(stderr, "SCPU_INIT: searching for magic=%0lx\n", magic);
  while(t->name) {
    if (t->haslit) {
      fprintf(stderr, "SCPU_IHIT: magic for %s\t", t->name);
      size = t->end - t->start;
      found = 0;
      for (i = 0; i <= (size - sizeof(magic)); i += INSTR_ALIGN) {
	if (SCPU_TEST_LIT_H(t->start + i) &&
	    SCPU_TEST_LIT_L(t->start + i + INSTR_ALIGN)) {
	  fprintf(stderr, " found at offset %d\n", i);
	  found = 1;
	  t->litofs = i;
	  break;
	}
      }
      if (!found) {
	fprintf(stderr, " NOT FOUND\n");
	exit(1);
      }
    }
    t++;
  }
#endif
}

/* simple scpu assembler */
static char *parseword(char *p, char **after)
{
  char *q;
  while(*p && isspace(*p)) p++; /* skip while blank */
  q = p;
  while(*q && !isspace(*q)) q++;
  if (*q != 0)
    *q++ = 0;
  else 
    *q = 0;
  *after = q;
  return(p);
}

typedef struct {
  int	lab_nr;
  int	lit_nr;
  int	resolved;
} asm_lab;

static asm_lab *search_lab(asm_lab *l, int cnt, int lab_nr)
{
  int i;
  for (i = 0; i < cnt; i++) {
    if (l->lab_nr == lab_nr)	return(l);
    l++;
  }
  return(NULL);
}

static void genop(SCPU *cpu, SCPU_OPCODE *op)
{
  memcpy(cpu->cp, op->start, op->end - op->start);
  cpu->cp += (op->end - op->start);
}

void scpu_assembler(SCPU *cpu, SCPU_OPCODE *t)
{
  char 		*str;
  char 		*p, *q;
  SCPU_WORD 	*lit;
  int		nlits;
  int		maxlits;
  void 		*code;
  void 		*cp;
  int		code_asize;
  asm_lab	lab[256];
  int 		nlabs;
  int 		i;		/* general counter */
  int		ln;		/* lab number */
  asm_lab	*lp;		/* pointer to asm_lab entry */
  SCPU_OPCODE 	*op;
  
  cpu->cp = cpu->code;
  cpu->lit_cnt = 0;

  nlabs = 0;
  str = (char *)strdup(cpu->in_asm_str);
  p = str;
  q = p;
  while(*q) {			/* while more chars */
    p = parseword(q, &q);

    /* LIT <string> | <number> | <label> | <opc> */
    if (strcmp("lit", p) == 0) {
      p = parseword(q, &q);
      if (p[0] == '\"') {		/* quoted string */
	p[strlen(p)-1] = 0;	/* remove trailing quote */
	genop(cpu, t + SCPU_OP_LIT0 + cpu->lit_cnt);
	cpu->lit[cpu->lit_cnt++] = (SCPU_WORD)strdup(p);
	
      } else if (isdigit(*p)) {	/* number */

	genop(cpu, t + SCPU_OP_LIT0 + cpu->lit_cnt);
	cpu->lit[cpu->lit_cnt++] = atoi(p);

      } else if (p[0] == '@') {	/* label */

	ln = atoi(p+1);		/* get lab number */
	if ((lp = search_lab(lab, nlabs, ln)) == NULL) { /* not found */
	  genop(cpu, t + SCPU_OP_LIT0 + cpu->lit_cnt);
	  lab[nlabs].lab_nr = ln;
	  lab[nlabs].lit_nr = cpu->lit_cnt;
	  lab[nlabs].resolved = 0;
	  nlabs++;
	  cpu->lit_cnt++;
	} else {		/* found */
	  genop(cpu, t + SCPU_OP_LIT0 + lp->lit_nr);
	}
      } else {			/* should be a symbol */
	if ((op = scpu_lookup(t, p)) == NULL) {
	  fprintf(stderr, "ASM_ERROR: bad literal '%s'\n", p);
	  exit(1);
	}
	genop(cpu, t + SCPU_OP_LIT0 + cpu->lit_cnt);
	cpu->lit[cpu->lit_cnt++] = (SCPU_WORD)op->start;
      }
      continue;
    }
    if (strcmp("setj", p) == 0) {
      p = parseword(q, &q);
      if (p[0] == '@') {
	ln = atoi(p+1);		/* get lab number */
	if ((lp = search_lab(lab, nlabs, ln)) == NULL) { /* not found */
	  genop(cpu, t + SCPU_OP_SETJ0 + cpu->lit_cnt);
	  lab[nlabs].lab_nr = ln;
	  lab[nlabs].lit_nr = cpu->lit_cnt;
	  lab[nlabs].resolved = 0;
	  nlabs++;
	  cpu->lit_cnt++;
	} else {		/* found */
	  genop(cpu, t + SCPU_OP_SETJ0 + lp->lit_nr);
	}
      } else if ((op = scpu_lookup(t, p)) != NULL) {
	genop(cpu, t + SCPU_OP_SETJ0 + cpu->lit_cnt);
	cpu->lit[cpu->lit_cnt++] = (SCPU_WORD)op->start;
      } else {
	fprintf(stderr, "ASM_ERROR: bad label '%s'\n",p);
	exit(1);
      }
      continue;
    }

    /* LAB <label> */
    if (strcmp("lab", p) == 0) {
      asm_lab *lp;

      p = parseword(q, &q);
      if (*p != '@') {
	fprintf(stderr, "bad label '%s'\n", p);
	return;
      }
      ln = atoi(p+1);
      if ((lp = search_lab(lab, nlabs, ln)) == NULL) { /* not found */
	cpu->lit[cpu->lit_cnt] = (SCPU_WORD)cpu->cp;
	lab[nlabs].lab_nr = ln;
	lab[nlabs].lit_nr = cpu->lit_cnt;
	lab[nlabs].resolved=1;
	nlabs++;
	cpu->lit_cnt++;
      } else {
	cpu->lit[lp->lit_nr] = (SCPU_WORD)cpu->cp;
	lp->resolved = 1;
      }
      continue;
    }

    /* <intr> */
    if ((op = scpu_lookup(t, p)) == NULL) {
      fprintf(stderr, "ASM_ERROR: illegal opcode '%s'\n", p);
      exit(1);
      return;
    }
    genop(cpu, op);
  } /* while */
  for (i = 0; i < nlabs; i++) {
    if (!lab[i].resolved) {
      fprintf(stderr, "ASM_ERROR: label @%d not resolved\n", lab[i].lab_nr);
    }
  }
  for (i = 0; i < 10; i++) {
    genop(cpu, t + SCPU_OP_NOP);
  }
}

/* generate opcode */
void scpu_genop(SCPU *cpu, SCPU_OPCODE *t, char *name, long arg)
{
  SCPU_OPCODE *te;
  int size;

  fprintf(stderr, "SCPU_GENOP: gen %s\n", name);
  if ((te = scpu_lookup(t, name)) == NULL) {
    fprintf(stderr, "bad opcode %s\n", name);
    exit(1);
  }
  size = te->end - te->start;

  memcpy(cpu->cp, te->start, size);
  if (te->haslit) {
    SCPU_STORE_LIT_H(cpu->cp + te->litofs, arg);
    SCPU_STORE_LIT_L(cpu->cp + te->litofs + INSTR_ALIGN, arg);
  }
  cpu->cp += size;
}

void scpu_generate(SCPU *cpu, SCPU_OPCODE *t)
{
  scpu_genop(cpu, t, "loadq_r0",	10);
  scpu_genop(cpu, t, "loadq_r1",	20);
  scpu_genop(cpu, t, "add_r0_r1",	0);
  scpu_genop(cpu, t, "loadq_r1",	(long)scpu_lookup(t,"end")->start);
  scpu_genop(cpu, t, "jump_r1",		0);
}

void hello()
{
  printf("Hello world\n");
}

typedef void (*PF)();


void scpu(SCPU *cpu, int mode)
{
  register SCPU_WORD  t 	asm("ebx")		;
  register SCPU_WORD *j		/*asm("ebp")*/		; /* the Jmp reg */
  register SCPU_WORD *l 	asm("edi")		;
  register SCPU_WORD *sp 	asm("ecx")		;
  register SCPU_WORD *ip, *cont, *env;
  SCPU_WORD tmp;
  static SCPU_OPCODE labs[] = {
    DECL(nop,		0),
    DECL(load_cpu,	0),
    DECL(save_cpu,	0),
    DECL(end,		0),
    DECL(push,		0),
    DECL(pop,		0),
    DECL(drop,		0),
    DECL(add,		0),
    DECL(sub,		0),
    DECL(mul,		0),
    DECL(div,		0),
    DECL(mod,		0),
    DECL(neg,		0),
    
    DECL(incr,		0),
    DECL(decr,		0),

    DECL(0,		0),
    DECL(1,		0),
    DECL(2,		0),
    DECL(3,		0),
    DECL(4,		0),
    DECL(8,		0),

    DECL(lt,		0),
    DECL(le,		0),
    DECL(eq,		0),
    DECL(ge,		0),
    DECL(gt,		0),

    DECL(jmp,		0),
    DECL(brf,		0),

    DECL(brt,		0),
    DECL(blt,		0),
    DECL(ble,		0),
    DECL(beq,		0),
    DECL(bne,		0),
    DECL(bge,		0),
    DECL(bgt,		0),
    
    DECL(env0,		0),
    DECL(env1,		0),

    DECL(setenv0,		0),
    DECL(setenv1,		0),

    
    /*
    DECL(
    */
#include "scpu-lit-decl.h"    
    { NULL }
  };

  if (mode != SCPU_EXECUTE) {
    switch(mode) {
    case SCPU_INITIALIZE:	scpu_initialize(cpu,labs);		break;
    case SCPU_ASSEMBLE:		scpu_assembler(cpu, labs);		break;
#ifdef OLD
    case SCPU_GENERATE:		scpu_generate(cpu,labs);		break;
#endif
    case SCPU_GETOPC:		cpu->out_opc = labs + cpu->in_opc;	break;
    }
    return;
  }
  INSTR(load_cpu,
	sp = cpu->sp;  ip = cpu->ip;  env= cpu->env;  cont=cpu->cont;
	t = cpu->t;    l = cpu->l);

  INSTR(nop, );

  INSTR(save_cpu,
	cpu->sp	= sp;	cpu->ip	= ip;  cpu->env = env;	cpu->cont = cont;
	cpu->t = t;  	cpu->l = l);

  INSTR(push,	*sp++ = t);
  INSTR(pop,	t = *(--sp));
  INSTR(drop,	sp--);

  /* arith */
  INSTR(add,	t = t + (*(--sp))); /* t + pop */
  INSTR(sub,	t = t - (*(--sp))); /* t - pop */
  INSTR(mul,	t = t * (*(--sp))); /* t * pop */
  INSTR(div,	t = t / (*(--sp))); /* t / pop */
  INSTR(mod,	t = t % (*(--sp))); /* t % pop */
  INSTR(neg,	t = -t           ); /* -t */
  INSTR(incr,	t++ );
  INSTR(decr,	t-- );
  
  /* const */
  INSTR(0,	t = 0);
  INSTR(1,	t = 1);
  INSTR(2,	t = 2);
  INSTR(3,	t = 3);
  INSTR(4,	t = 4);
  INSTR(8,	t = 8);

  /* comparison */
  INSTR(lt,	t = (*(--sp)) <  t);
  INSTR(le,	t = (*(--sp)) <= t);
  INSTR(eq,	t = (*(--sp)) == t);
  INSTR(ge,	t = (*(--sp)) >= t);
  INSTR(gt,	t = (*(--sp)) >  t);

  
  /* jumps */
  INSTR(jmp,	goto *((void*)j) );
  INSTR(brt, 	if (*(--sp) != 0) goto *((void*)j));
  INSTR(brf, 	if (*(--sp) == 0) goto *((void*)j));

  INSTR(blt,
  {SCPU_WORD n=*(--sp); if (t <  n) goto*((void*)j);});
  INSTR(ble,
  {SCPU_WORD n=*(--sp); if (t <= n) goto*((void*)j);});
  INSTR(beq,
  {SCPU_WORD n=*(--sp); if (t == n) goto*((void*)j);});
  INSTR(bne,
  {SCPU_WORD n=*(--sp); if (t != n) goto*((void*)j);});
  INSTR(bge,
  {SCPU_WORD n=*(--sp); if (t >= n) goto*((void*)j);});
  INSTR(bgt,
  {SCPU_WORD n=*(--sp); if (t <  n) goto*((void*)j);});

  INSTR(env0,	t = env[0]);
  INSTR(env1,	t = env[1]);
  
  INSTR(setenv0,	env[0] = t);
  INSTR(setenv1,	env[1] = t);

 s_LIT0: e_LIT0:
#include "scpu-lit-instr.h"

 s_end:	
  printf("t=%d l=%p\n", t, l);
  return;
 e_end:
}

/****
 * Scpu model:
 * Register: r0, r1
 *
 *
 *  Instruction set:
 */

SCPU_OPCODE *getopc(SCPU *cpu, int opc)
{
  cpu->in_opc = opc;  cpu->out_opc = NULL;  scpu(cpu, SCPU_GETOPC);
  return( cpu->out_opc );
}

void old_genop(SCPU *cpu, int opc, SCPU_WORD lit)
{
  SCPU_OPCODE *te;
  int size;
  
  te = getopc(cpu, opc);
  size = te->end - te->start;
  printf("GENOP: %s size=%d cp=%p\n", te->name, size, cpu->cp);
  memcpy(cpu->cp, te->start, size);
  if (te->haslit) {
    SCPU_STORE_LIT_H(cpu->cp + te->litofs, lit);
    SCPU_STORE_LIT_L(cpu->cp + te->litofs + INSTR_ALIGN, lit);
  }
  cpu->cp += size;
}

int main()
{
  SCPU cpu;
  /* test program: a loop for 0 to 30000000 */
  char *loop_prog = "
    lit 3000000 setenv1    lit 0 setenv0
    setj @1 jmp
    lab @2	env0 incr setenv0
    lab @1	push env1 setj @2 bge
    setj end jmp";

  char *simple_prog="lit 10 push lit 20 add exj lit end exj jmp";

  scpu(&cpu, SCPU_INITIALIZE);

  cpu.in_asm_str = loop_prog;
  scpu(&cpu, SCPU_ASSEMBLE);

  cpu.env = cpu.stack;
  cpu.sp  = cpu.stack + 2;
  cpu.ip  = (void*)cpu.code;
  cpu.l   = cpu.lit;		/* set cpu literal */
  scpu(&cpu, SCPU_EXECUTE);

  exit(0);
  
  cpu.ip = (void*)cpu.code;
  cpu.sp = cpu.stack;
  scpu(&cpu, SCPU_EXECUTE);

}
