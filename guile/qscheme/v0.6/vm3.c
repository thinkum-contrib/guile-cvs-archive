/* -*- tab-width: 8; -*-
 *
 * VM3 engine:
 *
 * $Id$
 *
 */
#ifndef lint
static char vcid[] = "$Id$";
#endif /* lint */

#include "s.h"

/*#define VM3_DEBUG */
/*#define DEBUG_DICT */

#include "vm3.h"

#define SCM_DICT_MAX_DEPTH	4

#define SCM_DICT	struct _SCM_DICT
#define SCM_DICT_ENTRY	struct _SCM_DICT_ENTRY

SCM_DICT_ENTRY {
  SCM_DICT_ENTRY *next;
  void *key;
  void *value;
};

SCM_DICT {
  int  	size;			/* number of entries in dict array */
  int	nkeys;			/* current number of keys stored */
  int 	maxkeys;		/* max depth */
  SCM_DICT_ENTRY *(*newkey)(); 	/* key,value */
  void	(*setkey)();		/* DICT_ENTRY *, key */
  Uint	(*hash)();		/* hash function */
  int	(*cmp)();		/* compare keys: result like strcmp */
  SCM_DICT_ENTRY **entry;	/* entries in dictionnary */
};

static SCM_VMD 		scm_vm3_vmd;
static void 		*call_table[SOBJ_T_MAX];
static SCM_VM3_PRIM 	*prim_table;
static SCM_DICT		*prim_dict;

/****************************************************************
 * Dict tools
 ****************************************************************/

/*** Driver for hash where keys are strings */
Uint scm_dict_string_hash(unsigned char *s)
{
  Uint c, r=0;
  while((c = *s++) != 0)  r = (r*7) ^ c;
  return(r);
}

SCM_DICT_ENTRY *scm_dict_string_newkey(char *key, void *value)
{
  SCM_DICT_ENTRY *de = scm_must_alloc(sizeof(SCM_DICT_ENTRY));

  de->next = NULL;
  de->key = scm_must_strdup(key);
  de->value = value;
  return(de);
}

void scm_dict_string_setkey(SCM_DICT_ENTRY *de, void *value)
{
  de->value = value;
}

SCM_DICT *scm_dict_new()
{
  int i;
  SCM_DICT *dict = scm_must_alloc(sizeof(SCM_DICT));
  dict->size = 4;
  dict->nkeys = 0;

  dict->newkey 	= scm_dict_string_newkey;
  dict->setkey 	= scm_dict_string_setkey;
  dict->hash 	= scm_dict_string_hash;
  dict->cmp  	= strcmp;
  
  dict->maxkeys = dict->size * SCM_DICT_MAX_DEPTH;
  dict->entry = scm_must_alloc(dict->size * sizeof(SCM_DICT_ENTRY *));
  for (i = 0; i < dict->size; i++) dict->entry[i] = NULL;
  return(dict);
}

void scm_dict_dump(SCM_DICT *dict)
{
  SCM_DICT_ENTRY *l;
  int i;
  fprintf(stderr, "dict at %p: size=%d nkeys=%d maxkeys=%d\n",
	  dict, dict->size, dict->nkeys, dict->maxkeys);
  for (i = 0; i < dict->size; i++) {
    fprintf(stderr, "entry[%d]=%p:", i, dict->entry[i]);
    for (l = dict->entry[i]; l; l = l->next) {
      fprintf(stderr, "(%s %p) ", l->key, l->value);
    }
    fprintf(stderr, "\n");
  }
}

SCM_DICT_ENTRY *scm_dict_search(SCM_DICT *dict, void *key)
{
  SCM_DICT_ENTRY *de;
  int hcode;

  hcode = (*dict->hash)(key) % dict->size;
#ifdef DEBUG_DICT
  scm_dict_dump(dict);
  fprintf(stderr, "dict-search: %s (%d)\n", key, hcode);
#endif
  de = dict->entry[hcode];
  while(de != NULL) {
    if ( (*dict->cmp)(key, de->key) == 0)  break;
    de = de->next;
  }
  return(de);
}

SCM_DICT_ENTRY *scm_dict_set(SCM_DICT *dict, void *key, void *value)
{
  SCM_DICT_ENTRY *de;
  int hcode;
#ifdef DEBUG_DICT
  scm_dict_dump(dict);
#endif

  hcode = (*dict->hash)(key) % dict->size;
#ifdef DEBUG_DICT
  fprintf(stderr, "dict-set: key=%s hcode=%d value=%p\n",
	  key, hcode, value);
#endif
  de = dict->entry[hcode];
  while(de != NULL) {
    if ( (*dict->cmp)(key, de->key) == 0) {
      (*dict->setkey)(de, value);
      return(de);
    }
    de = de->next;
  }
  de = (*dict->newkey) (key, value);
  de->next = dict->entry[hcode];
  dict->entry[hcode] = de;
  dict->nkeys++;
  if (dict->nkeys >= dict->maxkeys) {
    SCM_DICT_ENTRY **ref, *l;
    int i, nr, hc;
#ifdef DEBUG_DICT
    fprintf(stderr, "dict_set: must grow (nkeys=%d maxkeys=%d)\n",
	    dict->nkeys, dict->maxkeys);
#endif
    ref = scm_must_alloc(dict->nkeys * sizeof(SCM_DICT_ENTRY*));
    nr = 0;
    for (i = 0; i < dict->size; i++) {
      for (l = dict->entry[i]; l; l = l->next) {
	ref[nr++] = l;
      }
    }
    dict->size *= 4;
    dict->maxkeys = dict->size * SCM_DICT_MAX_DEPTH;
    dict->entry = scm_must_realloc(dict->entry, dict->size*sizeof(SCM_DICT_ENTRY*));
    for (i = 0; i < dict->size; i++)
      dict->entry[i] = NULL;
    for (i = 0; i < nr; i++) {	/* rehash now */
      hc = (*dict->hash)(ref[i]->key) % dict->size;
      ref[i]->next = dict->entry[hc];
      dict->entry[hc] = ref[i];
    }
    scm_free(ref);
  }
  return(de);
}


/****************************************************************
 * Debugging / Trace tools
 ****************************************************************/
void scm_vm3_dump_env(ENVFRAME *e)
{
  int i, ns;
  while(e) {
    ns = SCM_INUM(e->nslots);
    printf("env %p: nslots=%d\n", e, ns);
    if (ns > 10) ns = 10;
    for (i = 0; i < ns; i++) {
      printf("  slot[%d] : "); scm_cprint(e->slot[i]);
    }
    if (ns != SCM_INUM(e->nslots))
      printf("  ...\n");

    printf("\n");
    e = e->next;
  }
}

void scm_vm3_dump_cont(CONTEXT *c)
{
  while(c) {
    printf("context %p: next=%p env=%p ip=%p\n",
	   c->next, c->env, c->ip);
    c = c->next;
  }
}

void scm_vm3_dump_stack(SOBJ *sp)
{
  SOBJ *p;
  p = scm_vm3_vmd.stack_base;
  while(p < sp) {
    printf("%p: %p\t", p, *p);
    if ((void*)*p >= (void*)scm_vm3_vmd.stack_base &&
	(void*)*p <  (void*)scm_vm3_vmd.stack_limit) {
      printf("<stack ref>\n");
    } else {
      SCM_VM3_PRIM *te = prim_table;
      void *addr = *p;
      while (te->name && te->start != addr)  te++;
      if (te->name) {
	printf("#<prim %s>\n", te->name);
      } else {
	scm_cprint(addr);
      }
    }
    p++;
  }
}

/****************************************************************
 * vm3 search 
 ****************************************************************/

SCM_VM3_PRIM *scm_vm3_search_name(SCM_VM3_PRIM *t, char *name)
{
  /*  fprintf(stderr, "scm_vm3_search_name: searching %s\n", name); */
  while(t->name != NULL) {
    /* fprintf(stderr, "scm_vm3_search_name: testing %s\n", t->name); */
    if (strcmp(t->name, name) == 0)	return(t);
    t++;
  }
  return(NULL);
}

SCM_VM3_PRIM *scm_vm3_search_addr(SCM_VM3_PRIM *t, void *addr)
{
  /*  fprintf(stderr, "scm_vm3_search_addr: searching %p\n", addr); */
  while(t->name != NULL) {
    /* fprintf(stderr, "scm_vm3_search_addr: testing %p\n", t->start); */
    if (t->start == addr)	return(t);
    t++;
  }
  return(NULL);
}


/****************************************************************
 * vm3 callbacks
 ****************************************************************/

void scm_vm3_engine_init(SCM_VMD *vm, SCM_VM3_PRIM *t)
{
  SCM_VM3_PRIM *te;
  int i;

  fprintf(stderr, "scm_vm3_engine_init:\n");
  
  fprintf(stderr, "scm_vm3_engine_init: stack init\n");

  vm->stack_size = 4096;
  vm->stack_base = scm_must_alloc(sizeof(SOBJ)*vm->stack_size);
  vm->stack_limit = vm->stack_base + vm->stack_size;

  fprintf(stderr, "scm_vm3_engine_init: regs init\n");
  vm->reg.sp = vm->stack_base;
  vm->reg.ip = NULL;
  vm->reg.env = NULL;
  vm->reg.cont= NULL;

  fprintf(stderr, "scm_vm3_engine_init: setup of call table\n");
  for (i = 0; i < SOBJ_T_MAX; i++) {
    call_table[i] = t[SCM_OP_NOP].start; /* default to nop */
  }
  call_table[SOBJ_T_PRIM] 	= t[SCM_OP_GEN_CALLPRIM].start;
  call_table[SOBJ_T_CPRIM] 	= t[SCM_OP_GEN_CALLCPRIM].start;
  call_table[SOBJ_T_CODE] 	= t[SCM_OP_GEN_CALLCODE].start;
  call_table[SOBJ_T_PROC] 	= t[SCM_OP_GEN_CALLPROC].start;
  call_table[SOBJ_T_CLOSURE] 	= t[SCM_OP_GEN_CALLCLOS].start;
  call_table[SOBJ_T_MACRO] 	= t[SCM_OP_GEN_CALLMACRO].start;

  for (i = 0; i < scm_type_next_descr; i++) {
    call_table[i] = t[scm_type_hook[i].execute].start;
  }
  while(i < SOBJ_T_MAX) {
    call_table[i] = t[0].start; /* default to nop */
    i++;
  }

  /* build a dictionnary with key=(char*)name and value=(SCM_VM3_PRIM*)
   * for later use in assembler.
   */
  prim_dict = scm_dict_new();
  for (te = t; te->name; te++) {
    scm_dict_set(prim_dict, te->name, te);
  }
}


/*** straight forward assembler */
/* The only special stuff about this assembler is that it reconizes
 * this special constructs:
 *   (%lab <lab>)
 *   (%bra|%brf|%brt <lab>)
 *   (%asm <array>)
 *
 * Labels are resolved using one pass logic.
 * %asm recursively assembles a piece of code.
 */
#define SCM_LABREF 	struct _SCM_LABREF
#define SCM_LABNODE	struct _SCM_LABNODE

SCM_LABREF {
  int resolved;
  SOBJ lab;
  union {
    int		ofs;		/* if resolved */
    SCM_LABNODE *node;		/* if ! resolved */
  } ref;
};

SCM_LABNODE {
  SCM_LABNODE 	*next;
  int		ofs;
};

static void labnode_new(SCM_LABREF *lr, int ofs)
{
  SCM_LABNODE *node;
  node = scm_must_alloc(sizeof(SCM_LABNODE));
  node->next = lr->ref.node;    lr->ref.node = node;
  node->ofs = ofs;
}

static SCM_LABREF *label_search(SCM_LABREF *lr, int count, SOBJ lab)
{
  while(--count >= 0) {
    if (lr->lab == lab) return(lr);
    lr++;
  }
  return(NULL);
}

static void label_resolve(SCM_LABREF *lr, int ofs, SOBJ code)
{
  SCM_LABNODE *ln, *node;
  if (lr->resolved) {
    /* should already have been used */
    fprintf(stderr, "asm:error: label already resolved\n");
    exit(1);
  }
  ln = lr->ref.node;
  while(ln != NULL) {		/* scan chain of labels to resolve */
    SCM_AREF(code, ln->ofs) = SCM_MKINUM(ofs - ln->ofs);
    node = ln;  ln = ln->next;
    scm_free(node);
  }
  lr->resolved = TRUE;
  lr->ref.ofs = ofs;
}

static SCM_LABREF *label_add_def(SCM_LABREF **lr, int *count, int *alloc,
				 int flag, SOBJ lab)
{
  SCM_LABREF *lp;
  int n;

  n = *count;
  
  if ((n+1) >= *alloc) {
    *alloc += 128;
    *lr = scm_must_realloc(*lr, *alloc * sizeof(SCM_LABREF));
  }
  lp = *lr + n;
  lp->resolved = flag;
  lp->lab = lab;
  lp->ref.node = NULL;
  *count = n + 1;
  return(lp);
}

static void labref_zap(SCM_LABREF *lr, int count)
{
  int i;
  SCM_LABNODE *ln, *node;
  for (i = 0; i < count; i++) {
    if (!lr[i].resolved) {
      ln = lr[i].ref.node;
      while (ln) {
	node = ln; ln = ln->next; scm_free(node);
      }
    }
  }
  scm_free(lr);
}

static SOBJ scm_vm3_engine_asm(SOBJ code, SCM_VM3_PRIM *t, int inl)
{
  SOBJ inst, opc, list;
  SOBJ out;
  SCM_DICT_ENTRY *de;
  SCM_VM3_PRIM *te;
  SCM_LABREF *labref, *lr;
  int 	labref_alloc;
  int	labref_count;
  int i, size, cp;
  char *str;

  if (!SCM_ARRAYP(code))	SCM_ERR("bad code", code);

  labref_alloc = 128;
  labref_count = 0;
  labref = scm_must_alloc(labref_alloc * sizeof(SCM_LABREF));

  out = scm_mkarray(0,scm_false);
  for (cp = 0; cp < SCM_ASIZE(code); cp++) {
    inst = SCM_AREF(code, cp);
    if (!SCM_PAIRP(inst))	SCM_ERR("bad instruction", inst);

    opc = SCM_CAR(inst);
    str = SCM_ATOM_NAME(opc);

    /* handle special cases */
    if (streq(str, "%lab")) {	/* label definition */
      if ((lr = label_search(labref, labref_count, SCM_CADR(inst))) != NULL) {
	/* resolve location where this label was used */
	label_resolve(lr, SCM_ASIZE(out), out);
      } else {
	/* new label, never used before */
	lr = label_add_def(&labref, &labref_count, &labref_alloc,
			   TRUE, SCM_CADR(inst));
	lr->ref.ofs = SCM_ASIZE(out);
      }
      continue;
    } 
    if (streq(str, "%bra") ||
	streq(str, "%brf") ||
	streq(str, "%brt")) {	/* label consumer */

      if ((de = scm_dict_search(prim_dict, str)) == NULL) {
	labref_zap(labref, labref_count);
	SCM_ERR("instr not found", opc);
      }
      te = de->value;
      size = SCM_ASIZE(out);
      out = scm_vector_resize(out, SCM_MKINUM(size + te->follow +1));
      SCM_AREF(out, size++) = (void*)te->start;
      if ((lr = label_search(labref, labref_count, SCM_CADR(inst))) != NULL) {
	/* already defined */
	SCM_AREF(out, size  ) = SCM_MKINUM(lr->ref.ofs - size);
      } else {
	/* new label, never seen before */
	lr = label_add_def(&labref, &labref_count, &labref_alloc,
			   FALSE, SCM_CADR(inst));
	labnode_new(lr, size);
	SCM_AREF(out, size  ) = NULL;
      }
      continue;
    }

    if (streq(str, "%asm")) {	/* something to assemble */
      SCM_ERR("%asm is not supported yet", NULL);
      continue;
    }
    
    if ((de = scm_dict_search(prim_dict, str)) == NULL) {
      SCM_ERR("instr not found", opc);
    }
    te = de->value;
    size = SCM_ASIZE(out);
    out = scm_vector_resize(out, SCM_MKINUM(size + te->follow +1));
    SCM_AREF(out, size++) = (void*)te->start;

    list = SCM_CDR(inst);
    for (i = 0; i < te->follow; i++) {
      if (!SCM_PAIRP(list)) SCM_ERR("not enough args for", opc);
      SCM_AREF(out, size++) = SCM_CAR(list);
      list = SCM_CDR(list);
    }
  }
  labref_zap(labref, labref_count); /* no leak please */
  return(out);
}

static SOBJ scm_vm3_engine_dis(SOBJ code, SCM_VM3_PRIM *t)
{
  int i, n;
  SCM_VM3_PRIM *te;
  
  if (!SCM_ARRAYP(code))	SCM_ERR("bad array", code);
  i = 0;
  while(i < SCM_ASIZE(code)) {
    te = scm_vm3_search_addr(t, SCM_AREF(code, i));
    if (te == NULL) {
      fprintf(stderr, "scm_vm3_dis: no opcode matching %p\n",
	      SCM_AREF(code, i));
      SCM_ERR("bad opcode", NULL);
    }
    printf("%p: %s ", &SCM_AREF(code, i), te->name);
    i++;
    n = te->follow;
    while(--n >= 0) {
      scm_cdisplay(SCM_AREF(code, i)); printf(" ");
      i++;
    }
    printf("\n");
  }
  return(scm_undefined);
}


/********************************
 * VM3 Env utils
 ********************************/
static SOBJ env_clone(SOBJ env)
{
  SOBJ new;
  SCM_EnvFrame *ef;
  int nbytes;
  
  new = scm_newcell(SOBJ_T_ENV);
  ef = SCM_ENV_FRAME(env);
  nbytes = offsetof(SCM_EnvFrame, binding[SCM_INUM(ef->nslots)]);
  
  /*  SCM_ENV_NEXT(new) = NULL; */
  SCM_ENV_FRAME(new) = scm_must_alloc(nbytes);
  memcpy(SCM_ENV_FRAME(new), ef, nbytes);
  fprintf(stderr, "VM3:env_clone: cloning env %p -> %p\n", env, new);
  return(new);
}

static SOBJ env_deep_copy(SOBJ env, void *sbase, void *slimit)
{
  SOBJ e, new, envchain, *ptr;
  if ((void*)env >= sbase && (void*)env < slimit) {
    fprintf(stderr, "VM3:env_deep_copy: starting with env %p\n", env);
    envchain = NULL;
    ptr = &SCM_ENV_NEXT(envchain);
    for (e = (void*)env; e; e = SCM_ENV_NEXT(e)) {
      new = env_clone(env);
      *ptr = new;
      ptr = &SCM_ENV_NEXT(new);
    }
  } else {
    fprintf(stderr, "VM3:env_deep_copy: env %p is not on stack\n", env);
    envchain = env;
  }
  return(envchain);
}


/*** Initialize VM3 engine */



void scm_vm3(SCM_VMD *vm)
{
  register SOBJ		T;	/* scpu registers */
  register SOBJ 	*ip;
  register SOBJ 	*sp;
  /*register*/ CONTEXT 		*cont;
  /*register*/ ENVFRAME 	*env;

  static SCM_VM3_PRIM symbol[] = {
    /* name             arity/follow/term */
    DECL(nop,		0, 0, 0),
    DECL(end, 		0, 0, 1),

    DECL(pusht,		1, 0, 0),
    DECL(popt,		1, 0, 0),
    DECL(drop,		1, 0, 0),
    DECL(addt,		1, 0, 0),
    
    DECL(pushlit,	0, 1, 0),
    DECL(pushsym,	0, 1, 0),
    DECL(pushloc,	0, 1, 0),
    DECL(pushext,	0, 1, 0),

    DECL(setsym,	1, 1, 0),
    DECL(setloc,	1, 1, 0),
    DECL(setext,	1, 1, 0),

    DECL(pushenv,	0, 0, 0),
    DECL(popenv,	0, 0, 0),

    DECL(bra,		0, 1, 0),
    DECL(brf,		1, 1, 0),
    DECL(brt,		1, 1, 0),

    DECL(prepcall,	0, 0, 0),
    DECL(makeclos,	1, 0, 0),
    DECL(callclos,	0, 0, 0),
    DECL(return,	0, 0, 0),

    DECL(letstart,	0, 0, 0),
    DECL(letbody,	0, 1, 0),
    DECL(letend,	0, 0, 0),
    DECL(letrec,	0, 1, 0),

    DECL(gen_callprim,	-1, 0, 0),
    DECL(gen_callcprim,	-1, 0, 0),
    DECL(gen_callcode,	-1, 0, 0),
    DECL(gen_callproc,	-1, 0, 0),
    DECL(gen_callclos,	-1, 0, 0),
    DECL(gen_callmacro,	-1, 0, 0),

    DECL(call,		1, 0, 0),

    DECL(callcprim,	-1, 1, 0),
    DECL(callcprim0,	0, 1, 0),
    DECL(callcprim1,	1, 1, 0),
    DECL(callcprim2,	2, 1, 0),
    DECL(callcprim3,	3, 1, 0),
    DECL(callcprim4,	4, 1, 0),
    DECL(callcprim5,	5, 1, 0),
    DECL(callcprim6,	6, 1, 0),
    DECL(callcprim7,	7, 1, 0),
    DECL(callcprim8,	8, 1, 0),
    DECL(callcprim9,	9, 1, 0),
    DECL(callcprim10,	10, 1, 0),

    
#include "prim3.x"

    {NULL}
  };

  if (vm->code != SCM_VM3_EXECUTE) {
    switch(vm->code) {
    case SCM_VM3_INIT:
      scm_vm3_engine_init(vm, symbol);
      break;
    case SCM_VM3_ASSEMBLE:
      vm->ret.obj = scm_vm3_engine_asm(vm->arg.addr, symbol, 0);
      break;
    case SCM_VM3_DISASSEMBLE:
      vm->ret.obj = scm_vm3_engine_dis(vm->arg.addr, symbol);
      break;
    }
    return;
  }
  
  /* load initial register state */
  ip 	= vm->reg.ip;
  sp  	= vm->reg.sp;
  cont 	= (CONTEXT*)vm->reg.cont;
  env  	= (ENVFRAME*)vm->reg.env;
  
  START_VMCODE
  INSTR(nop) {	END(nop); }
  INSTR(end) {
    spop(vm->ret.obj);
    supdate();
    vm->reg.sp = sp;
    return;
    END(end);
  }

  /* Experimental instructions */
  INSTR(pusht) {
    spush(T);
    END(pusht);
  }
  INSTR(popt) {
    spop(T);
    END(popt);
  }
  INSTR(drop) {
    sp--;
    END(drop);
  }
  INSTR(addt) {
    (long)T += (long)(TOS) & ~1;
    END(addt);
  }



  /* Regular instruction */
  INSTR(pushlit) {
    spush(*ip++);
    END(pushlit);
  }
  INSTR(pushsym) {
    spush(SCM_SYM_VALUE((SOBJ)(*ip++)));
    if (TOS == scm_unbound) SCM_ERR("symbol unbound", SCM_SYM_NAME(ip[-1]));
    END(pushsym);
  }
  INSTR(pushloc) {		/* -- value */
    /* nible is: LLII */
    long  nibble = SCM_INUM(*ip++);
    short depth = nibble >> 16;
    ENVFRAME *e = env;
    while(--depth >= 0) e = e->next;
    spush(e->slot[(nibble & 0xffff)]);
    END(pushloc);
  }

  INSTR(pushext) {		/* -- value */
    SOBJ var = *ip++;
    spush( (*(SCM_VAR_AUX(TOS)->get))(var,NULL) );
    END(pushext);
  }

  INSTR(setsym) {		/* value -- value */
    SOBJ var = *ip++;
    SCM_SYM_VALUE(var) = TOS;
    END(setsym);
  }

  INSTR(setloc) {		/* value -- value */
    long  nibble = SCM_INUM(*ip++);
    short depth = nibble >> 16;
    ENVFRAME *e = env;
    while(--depth >= 0) e = e->next;
    e->slot[nibble && 0xffff] = TOS;
    END(setloc);
  }

  INSTR(setext) {		/* value -- value */
    SOBJ var = *ip++;
    (*(SCM_VAR_AUX(var)->set))(var,NULL,TOS);
    END(setext);
  }


  /* 	Stack layout for pushenv
   *	Before			After
   *    			[ ??? ]	<-sp		.slot[0]
   *    		     +->[ ??? ]			.nslots
   *    		     +--+--*  ]			ENV_FRAME(env)
   *    		     +--+--*  ]			ENV_NEXT(env)
   *	[ ... ] <- sp	     |  | ??? ] <-env		.type
   *	[ ... ]		     |  [ ... ]		
   *	[ ... ]		     |  [ ... ]		
   *	[ ... ] <--env	     +->[ ... ]
   */
  INSTR(pushenv) {
    ENVFRAME *e = (void*)sp;
    e->frame = (void*)&e->nslots;
    e->next  = env;
    env = e;
    sp = (void*)&e->slot;
    END(pushenv);
  }

  /*	Stack layout for popenv
   *	Before			After
   *    [ ... ] <-sp
   *    [ v#1 ]
   *    [ v#0 ]
   *    [ l#1 ]
   *    [ l#0 ] 		[ ... ]<-sp
   *    [  2  ]			[ v#1 ]
   * +--+--*. ] <-env		[ v#0 ]
   * |  [ ... ]		       	[ ... ]
   * |  [ ... ]		       	[ ... ]
   * +->[ ... ]			[ ... ]<-env
   */
  INSTR(popenv) {
    ENVFRAME *e = env->next;
    sp = (void*)env;
    env = e;
    END(popenv);
  }

  /* Branches */
  INSTR(bra) {
    ip += SCM_INUM(*ip);
    END(bra);
  }

  INSTR(brf) {
    SOBJ flag;
    spop(flag);
    if (flag == scm_false) {
      ip += SCM_INUM(flag);
    }
    END(brf);
  }

  INSTR(brt) {
    SOBJ flag;
    spop(flag);
    if (flag != scm_false) {
      ip += SCM_INUM(flag);
    }
    END(brt);
  }

  /*	Stack layout for prepcall
   *	Before			After
   *	[ ... ]			[ ... ]<-sp
   *	[ ... ]		  +-----+--*. ]<-env
   *	[ ... ]<-sp	  |  +--+--*. ]<-cont
   *	~ ... ~		  |  |	~ ... ~
   *	[ ... ]<-env	  +--+->[ ... ]
   *	~ ... ~		     |	~ ... ~
   *	[ ... ]<-cont	     +->[ ... ]
   */
  INSTR(prepcall) {
    supdate();
    ((CONTEXT*)sp)->next = cont;
    ((CONTEXT*)sp)->env  = env;
    cont = (void*)sp;
    env  = (void*)(sp + CONT_TOTAL_SLOTS);
    env->frame = (void*)&env->nslots;
    sp  += CONT_TOTAL_SLOTS + ENV_SLOT;
    END(prepcall);
  }
  INSTR(makeclos) {
    SOBJ closure = scm_newcell(SOBJ_T_CLOSURE);
    SCM_CLOSURE_CODE(closure) = TOS;
    SCM_CLOSURE_ENV(closure) = env_deep_copy((void*)env, vm->stack_base, sp);
    TOS = closure;
    END(makeclos);
  }
  
  INSTR(callclos) {
    SOBJ closure;
    SCM_Code *code;
    spop(closure);
    cont->ip = ip;
    env->next   = (ENVFRAME*)SCM_CLOSURE_ENV(closure);
    env->nslots = SCM_MKINUM(sp - env->slot);
    code = SCM_PROC_CODE(SCM_CLOSURE_CODE(closure));
    ip   = code->code;
    END(callclos);
  }
  INSTR(return) {
    SOBJ val = TOS;
    ip = cont->ip;
    env = cont->env;
    sp = (void*)cont;
    cont = cont->next;
    spush(val);
    END(return);
  }
  INSTR(letstart) {
    sp +=2;			/* reserve space for ENVFRAME header */
    END(letstart);
  }
  INSTR(letbody) {		/* nslots */
    SOBJ ns;
    ENVFRAME *ep;
    ns = *ip++;
    /* find addr of current env */
    ep = (ENVFRAME*)(sp - (SCM_INUM(ns) + ENV_HEADER_SLOTS));
    ep->next = env;		/* build envframe header */
    ep->nslots = ns;
    env = ep;
    END(letbody);
  }
  INSTR(letend) {
    SOBJ result = TOS;
    sp  = (void*)env;
    env = env->next;
    spush(result);
    END(letend);
  }

  /* letrec nslots */
  INSTR(letrec) {				/* just push env frame */
    ENVFRAME *ep;
    SOBJ *p, *l;
    ep = (ENVFRAME*)sp;
    ep->next = env;
    ep->frame = (void*)ep->slot;
    ep->nslots = *ip++;
    p = ep->slot;
    l = ep->slot + SCM_INUM(ep->nslots);
    while(p < l) *p++ = scm_unbound;
    sp = ep->slot;
    env = ep;

    END(letrec);
  }
  
  /* Generic call functions, to be used by %call
   * Stack before:
   * [ cont envframe arg1 arg2 .. argn func ]
   * Stack after:
   * [ result ]
   */
  
  INSTR(gen_callprim) {
    SOBJ prim;
    SCM_VM3_PRIM *te;
    spop(prim);
    te = SCM_PRIM(prim);
    if (te->arity < 0) {	/* prim with variable number of args */
      /* Algo:
       * pop the cont 
       * move the env to the location where cont was
       * jump to primitive */
      SOBJ *src, *dst, *newenv;
      src = env;
      dst = cont;
      cont = cont->next;
      env  = dst;
      while(src < sp) {  *dst++ = *src++; }
      sp = dst;
      goto *(te->start);

    } else {
      /* Algo:
       * pop the cont and the env
       * move the arguments where the cont was
       * Jump to primitive
       */
      SOBJ *src, *dst, *newenv;
      src = env->slot;
      dst = cont;
      cont = cont->next;
      env  = env->next;
      while(src < sp) {  *dst++ = *src++; }
      sp = dst;
      goto *(te->start);
    }
    END(gen_callprim);
  }
  INSTR(gen_callcprim) {
    SOBJ func, r, *a;
    spop(func);
    a = env->slot;
    switch(SCM_CPRIM_NARGS(func)) {
    case 0: r=(*SCM_CPRIM_FUNC(func))(); break;
    case 1: r=(*SCM_CPRIM_FUNC(func))(a[0]); break;
    case 2: r=(*SCM_CPRIM_FUNC(func))(a[0],a[1]); break;
    case 3: r=(*SCM_CPRIM_FUNC(func))(a[0],a[1],a[2]); break;
    case 4: r=(*SCM_CPRIM_FUNC(func))(a[0],a[1],a[2],a[3]); break;
    case 5: r=(*SCM_CPRIM_FUNC(func))(a[0],a[1],a[2],a[3],a[4]); break;
    case 6: r=(*SCM_CPRIM_FUNC(func))(a[0],a[1],a[2],a[3],a[4],a[5]); break;
    case 7: r=(*SCM_CPRIM_FUNC(func))(a[0],a[1],a[2],a[3],a[4],a[5],a[6]); break;
    case 8: r=(*SCM_CPRIM_FUNC(func))(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]); break;
    case 9: r=(*SCM_CPRIM_FUNC(func))(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]); break;
    case 10:r=(*SCM_CPRIM_FUNC(func))(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],a[9]); break;
    default:r=(*SCM_CPRIM_FUNC(func))(sp - env->slot, env->slot);
    }
    sp = cont;
    cont=cont->next;
    env=env->next;
    spush(r);
    END(gen_callcprim);
  }
  INSTR(gen_callcode) {
    SCM_ERR("gen callcode not implemented", TOS);
    END(gen_callcode);
  }
  INSTR(gen_callproc) {
    SCM_ERR("gen callproc not implemented", TOS);
    END(gen_callproc);
  }
  INSTR(gen_callclos) {
    SCM_ERR("gen callclos not implemented", TOS);
    END(gen_callclos);
  }
  INSTR(gen_callmacro) {
    SCM_ERR("gen callmacro not implemented", TOS);
    END(gen_callmacro);
  }

  /* general call of whatever
   *
   * Satck layout before:
   * [ cont envframe arg1 arg2 .. argn func ]
   * 
   * Stack layout after:
   * [ result ]
   */
  INSTR(call) {
    SOBJ func = TOS;
    if (func == NULL || SCM_INUMP(func)) SCM_ERR("illegal func", func);
#ifdef VM3_DEBUG
    fprintf(stderr, "VM3:call:func->type=%d\n", func->type);
#endif
    goto *(call_table[func->type]);
    END(call);
  }
  
  /* Stack layout when callprim:
   *
   * Before
   *
   * [ envframe arg1 arg2 .. argn ]
   *   ^                           ^
   *   env                         sp
   *
   * After
   *
   * [ result ]
   *   ^
   *   sp
   */
  INSTR(callcprim) {
    SOBJ func, ret;
    func = *ip++;
    ret = (*SCM_CPRIM_FUNC(func))(sp - env->slot, env->slot);
    sp = (void*)env;
    env = env->next;
    spush(ret);
    END(callcprim);
  }
  /* stack layout before callcprim?
   * [ arg0 ... argn-1 ]
   * after:
   * [ result ]
   */
  INSTR(callcprim0) {
    SOBJ func = *ip++, ret;
    ret = (*SCM_CPRIM_FUNC(func))();
    spush(ret);
    END(callcprim0);
  }
  INSTR(callcprim1) {
    SOBJ func = *ip++, ret;
    sp -= 1;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0]);
    spush(ret);
    END(callcprim1);
  }
  INSTR(callcprim2) {
    SOBJ func = *ip++, ret;
    sp -= 2;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0],sp[1]);
    spush(ret);
    END(callcprim2);
  }
  INSTR(callcprim3) {
    SOBJ func = *ip++, ret;
    sp -= 3;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0],sp[1],sp[2]);
    spush(ret);
    END(callcprim3);
  }
  INSTR(callcprim4) {
    SOBJ func = *ip++, ret;
    sp -= 4;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0],sp[1],sp[2],sp[3]);
    spush(ret);
    END(callcprim4);
  }
  INSTR(callcprim5) {
    SOBJ func = *ip++, ret;
    sp -= 5;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0],sp[1],sp[2],sp[3],sp[4]);
    spush(ret);
    END(callcprim5);
  }
  INSTR(callcprim6) {
    SOBJ func = *ip++, ret;
    sp -= 6;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0],sp[1],sp[2],sp[3],sp[4],sp[5]);
    spush(ret);
    END(callcprim6);
  }
  INSTR(callcprim7) {
    SOBJ func = *ip++, ret;
    sp -= 7;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6]);
    spush(ret);
    END(callcprim7);
  }
  INSTR(callcprim8) {
    SOBJ func = *ip++, ret;
    sp -= 8;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6],sp[7]);
    spush(ret);
    END(callcprim8);
  }
  INSTR(callcprim9) {
    SOBJ func = *ip++, ret;
    sp -= 9;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6],sp[7],sp[8]);
    spush(ret);
    END(callcprim9);
  }
  INSTR(callcprim10) {
    SOBJ func = *ip++, ret;
    sp -= 10;
    ret = (*SCM_CPRIM_FUNC(func))(sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6],sp[7],sp[8],sp[9]);
    spush(ret);
    END(callcprim10);
  }

  /* KKK */
#include "prim3.i"

  END_VMCODE;
}

SOBJ scm_vm3_init()
{
  scm_vm3_vmd.code = SCM_VM3_INIT;
  scm_vm3(&scm_vm3_vmd);
}

SOBJ scm_vm3_asm(SOBJ code)
{
  SCM_VMD vm;

  vm.code = SCM_VM3_ASSEMBLE;  vm.arg.addr = code;
  scm_vm3(&vm);
  return(vm.ret.obj);
}

SOBJ scm_vm3_dis(SOBJ code)
{
  SCM_VMD vm;

  vm.code = SCM_VM3_DISASSEMBLE;  vm.arg.addr = code;
  scm_vm3(&vm);
  return(vm.ret.obj);
}

SOBJ scm_vm3_exec(SOBJ code)
{
  scm_vm3_vmd.code = SCM_VM3_EXECUTE;
  scm_vm3_vmd.reg.ip = &SCM_AREF(code, 0);
  scm_vm3(&scm_vm3_vmd);
  return(scm_vm3_vmd.ret.obj);
}

/*E* (make-proc VECTOR ENV NARG OPTARG?) => PROC */
/*D* Create a new procedure. The VECTOR contains the code of the body
  of the procedure, the ENV is the full compile-time environment
  chain, the NARGS is the number of arguments (an optionnal argument
  counts as one argument) and OPTARG? is #t when last argument is
  optionnal. */
SOBJ scm_make_proc(SOBJ v, SOBJ env, SOBJ nargs, SOBJ optarg)
{
  SOBJ proc;
  SCM_Code *code;
  long size;
  
  if (!SCM_ARRAYP(v))		SCM_ERR("bad vector", v);
  if (!SCM_PAIRP(env))		SCM_ERR("bad env chain", env);
  if (!SCM_INUMP(nargs)) 	SCM_ERR("bad nargs", nargs);

  size 		= SCM_ASIZE(v);
  code 		= scm_must_alloc(offsetof(SCM_Code, code[size]));
  code->envlist = NULL;
  code->nargs 	= SCM_INUM(nargs);
  code->optargs	= (optarg != scm_false);
  code->nlocals = 0;
  code->size 	= size;

  memcpy(code->code, SCM_ARRAY(v), size * sizeof(SOBJ*));

  proc = scm_newcell(SOBJ_T_PROC);

  SCM_PROC_CODE(proc) = code;
  SCM_PROC_ENV(proc) = NULL;
  return(proc);
}


void scm_init_vm3()
{
  scm_vm3_init();
  scm_add_cprim("make-procedure",	scm_make_proc,	4);
  scm_add_cprim("vm3-asm",	scm_vm3_asm,	1);
  scm_add_cprim("vm3-dis",	scm_vm3_dis,	1);
  scm_add_cprim("vm3-exec",	scm_vm3_exec,	1);
}

