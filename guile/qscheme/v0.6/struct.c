/* -*- tab-width:4; -*- */
/*
 * Structure implementation
 */
#include "s.h"
#include "struct.h"

int SOBJ_T_DBLOCK;				/* data block */
int SOBJ_T_STRUCT_DEF;			/* struct definition */
int SOBJ_T_STRUCT_INST;			/* struct instance */

/****************************************************************
 * STRUCT_DEF part
 ****************************************************************/

/*** Create a new dblock. If pointer is NULL, a new block is
 * allocated. If ptr is given, block is assumed to be statically
 * allocated. */
SOBJ scm_dblock_new(void *ptr, int size)
{
  SOBJ new = scm_newcell(SOBJ_T_DBLOCK);

  if (ptr == NULL) {
	ptr = scm_must_alloc(size);
	size |= SCM_DBLOCK_ALLOCATED;
  }
  SCM_DBLOCK_ADDR(new) = ptr;
  SCM_DBLOCK_SIZE(new) = size;
  return(new);
}

static void scm_dblock_sweep(SOBJ x)
{
  if (SCM_DBLOCK_SIZE(x) & SCM_DBLOCK_ALLOCATED) {
	scm_free(SCM_DBLOCK_ADDR(x));
	SCM_DBLOCK_ADDR(x) = NULL;
	SCM_DBLOCK_SIZE(x) = 0;
  }
}

static void scm_dblock_print(SOBJ x, PORT *p)
{
  port_puts(p, "#<dblock>");
}

static void scm_dblock_write(SOBJ x, PORT *p)
{
  port_puts(p, "#<dblock addr=");
  port_putx(p, SCM_DBLOCK_ADDR(x));
  port_puts(p, " size=");
  port_putn(p, SCM_DBLOCK_SIZE(x) & SCM_DBLOCK_SIZE_MASK);
  port_puts(p, " allocated=");
  port_puts(p,  (SCM_DBLOCK_SIZE(x) & SCM_DBLOCK_ALLOCATED) != 0 ?"yes":"no");
  port_puts(p, ">");
}

SOBJ_TYPE_DESCR scm_dblock_type = {
  0,
  "data-block",
  NULL,					scm_dblock_sweep,	/* mark / sweep */
  scm_dblock_print,		scm_dblock_write,	/* print / write */
  NULL,	NULL,			NULL,	NULL,		/* cparse / wparse */
  NULL,									   	/* compare */
  NULL,					NULL,				/* convert dblock */
};

/*E* (make-dblock SIZE) => DATABLOCK */
/*D* Create a new data block from size */
SOBJ scm_make_dblock(SOBJ size)
{
  if (!SCM_INUMP(size))	SCM_ERR("make-dblock: bad size", size);

  return(scm_dblock_new(NULL, SCM_INUM(size)));
}

/*E* (make-dblock-from-pointer PTR SIZE) => DBLOCK */
/*D* Create a data block from the area pointed by PTR. */
SOBJ scm_make_dblock_from_pointer(SOBJ ptr, SOBJ size)
{
  if (!SCM_POINTERP(ptr))	SCM_ERR("bad pointer", 	ptr);
  if (!SCM_INUMP(size))		SCM_ERR("bad size",		size);
  
  return(scm_dblock_new(SCM_POINTER(ptr), SCM_INUM(size)));
}

/*E* (dblock-copy DBLOCK) => NEW */
/*D* Make a new data block and copy contents of DBLOCK into it */
SOBJ scm_dblock_copy(SOBJ x)
{
  SOBJ new;
  int  size;
  
  if (!SCM_DBLOCKP(x))	SCM_ERR("bad data block", x);
  size = SCM_DBLOCK_SIZE(x) & SCM_DBLOCK_SIZE_MASK;
  new = scm_dblock_new(NULL, size);
  memcpy(SCM_DBLOCK_ADDR(new), SCM_DBLOCK_ADDR(x), size);
  return(new);
}

/*E* (make-pointer-to-object OBJ) => PTR */
/*D* Make a pointer to the object's cell */
SOBJ scm_pointer_to_object(SOBJ x)
{
  SOBJ new;
  if (SCM_INUMP(x))		SCM_ERR("cannot convert to pointer", x);
  
  new = scm_mkpointer(x);
  SCM_POINTER_ATTRIB(new) = SCM_POINTER_FLAG_CELL;
  return(new);
}

/*E* (make-pointer-to-aux OBJ) => PTR */
/*D* Make a pointer from the aux field of the object */
SOBJ scm_pointer_to_aux(SOBJ x)
{
  SOBJ new;
  if (SCM_INUMP(x))		SCM_ERR("cannot convert to pointer", x);
  
  new = scm_mkpointer(SCM_AUX(x));
  SCM_POINTER_ATTRIB(new) = SCM_POINTER_FLAG_CELL;
  return(new);
}



/****************************************************************
 * STRUCT_DEF part
 ****************************************************************/
/*** Alloc a struct def aux large enough to hold nfields */
SCM_StructDefAux *scm_struct_def_alloc(int nfields)
{
  return(scm_must_alloc(offsetof(SCM_StructDefAux, field[nfields])));
}
  
/*** Make a new struct_def object */
SOBJ scm_struct_def_new(int nfields)
{
  SOBJ new = scm_newcell(SOBJ_T_STRUCT_DEF);
  SCM_STRUCT_DEF(new) = scm_struct_def_alloc(nfields);
  return(new);
}

static void scm_struct_def_mark(SOBJ x)
{
  int i;
  SCM_StructDefAux *def;

  def = SCM_STRUCT_DEF(x);
  for (i = 0; i < def->nfields; i++) {
	if (def->field[i].name) scm_gc_mark(def->field[i].name);
	if (def->field[i].var)  scm_gc_mark(def->field[i].var);
  }
}

static void scm_struct_def_sweep(SOBJ x)
{
  if (SCM_STRUCT_DEF(x)) {
	scm_free(SCM_STRUCT_DEF(x));
	SCM_STRUCT_DEF(x) = NULL;
  }
}

static void scm_struct_def_print(SOBJ obj, PORT *p)
{
  port_puts(p, "#<struct-def>");
}


static SOBJ_TYPE_DESCR struct_def_type_descr = {
  0,
  "struct-def",
  scm_struct_def_mark,	scm_struct_def_sweep, 			/* mark / sweep */
  scm_struct_def_print,	NULL,							/* print / write */
  NULL, NULL,			NULL, NULL,						/* parse */
  NULL,													/* compare */
  NULL, 				NULL,							/* get / set */
};

/*E* (make-struct-def NAME [FNAME|'(FSPEC)]...) => SDEF */
/*D* Create a new struct-def object with fields specified. FNAME is a
  field name indicating the presence of a Scheme object. FSPEC is used
  to specify foreign data. FSPEC has the form: '(NAME [TYPE [REPT [OFS]]])
  where NAME is the name of the field and TYPE is a foreign TYPE and
  OFS is the offset from start of structure. If no TYPE is given,
  object is assumed to be a Scheme object.*/

SOBJ scm_make_struct_def(int nargs, SOBJ *arg)
{
  SOBJ sname, new, field;
  int nf;
  SOBJ fname, ftype;
  int frept, fofs, fsize, falign;
  SCM_StructDefAux *def;

  if (nargs < 2)  SCM_ERR("make-struct: bad arg count", SCM_MKINUM(nargs));
  
  sname = *arg++; nargs--;

  new = scm_struct_def_new(nargs);
  def = SCM_STRUCT_DEF(new);
  
  def->name = sname;
  def->nfields = 0;

  fofs = 0;
  while (--nargs >= 0) {
	field = *arg++;
	ftype = NULL;				/* default to native */
	frept = 1;					/* default repetition */
	fsize = sizeof(SOBJ);		/* default size */
	falign= SCM_ALIGNOF(SOBJ);	/* default alignement */

	if (!SCM_PAIRP(field)) {	/* only name specified (scheme object) */
	  if (!SCM_ATOMP(field)) 	SCM_ERR("bad atom", field);
	  fname = field;
	} else {					/* complex format */
	  
	  /* at least one argument */
	  if ((nf = scm_list_length(field)) < 1)
		SCM_ERR("bad field spec list", field);
	
	  fname = SCM_CAR(field);
	  field = SCM_CDR(field);
	
	  if (field) {				/* type specified */
		ftype = scm_mkvar(SCM_CAR(field), NULL);
		fsize = SCM_VAR_AUX(ftype)->size;
		falign= SCM_VAR_AUX(ftype)->align;
		field = SCM_CDR(field);
	  }
	  
	  if (field) {				/* field rept specified */
		if (!SCM_INUMP(SCM_CAR(field)))
		  SCM_ERR("make-struct: bad rept for field", arg[-1]);
		frept = SCM_INUM(SCM_CAR(field));
		field = SCM_CDR(field);
	  }
	  
	  if (field) {				/* offset specified */
		if (!SCM_INUMP(SCM_CAR(field)))
		  SCM_ERR("make-struct: bad offset for field", arg[-1]);
		fofs  = SCM_INUM(SCM_CAR(field));
		field = SCM_CDR(field);
	  }
	}

	fofs = SCM_ALIGN_OFS(fofs, falign);
	def->field[def->nfields].name = fname;
	def->field[def->nfields].var  = ftype;
	def->field[def->nfields].rept = frept;
	def->field[def->nfields].offset = fofs;
	def->nfields++;
	fofs += fsize * frept;
  }
  def->size = fofs;
  return(new);
}

static SCM_StructDefField *scm_struct_def_search(SOBJ def, SOBJ name)
{
  SCM_StructDefAux *da;
  SCM_StructDefField *pf, *pl;

  da = SCM_STRUCT_DEF(def);
  pf = da->field;
  pl = pf + da->nfields;
  while(pf < pl) {
	if (pf->name == name)	return(pf);
	pf++;
  }
  return(NULL);
}

/*E* (struct-def-dump SDEF) => #undefined */
/*D* Dump the content of a struct-def. Mainly for debugging. */
SOBJ scm_struct_def_dump(SOBJ x)
{
  char buf[128];
  SCM_StructDefAux *def;
  int i;

  if (!SCM_STRUCT_DEFP(x))	SCM_ERR("bad struct-def", x);

  def = SCM_STRUCT_DEF(x);
  scm_puts("struct-def "); 	scm_cdisplay(def->name);
  scm_puts(" at "); 		scm_putx(x);
  scm_puts(" aux=");		scm_putx(def);				scm_puts("\n");

  scm_puts("  size=");   	scm_putn(def->size);
  scm_puts("  nfields=");   scm_putn(def->nfields);  	scm_puts("\n");

  scm_puts("fields:\n");

  for (i = 0; i < def->nfields; i++) {
	sprintf(buf, "  field %2d: name=", i);
	scm_puts(buf);			scm_cdisplay(def->field[i].name);
	scm_puts("  var=");		scm_cdisplay(def->field[i].var);
	scm_puts("  rept=");	scm_putn(def->field[i].rept);
	scm_puts("  offset=");	scm_putn(def->field[i].offset);
	scm_puts("\n");
  }
  return(scm_undefined);
}

/*E* (struct-def-size STRUCT_DEF) => SIZE */
/*D* Returns the size in byte of a structure */
SOBJ scm_struct_def_size(SOBJ x)
{
  if (!SCM_STRUCT_DEFP(x))	SCM_ERR("bad struct-def", x);
  return(SCM_MKINUM(SCM_STRUCT_DEF(x)->size));
}

/****************************************************************
 * STRUCT_INST part
 ****************************************************************/
SOBJ scm_mkstruct_inst(SOBJ def, SOBJ data)
{
  SOBJ new = scm_newcell(SOBJ_T_STRUCT_INST);
  SCM_STRUCT_INST_DEF(new) = def;
  SCM_STRUCT_INST_DATA(new) = data;
  return(new);
}

static void scm_struct_inst_mark(SOBJ x)
{
  SCM_StructDefAux *def;
  void *data;
  SCM_StructDefField *f;
  int i;

  scm_gc_mark(SCM_STRUCT_INST_DEF(x));
  if (SCM_STRUCT_INST_DATA(x) != NULL) {
	scm_gc_mark(SCM_STRUCT_INST_DATA(x));

	/* search the data block for native scheme objects and mark them */
	if (SCM_STRUCT_INST_DEF(x) &&
		(def = SCM_STRUCT_DEF(SCM_STRUCT_INST_DEF(x))) != NULL) {
	  data = SCM_DBLOCK_ADDR(SCM_STRUCT_INST_DATA(x));
	  f = def->field;
	  for (i = 0; i < def->nfields; i++) {
		if (f[i].var == NULL) {
		  SOBJ obj = *((SOBJ*)(data+f[i].offset));
		  if (!SCM_INUMP(obj) && obj != NULL)
			scm_gc_mark(obj);
		}
	  }
	}
  }
}

static void scm_struct_inst_print(SOBJ x, PORT *p)
{
  port_puts(p, "#<struct-inst ");
  if (SCM_STRUCT_INST_DEF(x) != NULL) {
	port_puts(p,SCM_ATOM_NAME(SCM_STRUCT_DEF(SCM_STRUCT_INST_DEF(x))->name));


  } else {
	port_puts(p,"nil");
  }
  port_puts(p, ">");
}


static SOBJ_TYPE_DESCR struct_inst_type_descr = {
  0,
  "struct-inst",
  scm_struct_inst_mark,	  NULL,			/* mark / sweep */
  scm_struct_inst_print,  scm_struct_inst_print,			/* print / write */
  NULL, NULL,		NULL, NULL,			/* parse */
  NULL,									/* compare */
  NULL, 			NULL,				/* get / set */
};

/*E* (make-struct-inst STRUCT [DATA]) => INST */
/*D* Create a new instance of a structure. If no DATA is specified, a
 * new DBLOCK is allocated. */
SOBJ scm_make_struct_inst(int nargs, SOBJ *arg)
{
  SOBJ def, data;
  
  if (nargs < 1 || nargs > 2) SCM_ERR("bad number of args", NULL);
  
  def = arg[0];
  if (!SCM_STRUCT_DEFP(def))		SCM_ERR("bad structure", def);
  
  data = (nargs == 2) ?
	arg[1] :
	scm_dblock_new(NULL, SCM_STRUCT_DEF(def)->size);

  return(scm_mkstruct_inst(def, data));
}

/*** struct-get helper functions */
static SOBJ struct_get(SOBJ data, SCM_StructDefField *field)
{
  void *ptr = SCM_DBLOCK_ADDR(data) + field->offset;
  return(  (field->var) ? scm_var_get(field->var, ptr) : *((SOBJ*)(ptr)) );
}

static SOBJ struct_at_get(SOBJ s, int n)
{
  return( struct_get(SCM_STRUCT_INST_DATA(s),
					 SCM_STRUCT_DEF(SCM_STRUCT_INST_DEF(s))->field + n));
}

/*E* (struct-at-get  STRUCT N) => VALUE */
/*D* Get value of Nth field of structure */
SOBJ scm_struct_at_get(SOBJ s, SOBJ n)
{
  if (!SCM_STRUCT_INSTP(s)) 	SCM_ERR("bad structure instance", s);
  if (!SCM_INUMP(n)) 			SCM_ERR("bad field number", n);

  return(struct_at_get(s, SCM_INUM(n)));
}

/*E* (struct-get STRUCT FIELD) => VALUE */
/*D* Get value of a field */
SOBJ scm_struct_get(SOBJ s, SOBJ f)
{
  SCM_StructDefField *field;
  
  if (!SCM_STRUCT_INSTP(s))	SCM_ERR("bad structure instance", s);
  if (!SCM_ATOMP(f))		SCM_ERR("bad atom", f);

  field = scm_struct_def_search(SCM_STRUCT_INST_DEF(s), f);
  if (field == NULL) 		SCM_ERR("unknow field", f);

  return(struct_get(SCM_STRUCT_INST_DATA(s), field));
}

static void struct_set(SOBJ data, SCM_StructDefField *field, SOBJ value)
{
  void *ptr = SCM_DBLOCK_ADDR(data) + field->offset;
  if (field->var == NULL)		/* native object */
	*((SOBJ*)(ptr)) = value;
  else							/* foreign object */
	scm_var_set(field->var, ptr, value);
}

static void struct_at_set(SOBJ s, int n, SOBJ value)
{
  struct_set(SCM_STRUCT_INST_DATA(s),
			 SCM_STRUCT_DEF(SCM_STRUCT_INST_DEF(s))->field + n,
			 value);
}

/*E* (struct-at-set! INST N VALUE) => #undefined */
/*D* Stores VALUE in the Nth field of the structure instance INST. */

SOBJ scm_struct_at_set(SOBJ s, SOBJ n, SOBJ value)
{
  if (!SCM_STRUCT_INSTP(s)) 	SCM_ERR("bad structure instance", s);
  if (!SCM_INUMP(n)) 			SCM_ERR("bad field number", n);

  struct_at_set(s, SCM_INUM(n), value);
  return(scm_undefined);
}

/*E* (struct-set! STRUCT FIELD VALUE) => #undefined */
/*D* Set value of a field */
SOBJ scm_struct_set(SOBJ s, SOBJ f, SOBJ value)
{
  SCM_StructDefField *field;
  
  if (!SCM_STRUCT_INSTP(s))	SCM_ERR("bad structure instance", s);
  if (!SCM_ATOMP(f))		SCM_ERR("bad atom", f);

  field = scm_struct_def_search(SCM_STRUCT_INST_DEF(s), f);
  if (field == NULL) 		SCM_ERR("unknow field", f);

  struct_set(SCM_STRUCT_INST_DATA(s), field, value);
  return(scm_undefined);
}

/*E* (struct-inst-dump INST) => #undefined */
/*D* Dumps the content of structure INST */
SOBJ scm_struct_inst_dump(SOBJ x)
{
  scm_puts("struct inst: def=");
  scm_putx(SCM_STRUCT_INST_DEF(x));
  scm_puts(" ");
  scm_cwrite(SCM_STRUCT_INST_DEF(x));
  scm_puts(" data=");
  scm_putx(SCM_STRUCT_INST_DATA(x));
  scm_puts(" ");
  scm_cwrite(SCM_STRUCT_INST_DATA(x));
  scm_puts("\n");
  return(scm_undefined);
}

/*E* (struct-inst->list INST) => ALIST */
/*D* Build ALIST from the content of the structure INST */
SOBJ scm_struct_inst_to_list(SOBJ x)
{
  SOBJ def;
  SOBJ lst, *l;
  SCM_StructDefField *f, *limit;

  if (!SCM_STRUCT_INSTP(x))	SCM_ERR("bad structure instance", x);

  def = SCM_STRUCT_INST_DEF(x);
  f = SCM_STRUCT_DEF(def)->field;
  limit = f + SCM_STRUCT_DEF(def)->nfields;
  lst = NULL;
  l = &lst;
  while(f < limit) {
	*l = scm_cons(scm_cons(f->name, struct_get(SCM_STRUCT_INST_DATA(x), f)),
				  NULL);
	l = &SCM_CDR(*l);
	f++;
  }
  return(lst);
}

void scm_init_struct()
{
  SOBJ_T_DBLOCK		 = scm_add_type(&scm_dblock_type);
  SOBJ_T_STRUCT_DEF  = scm_add_type(&struct_def_type_descr);
  SOBJ_T_STRUCT_INST = scm_add_type(&struct_inst_type_descr);

  /* DATA BLOCK */
  scm_add_cprim("make-dblock",		scm_make_dblock,		1);
  scm_add_cprim("make-dblock-from-pointer",
				scm_make_dblock_from_pointer,				2);
  scm_add_cprim("dblock-copy",		scm_dblock_copy,		1);

  /*** Structure definition */
  scm_add_cprim("make-struct-def", 	scm_make_struct_def,	-1);
  scm_add_cprim("struct-def-dump",	scm_struct_def_dump,	1);
  scm_add_cprim("struct-def-size",	scm_struct_def_size,	1);

  /*** Structure instance */
  scm_add_cprim("make-struct-inst",	scm_make_struct_inst,	-1);

  scm_add_cprim("struct-get",		scm_struct_get,			2);
  scm_add_cprim("struct-at-get",	scm_struct_at_get,		2);
  scm_add_cprim("struct-set!",		scm_struct_set,			3);
  scm_add_cprim("struct-at-set!",	scm_struct_at_set,		3);
  scm_add_cprim("struct-inst-dump",	scm_struct_inst_dump,	1);
  scm_add_cprim("struct->list",		scm_struct_inst_to_list,	1);

  /*** MISC ***/
  scm_add_cprim("make-pointer-to-object", 	scm_pointer_to_object, 	1);
  scm_add_cprim("make-pointer-to-aux", 		scm_pointer_to_aux, 	1);
}
