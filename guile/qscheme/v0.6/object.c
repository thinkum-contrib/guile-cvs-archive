/* -*- tab-width:4; -*- */
#include "s.h"
#include "vm2.h"
#include "stack.h"
#include "object.h"

static int SOBJ_T_OBJECT;		/* object */
static int SOBJ_T_OBJDEF;		/* object definition */

static char *scm_object_type_name[] = {
  "get",
  "set",
  "call",
  "parent",
  NULL };

/****************************************************************
 * Object's Slot Definitions
 ****************************************************************/
SOBJ scm_mkobjdef(int ndefs)
{
  SOBJ new = scm_newcell(SOBJ_T_OBJDEF);
  SCM_OBJDEF_AUX(new) =
	scm_must_alloc(sizeof(SCM_ObjDefAux) + ((ndefs - 1) * sizeof(SCM_ObjDef)));

  SCM_OBJDEF_AUX(new)->ndefs = ndefs;
  return(new);
}

static void scm_objdef_mark(SOBJ x)
{
  SCM_ObjDefAux *oda;
  int i;

  if ((oda = SCM_OBJDEF_AUX(x)) != NULL) {
	for (i = 0; i < oda->ndefs; i++) {
	  scm_gc_mark(oda->def[i].name);
	}
  }
}

static void scm_objdef_sweep(SOBJ x)
{
  if (SCM_OBJDEF_AUX(x)) {
	scm_free(SCM_OBJDEF_AUX(x));
	SCM_OBJDEF_AUX(x) = NULL;
  }
}

static SOBJ_TYPE_DESCR scm_objdef_type_descr = {
  0,
  "objdef",
  scm_objdef_mark,		scm_objdef_sweep,			/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  NULL,												/* compare */
  NULL,	NULL										/* get / set */
};

/****************************************************************
 * Object type support
 ****************************************************************/
SOBJ scm_mkobject(SOBJ def, int nslots)
{
  SOBJ new = scm_newcell(SOBJ_T_OBJECT);
  
  SCM_OBJECT_DEF(new) = def;
  SCM_OBJECT_VAL(new) =
	scm_must_alloc(sizeof(SCM_ObjVal) + ((nslots - 1) * sizeof(SOBJ)));

  SCM_OBJECT_VAL(new)->nslots = nslots;
  return(new);
}

static void scm_object_mark(SOBJ x)
{
  SCM_ObjVal *ov;
  int i;

  if ((ov = SCM_OBJECT_VAL(x)) != NULL) {
	for (i = 0; i < ov->nslots; i++) 
	  scm_gc_mark(ov->slot[i]);
  }
  scm_gc_mark(SCM_OBJECT_DEF(x));
}

static void scm_object_sweep(SOBJ x)
{
  if (SCM_OBJECT_VAL(x)) 
	scm_free(SCM_OBJECT_VAL(x));
}

static SOBJ_TYPE_DESCR scm_object_type_descr = {
  0,
  "object",
  scm_object_mark,		scm_object_sweep,			/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  NULL,												/* compare */
  NULL,	NULL										/* get / set */
};


void scm_obj_dump(SOBJ x)
{
  SCM_ObjDefAux *od = SCM_OBJDEF_AUX(SCM_OBJECT_DEF(x));
  SCM_ObjVal    *ov = SCM_OBJECT_VAL(x);
  int i;

  scm_puts("Object @");  scm_putx(x);
  scm_puts(" def@"); 	 scm_putx(od);
  scm_puts(" val@"); 	 scm_putx(ov);
  scm_puts("\n");

  for (i = 0; i < od->ndefs; i++) {
	scm_puts("\t");		scm_cdisplay(od->def[i].name);
    scm_puts("\t"); 	scm_puts(scm_object_type_name[od->def[i].type]);
    scm_puts("\t"); 	scm_putn(od->def[i].index);
    scm_puts("\t"); 	scm_cdisplay(ov->slot[od->def[i].index]);
    scm_puts("\n");
  }
}


void scm_obj_inspect(SOBJ x)
{
  SCM_ObjDefAux *od = SCM_OBJDEF_AUX(SCM_OBJECT_DEF(x));
  SCM_ObjVal    *ov = SCM_OBJECT_VAL(x);
  int i, j, t;
  SOBJ n;

  scm_puts("(let ((x (object-new)))\n");

  for (i = 0; i < ov->nslots; i++) {
	t = -1;
	n = NULL;
	for (j = 0; j < od->ndefs; j++) {
	  if (od->def[j].index == i) {
		if (od->def[j].type == SCM_SLOT_PARENT ||
			od->def[j].type == SCM_SLOT_CALL) {
		  n = od->def[j].name;
		  t = od->def[j].type;
		  break;
		}
		if (od->def[j].type == SCM_SLOT_SET) {
		  t = SCM_SLOT_SET;
		  if (n != NULL) 	break;
		}
		if (od->def[j].type == SCM_SLOT_GET) {
		  if (t != -1) {		/* already seen a matching set slot */
			n = od->def[j].name;
			break;
		  }
		  t = od->def[j].type;
		  n = od->def[j].name;
		}
	  }
	}
	if (n == NULL || t == -1) {
	  fprintf(stderr, "OOPS: bad type and name for slot %d\n", i);
	}
	scm_puts("\t(object-add-");
	switch(t) {
	case SCM_SLOT_GET:
	  scm_puts("const  "); 
	  break;

	case SCM_SLOT_SET:
	  scm_puts("var    "); 
	  break;
	  
	case SCM_SLOT_CALL:
	  scm_puts("method "); 
	  break;
	  
	case SCM_SLOT_PARENT:
	  scm_puts("parent "); 
	  break;
	}
	scm_cdisplay(n);  scm_puts(" ");  scm_cdisplay(ov->slot[i]);
	scm_puts(")\n");
  }
  scm_puts(")\n");
}

/****************************************************************
 * Field maintenance utilities
 ****************************************************************/

static SCM_ObjDef *objdef_lookup(SCM_ObjDefAux *aux, SOBJ name)
{
  SCM_ObjDef *p, *l;
  p = aux->def;
  l = p + aux->ndefs;
  while(p < l) {
	if (p->name == name)  return(p);
	p++;
  }
  return(NULL);
}

#ifdef UNUSED
static SCM_ObjDef *objdef_lookup_type(SCM_ObjDefAux *aux, int type)
{
  SCM_ObjDef *p, *l;
  p = aux->def;
  l = p + aux->ndefs;
  while(p < l) {
	if (p->type == type)  return(p);
	p++;
  }
  return(NULL);
}
#endif
static SCM_ObjDefAux *objdef_append_fields(SCM_ObjDefAux *aux, int more)
{
  SCM_ObjDefAux *new;
  int i;
  int ndefs;

  ndefs = aux->ndefs + more;
  new   = scm_must_alloc(offsetof(SCM_ObjDefAux, def[ndefs]));

  memcpy(new, aux, offsetof(SCM_ObjDefAux, def[aux->ndefs]));

  new->ndefs = ndefs;
  for (i = aux->ndefs; i < new->ndefs; i++) {
	new->def[i].name = NULL;
  }
  return(new);
}

/* add a new slot to an object */
static int object_new_slot(SOBJ obj)
{
  SCM_ObjVal *ov;
  int nslots;

  if (SCM_OBJECT_VAL(obj) == NULL) {
	SCM_OBJECT_VAL(obj) = scm_must_alloc(offsetof(SCM_ObjVal, slot[1]));
	SCM_OBJECT_VAL(obj)->nslots = 1;
	return(0);
  }
  nslots = SCM_OBJECT_NSLOTS(obj);
  ov = scm_must_alloc(offsetof(SCM_ObjVal, slot[nslots+1]));
  memcpy(ov, SCM_OBJECT_VAL(obj), offsetof(SCM_ObjVal, slot[nslots]));
  ov->nslots++;
  ov->slot[nslots] = NULL;
  scm_free(SCM_OBJECT_VAL(obj));   SCM_OBJECT_VAL(obj) = ov;
  return(nslots);
}

/****************************************************************
 * Utilities
 ****************************************************************/
SOBJ scm_get_atom(SOBJ x)
{
  switch(SCM_OBJTYPE(x)) {
  case SOBJ_T_SYMBOL:	return(SCM_SYM_NAME(x));
  case SOBJ_T_STRING:	return(scm_mksymbol(SCM_STR_VALUE(x)));
  case SOBJ_T_KEYWORD:	return(SCM_KEYW_NAME(x));
  case SOBJ_T_ATOM:		return(x);
  }
  SCM_ERR("cannot convert to atom", x);
  return(scm_undefined);
}

/****************************************************************
 * Object procedures
 ****************************************************************/

/*E* (object-new) => OBJECT */
/*D* Create a new object. */
SOBJ scm_object_new()
{
  SOBJ new = scm_mkobject(scm_mkobjdef(0), 0);
  return(new);
}

/*E* (object-clone OBJ) => NEW */
/*D* Generate a fresh copy of OBJ. */
SOBJ scm_object_clone(SOBJ obj)
{
  int nslots, size;
  SOBJ new = scm_newcell(SOBJ_T_OBJECT);
  
  SCM_OBJECT_DEF(new) = SCM_OBJECT_DEF(obj);
  
  nslots = SCM_OBJECT_VAL(obj)->nslots;
  size   = offsetof(SCM_ObjVal, slot[nslots]);
  SCM_OBJECT_VAL(new) = scm_must_alloc(size);
  memcpy(SCM_OBJECT_VAL(new), SCM_OBJECT_VAL(obj), size);
  return(new);
}

/*E* (object-fields OBJ) => LIST */
/*D* Return a list of all object's slot. */
SOBJ scm_object_fields(SOBJ obj)
{
  SOBJ flist,objdef;
  int i;

  if (!SCM_OBJECTP(obj)) SCM_ERR("bad object", obj);
  if ((objdef = SCM_OBJECT_DEF(obj)) == NULL) SCM_ERR("bad objdef", objdef);

  flist = NULL;
  for (i = 0; i < SCM_OBJDEF_AUX(objdef)->ndefs; i++) {
	flist = scm_cons(SCM_OBJDEF_AUX(objdef)->def[i].name, flist);
  }
  return(flist);
}

/*E* (object-field-number OBJ) => NUMBER */
/*D* Returns the number of fields in object. */

SOBJ scm_object_field_number(SOBJ obj)
{
  if (!SCM_OBJECTP(obj)) 		SCM_ERR("bad object", obj);
  return(SCM_MKINUM(SCM_OBJDEF(obj)->ndefs));
}

/*E* (object-slots OBJ) => LIST */
/*D* Returns the list of values associated with fields. */
SOBJ scm_object_slots(SOBJ obj)
{
  SOBJ flist;
  int i;

  if (!SCM_OBJECTP(obj)) 		SCM_ERR("bad object", obj);
  if (!SCM_OBJECT_VAL(obj))		SCM_ERR("bad objval", obj);

  flist = NULL;
  for (i = 0; i < SCM_OBJECT_NSLOTS(obj); i++) {
	flist = scm_cons(SCM_OBJECT_SLOT(obj, i), flist);
  }
  return(flist);
}

/*E* (object-slot-number OBJ) => NUMBER */
/*D* Returns the number of slots. */
SOBJ scm_object_slot_number(SOBJ obj)
{
  if (!SCM_OBJECTP(obj)) 		SCM_ERR("bad object", obj);
  return(SCM_MKINUM(SCM_OBJECT_NSLOTS(obj)));
}

SOBJ scm_object_new_slot(SOBJ obj)
{
  if (!SCM_OBJECTP(obj)) SCM_ERR("bad object", obj);
  return(SCM_MKINUM(object_new_slot(obj)));
}


/* adding a var:
 * - add 2 slots (x and x!) in defs and one slot in val
 */

/*E* (object-add-var OBJ NAME VALUE) => OBJ */
/*D* Add 2 fields to object OBJ, respectively NAME and NAME! pointing
  to one fresh slot containing VALUE. The NAME message will later
  fetch the value of the slot and the NAME! message will assign a
  value to the slot.*/
SOBJ scm_object_add_var(SOBJ obj, SOBJ name, SOBJ value)
{
  SOBJ olddef, newdef;
  int  i, slotnr;
  char *str;
  SCM_ObjDefAux *d;
  
  if (!SCM_OBJECTP(obj)) SCM_ERR("bad object", obj);

  if ((olddef = SCM_OBJECT_DEF(obj)) == NULL) SCM_ERR("bad objdef for object",obj);

  /* cast to atom */
  name = scm_get_atom(name);

  /* make a new objdef with space for 2 more defs */
  newdef = scm_newcell(SOBJ_T_OBJDEF);
  d = objdef_append_fields(SCM_OBJDEF_AUX(olddef), 2);
  SCM_OBJDEF_AUX(newdef) = d;

  /* prepare atom for the set! name */
  i = strlen(SCM_ATOM_NAME(name));
  str = scm_must_alloc(i+2);
  strcpy(str, SCM_ATOM_NAME(name));
  strcpy(str + i, "!");

  slotnr = object_new_slot(obj); /* alloc new slot and get index */

  i = SCM_OBJDEF_NDEFS(olddef);	/* first of new def index */

  d->def[i].name = name;
  d->def[i].type = SCM_SLOT_GET;
  d->def[i].index= slotnr;
  i++;
  d->def[i].name = scm_mkatom(str);
  d->def[i].type = SCM_SLOT_SET;
  d->def[i].index= slotnr;

  scm_free(str);				/* release name! string */

  SCM_OBJECT_DEF(obj) = newdef;	/* adjust object def pointer */
  SCM_OBJECT_SLOT(obj,slotnr) = value;
  return(obj);
}

static SOBJ scm_object_add_slot(SOBJ obj, SOBJ name, int type, SOBJ value)
{
  SOBJ olddef, newdef;
  int i, slotnr;
  SCM_ObjDefAux *d;

  /* make a new objdef with space for 1 more defs */
  if ((olddef = SCM_OBJECT_DEF(obj)) == NULL) SCM_ERR("bad objdef for object",obj);
  newdef = scm_newcell(SOBJ_T_OBJDEF);
  d = objdef_append_fields(SCM_OBJDEF_AUX(olddef), 1);
  SCM_OBJDEF_AUX(newdef) = d;

  slotnr = object_new_slot(obj); /* alloc new slot and get index */

  i = SCM_OBJDEF_NDEFS(olddef);	/* first of new def index */

  d->def[i].name = name;
  d->def[i].type = type;
  d->def[i].index= slotnr;

  SCM_OBJECT_DEF(obj) = newdef;	/* adjust object def pointer */
  SCM_OBJECT_SLOT(obj,slotnr) = value;
  return(obj);
}

/*E* (object-add-const OBJ NAME VALUE) => OBJ */
/*D* Add a new field NAME to the object OBJ and a new slot containing
  VALUE. Later, message NAME could be use to fetch the VALUE */
SOBJ scm_object_add_const(SOBJ obj, SOBJ name, SOBJ value)
{
  if (!SCM_OBJECTP(obj)) SCM_ERR("bad object", obj);
  return(scm_object_add_slot(obj, scm_get_atom(name), SCM_SLOT_GET, value));
}

/*E* (object-add-method OBJ NAME PROC) => OBJ */
/*D* Add a new field NAME to the object OBJ and bind it to a fresh
  slot refering the PROC. */
SOBJ scm_object_add_method(SOBJ obj, SOBJ name, SOBJ proc)
{
  if (!SCM_OBJECTP(obj)) 	SCM_ERR("bad object", obj);
  /*  if (!SCM_CLOSUREP(proc))	SCM_ERR("bad procedure", proc); */
  return(scm_object_add_slot(obj, scm_get_atom(name), SCM_SLOT_CALL, proc));
}

/*E* (object-add-parent OBJ NAME PARENT) => OBJ */
/*D* Add a new field NAME to the object OBJ and bind it to a fresh
  slot refering the parent PARENT. */
SOBJ scm_object_add_parent(SOBJ obj, SOBJ name, SOBJ parent)
{
  if (!SCM_OBJECTP(obj)) 	SCM_ERR("bad object", obj);
  if (!SCM_OBJECTP(parent))	SCM_ERR("bad parent", parent);
  return(scm_object_add_slot(obj, scm_get_atom(name), SCM_SLOT_PARENT,parent));
}

/*E* (object-dump OBJ) => #t */
/*D* Dump content of object OBJ. */
SOBJ scm_object_dump(SOBJ obj)
{
  if (!SCM_OBJECTP(obj)) SCM_ERR("bad object", obj);
  scm_obj_dump(obj);
  return(scm_true);
}

/*E* (object-inspect OBJ) => #t */
/*D* Inspect content of object. */
SOBJ scm_object_inspect(SOBJ obj)
{
  if (!SCM_OBJECTP(obj)) SCM_ERR("bad object", obj);
  scm_obj_inspect(obj);
  return(scm_true);
}

/* search a slot in the sender and the parent chain. Returns the slot
 * definition and the object in which the slot was found
 */

static SCM_ObjDef *scm_search_msg(SOBJ sender, SOBJ msg, SOBJ *obj)
{
  SCM_ObjDefAux *aux;
  SCM_ObjDef *def, *limit, *p, *d;
  SOBJ parent;

  aux = SCM_OBJDEF(sender);
  def = aux->def;
  limit = def + aux->ndefs;
  
  /* search in sender msg */
  for (p = def; p < limit; p++) {
	if (p->name == msg) {  *obj = sender;  return(p); }
  }

  /* the sender has no matching slot: try to delegate to its parents */
  
  /* search for parent slot */
  for (p = def; p < limit; p++) {
	if (p->type == SCM_SLOT_PARENT) {
	  parent = SCM_OBJECT_SLOT(sender, p->index);
	  if (SCM_OBJECTP(parent)) {
		/* parent is an object, try to search in its method */
		if ((d = objdef_lookup(SCM_OBJDEF(parent), msg)) != NULL) {
		  *obj = parent;
		  return(d);
		}
	  } else {
		SCM_ERR("bad parent", parent);
	  }
	}
  }

  /* no parent or msg not found in parent slots: try to send to parent
     recursively: note that we do not check parent type, because bad
     parents must have genereate errors in the loop below */

  for (p = def; p < limit; p++) {
	if (p->type == SCM_SLOT_PARENT) {
	  parent = SCM_OBJECT_SLOT(sender, p->index);
	  if (SCM_OBJECTP(parent)) {
		if ((d = scm_search_msg(parent, msg, obj)) != NULL) {
		  return(d);
		}
	  }
	}
  }
  return(NULL);
}

SOBJ scm_internal_object_send2(SOBJ sender, SOBJ msg, int narg, SOBJ *arg)
{
  SOBJ obj;
  SCM_ObjDef *def;
  SCM_VMD vm;

  if ((def = scm_search_msg(sender, msg, &obj)) == NULL) {
	scm_puts("oops: object '"); scm_cdisplay(sender);
	scm_puts("' cannot answer ot msg '"); scm_cdisplay(msg);
	scm_puts("'\n");
	return(NULL);
  }
  switch(def->type) {
  case SCM_SLOT_GET:
	return(SCM_OBJECT_SLOT(obj, def->index));
	
  case SCM_SLOT_SET:
	if (narg < 1) SCM_ERR("bad number of arg for", msg);
	return(SCM_OBJECT_SLOT(obj, def->index) = *arg);

  case SCM_SLOT_CALL:
	{
	  SOBJ code[4], *old_sp, retval;
	  SCM_ContFrame old_cf, *cf;

	  /* WARNING: DIRTY TRICK AHEAD:
	   *
	   * stack at the begin of the object-send function:
	   *     mark argn-1 ... arg0 msg obj
	   *
	   * Stack we need:
	   *	   mark argn-1 ... arg0 self func
	   *
	   * It's very important to save the contframe, because when the
	   * vm terminates, it will overwrite the frame with the result.
	   */

	  *(--arg) = sender; 
	  *(--arg) = SCM_OBJECT_SLOT(obj, def->index);
	  narg += 2;

	  /* build the code to call the method */
	  code[0] = SCM_OPCODE(SCM_OP_CALL);
	  code[1] = SCM_OPCODE(SCM_OP_END);
		
	  cf = (SCM_ContFrame *)(arg + narg);
	  old_sp = scm_sp;
	  old_cf = *cf;
	  vm.code = SCM_VM_DO_EXECUTE;
	  vm.reg.cont = (SCM_ContFrame *)(arg + narg);
	  vm.reg.env  = NULL;
	  vm.reg.ip   = code;
	  vm.reg.sp   = arg;
	  scm_vm(&vm);
	  retval = vm.ret.obj;
	  scm_sp = old_sp;
	  *cf    = old_cf;
	  return(retval);
	}
  case SCM_SLOT_PARENT:
	printf("oops: don't know how to send to parent\n");
	return(NULL);
  }
  return(NULL);
}


static SOBJ scm_internal_object_send(SOBJ sender, SOBJ msg, SOBJ obj,
							 int narg, SOBJ *arg)
{
  SCM_VMD vm;
  SCM_ObjDef *def;

  def  = objdef_lookup(SCM_OBJDEF(obj), msg);

  scm_puts("object-send: obj=");  	scm_cdisplay(obj);
  scm_puts(" msg=");				scm_cdisplay(msg);
  scm_puts(" def=");				scm_putx(def);
  scm_puts("\n");
  
  if (def != NULL) {

	switch(def->type) {
	case SCM_SLOT_GET:
	  return(SCM_OBJECT_SLOT(obj, def->index));

	case SCM_SLOT_SET:
	  if (narg < 1) SCM_ERR("bad number of arg for", msg);
	  return(SCM_OBJECT_SLOT(obj, def->index) = *arg);

	case SCM_SLOT_CALL:
	  {
		SOBJ code[4], *old_sp, retval;
		SCM_ContFrame old_cf, *cf;

		/* WARNING: DIRTY TRICK AHEAD:
		 *
		 * stack at the begin of the object-send function:
		 *     mark argn-1 ... arg0 msg obj
		 *
		 * Stack we need:
		 *	   mark argn-1 ... arg0 self func
		 *
		 * It's very important to save the contframe, because when the
		 * vm terminates, it will overwrite the frame with the result.
		 */

		*(--arg) = sender; 
		*(--arg) = SCM_OBJECT_SLOT(obj, def->index);
		narg += 2;

		/* build the code to call the method */
		code[0] = SCM_OPCODE(SCM_OP_CALL);
		code[1] = SCM_OPCODE(SCM_OP_END);
		
		cf = (SCM_ContFrame *)(arg + narg);
		old_sp = scm_sp;
		old_cf = *cf;
		vm.code = SCM_VM_DO_EXECUTE;
		vm.reg.cont = (SCM_ContFrame *)(arg + narg);
		vm.reg.env  = NULL;
		vm.reg.ip = code;
		vm.reg.sp = arg;
		scm_vm(&vm);
		retval = vm.ret.obj;
		scm_sp = old_sp;
		*cf    = old_cf;
		return(retval);
	  }
	default:
	  SCM_ERR("bad slot type", SCM_MKINUM(def->type));
	}
  } else {
	scm_puts("OOPS: Slot not found, trying to delegate...\n");
	def = objdef_lookup(SCM_OBJDEF(obj), scm_mkatom("*parent*"));
	if (def != NULL) {			/* found parent slot */
	  SOBJ parent = SCM_OBJECT_SLOT(obj, def->index);
	  scm_puts("yep: got parent to delegate\n");
	  
	  if (SCM_OBJECTP(parent)) {
		return(scm_internal_object_send(sender, msg, parent, narg, arg));
	  } else {
		SCM_ERR("bad parent type", parent);
	  }
	}
	SCM_ERR("cannot send", msg);
  }
  return(scm_true);
}



/*E* (object-send OBJ MSG ARG...) => RESULT */
/*D* Send message MSG to object OBJ with argument list ARG. */
SOBJ scm_object_send(int narg, SOBJ *arg)
{
  if (narg < 2) SCM_ERR("bad number of args", NULL);
  return(scm_internal_object_send(arg[0], arg[1], arg[0], narg-2, arg+2));
}

/*E* (object-send2 OBJ MSG ARG...) => RESULT */
/*D* Send message MSG to object OBJ with argument list ARG. This
  version does not delegate to PARENT object. */
SOBJ scm_object_send2(int narg, SOBJ *arg)
{
  if (narg < 2) SCM_ERR("bad number of args", NULL);
  return(scm_internal_object_send2(arg[0], arg[1], narg-2, arg+2));
}


void scm_init_object()
{
  SOBJ_T_OBJECT = scm_add_type(&scm_object_type_descr);
  SOBJ_T_OBJDEF = scm_add_type(&scm_objdef_type_descr);

  scm_add_cprim("object-new",			scm_object_new,				0);
  scm_add_cprim("object-clone",			scm_object_clone,			1);
  scm_add_cprim("object-fields",		scm_object_fields,			1);
  scm_add_cprim("object-slots",			scm_object_slots,			1);
  scm_add_cprim("object-slot-number",	scm_object_slot_number,		1);
  scm_add_cprim("object-field-number",	scm_object_field_number,	1);

  scm_add_cprim("object-add-var",		scm_object_add_var,			3);
  scm_add_cprim("object-add-const",		scm_object_add_const,		3);
  scm_add_cprim("object-add-method",	scm_object_add_method,		3);
  scm_add_cprim("object-add-parent",	scm_object_add_parent,		3);

  scm_add_cprim("object-dump",			scm_object_dump,			1);
  scm_add_cprim("object-inspect",		scm_object_inspect,			1);

  scm_add_cprim("object-send",			scm_object_send,			-1);
  scm_add_cprim("object-send2",			scm_object_send2,			-1);
}
