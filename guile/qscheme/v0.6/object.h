/* Objects have to fields:
 * - defcriptor
 * - values
 */
/****************************************************************
 * 
 * an object is represented in scheme with 2 pointers:
 *
 * - a pointer to an object of type OBJDEF
 * - a pointer to a SCM_Objval structure
 *
 * The OBJDEF pointer is stored in the CAR and the SCM_objval pointer
 * in the CDR.
 *
 * The Objdef is a pointer to a scheme object and not a direct pointer
 * to a structure because objects can share objedefs and so we do not
 * have to care about reference counting or such stuff, because the gc
 * will do it for us.
 *
 ****************************************************************/

#define SCM_ObjDefAux	struct _SCM_ObjDefAux
#define SCM_ObjDef		struct _SCM_ObjDef
#define SCM_ObjVal		struct _SCM_ObjVal

SCM_ObjDef {
  SOBJ 		name;
  int		type;				/* type of slot */
  int 		index;
};

/****************************************************************
 * The func contains either (depending on flag contents):
 * - a pointer to a c func
 * - a pointer to a scheme func
 ****************************************************************/

SCM_ObjDefAux {
  int			ndefs;
  SCM_ObjDef	def[1];
};

SCM_ObjVal {
  int nslots;
  SOBJ slot[1];
};
static int SOBJ_T_OBJECT;		/* object */
static int SOBJ_T_OBJDEF;		/* object definition */

/*** Accessing fields of an object */

#define SCM_OBJECT_VAL(x)		((SCM_ObjVal*)(SCM_CAR(x)))
#define SCM_OBJECT_DEF(x)		SCM_CDR(x)

/*** Accessing objdef fields */

#define SCM_OBJDEF_AUX(x)		((SCM_ObjDefAux*)(SCM_AUX(x)))
#define SCM_OBJDEF_NDEFS(x)		SCM_OBJDEF_AUX(x)->ndefs
#define SCM_OBJDEF_DEF(x,i)		(SCM_OBJDEF_AUX(x)->def + i)

/*** Accessing object values */
#define SCM_OBJECT_SLOT(obj,i)	(SCM_OBJECT_VAL(obj)->slot[i])
#define SCM_OBJECT_NSLOTS(obj)	(SCM_OBJECT_VAL(obj)->nslots)

/*** Accessing def from object */
#define SCM_OBJDEF(x) \
({ SOBJ od; \
 if ((od = SCM_OBJECT_DEF(x)) == NULL) SCM_ERR("bad objdef",x); \
 SCM_OBJDEF_AUX(od); \
})

/****************************************************************
 * Note: maintining a cell for the objdef make sense because the
 * objdef part of an object may be shared between objects. Having
 * objdef permits to let the gc collect them automatically.
 ****************************************************************/

#define SCM_OBJECTP(x)			(SCM_OBJTYPE(x) == SOBJ_T_OBJECT)

enum SCM_OBJDEF_TYPE {
  SCM_SLOT_GET,
  SCM_SLOT_SET,
  SCM_SLOT_CALL,
  SCM_SLOT_PARENT,
  SCM_SLOT_MAX
};
  
static char *scm_object_type_name[] ;	/* keep enum in sync with this */
