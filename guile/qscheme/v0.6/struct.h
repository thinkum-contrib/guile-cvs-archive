/* -*- tab-width:4; -*- */

/* To define structure we need 2 types of scheme objects:
 * - a data block structure
 * - a structure definition
 * - a structure instance
 */
extern int SOBJ_T_DBLOCK;

#define SCM_DBLOCKP(x)			(SCM_OBJTYPE(x) == SOBJ_T_DBLOCK)
#define SCM_DBLOCK_ADDR(x)		((void*)SCM_CAR(x))
#define SCM_DBLOCK_SIZE(x)		((long)(SCM_CDR(x)))

#define SCM_DBLOCK_ALLOCATED	(1L << ((sizeof(long)*8)-1))
#define SCM_DBLOCK_SIZE_MASK	~(SCM_DBLOCK_ALLOCATED)

extern int SOBJ_T_STRUCT_DEF;			/* struct definition */

#define SCM_StructDefField	struct _SCM_StructDefField
SCM_StructDefField {
  SOBJ 	name;					/* name of field (atom) */
  SOBJ	var;					/* template variable */
  int 	rept;					/* number of repetitions */
  int 	offset;					/* offset from start of structure */
};

#define SCM_StructDefAux		struct _SCM_StructDefAux
SCM_StructDefAux {
  SOBJ 	name;					/* struct name for debugging and printing */
  int 	size;					/* normal structure size */
  int 	nfields;				/* number of fields */
  SCM_StructDefField field[1];	/* fields */
};

#define SCM_STRUCT_DEFP(x)		(SCM_OBJTYPE(x) == SOBJ_T_STRUCT_DEF)
#define SCM_STRUCT_DEF(x)		((SCM_StructDefAux *)SCM_AUX(x))

/*** Instance of a struct:
 *	- car points to struct def object,
 *	- cdr points to a DBLOCK object
 */
extern int SOBJ_T_STRUCT_INST;			/* struct instance */

#define SCM_STRUCT_INSTP(x)		(SCM_OBJTYPE(x) == SOBJ_T_STRUCT_INST)
#define SCM_STRUCT_INST_DEF(x)	SCM_CAR(x)
#define SCM_STRUCT_INST_DATA(x)	SCM_CDR(x)



