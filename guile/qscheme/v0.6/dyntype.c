/* -*- tab-width:4; -*- */
/*
 * Template of a runtime module adding a new type to qscheme.
 *
 * The new type is supposely named xxx. Just replace xxx by a better name
 *
 */
#include "s.h"

int SOBJ_T_XXX;				/* xxx type */

/* forward declaration for type descriptor functions */
static void scm_xxx_mark(SOBJ obj);
static void scm_xxx_sweep(SOBJ obj);
static void scm_xxx_print(SOBJ obj, PORT *p);
static void scm_xxx_write(SOBJ obj, PORT *p);
static int  scm_xxx_creconize(PORT *p, int c);
static SOBJ scm_xxx_cparse(PORT *p, int c);
static int  scm_xxx_wreconize(PORT *p, char *s);
static SOBJ scm_xxx_wparse(PORT *p, char *s);
static SOBJ scm_xxx_compare(SOBJ obj1, SOBJ obj2);
static SOBJ scm_xxx2obj(void *x);
static void *scm_obj2xxx(SOBJ obj);

/* the type descriptor */

SOBJ_TYPE_DESCR scm_xxx_type = {
  0,							/* nothing here */
  "xxx",						/* type name */
  scm_xxx_mark,					/* code to mark */
  scm_xxx_sweep,				/* code to sweep */
  scm_xxx_print,				/* howto print */
  scm_xxx_write,				/* howto write */
  scm_xxx_creconize,			/* char reconizer */
  scm_xxx_cparse,				/* char parser */
  scm_xxx_wreconize,			/* word reconizer */
  scm_xxx_wparse,				/* word parser */
  scm_xxx_compare				/* type value comparison */
  scm_xxx2obj,					/* convert to obj */
  scm_obj2xxx,					/* convert to xxx */
};

/* private functions */
static void scm_xxx_mark(SOBJ obj)
{
}

static void scm_xxx_sweep(SOBJ obj)
{
}

static void scm_xxx_print(SOBJ obj, PORT *p)
{
}

static void scm_xxx_write(SOBJ obj, PORT *p)
{
}

static int  scm_xxx_creconize(PORT *p, int c)
{
  return(FALSE);
}

static SOBJ scm_xxx_cparse(PORT *p, int c)
{
  return(scm_undefined);
}

static int  scm_xxx_wreconize(PORT *p, char *s)
{
  return(FALSE);
}

static SOBJ scm_xxx_wparse(PORT *p, char *s)
{
  return(scm_undefined);
}

static SOBJ scm_xxx_compare(SOBJ obj1, SOBJ obj2)
{
  return(obj1 == obj2);
}

/* public functions */

SOBJ scm_xxxp(SOBJ obj)
{
  return( (SCM_TYPEP(obj, SOBJ_T_XXX) ? scm_true : scm_false );
}

void scm_init_xxx()
{
  SOBJ_T_XXX = scm_add_type(&scm_xxx_type);

  scm_add_cprim("xxx?",		scm_xxxp,		1);

  /* fill the rest here */
}
