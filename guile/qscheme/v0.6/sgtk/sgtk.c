/* -*- tab-width:4; -*- */
/*
 * GTK binding for QSCHEME
 */

#include "s.h"
#include "vm2.h"
#include "stack.h"
#include "sgtk.h"


/*** Debugging for object deallocation */
#ifdef DEBUG_SWEEP
void SWEEPING(SOBJ x)
{
  scm_puts("; SWEEPING ");  scm_cprint(x);
}
#endif

/*-- new object types */
/*** GDK types */
int SOBJ_T_GdkFont;
int SOBJ_T_GdkColor;
int SOBJ_T_GdkEvent;
int SOBJ_T_GdkWindow;
int SOBJ_T_GdkGC;
int SOBJ_T_GdkVisual;
int SOBJ_T_GdkColormap;
int SOBJ_T_GdkDragContext;
int SOBJ_T_GdkAtom;
int SOBJ_T_GdkCursor;
int SOBJ_T_GtkCTreeNode;

/*** GTK Types */
int SOBJ_T_GtkObject;			/* the gtk objects */
int SOBJ_T_GtkAccelGroup;		/* the accell group */
int SOBJ_T_GtkStyleHelper;
int SOBJ_T_GtkStyle;
int SOBJ_T_GtkSelectionData;

#ifdef LIBGLADE_SUPPORT
int SOBJ_T_GLADE_XML;			/* glade xml */
#endif /* LIBGLADE_SUPPORT */


/* Hash to keep track of objects: objects are hidden from GC behind a
 * pointer.
 */
SOBJ sgtk_obj_cache;			/* keep track of alive objects */

void sgtk_obj_cache_add(gpointer gtkobj, SOBJ scmobj)
{
#ifdef DEBUG_ALIVE
  scm_puts("DEBUG: obj_cache_add ("); scm_putx(gtkobj);
  scm_puts(","); scm_cdisplay(scmobj);  scm_puts(")\n");
#endif
  scm_hash_set(sgtk_obj_cache, scm_mkpointer(gtkobj), scm_mkpointer(scmobj));
}

static SOBJ sgtk_obj_cache_get(gpointer gtkobj)
{
  SOBJ res;
  res = scm_hash_ref(sgtk_obj_cache, scm_mkpointer(gtkobj));
  if (res != scm_undefined)
	res = SCM_POINTER(res);
  	
#ifdef DEBUG_ALIVE
  scm_puts("DEBUG: obj_cache_get ("); scm_putx(gtkobj); scm_puts(")->");
  scm_cprint(res);
#endif
  return(res);
}

void sgtk_obj_cache_remove(gpointer gtkobj)
{
#ifdef DEBUG_ALIVE
  scm_puts("DEBUG: obj_cache_remove ("); scm_putx(gtkobj);  scm_puts(")\n");
#endif
  scm_hash_remove(sgtk_obj_cache, scm_mkpointer(gtkobj));
}

/*** GC protection scheme
 * ALL alive gtk objects are recenced in this hash table.
 * The key is a pointer to a gtk object
 * The value associated is the pointer to the scheme object
 */

/*** Func hash: Keeps track of functions which are not referenced from
 * the Scheme side, like callbacks functions.
 *
 * The GC will keep this functions because they are referenced here.
 */

SOBJ sgtk_func_hash;			/* keep track of callbacks functions */

void sgtk_func_add(gpointer gtkobj, SOBJ scmobj)
{
#ifdef DEBUG_ALIVE
  scm_puts("DEBUG: func_add ("); scm_putx(gtkobj);
  scm_puts(","); scm_cdisplay(scmobj);  scm_puts(")\n");
#endif
  scm_hash_set(sgtk_func_hash, scm_mkpointer(gtkobj), scmobj);
}

static SOBJ sgtk_func_get(gpointer gtkobj)
{
#ifdef DEBUG_ALIVE
  scm_puts("DEBUG: func_get ("); scm_putx(gtkobj); scm_puts(")\n");
#endif
  return(scm_hash_ref(sgtk_func_hash, scm_mkpointer(gtkobj)));
}

static void sgtk_func_remove(gpointer gtkobj)
{
#ifdef DEBUG_ALIVE
  scm_puts("DEBUG: func_remove ("); scm_putx(gtkobj);  scm_puts(")\n");
#endif
  scm_hash_remove(sgtk_func_hash, scm_mkpointer(gtkobj));
}


/****************************************************************
 * utilities
 ****************************************************************/

/*** utilities ***/

static void *scm_fetch_aux(SOBJ x)
{
  return(SCM_AUX(x));
}

SOBJ sgtk_aux_compare(SOBJ x, SOBJ y)
{
  return(SCM_MKBOOL(SCM_AUX(x) == SCM_AUX(y)));
}

/*** Enum */

/*** Determine the enum value given the type and the type member */
static int enum_getv(GtkType type, SOBJ obj)
{
  char *member;
  GtkEnumValue *info;

  if (SCM_INUMP(obj)) 	return SCM_INUM(obj);
  if (SCM_NUMBERP(obj)) return scm_number2long(obj);

  if ( (member = scm_getstr(obj)) == NULL)
	SCM_ERR("enum values must be string or int", obj);
  
  if ( (info = gtk_type_enum_find_value(type, member)) == NULL)
	return(-1);

  return(info->value);
}

/*** func called by wrappers */
int sgtk_enumget(char *typename, SOBJ member)
{
  GtkType type;
  int value;

  if ((type = gtk_type_from_name(typename)) == 0)
	SCM_ERR("unknown enum type", scm_mkstring(typename));
  
  if ((value = enum_getv(type, member)) == -1)
	SCM_ERR("unknow enum member", scm_cons(scm_mkstring(typename), member));

  return(value);
}

/*E* (gtk-enum-get TYPE MEMBER) => INT */
/*D* Returns the value of the MEMBER of enum TYPE. MEMBER is a string
  and TYPE is either an INT or a STRING */

SOBJ sgtk_enum_get(SOBJ type, SOBJ member)
{
  char *typename;
  if (SCM_INUMP(member) || SCM_NUMBERP(member))		return(member);
  if ((typename = scm_getstr(type)) == NULL)  SCM_ERR("bad type name", type);
  return(SCM_MKINUM(sgtk_enumget(typename, member)));
}

static int flag_getv(GtkType flag_type, SOBJ obj)
{
  int value;
  char *member;
  GtkFlagValue *info;
  
  if (SCM_INUMP(obj))	return(SCM_INUM(obj));
  if (SCM_NUMBERP(obj))	return(scm_number2long(obj));

  if ( (member = scm_getstr(obj)) != NULL) {
	if ((info = gtk_type_flags_find_value(flag_type, member)) == NULL)
	  SCM_ERR("bad flag member", obj);

	return(info->value);
  }
  if (!SCM_PAIRP(obj))
	SCM_ERR("flag_getv: bad int|string|list", obj);

  value = 0;
  while(obj) {
	if (!SCM_PAIRP(obj))  SCM_ERR("flag_getv: bad list", obj);
	value |= flag_getv(flag_type, SCM_CAR(obj));
	obj = SCM_CDR(obj);
  }
  return(value);
}

/*** func called by wrappers */
int sgtk_flagsget(char *typename, SOBJ member)
{
  GtkType type;
  int value;

  if ((type = gtk_type_from_name(typename)) == 0)
	SCM_ERR("unknown flag type", scm_mkstring(typename));
  
  if ((value = flag_getv(type, member)) == -1)
	SCM_ERR("unknow flag member", scm_cons(scm_mkstring(typename), member));

  return(value);
}

/*E* (sgtk-flags-get TYPE MEMBER) => INT */
/*D* Returns the value of the MEMBER of flags TYPE. MEMBER is a string
  and TYPE is either an INT or a STRING */

SOBJ sgtk_flags_get(SOBJ type, SOBJ member)
{
  char *typename;
  if (SCM_INUMP(member) || SCM_NUMBERP(member))		return(member);
  if ((typename = scm_getstr(type)) == NULL)  SCM_ERR("bad type name", type);
  return(SCM_MKINUM(sgtk_flagsget(typename, member)));
}

/****************************************************************
 * GDK
 ****************************************************************/

/*** GdkFont ***/

SOBJ sgdk_Font_new(GdkFont *obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkFont);
  SGDK_FONT(new) = obj;
  gdk_font_ref(obj);
  sgtk_obj_cache_add(obj, new);
  return(new);
}

static void sgdk_Font_sweep(SOBJ x)
{
  SWEEPING(x);
  gdk_font_unref(SCM_AUX(x));
  sgtk_obj_cache_remove(SCM_AUX(x));
}

static SOBJ sgdk_Font_compare(SOBJ x, SOBJ y)
{
  return(SCM_MKBOOL(gdk_font_equal(SGDK_FONT(x), SGDK_FONT(y))));
}

static SOBJ GdkFont2scm(int type, void *p)
{
  return(sgdk_Font_new(p));
}

void *sgdk_get_Font(SOBJ obj)
{
  return(obj ? SGDK_FONT(obj) : NULL);
}

static SOBJ_TYPE_DESCR sgdk_Font_type_descr = {
  0,
  "GdkFont",
  NULL,					sgdk_Font_sweep,			/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  sgdk_Font_compare,								/* compare */
  GdkFont2scm,			sgdk_get_Font					/* get / set */
};

/*** GdkColor ***/

SOBJ sgdk_Color_new(GdkColor *obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkColor);
  SGDK_COLOR(new) = scm_must_alloc(sizeof(GdkColor));
  *SGDK_COLOR(new) = *obj;
  return(new);
}

static void sgdk_Color_sweep(SOBJ obj)
{
  SWEEPING(obj);
  if (SGDK_COLOR(obj)) {
	scm_free(SGDK_COLOR(obj));
	SGDK_COLOR(obj) = NULL;
  }
}

static void sgdk_color_print(SOBJ x, PORT *p)
{
  char buf[128];
  GdkColor *col = SGDK_COLOR(x);
  sprintf(buf, "#<color index=%d rgb=%d %d %d>",
		  col->pixel, col->red, col->green, col->blue);
  port_puts(p, buf);
}

static SCM_STRBUF *sgdk_color2str(SCM_STRBUF *sb, SOBJ x, int raw)
{
  GdkColor *col = SGDK_COLOR(x);
  return(scm_strbuf_concat_sprintf(sb, "#<color index=%d rgb=%d %d %d>",
								   col->pixel, col->red,
								   col->green, col->blue));
}

static SOBJ GdkColor2scm(int type, void *p)
{
  return(sgdk_Color_new(p));
}

void *sgdk_get_Color(SOBJ obj)
{
  return(obj ? SGDK_COLOR(obj) : NULL);
}

static SOBJ_TYPE_DESCR sgdk_Color_type_descr = {
  0,
  "GdkColor",
  NULL,					sgdk_Color_sweep,			/* mark / sweep */
  sgdk_color_print,		sgdk_color_print,			/* print */
  sgdk_color2str,
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  NULL,												/* compare */
  GdkColor2scm,			sgdk_get_Color				/* get / set */
};


/*** GdkWindow ***/
SOBJ sgdk_Window_new(GdkWindow *win)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkWindow);
  /*  g_print("sgdk_Window_new: window %p\n", win); */
  SGDK_WINDOW(new) = win;
  /*sgtk_obj_cache_add(win, new); */
  return(new);
}

static void sgdk_Window_sweep(SOBJ x)
{
  GdkWindow *w = SGDK_WINDOW(x);

  SWEEPING(x);
  /*
  if (gdk_window_get_type(w) == GDK_WINDOW_PIXMAP)
	gdk_pixmap_unref(w);
  else 
	gdk_window_unref(w);
  */
  /* sgtk_obj_cache_remove(w); */
}

static SOBJ GdkWindow2scm(int type, void *p)
{
  /*  SOBJ ret;
  if ((ret = sgtk_obj_cache_get(p)) != scm_undefined) {
	return(ret);
  }
  */
  return(sgdk_Window_new(p));
}

void *sgdk_get_Window(SOBJ obj)
{
  return(obj == NULL ? NULL : SGDK_WINDOW(obj));
}



static SOBJ_TYPE_DESCR sgdk_Window_type_descr = {
  0,
  "GdkWindow",
  NULL,					sgdk_Window_sweep,			/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  sgtk_aux_compare,									/* compare */
  GdkWindow2scm,		sgdk_get_Window				/* get / set */
};


/*** GdkGC ***/
SOBJ sgdk_GC_new(GdkGC *gc)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkGC);
  SGDK_GC(new) = gc;
  gdk_gc_ref(gc);
  sgtk_obj_cache_add(gc, new);
  return(new);
}

static void sgdk_GC_sweep(SOBJ x)
{
  SWEEPING(x);
  gdk_gc_unref(SGDK_GC(x));
  sgtk_obj_cache_remove(SGDK_GC(x));
}

static SOBJ GdkGC2scm(int type, void *p)
{
  return(sgdk_GC_new(p));
}

void *sgdk_get_GC(SOBJ obj)
{
  return(SGDK_GC(obj));
}

static SOBJ_TYPE_DESCR sgdk_GC_type_descr = {
  0,
  "GdkGC",
  NULL,					sgdk_GC_sweep,				/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  sgtk_aux_compare,									/* compare */
  GdkGC2scm,			sgdk_get_GC					/* get / set */
};

/*** GdkVisual ***/
SOBJ sgdk_Visual_new(GdkVisual *gc)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkVisual);
  SGDK_VISUAL(new) = gc;
  gdk_visual_ref(gc);
  sgtk_obj_cache_add(gc, new);
  return(new);
}

static void sgdk_Visual_sweep(SOBJ x)
{
  SWEEPING(x);
  gdk_visual_unref(SGDK_VISUAL(x));
  sgtk_obj_cache_remove(SGDK_VISUAL(x));
}

static SOBJ GdkVisual2scm(int type, void *p)
{
  return(sgdk_Visual_new(p));
}

void *sgdk_get_Visual(SOBJ obj)
{
  return(SGDK_VISUAL(obj));
}

static SOBJ_TYPE_DESCR sgdk_Visual_type_descr = {
  0,
  "GdkVisual",
  NULL,					sgdk_Visual_sweep,			/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  sgtk_aux_compare,									/* compare */
  GdkVisual2scm,		sgdk_get_Visual				/* get / set */
};

/*** GdkColorMap ***/
SOBJ sgdk_Colormap_new(GdkColormap *obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkColormap);
  SGDK_COLORMAP(new) = obj;
  gdk_colormap_ref(obj);
  sgtk_obj_cache_add(obj, new);
  return(new);
}

static void sgdk_Colormap_sweep(SOBJ x)
{
  SWEEPING(x);
  gdk_colormap_unref(SGDK_COLORMAP(x));
  sgtk_obj_cache_remove(SGDK_COLORMAP(x));
}

static SOBJ GdkColormap2scm(int type, void *p)
{
  return(sgdk_Colormap_new(p));
}

void *sgdk_get_Colormap(SOBJ obj)
{
  return(SGDK_COLORMAP(obj));
}

static SOBJ_TYPE_DESCR sgdk_Colormap_type_descr = {
  0,
  "GdkColormap",
  NULL,					sgdk_Colormap_sweep,		/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  sgtk_aux_compare,									/* compare */
  GdkColormap2scm,		sgdk_get_Colormap				/* get / set */
};

/*** GdkDragContext ***/
SOBJ sgdk_DragContext_new(GdkDragContext *obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkDragContext);
  SGDK_DRAGCONTEXT(new) = obj;
  gdk_drag_context_ref(obj);
  sgtk_obj_cache_add(obj, new);
  return(new);
}

static void sgdk_DragContext_sweep(SOBJ x)
{
  SWEEPING(x);
  gdk_drag_context_unref(SGDK_DRAGCONTEXT(x));
  sgtk_obj_cache_remove(SGDK_DRAGCONTEXT(x));
}

static SOBJ GdkDragContext2scm(int type, void *p)
{
  return(sgdk_DragContext_new(p));
}

void *sgdk_get_DragContext(SOBJ obj)
{
  return(SGDK_DRAGCONTEXT(obj));
}

static SOBJ_TYPE_DESCR sgdk_DragContext_type_descr = {
  0,
  "GdkDragContext",
  NULL,					sgdk_DragContext_sweep,		/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  sgtk_aux_compare,									/* compare */
  GdkDragContext2scm,	sgdk_get_DragContext			/* get / set */
};

/*** GdkAtom ***/
SOBJ sgdk_Atom_new(GdkAtom obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkAtom);
  SGDK_ATOM_NAME(new) = NULL;
  SGDK_ATOM_ATOM(new) = obj;
  return(new);
}

static void sgdk_Atom_sweep(SOBJ obj)
{
  SWEEPING(obj);
  if (SGDK_ATOM_NAME(obj)) {
	scm_free(SGDK_ATOM_NAME(obj));  SGDK_ATOM_NAME(obj) = NULL;
  }
}

static SOBJ GdkAtom2scm(int type, GdkAtom atom)
{
  return(sgdk_Atom_new(atom));
}

static GdkAtom sgdk_get_Atom(SOBJ obj)
{
  return( SGDK_ATOM_ATOM(obj) );
}

static SOBJ_TYPE_DESCR sgdk_Atom_type_descr = {
  0,
  "GdkAtom",
  NULL,					sgdk_Atom_sweep,			/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  NULL,												/* compare */
  (void*)GdkAtom2scm,	(void*)sgdk_get_Atom			/* get / set */
};

/*** GdkCursor ***/
SOBJ sgdk_Cursor_new(GdkCursor *obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkCursor);
  SGDK_CURSOR(new) = obj;
  sgtk_obj_cache_add(obj, new);
  return(new);
}

static void sgdk_Cursor_sweep(SOBJ x)
{
  SWEEPING(x);
  gdk_cursor_destroy(SGDK_CURSOR(x));
  sgtk_obj_cache_remove(SGDK_CURSOR(x));
}

static SOBJ GdkCursor2scm(int type, void *p)
{
  return(sgdk_Cursor_new(p));
}

void *sgdk_get_Cursor(SOBJ obj)
{
  return(SGDK_CURSOR(obj));
}

static SOBJ_TYPE_DESCR sgdk_Cursor_type_descr = {
  0,
  "GdkCursor",
  NULL,					sgdk_Cursor_sweep,			/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  sgtk_aux_compare,									/* compare */
  GdkCursor2scm,		sgdk_get_Cursor				/* get / set */
};

/*** GdkEvent ***/
SOBJ sgdk_Event_new(GdkEvent *obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GdkEvent);
  SGDK_EVENT(new) = scm_must_alloc(sizeof(GdkEvent));
  *(SGDK_EVENT(new)) = *obj;
  /* sgtk_obj_cache_add(obj, new); */
  return(new);
}

static void sgdk_Event_sweep(SOBJ x)
{
  SWEEPING(x);
  scm_free(SGDK_EVENT(x));
  /* sgtk_obj_cache_remove(SGDK_EVENT(x)); */
}

static SOBJ GdkEvent2scm(int type, void *p)
{
  return(sgdk_Event_new(p));
}

void *sgdk_get_Event(SOBJ obj)
{
  return(SGDK_EVENT(obj));
}

static SOBJ_TYPE_DESCR sgdk_Event_type_descr = {
  0,
  "GdkEvent",
  NULL,					sgdk_Event_sweep,		/* mark / sweep */
  NULL,					NULL,					/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL, 			/* parse */
  NULL,											/* compare */
  GdkEvent2scm,			sgdk_get_Event			/* get / set */
};


/****************************************************************
 * GTK
 ****************************************************************/

/*** GtkObject ***/
SOBJ sgtk_object_new(GtkObject *o)
{
  SOBJ new = scm_newcell(SOBJ_T_GtkObject);
  SGTK_OBJECT(new) = o;
  gtk_object_ref(o);
  sgtk_obj_cache_add(o, new);
  return(new);
}

static void sgtk_Object_sweep(SOBJ x)
{
  SWEEPING(x);
  if (SGTK_OBJECT(x)) {
	gtk_object_unref(SGTK_OBJECT(x));
	sgtk_obj_cache_remove(SGTK_OBJECT(x));
	SGTK_OBJECT(x) = NULL;
  }
}

static void sgtk_Object_print(SOBJ x, PORT *p)
{
  char buf[100];
  sprintf(buf, "#<GtkObject of type %s at %p>",
		  gtk_type_name(GTK_OBJECT_TYPE(SGTK_OBJECT(x))),
		  SGTK_OBJECT(x));
  port_puts(p, buf);
}

static SCM_STRBUF *sgtk_Object2str(SCM_STRBUF *sb, SOBJ x, int raw)
{
  return(scm_strbuf_concat_sprintf(sb, "#<GtkObject of type %s at %p>",
	  gtk_type_name(GTK_OBJECT_TYPE(SGTK_OBJECT(x))),
	  SGTK_OBJECT(x)));
}

static SOBJ sgtk_Object2obj(int type, void *p)
{
  return(sgtk_object_new(p));
}

void *sgtk_get_Object(SOBJ obj)
{
  return( (obj != NULL) ? SGTK_OBJECT(obj) : NULL);
}

static SOBJ_TYPE_DESCR sgtk_Object_type_descr = {
  0,
  "GtkObject",
  NULL,						sgtk_Object_sweep,
  sgtk_Object_print,		sgtk_Object_print,
  sgtk_Object2str,
  NULL,  NULL,  			NULL,  NULL,
  sgtk_aux_compare,
  sgtk_Object2obj,				/* get */
  sgtk_get_Object				/* set */
};

/*** GtkAccelGroup type ***/

SOBJ sgtk_AccelGroup_new(GtkAccelGroup *obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GtkAccelGroup);
  SGTK_ACCELGROUP(new) = obj;
  gtk_accel_group_ref(obj);
  sgtk_obj_cache_add(obj, new);
  return(new);
}

static void sgtk_AccelGroup_sweep(SOBJ obj)
{
  SWEEPING(obj);
  if (SGTK_ACCELGROUP(obj)) {
	gtk_accel_group_unref(SGTK_ACCELGROUP(obj));
	sgtk_obj_cache_remove(SGTK_ACCELGROUP(obj));
	SGTK_ACCELGROUP(obj) = NULL;
  }
}

static SOBJ sgtk_AccelGroup2obj(int type, void *p)
{
  return(sgtk_AccelGroup_new(p));
}

void *sgtk_get_AccelGroup(SOBJ obj)
{
  return(SGTK_ACCELGROUP(obj));
}


static SOBJ_TYPE_DESCR sgtk_AccelGroup_type_descr = {
  0,
  "GtkAccelGroup",
  NULL,					sgtk_AccelGroup_sweep,
  NULL,					NULL,
  NULL,
  NULL,  NULL,  		NULL,  NULL,
  sgtk_aux_compare,
  sgtk_AccelGroup2obj,	sgtk_get_AccelGroup,
};

/*** GtkStyle ***/
SOBJ sgtk_Style_new(GtkStyle *obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GtkStyle);
  SGTK_STYLE(new) = obj;
  gtk_style_ref(obj);
  sgtk_obj_cache_add(obj, new);
  return(new);
}

static void sgtk_Style_sweep(SOBJ x)
{
  SWEEPING(x);
  gtk_style_unref(SGTK_STYLE(x));
  sgtk_obj_cache_remove(SGTK_STYLE(x));
}

static SOBJ GtkStyle2scm(int type, void *p)
{
  return(sgtk_Style_new(p));
}

void *sgtk_get_Style(SOBJ obj)
{
  return(SGTK_STYLE(obj));
}

static SOBJ_TYPE_DESCR sgtk_Style_type_descr = {
  0,
  "GtkStyle",
  NULL,					sgtk_Style_sweep,			/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL,				/* parse */
  sgtk_aux_compare,									/* compare */
  GtkStyle2scm,			sgtk_get_Style				/* get / set */
};

/*** GtkSelectionData ***/
static SOBJ sgtk_SelectionData_new(GtkSelectionData *obj)
{
  SOBJ new = scm_newcell(SOBJ_T_GtkSelectionData);
  SGTK_SELECTIONDATA(new) = obj;
  sgtk_obj_cache_add(obj, new);
  return(new);
}

static void sgtk_SelectionData_sweep(SOBJ obj)
{
  sgtk_obj_cache_remove(SGTK_SELECTIONDATA(obj));
}

static SOBJ GtkSelectionData2scm(int type, void *p)
{
  return(sgtk_SelectionData_new(p));
}

void *sgtk_get_SelectionData(SOBJ obj)
{
  return(SGTK_SELECTIONDATA(obj));
}

static SOBJ_TYPE_DESCR sgtk_SelectionData_type_descr = {
  0,
  "GtkSelectionData",
  NULL,					sgtk_SelectionData_sweep,	/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  sgtk_aux_compare,									/* compare */
  GtkSelectionData2scm,	sgtk_get_SelectionData		/* get / set */
};

/*** GtkCtreeNode ***/
SOBJ sgtk_CTreeNode_new(GtkCTreeNode *node)
{
  SOBJ new = scm_newcell(SOBJ_T_GtkCTreeNode);
  SGTK_CTREE_NODE(new) = node;
  sgtk_obj_cache_add(node, new);
  return(new);
}

static void sgtk_CTreeNode_sweep(SOBJ obj)
{
  sgtk_obj_cache_remove(SGTK_CTREE_NODE(obj));
}

static SOBJ GtkCTreeNode2scm(int type, void *p)
{
  return(sgtk_CTreeNode_new(p));
}

void *sgtk_get_CTreeNode(SOBJ obj)
{
  return(SGTK_CTREE_NODE(obj));
}

static SOBJ_TYPE_DESCR sgtk_CTreeNode_type_descr = {
  0,
  "GtkCTreeNode",
  NULL,					sgtk_CTreeNode_sweep, 		/* mark / sweep */
  NULL,					NULL,
  NULL,
  NULL,  NULL,  		NULL,  NULL,
  sgtk_aux_compare,
  GtkCTreeNode2scm,		sgtk_get_CTreeNode			/* get / set */
};


/*** GtkStyleHelper ***/
static SOBJ sgtk_StyleHelper_new(GtkStyle *style, int type, gpointer array)
{
  SOBJ new = scm_newcell(SOBJ_T_GtkStyleHelper);
  sgtk_StyleHelper_Aux *aux = scm_must_alloc(sizeof(sgtk_StyleHelper_Aux));

  SGTK_STYLE_HELPER_AUX(new) = aux;
  aux->style = gtk_style_ref(style);
  aux->type = type;
  aux->array = array;
  sgtk_obj_cache_add(style, new);
  return(new);
}

static void sgtk_StyleHelper_sweep(SOBJ x)
{
  SWEEPING(x);
  if (SGTK_STYLE_HELPER_AUX(x)) {
	gtk_style_unref(SGTK_STYLE_HELPER_AUX(x)->style);
	sgtk_obj_cache_remove(SGTK_STYLE_HELPER_AUX(x)->style);
	scm_free(SGTK_STYLE_HELPER_AUX(x));
  }
}

static SOBJ sgtk_StyleHelper_get(SOBJ x, int pos)
{
  sgtk_StyleHelper_Aux *aux = SGTK_STYLE_HELPER_AUX(x);

  if (pos < 0 || pos >= 5)	SCM_ERR("index out of range", SCM_MKINUM(pos));
  switch(aux->type) {
  case STYLE_COLOUR_ARRAY:
	{
	  GdkColor *array = (GdkColor *)aux->array;
	  return(sgdk_Color_new(&array[pos]));
	}
  case STYLE_GC_ARRAY:
	{
	  GdkGC **array = (GdkGC **)aux->array;
	  return(sgdk_GC_new(array[pos]));
	}
  case STYLE_PIXMAP_ARRAY:
	{
	  GdkWindow **array = (GdkWindow **)aux->array;
	  if (array[pos]) return(sgdk_Window_new(array[pos]));
	  return(NULL);
	}
  }
  g_assert_not_reached();
  return(NULL);
}

static int sgtk_StyleHelper_set(SOBJ x, int pos, SOBJ value)
{
  sgtk_StyleHelper_Aux *aux = SGTK_STYLE_HELPER_AUX(x);

  if (pos < 0 || pos >= 5)	SCM_ERR("index out of range", SCM_MKINUM(pos));

  switch(aux->type) {
  case STYLE_COLOUR_ARRAY:
	{
	  GdkColor *array = (GdkColor *)aux->array;
	  if (SCM_OBJTYPE(value) != SOBJ_T_GdkColor) SCM_ERR("bad color", value);
	  array[pos] = *SGDK_COLOR(value);
	  return 0;
	}
  case STYLE_GC_ARRAY:
	{
	   GdkGC **array = (GdkGC **)aux->array;
	   if (SCM_OBJTYPE(value) != SOBJ_T_GdkGC)	SCM_ERR("bad gc", value);
	   if (array[pos]) 	gdk_gc_unref(array[pos]);
	   array[pos] =	gdk_gc_ref(SGDK_GC(x));
	   return 0;
	}
  case STYLE_PIXMAP_ARRAY:
	{
	  GdkWindow **array = (GdkWindow **)aux->array;
	  
	  if (value != NULL && SCM_OBJTYPE(value) != SOBJ_T_GdkWindow)
		SCM_ERR("bad window", value);
	  if (array[pos])	gdk_pixmap_unref(array[pos]);
	  array[pos] = gdk_pixmap_ref(SGDK_WINDOW(value));
	  return 0;
	}
  }
  g_assert_not_reached();
  return -1;
}

static SOBJ_TYPE_DESCR sgtk_StyleHelper_type_descr = {
  0,
  "GtkStyleHelper",
  NULL,					sgtk_StyleHelper_sweep,		/* mark / sweep */
  NULL,					NULL,						/* print */
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  NULL,												/* compare */
  NULL,	NULL										/* get / set */
};

static SOBJ sgtk_Style_get(SOBJ self, char *attr)
{
  SCM_ERR("sgtk_Style_get: not implemented", NULL);
}

static int sgtk_Style_set(SOBJ self, char *key, SOBJ value)
{
  SCM_ERR("sgtk_Style_set: not implemented", NULL);
}

/****************************************************************
 * convert arguments
 ****************************************************************/

/*** destroy notify for SGTK_OBJECTS */
void sgtk_func_destroy_notify(gpointer data)
{
  SOBJ obj = data;
#ifdef DEBUG_ALIVE
  scm_puts("sgtk_func_destroy_notify: ");  scm_putx(data);
  scm_puts(" object=");  scm_cprint(obj);
#endif
  
  if (sgtk_func_get(data)) {
#ifdef DEBUG_ALIVE
	scm_puts("DEBUG: remove func object "); scm_putx(data); scm_puts("\n");
#endif
	sgtk_func_remove(data);
  }
}


/*** boxed functions */

typedef struct {
  SOBJ		(*fromarg)(gpointer boxed);
  int		(*toarg)(gpointer *boxed, SOBJ obj);
} SGTK_BOX_FUNCS;

static GHashTable	*boxed_funcs;

static void sgtk_register_boxed(GtkType boxed_type,
								SOBJ (*fromarg)(gpointer boxed),
								int  (*toarg)(gpointer *boxed, SOBJ obj))
{
  SGTK_BOX_FUNCS *fs = g_new(SGTK_BOX_FUNCS, 1);
  fs->fromarg = fromarg;
  fs->toarg   = toarg;
  g_hash_table_insert(boxed_funcs, GUINT_TO_POINTER(boxed_type), fs);
}

#define sgtk_get_boxed(type) \
  (SGTK_BOX_FUNCS *)g_hash_table_lookup(boxed_funcs, GUINT_TO_POINTER(type))


/*** Signal handling */
/* forward decl */
static SOBJ sgtk_args2list(guint nargs, GtkArg *args);

/* GtkArg_FromPyObject */
static int sgtk_scm2arg(GtkArg *arg, SOBJ obj)
{
  SOBJ tmp;
  int value;
  
  switch (GTK_FUNDAMENTAL_TYPE(arg->type)) {
  case GTK_TYPE_NONE:
  case GTK_TYPE_INVALID:GTK_VALUE_INT(*arg) = 0;					break;
  case GTK_TYPE_BOOL:	GTK_VALUE_BOOL(*arg) = (obj != scm_false); 	break;
  case GTK_TYPE_CHAR:
	if (!SCM_CHARP(obj)) return -1;
	GTK_VALUE_CHAR(*arg) = SCM_CHAR(obj);
    break;
  case GTK_TYPE_ENUM:
	if ((value = enum_getv(arg->type, obj)) == -1) 	return -1;
	GTK_VALUE_ENUM(*arg) = value;
	break;
  case GTK_TYPE_FLAGS:
	if ((value = flag_getv(arg->type, obj)) == -1) 	return -1;
    GTK_VALUE_FLAGS(*arg) = value;
    break;
  case GTK_TYPE_INT:
	if (!SCM_NUMBERP(obj)) return -1;
	GTK_VALUE_INT(*arg) = scm_number2long(obj);
    break;
  case GTK_TYPE_UINT:
	if (!SCM_NUMBERP(obj)) return -1;
	GTK_VALUE_UINT(*arg) = scm_number2long(obj);
    break;
  case GTK_TYPE_LONG:
	if (!SCM_NUMBERP(obj)) return -1;
	GTK_VALUE_LONG(*arg) = scm_number2long(obj);
    break;
  case GTK_TYPE_ULONG:
	if (!SCM_NUMBERP(obj)) return -1;
	GTK_VALUE_ULONG(*arg) = scm_number2long(obj);
    break;
  case GTK_TYPE_FLOAT:
	if (!SCM_NUMBERP(obj)) return -1;
	GTK_VALUE_FLOAT(*arg) = scm_number2double(obj);
    break;
  case GTK_TYPE_DOUBLE:
	if (!SCM_NUMBERP(obj)) return -1;
	GTK_VALUE_DOUBLE(*arg) = scm_number2double(obj);
    break;
  case GTK_TYPE_STRING:
	if (!SCM_STRINGP(obj))	return -1;
	GTK_VALUE_STRING(*arg) = SCM_STR_VALUE(obj);
    break;
  case GTK_TYPE_OBJECT:
	if (!SGTK_OBJECTP(obj))	return -1;
	GTK_VALUE_OBJECT(*arg) = SGTK_OBJECT(obj);
  case GTK_TYPE_BOXED:
    if (arg->type == GTK_TYPE_ACCEL_GROUP) {
      if (SCM_OBJTYPE(obj) != SOBJ_T_GtkAccelGroup)		return -1;
	  GTK_VALUE_BOXED(*arg) = SGTK_ACCELGROUP(obj);
    } else if (arg->type == GTK_TYPE_STYLE) {
      if (SCM_OBJTYPE(obj) != SOBJ_T_GtkStyle)			return -1;
	  GTK_VALUE_BOXED(*arg) = SGTK_STYLE(obj);
    } else if (arg->type == GTK_TYPE_GDK_EVENT) {
      if (SCM_OBJTYPE(obj) != SOBJ_T_GdkEvent) 			return -1;
	  GTK_VALUE_BOXED(*arg) = SGDK_EVENT(obj);
    } else if (arg->type == GTK_TYPE_GDK_FONT) {
      if (SCM_OBJTYPE(obj) != SOBJ_T_GdkFont) 			return -1;
	  GTK_VALUE_BOXED(*arg) = SGDK_FONT(obj);
    } else if (arg->type == GTK_TYPE_GDK_COLOR) {
      if (obj && SCM_OBJTYPE(obj) != SOBJ_T_GdkColor) 	return -1;
	  GTK_VALUE_BOXED(*arg) = (obj) ? SGDK_COLOR(obj) : NULL;
    } else if (arg->type == GTK_TYPE_GDK_WINDOW) {
      if (obj && SCM_OBJTYPE(obj) != SOBJ_T_GdkWindow) 	return -1;
	  GTK_VALUE_BOXED(*arg) = (obj) ? SGDK_WINDOW(obj) : NULL;
    } else if (arg->type == GTK_TYPE_GDK_COLORMAP) {
      if (obj && SCM_OBJTYPE(obj) != SOBJ_T_GdkColormap) 	return -1;
	  GTK_VALUE_BOXED(*arg) = (obj) ? SGDK_COLORMAP(obj) : NULL;
    } else if (arg->type == GTK_TYPE_GDK_DRAG_CONTEXT) {
      if (obj && SCM_OBJTYPE(obj) != SOBJ_T_GdkDragContext) 	return -1;
	  GTK_VALUE_BOXED(*arg) = (obj) ? SGDK_DRAGCONTEXT(obj) : NULL;
    } else if (arg->type == GTK_TYPE_SELECTION_DATA) {
      if (SCM_OBJTYPE(obj) != SOBJ_T_GtkSelectionData) 	return -1;
	  GTK_VALUE_BOXED(*arg) = SGTK_SELECTIONDATA(obj);
    } else if (arg->type == GTK_TYPE_CTREE_NODE) {
	  if (obj && SCM_OBJTYPE(obj) != SOBJ_T_GtkSelectionData) return -1;
	  GTK_VALUE_BOXED(*arg) = (obj) ? SGTK_SELECTIONDATA(obj) : NULL;
    } else {
      SGTK_BOX_FUNCS *fs= sgtk_get_boxed(arg->type);
      if (fs && fs->toarg) {
		if (fs->toarg(&(GTK_VALUE_BOXED(*arg)), obj))
		  return -1;
      } else if (SCM_POINTERP(obj)) 
		GTK_VALUE_BOXED(*arg) = SCM_POINTER(obj);
      else
		return -1;
    }
    break;
  case GTK_TYPE_POINTER:
	if (!SCM_POINTERP(obj))	return -1;
	GTK_VALUE_BOXED(*arg) = SCM_POINTER(obj);
	break;
  case GTK_TYPE_FOREIGN:
    GTK_VALUE_FOREIGN(*arg).data = obj;
    GTK_VALUE_FOREIGN(*arg).notify = sgtk_func_destroy_notify;
    break;
  case GTK_TYPE_SIGNAL:
	if (!SCM_PROCP(obj)) 	return -1;
	GTK_VALUE_SIGNAL(*arg).f = NULL;
	GTK_VALUE_SIGNAL(*arg).d = obj;
    break;
  case GTK_TYPE_CALLBACK:
	if (!SCM_PROCP(obj)) 	return -1;
	GTK_VALUE_CALLBACK(*arg).marshal =
	  (GtkCallbackMarshal)sgtk_callback_marshal;
	GTK_VALUE_CALLBACK(*arg).data = obj;
	GTK_VALUE_CALLBACK(*arg).notify = sgtk_func_destroy_notify;
    break;
  case GTK_TYPE_ARGS:
  case GTK_TYPE_C_CALLBACK:
    fprintf(stderr, "unsupported type");
    g_assert_not_reached();
    return -1;
  }
  return 0;
}


/* GtkArg_AsPyObject */
static SOBJ sgtk_arg2scm(GtkArg *arg)
{
  switch (GTK_FUNDAMENTAL_TYPE(arg->type)) {
  case GTK_TYPE_INVALID:
  case GTK_TYPE_NONE:
	return NULL;

  case GTK_TYPE_CHAR:	return scm_mkchar(GTK_VALUE_CHAR(*arg));
  case GTK_TYPE_BOOL:   return SCM_MKBOOL(GTK_VALUE_BOOL(*arg));
  case GTK_TYPE_ENUM:
  case GTK_TYPE_FLAGS:
  case GTK_TYPE_INT:    return scm_int2num(GTK_VALUE_INT(*arg));
  case GTK_TYPE_UINT:   return scm_int2num(GTK_VALUE_UINT(*arg));
  case GTK_TYPE_LONG:   return scm_int2num(GTK_VALUE_LONG(*arg));
  case GTK_TYPE_ULONG:  return scm_int2num(GTK_VALUE_ULONG(*arg));
  case GTK_TYPE_FLOAT:  return scm_flt2num(GTK_VALUE_FLOAT(*arg));
  case GTK_TYPE_DOUBLE: return scm_flt2num(GTK_VALUE_DOUBLE(*arg));
  case GTK_TYPE_STRING:
	return GTK_VALUE_STRING(*arg) ?
	  scm_mkstring(GTK_VALUE_STRING(*arg)) : NULL;
  case GTK_TYPE_ARGS:
    return sgtk_args2list(GTK_VALUE_ARGS(*arg).n_args,
						  GTK_VALUE_ARGS(*arg).args);
  case GTK_TYPE_OBJECT:
	{
	  SOBJ obj = sgtk_obj_cache_get(GTK_VALUE_OBJECT(*arg));
	  if (obj == scm_undefined) {
		scm_puts("sgtk_arg2scm: oops: object not alive");
		scm_putx(GTK_VALUE_OBJECT(*arg));
		scm_puts("\n");
		return(NULL);
	  }
	  return obj;
	}
	
  case GTK_TYPE_POINTER:
	return scm_mk_static_pointer(GTK_VALUE_POINTER(*arg));
	
  case GTK_TYPE_BOXED:
    if (arg->type == GTK_TYPE_ACCEL_GROUP)
      return sgtk_AccelGroup_new(GTK_VALUE_BOXED(*arg));
    else if (arg->type == GTK_TYPE_STYLE)
      return sgtk_Style_new(GTK_VALUE_BOXED(*arg));
    else if (arg->type == GTK_TYPE_GDK_EVENT)
      return sgdk_Event_new(GTK_VALUE_BOXED(*arg));
    else if (arg->type == GTK_TYPE_GDK_FONT)
      return sgdk_Font_new(GTK_VALUE_BOXED(*arg));
    else if (arg->type == GTK_TYPE_GDK_COLOR)
      return sgdk_Color_new(GTK_VALUE_BOXED(*arg));
    else if (arg->type == GTK_TYPE_GDK_WINDOW)
      return sgdk_Window_new(GTK_VALUE_BOXED(*arg));
    else if (arg->type == GTK_TYPE_GDK_COLORMAP)
      return sgdk_Colormap_new(GTK_VALUE_BOXED(*arg));
    else if (arg->type == GTK_TYPE_GDK_DRAG_CONTEXT)
      return sgdk_DragContext_new(GTK_VALUE_BOXED(*arg));
    else if (arg->type == GTK_TYPE_SELECTION_DATA)
      return sgtk_SelectionData_new(GTK_VALUE_BOXED(*arg));
    else if (arg->type == GTK_TYPE_CTREE_NODE) {
      if (GTK_VALUE_BOXED(*arg))
	return sgtk_CTreeNode_new(GTK_VALUE_BOXED(*arg));
	  return NULL;
    } else {
      SGTK_BOX_FUNCS *fs = sgtk_get_boxed(arg->type);
      if (fs && fs->fromarg)
		return fs->fromarg(GTK_VALUE_BOXED(*arg));
      return scm_mk_static_pointer(GTK_VALUE_BOXED(*arg));
    }
  case GTK_TYPE_FOREIGN:	return (SOBJ)GTK_VALUE_FOREIGN(*arg).data;
  case GTK_TYPE_CALLBACK:	return (SOBJ)GTK_VALUE_CALLBACK(*arg).data;
  case GTK_TYPE_SIGNAL:		return (SOBJ)GTK_VALUE_SIGNAL(*arg).d;
  default:
    g_assert_not_reached();
    break;
  }
  return NULL;
}

/*** GtkRet_FromPyObject ***/
/* set a GtkArg structure's data from a scheme object, using the GTK_RETLOC_*
 * routines.  If it can't make the conversion, set the return to a zero
 * equivalent. */
static void sgtk_scm2ret(GtkArg *ret, SOBJ obj)
{
  int value;
  
  switch(GTK_FUNDAMENTAL_TYPE(ret->type)) {
  case GTK_TYPE_NONE:
  case GTK_TYPE_INVALID:    break;
  case GTK_TYPE_BOOL:	
	*GTK_RETLOC_BOOL(*ret) = (obj != scm_false);
	break;
  case GTK_TYPE_CHAR:
	if (SCM_CHARP(obj))
	  *GTK_RETLOC_CHAR(*ret) = SCM_CHAR(obj);
	else if (SCM_STRINGP(obj))
	  *GTK_RETLOC_CHAR(*ret) = SCM_STR_VALUE(obj)[0];
	else
	  *GTK_RETLOC_CHAR(*ret) = 0;
	break;
  case GTK_TYPE_ENUM:
	value = enum_getv(ret->type, obj);
	*GTK_RETLOC_ENUM(*ret) = (value == -1) ? 0 : value;
    break;
  case GTK_TYPE_FLAGS:
	value = flag_getv(ret->type, obj);
	*GTK_RETLOC_FLAGS(*ret) = (value == -1) ? 0 : value;
    break;
  case GTK_TYPE_INT:
	*GTK_RETLOC_INT(*ret) = SCM_NUMBERP(obj) ? scm_number2long(obj) : 0;
	break;
  case GTK_TYPE_UINT:
	*GTK_RETLOC_UINT(*ret) = SCM_NUMBERP(obj) ? scm_number2long(obj) : 0;
    break;
  case GTK_TYPE_LONG:
	*GTK_RETLOC_LONG(*ret) = SCM_NUMBERP(obj) ? scm_number2long(obj) : 0;
    break;
  case GTK_TYPE_ULONG:
	*GTK_RETLOC_ULONG(*ret) = SCM_NUMBERP(obj) ? scm_number2long(obj) : 0;
    break;
  case GTK_TYPE_FLOAT:
	*GTK_RETLOC_FLOAT(*ret) = SCM_NUMBERP(obj) ? scm_number2double(obj) : 0;
    break;
  case GTK_TYPE_DOUBLE:
	*GTK_RETLOC_DOUBLE(*ret) = SCM_NUMBERP(obj) ? scm_number2double(obj) : 0;
    break;
  case GTK_TYPE_STRING:
	*GTK_RETLOC_STRING(*ret) =
	  SCM_STRINGP(obj) ? g_strdup(SCM_STR_VALUE(obj)) : NULL;
    break;
  case GTK_TYPE_OBJECT:
	*GTK_RETLOC_OBJECT(*ret) = SGTK_OBJECTP(obj) ? SGTK_OBJECT(obj) : NULL;
    break;
  case GTK_TYPE_BOXED:
	if (ret->type == GTK_TYPE_ACCEL_GROUP) {
	  *GTK_RETLOC_BOXED(*ret) =
		(SCM_OBJTYPE(obj)==SOBJ_T_GtkAccelGroup) ? SGTK_ACCELGROUP(obj) : NULL;
	} else if (ret->type == GTK_TYPE_STYLE) {
	  *GTK_RETLOC_BOXED(*ret) =
		(SCM_OBJTYPE(obj)==SOBJ_T_GtkStyle) ? SGTK_STYLE(obj) : NULL;
	} else if (ret->type == GTK_TYPE_GDK_EVENT) {
	  *GTK_RETLOC_BOXED(*ret) =
		(SCM_OBJTYPE(obj)==SOBJ_T_GdkEvent) ? SGDK_EVENT(obj) : NULL;
	} else if (ret->type == GTK_TYPE_GDK_FONT) {
		*GTK_RETLOC_BOXED(*ret) =
		  (SCM_OBJTYPE(obj)==SOBJ_T_GdkFont) ? SGDK_FONT(obj) : NULL;
	} else if (ret->type == GTK_TYPE_GDK_COLOR) {
	  *GTK_RETLOC_BOXED(*ret) =
		(SCM_OBJTYPE(obj)==SOBJ_T_GdkColor) ? SGDK_COLOR(obj) : NULL;
	} else if (ret->type == GTK_TYPE_GDK_WINDOW) {
	  *GTK_RETLOC_BOXED(*ret) =
		(SCM_OBJTYPE(obj)==SOBJ_T_GdkWindow) ? SGDK_WINDOW(obj) : NULL;
	} else if (ret->type == GTK_TYPE_GDK_COLORMAP) {
	  *GTK_RETLOC_BOXED(*ret) =
		(SCM_OBJTYPE(obj)==SOBJ_T_GdkColormap) ? SGDK_COLORMAP(obj) : NULL;
	} else if (ret->type == GTK_TYPE_GDK_DRAG_CONTEXT) {
	  *GTK_RETLOC_BOXED(*ret) =
		(SCM_OBJTYPE(obj)==SOBJ_T_GdkDragContext) ? SGDK_DRAGCONTEXT(obj):NULL;
	} else if (ret->type == GTK_TYPE_SELECTION_DATA) {
	  *GTK_RETLOC_BOXED(*ret) =
		(SCM_OBJTYPE(obj)==SOBJ_T_GtkSelectionData) ?
		SGTK_SELECTIONDATA(obj) : NULL;
	} else if (ret->type == GTK_TYPE_CTREE_NODE) {
	  *GTK_RETLOC_BOXED(*ret) =
		(SCM_OBJTYPE(obj)==SOBJ_T_GtkCTreeNode) ? SGTK_CTREE_NODE(obj) : NULL;
	} else {
	  SGTK_BOX_FUNCS *fs = sgtk_get_boxed(ret->type);
	  if (fs && fs->toarg) {
		if (fs->toarg(GTK_RETLOC_BOXED(*ret), obj))
		  *GTK_RETLOC_BOXED(*ret) = NULL;
	  } else {
		*GTK_RETLOC_BOXED(*ret) =
		  SCM_POINTERP(obj) ? SCM_POINTER(obj) : NULL;
	  }
	}
    break;
  case GTK_TYPE_POINTER:
	*GTK_RETLOC_POINTER(*ret) = SCM_POINTERP(obj) ? SCM_POINTER(obj) : NULL;
	break;
  default:
    g_assert_not_reached();
    break;
  }
}

static SOBJ sgtk_args2list(guint nargs, GtkArg *args)
{
  SOBJ l;
  int i;

  l = NULL;
  for (i = 0; i < nargs; i++) {
	l = scm_cons(sgtk_arg2scm(args + i), l);
  }
  return(scm_reverse(l));
}

#ifdef OLD
static void sgtk_arg2array(guint narg_in, GtkArg *arg_in, SOBJ *out)
{
  int i;
  for (i = 0; i < narg_in; i++) {
	out[i] = sgtk_arg2scm(arg_in + i);
  }
}
#endif

void sgtk_callback_marshal(GtkObject *o, gpointer data,guint nargs, GtkArg *args)
{
  SOBJ ret;
  SOBJ funarg[SGTK_MAX_CALLBACK_ARGS];
  int  nfunarg, i;

  /* build argument list to call function
	 The argument list contains:
	 - the object
	 - the arguments converted from the GtkArg
	 - the extra list of arguments
	 The data argument contain a doted pair (func . extra_data)
  */

  if (nargs >=  (SGTK_MAX_CALLBACK_ARGS - 2)) {
	fprintf(stderr, "too many args: max=%d got=%d -- callback not run\n",
			SGTK_MAX_CALLBACK_ARGS - 2, nargs);
	return;
  }
  if (!SCM_PAIRP((SOBJ)data)) {
	fprintf(stderr, "expected (func . data) -- callback not run\n");
	return;
  }

  nfunarg = 0;
  if ((ret = sgtk_obj_cache_get(o)) == scm_undefined) {
	fprintf(stderr, "callback object %p not alive -- creating one\n", o);
	ret = sgtk_object_new(o);
  }
  /*** debugging message */
  /* scm_puts("CALLBACK for "); scm_cprint(ret); */

  funarg[nfunarg++] = ret;
  for (i = 0; i < nargs; i++) {
	funarg[nfunarg++] = sgtk_arg2scm(args + i);
  }
  funarg[nfunarg++] = SCM_CDR((SOBJ)data);
  if ((ret = scm_apply_v(SCM_CAR((SOBJ)data), nfunarg, funarg)) == NULL) {
	fprintf(stderr, "callback func returned NULL\n");
	return;
  }
  sgtk_scm2ret(args + nargs, ret);
}

SOBJ sgtk_signal_connect(SOBJ obj, SOBJ name, SOBJ func, SOBJ data)
{
  int sig;
  
  if (!SGTK_OBJECTP(obj)) 		SCM_ERR("bad gtk object", obj);
  if (!SCM_STRINGP(name))		SCM_ERR("bad signal name", name);
  if (!SCM_CLOSUREP(func))		SCM_ERR("bad function", func);

  func = scm_cons(func, data);
  sgtk_func_add(func, func);
  
  sig = gtk_signal_connect_full(SGTK_OBJECT(obj),
								SCM_STR_VALUE(name), NULL,
								sgtk_callback_marshal,
								func,
								sgtk_func_destroy_notify, FALSE, FALSE);
  return(scm_int2num(sig));
}

#ifdef LIBGLADE_SUPPORT

SOBJ sglade_xml_object_new(GladeXML *xml)
{
  SOBJ new = scm_newcell(SOBJ_T_GLADE_XML);
  SGLADE_XML(new) = xml;
  gtk_object_ref(GTK_OBJECT(xml));
  sgtk_obj_cache_add(xml, new);
  return(new);
}

static void sglade_xml_sweep(SOBJ x)
{
  SWEEPING(x);
  if (SGLADE_XML(x)) {
	GtkObject *o = GTK_OBJECT(SGLADE_XML(x));
	sgtk_obj_cache_remove(o);
	gtk_object_unref(o);
	SCM_CAR(x) = SCM_CDR(x) = NULL;
  }
}

static void sglade_xml_print(SOBJ x, PORT *p)
{
  port_puts(p, "#<GladeXML at ");
  port_putx(p, SGLADE_XML(x));
  port_puts(p, ">");
}

static SOBJ sglade_xml2scm(int type, void *p)
{
  return(sglade_xml_object_new(p));
}

static void *sglade_scm2xml(SOBJ x)
{
  return( (x == NULL) ? NULL : SGLADE_XML(x));
}

static SOBJ_TYPE_DESCR sglade_xml_type_descr = {
  0,
  "GladeXML",
  NULL,					sglade_xml_sweep,			/* mark / sweep */
  sglade_xml_print,		sglade_xml_print,			/* print */
  NULL,  NULL,  		NULL,  NULL, 				/* parse */
  sgtk_aux_compare,									/* compare */
  sglade_xml2scm,		sglade_scm2xml				/* get / set */
};

static void connect_one(const gchar *handler_name, GtkObject *object,
				   const gchar *signal_name,  const gchar *signal_data,
				   GtkObject *connect_object, gboolean after,
				   gpointer user_data)
{
  SOBJ cnxobj;
  SOBJ callback = user_data;
  SOBJ func;
  int sig;

  if (connect_object) {
	cnxobj = sgtk_object_new(connect_object);
	if (!SCM_CLOSUREP(callback)) SCM_ERR("bad closure", callback);
	if (SCM_PAIRP(callback)) {
	} else {
	}
  }
  func = scm_cons(callback, NULL);
  sgtk_func_add(func, func);
  gtk_signal_connect_full(object, signal_name, NULL,
						  sgtk_callback_marshal,
						  func,
						  sgtk_func_destroy_notify, FALSE, after);
}

SOBJ sglade_xml_signal_connect(SOBJ xml, SOBJ handlername, SOBJ func)
{
  if (!SGLADE_XMLP(xml))			SCM_ERR("bad glade xml", xml);
  if (!SCM_STRINGP(handlername))	SCM_ERR("bad handlername", handlername);
  if (!SCM_CLOSUREP(func))			SCM_ERR("bad func", func);

  glade_xml_signal_connect_full(SGLADE_XML(xml),
								SCM_STR_VALUE(handlername),
								connect_one,
								(void*)func);
  return(scm_undefined);
}

#endif /* LIBGLADE_SUPPORT */


/*** Idle functions ***/
static int sgtk_idle_handler(SOBJ proc)
{
  return( scm_apply0(proc) != scm_false );
}


SOBJ sgtk_idle_add(SOBJ proc)
{
  int id;

  sgtk_func_add(proc, proc);
  id = gtk_idle_add_full(GTK_PRIORITY_DEFAULT,
						 (void*)sgtk_idle_handler,
						 NULL,
						 (void*)proc,
						 sgtk_func_destroy_notify);
  return(SCM_MKINUM(id));
}


static int sgtk_timeout_handler(SOBJ proc)
{
  return( scm_apply0(proc) != scm_false );
}

SOBJ sgtk_timeout_add(SOBJ timeout, SOBJ proc)
{
  int id;
  if (!SCM_INUMP(timeout)) SCM_ERR("bad timeout", 	timeout);
  if (!SCM_CLOSUREP(proc)) SCM_ERR("bad func", 		proc);

  id = gtk_timeout_add_full(SCM_INUM(timeout),
							(void*)sgtk_timeout_handler,
							NULL,
							(void*)proc,
							sgtk_func_destroy_notify);
  return(SCM_MKINUM(id));
}


/*** Utilities ***/
#include "sgtk-typep.c"

/*** misc wrapper ***/

SOBJ sgdk_window_get_pointer(SOBJ win)
{
  int x, y;
  GdkModifierType state;
  if (SCM_OBJTYPE(win) != SOBJ_T_GdkWindow) 
	SCM_ERR("gdk-window-get-pointer: bad window", win);
  
  gdk_window_get_pointer(SGDK_WINDOW(win), &x, &y, &state);
  return(SCM_LIST3(SCM_MKINUM(x), SCM_MKINUM(y), SCM_MKINUM(state)));
}

#ifdef OOP
SOBJ sgtk_event_motion_get(GdkEventMotion *event)
{
  SOBJ ev = scm_object_clone(sgtk_event_object);
  scm_object_send(ev, scm_mkatom("type!"), 	SCM_MKINUM(event->type));
  scm_object_send(ev, scm_mkatom("window!"),sgtk_window_new(event->window));
  scm_object_send(ev, scm_mkatom("x"),  	scm_mkfnum(event->x));
  return(ev);
}
#endif

/*E* (gdk-event-window EVENT) => GDKWIN */
SOBJ sgdk_event_window(SOBJ e)
{
  return(sgdk_Window_new(SGDK_EVENT(e)->any.window));
}
/*E* (gdk-event-motion-hint EVENT) => BOOL */
SOBJ sgtk_event_motion_hint(SOBJ e)
{
  return(SCM_MKBOOL(((GdkEventMotion *)SGDK_EVENT(e))->is_hint));
}
/*E* (gdk-event-motion-x EVENT) => INT */
SOBJ sgtk_event_motion_x(SOBJ e)
{
  return(SCM_MKINUM((int)((GdkEventMotion *)SGDK_EVENT(e))->x));
}
/*E* (gdk-event-motion-y EVENT) => INT */
SOBJ sgtk_event_motion_y(SOBJ e)
{
  return(SCM_MKINUM((int)((GdkEventMotion *)SGDK_EVENT(e))->y));
}
/*E* (gdk-event-motion-state EVENT) => INT */
SOBJ sgtk_event_motion_state(SOBJ e)
{
  return(SCM_MKINUM((int)((GdkEventMotion *)SGDK_EVENT(e))->state));
}

/*E* (gdk-event-area-x EVENT) => INT */
SOBJ sgtk_event_area_x(SOBJ e)
{
  return(SCM_MKINUM((int)((GdkEventExpose *)SGDK_EVENT(e))->area.x));
}

/*E* (gdk-event-area-y EVENT) => INT */
SOBJ sgtk_event_area_y(SOBJ e)
{
  return(SCM_MKINUM((int)((GdkEventExpose *)SGDK_EVENT(e))->area.y));
}

/*E* (gdk-event-area-width EVENT) => INT */
SOBJ sgtk_event_area_width(SOBJ e)
{
  return(SCM_MKINUM((int)((GdkEventExpose *)SGDK_EVENT(e))->area.width));
}

/*E* (gdk-event-area-height EVENT) => INT */
SOBJ sgtk_event_area_height(SOBJ e)
{
  return(SCM_MKINUM((int)((GdkEventExpose *)SGDK_EVENT(e))->area.height));
}

/*E* (gdk-color-parse-new SPEC) => GDKCOLOR */
SOBJ sgtk_color_parse_new(SOBJ spec)
{
  GdkColor color;

  if (!SCM_STRINGP(spec))	SCM_ERR("bad spec", spec);
  if (gdk_color_parse(SCM_STR_VALUE(spec), &color)) {
	return(sgdk_Color_new(&color));
  }
  return(NULL);
}


/*** GTK_TEXT fields ***/
/*E* (gtk-text-get-hadj TEXT) => ADJ */
SOBJ sgtk_text_get_hadj(SOBJ text)
{
  return(sgtk_object_new((GtkObject*)(GTK_TEXT(SGTK_WIDGET(text))->hadj)));
}

/*E* (gtk-text-get-vadj TEXT) => ADJ */
SOBJ sgtk_text_get_vadj(SOBJ text)
{
  return(sgtk_object_new((GtkObject*)(GTK_TEXT(SGTK_WIDGET(text))->vadj)));
}

/*** modal file selection dialog */

static char *sgtk_filesel_fname;

/* save selected filename to sgtk_filesel_fname */
int sgtk_filesel_ok( GtkWidget *w, GtkFileSelection *fs )
{
  g_print("sgtk_filesel_ok:\n");
  sgtk_filesel_fname =
	strdup(gtk_file_selection_get_filename(GTK_FILE_SELECTION(fs)));
  gtk_widget_destroy(GTK_WIDGET(fs));
  return(FALSE);
}

static int sgtk_filesel_destroy( GtkWidget *widget, GtkFileSelection *fs)
{
  g_print("sgtk_filesel_destroy\n");
  gtk_main_quit();
  return(FALSE);
}

/*E* (gtk-file-select TITLE FNAME) => FNAME */
SOBJ sgtk_file_select(SOBJ title, SOBJ fname)
{
  GtkWidget *fs;
  
  if (!SCM_STRINGP(title))	SCM_ERR("bad title", title);
  if (fname && !SCM_STRINGP(fname))	SCM_ERR("bad filename", fname);

  if (sgtk_filesel_fname) free(sgtk_filesel_fname);
  sgtk_filesel_fname = NULL;
  
  fs = gtk_file_selection_new(SCM_STR_VALUE(fname));

  gtk_window_set_modal(GTK_WINDOW(fs), TRUE);

  gtk_signal_connect (GTK_OBJECT (fs), "destroy",
					  (GtkSignalFunc)sgtk_filesel_destroy, fs);

  /* Connect the ok_button to file_ok_sel function */
  gtk_signal_connect (GTK_OBJECT(GTK_FILE_SELECTION(fs)->ok_button),
					  "clicked", (GtkSignalFunc)sgtk_filesel_ok, fs );
  
  /* Connect the cancel_button to destroy the widget */
  gtk_signal_connect_object(GTK_OBJECT(GTK_FILE_SELECTION(fs)->cancel_button),
							"clicked", (GtkSignalFunc) gtk_widget_destroy,
							GTK_OBJECT(fs));
    
  /* Lets set the filename, as if this were a save dialog, and we are giving
     a default filename */
  if (fname) {
	gtk_file_selection_set_filename (GTK_FILE_SELECTION(fs), 
									 SCM_STR_VALUE(fname));
  }
    
  gtk_widget_show(fs);
  gtk_main();
  gtk_widget_destroy(fs);
  return(scm_mkstring(sgtk_filesel_fname));
}


/*E* (gtk-adjustment-set-all OBJ VALUE LOWER UPPER STEP_INCR PAGE_INCR PAGE_SIZE) => #undef */
SOBJ sgtk_adjustment_set_all(SOBJ obj, SOBJ val, SOBJ low, SOBJ upr,
							 SOBJ step_incr, SOBJ page_incr, SOBJ page_size)
{
  GtkAdjustment *adj;
  
  if (!SGTK_OBJECTP(obj)) 	SCM_ERR("bad gtk object", 	obj);
  if (!SCM_NUMBERP(val))	SCM_ERR("bad value",		val);
  if (!SCM_NUMBERP(low))	SCM_ERR("bad lower",		low);
  if (!SCM_NUMBERP(upr))	SCM_ERR("bad upper",		upr);
  if (!SCM_NUMBERP(step_incr))	SCM_ERR("bad step increment",		step_incr);
  if (!SCM_NUMBERP(page_incr))	SCM_ERR("bad page increment",		page_incr);
  if (!SCM_NUMBERP(page_size))	SCM_ERR("bad page size",			page_size);

  adj = GTK_ADJUSTMENT(SGTK_OBJECT(obj));
  adj->value = scm_number2double(val);
  adj->lower = scm_number2double(low);
  adj->upper = scm_number2double(upr);
  adj->step_increment = scm_number2double(step_incr);
  adj->page_increment = scm_number2double(page_incr);
  adj->page_size = scm_number2double(page_size);

  return(scm_undefined);
}

/*E* (gtk-adjustment-get RANGE) => ADJ */
SOBJ sgtk_adjustment_get(SOBJ range)
{
  if (!SGTK_OBJECTP(range))	SCM_ERR("bad object", range);
  return(sgtk_object_new
		 (GTK_OBJECT(GTK_RANGE(SGTK_OBJECT(range))->adjustment)));
}

/*E* (gtk-adjustment-get-value RANGE) => VALUE */
SOBJ sgtk_adjustment_get_value(SOBJ range)
{
  if (!SGTK_OBJECTP(range))	SCM_ERR("bad object", range);
  return(scm_mkfnum(GTK_RANGE(SGTK_OBJECT(range))->adjustment->value));
}

/*** get window */
/*E* (gtk-widget-get-window GTKOBJ) => GDKWIN */
SOBJ sgtk_widget_get_window(SOBJ x)
{
  if (!SGTK_OBJECTP(x)) SCM_ERR("bad widget", x);
  /*  printf("gtk-widget-get-window: window=%p\n", SGTK_WIDGET(x)->window); */
  return(sgdk_Window_new(SGTK_WIDGET(x)->window));
}

/*** get widget size */
/*E* (gtk-widget-get-size WIDGET) => (WIDTH HEIGHT) */
SOBJ sgtk_widget_get_size(SOBJ x)
{
  if (!SGTK_OBJECTP(x)) SCM_ERR("bad widget", x);
  return(SCM_LIST2(SCM_MKINUM(SGTK_WIDGET(x)->allocation.width),
				   SCM_MKINUM(SGTK_WIDGET(x)->allocation.height)));
}

/*E* (gtk-widget-get-width W) => INT */
SOBJ sgtk_widget_get_width(SOBJ x)
{
  if (!SGTK_OBJECTP(x)) SCM_ERR("bad widget", x);
  return(SCM_MKINUM(SGTK_WIDGET(x)->allocation.width));
}
/*E* (gtk-widget-get-height W) => INT */
SOBJ sgtk_widget_get_height(SOBJ x)
{
  if (!SGTK_OBJECTP(x)) SCM_ERR("bad widget", x);
  return(SCM_MKINUM(SGTK_WIDGET(x)->allocation.height));
}

/*** get style gc */
/*E* (gtk-widget-get-style-black-gc W) => GC */
SOBJ sgtk_widget_get_style_black_gc(SOBJ x)
{
  if (!SGTK_OBJECTP(x))	SCM_ERR("bad gtkobj", x);
  return(sgdk_GC_new(SGTK_WIDGET(x)->style->black_gc));
}

/*E* (gtk-widget-get-style-white-gc W) => GC */
SOBJ sgtk_widget_get_style_white_gc(SOBJ x)
{
  if (!SGTK_OBJECTP(x))	SCM_ERR("bad gtkobj", x);
  return(sgdk_GC_new(SGTK_WIDGET(x)->style->white_gc));
}

/*
 * (gtk-widget-get-style-gc wdraw :fg :prelight)
 * (gtk-widget-get-style-color wdraw :light :normal)
 * state-type: normal | active | prelight | selected | insensitive
*/

/*E* (gtk-widget-get-style-gc W FIELD TYPE) => GC */
/*D* FIELD=[:fg|:bg|:light|:dark|:mid|:text|:base],
 * TYPE=[:normal|:active|:prelight|:selected|:insensitive],
 */
SOBJ sgtk_widget_get_style_gc(SOBJ w, SOBJ f, SOBJ t)
{
  char *fieldname;
  int typenr;
  GdkGC **gcp;
  GtkWidget *wp;

  if (!SGTK_OBJECTP(w))		SCM_ERR("bad widget", 	w);
  if ((fieldname = scm_getstr(f)) == NULL)	SCM_ERR("bad field", f);

  typenr = sgtk_enumget("GtkStateType", t);

  wp = SGTK_WIDGET(w);

  if (streq(fieldname, "fg")) 			gcp = wp->style->fg_gc; 	
  else if (streq(fieldname, "bg")) 		gcp = wp->style->bg_gc;  	
  else if (streq(fieldname, "light")) 	gcp = wp->style->light_gc; 	
  else if (streq(fieldname, "dark")) 	gcp = wp->style->dark_gc; 	
  else if (streq(fieldname, "mid")) 	gcp = wp->style->mid_gc; 	
  else if (streq(fieldname, "text")) 	gcp = wp->style->text_gc; 	
  else if (streq(fieldname, "base")) 	gcp = wp->style->base_gc; 	
  else 									SCM_ERR("bad field name", f);
  return(sgdk_GC_new(gcp[typenr]));
}

/*E* (gtk-widget-get-style-color W FIELD TYPE) => COLOR */
/*D* FIELD=[:fg|:bg|:light|:dark|:mid|:text|:base],
 * TYPE=[:normal|:active|:prelight|:selected|:insensitive],
 */
SOBJ sgtk_widget_get_style_color(SOBJ w, SOBJ f, SOBJ t)
{
  char *fieldname;
  int typenr;
  GdkColor *gcp;
  GtkWidget *wp;

  if (!SGTK_OBJECTP(w))		SCM_ERR("bad widget", 	w);
  if ((fieldname = scm_getstr(f)) == NULL)	SCM_ERR("bad field", f);

  typenr = sgtk_enumget("GtkStateType", t);

  wp = SGTK_WIDGET(w);

  if (streq(fieldname, "fg")) 			gcp = wp->style->fg; 	
  else if (streq(fieldname, "bg")) 		gcp = wp->style->bg;  	
  else if (streq(fieldname, "light")) 	gcp = wp->style->light; 	
  else if (streq(fieldname, "dark")) 	gcp = wp->style->dark; 	
  else if (streq(fieldname, "mid")) 	gcp = wp->style->mid; 	
  else if (streq(fieldname, "text")) 	gcp = wp->style->text; 	
  else if (streq(fieldname, "base")) 	gcp = wp->style->base; 	
  else 									SCM_ERR("bad field name", f);
  return(sgdk_Color_new(gcp + typenr));
}

/*** CLIST ***/
static char **make_text_from_list(SOBJ l)
{
  char **text;
  int n, i;

  if (SCM_ARRAYP(l)) {
	text = scm_must_alloc(SCM_ASIZE(l) * sizeof(char *));
	for (i = 0; i < SCM_ASIZE(l); i++) {
	  text[i] = SCM_STRINGP(SCM_AREF(l, i)) ?
		SCM_STR_VALUE(SCM_AREF(l, i)) : NULL;
	}
	
  } else if (SCM_PAIRP(l)) {
	if ((n = scm_list_length(l)) == -1)    SCM_ERR("bad list", l);
	text = scm_must_alloc(n * sizeof(char *));
	for (i = 0; l; l=SCM_CDR(l), i++) {
	  text[i] = SCM_STRINGP(SCM_CAR(l)) ?
		SCM_STR_VALUE(SCM_CAR(l)) :
		NULL;
	}
  } else {
	SCM_ERR("bad list | array", l);
  }
  return(text);
}

/*E* (gtk-clist-append CLIST LIST|VECTOR) => ROW */
SOBJ sgtk_clist_append(SOBJ cl, SOBJ l)
{
  GtkCList 	*clist = sgtk_get_Object(cl);
  char **text = make_text_from_list(l);
  int n = gtk_clist_append(clist, text);
  scm_free(text);
  return(SCM_MKINUM(n));
}

/*E* (gtk-clist-prepend CLIST LIST|VECTOR) => ROW */
SOBJ sgtk_clist_prepend(SOBJ cl, SOBJ l)
{
  GtkCList 	*clist = sgtk_get_Object(cl);
  char **text = make_text_from_list(l);
  int n = gtk_clist_prepend(clist, text);
  scm_free(text);
  return(SCM_MKINUM(n));
}



/*** Initialization ***/

void scm_init_sgtk()
{
  SOBJ_T_GdkFont		= scm_add_type(&sgdk_Font_type_descr);
  SOBJ_T_GdkColor		= scm_add_type(&sgdk_Color_type_descr);
  SOBJ_T_GdkEvent		= scm_add_type(&sgdk_Event_type_descr);
  SOBJ_T_GdkWindow		= scm_add_type(&sgdk_Window_type_descr);
  SOBJ_T_GdkGC			= scm_add_type(&sgdk_GC_type_descr);
  SOBJ_T_GdkVisual		= scm_add_type(&sgdk_Visual_type_descr);
  SOBJ_T_GdkColormap	= scm_add_type(&sgdk_Colormap_type_descr);
  SOBJ_T_GdkDragContext	= scm_add_type(&sgdk_DragContext_type_descr);
  SOBJ_T_GdkAtom		= scm_add_type(&sgdk_Atom_type_descr);
  SOBJ_T_GdkCursor		= scm_add_type(&sgdk_Cursor_type_descr);

  SOBJ_T_GtkObject 		= scm_add_type(&sgtk_Object_type_descr);
  SOBJ_T_GtkAccelGroup 	= scm_add_type(&sgtk_AccelGroup_type_descr);
  SOBJ_T_GtkStyleHelper	= scm_add_type(&sgtk_StyleHelper_type_descr);
  SOBJ_T_GtkStyle 		= scm_add_type(&sgtk_Style_type_descr);
  SOBJ_T_GtkCTreeNode 	= scm_add_type(&sgtk_CTreeNode_type_descr);
  SOBJ_T_GtkSelectionData=scm_add_type(&sgtk_SelectionData_type_descr);

#ifdef LIBGLADE_SUPPORT
  SOBJ_T_GLADE_XML		= scm_add_type(&sglade_xml_type_descr);

  scm_add_cprim("glade-xml-signal-connect",	sglade_xml_signal_connect, 3);

#endif

  scm_add_cprim("gtk-idle-add",			sgtk_idle_add,			1);
  scm_add_cprim("gtk-timeout-add",		sgtk_timeout_add,		2);
  scm_add_cprim("gtk-signal-connect", 	sgtk_signal_connect, 	4);
  scm_add_cprim("gtk-enum-get", 		sgtk_enum_get,			2);
  scm_add_cprim("gtk-flags-get",		sgtk_flags_get,			2);

#include "sgtk-typep.i"

  scm_add_cprim("gdk-window-get-pointer",	sgdk_window_get_pointer,1);

  scm_add_cprim("gdk-event-window"	,		sgdk_event_window, 		1);
  scm_add_cprim("gdk-event-motion-hint",	sgtk_event_motion_hint, 1);
  scm_add_cprim("gdk-event-motion-x",		sgtk_event_motion_x, 	1);
  scm_add_cprim("gdk-event-motion-y",		sgtk_event_motion_y, 	1);
  scm_add_cprim("gdk-event-motion-state",	sgtk_event_motion_state,1);
  scm_add_cprim("gdk-event-area-x",			sgtk_event_area_x,		1);
  scm_add_cprim("gdk-event-area-y",			sgtk_event_area_y,		1);
  scm_add_cprim("gdk-event-area-width",		sgtk_event_area_width,	1);
  scm_add_cprim("gdk-event-area-height",	sgtk_event_area_height,	1);

  scm_add_cprim("gdk-color-parse-new",		sgtk_color_parse_new,	1);

  scm_add_cprim("gtk-text-get-hadj",		sgtk_text_get_hadj,		1);
  scm_add_cprim("gtk-text-get-vadj",		sgtk_text_get_vadj,		1);

  scm_add_cprim("gtk-file-select",			sgtk_file_select,		2);

  scm_add_cprim("gtk-adjustment-set-all",	sgtk_adjustment_set_all, 	7);
  scm_add_cprim("gtk-adjustment-get",		sgtk_adjustment_get,		1);
  scm_add_cprim("gtk-adjustment-get-value",	sgtk_adjustment_get_value,	1);

  scm_add_cprim("gtk-widget-get-window",   	sgtk_widget_get_window,	1);
  scm_add_cprim("gtk-widget-get-size",   	sgtk_widget_get_size,	1);
  scm_add_cprim("gtk-widget-get-width",   	sgtk_widget_get_width,	1);
  scm_add_cprim("gtk-widget-get-height",   	sgtk_widget_get_height,	1);

  scm_add_cprim("gtk-widget-get-style-black-gc",
				sgtk_widget_get_style_black_gc,						1);

  scm_add_cprim("gtk-widget-get-style-white-gc",
				sgtk_widget_get_style_white_gc,						1);

  scm_add_cprim("gtk-widget-get-style-gc",
				sgtk_widget_get_style_gc,							3);

  scm_add_cprim("gtk-widget-get-style-color",
				sgtk_widget_get_style_color,						3);

  /*** clist ***/
  scm_add_cprim("gtk-clist-append",			sgtk_clist_append,		2);
  scm_add_cprim("gtk-clist-prepend",		sgtk_clist_prepend,		2);

  sgtk_obj_cache = scm_mkhash(SCM_HASH_T_GEN);
  scm_gc_protect(&sgtk_obj_cache);

  sgtk_func_hash = scm_mkhash(SCM_HASH_T_GEN);
  scm_gc_protect(&sgtk_func_hash);

  scm_puts("; sgtk extension loaded...\n");
}
