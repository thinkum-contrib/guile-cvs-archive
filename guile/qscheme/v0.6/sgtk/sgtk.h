#include <gtk/gtk.h>

/*** support for libglade */
/* #define LIBGLADE_SUPPORT 1 */

/*** sweep debug flag */
/* #define DEBUG_SWEEP	1 */

/*** object and func hash debugging messages*/
/* #undef DEBUG_ALIVE	1 */

#ifdef DEBUG_SWEEP
extern void SWEEPING(SOBJ x);
#else
#define SWEEPING(x)
#endif

#ifdef LIBGLADE_SUPPORT
#include <glade/glade.h>
#endif

/* max number of arguments passed to a callback */
#define SGTK_MAX_CALLBACK_ARGS	16

#define SGTK_OBJECT(x)		((GtkObject*)SCM_AUX(x))
#define SGTK_WIDGET(x)		((GtkWidget*)SCM_AUX(x))
#define SGTK_ACCELGROUP(x) 	((GtkAccelGroup*)(SCM_AUX(x)))
#define SGTK_STYLE(x)		((GtkStyle*)(SCM_AUX(x)))
#define SGTK_SELECTIONDATA(x) ((GtkSelectionData *)(SCM_AUX(x)))

#define SGDK_FONT(x)		((GdkFont  *)(SCM_AUX(x)))
#define SGDK_COLOR(x) 		((GdkColor *)(SCM_AUX(x)))
#define SGDK_WINDOW(x) 		((GdkWindow *)(SCM_AUX(x)))
#define SGDK_GC(x) 			((GdkGC *)(SCM_AUX(x)))
#define SGDK_VISUAL(x) 		((GdkVisual *)(SCM_AUX(x)))
#define SGDK_COLORMAP(x) 	((GdkColormap *)(SCM_AUX(x)))
#define SGDK_DRAGCONTEXT(x)	((GdkDragContext *)(SCM_AUX(x)))

#define SGDK_ATOM_NAME(x) 	((char *)(SCM_CAR(x)))
#define SGDK_ATOM_ATOM(x) 	((GdkAtom)(SCM_CDR(x)))

#define SGDK_CURSOR(x)		((GdkCursor*)(SCM_AUX(x)))

#define SGTK_CTREE_NODE(x)	((GtkCTreeNode *)(SCM_AUX(x)))

#define SGDK_EVENT(x)		((GdkEvent *)(SCM_AUX(x)))

typedef struct {
  GtkStyle *style;				/* parent style */
  enum {STYLE_COLOUR_ARRAY, STYLE_GC_ARRAY, STYLE_PIXMAP_ARRAY} type;
  gpointer array;
} sgtk_StyleHelper_Aux;

#define SGTK_STYLE_HELPER_AUX(x)  ((sgtk_StyleHelper_Aux*)(SCM_AUX(x)))

extern int SOBJ_T_GtkObject;			/* the gtk objects */
extern int SOBJ_T_GtkAccelGroup;		/* the accell group */
extern int SOBJ_T_GtkStyleHelper;
extern int SOBJ_T_GtkStyle;
extern int SOBJ_T_GtkSelectionData;
extern int SOBJ_T_GtkCTreeNode;

extern int SOBJ_T_GdkFont;
extern int SOBJ_T_GdkColor;
extern int SOBJ_T_GdkEvent;
extern int SOBJ_T_GdkWindow;
extern int SOBJ_T_GdkGC;
extern int SOBJ_T_GdkColormap;
extern int SOBJ_T_GdkDragContext;
extern int SOBJ_T_GdkAtom;
extern int SOBJ_T_GdkCursor;


#ifdef LIBGLADE_SUPPORT

extern int SOBJ_T_GLADE_XML;

#define SGLADE_XML(x)			((GladeXML*)SCM_AUX(x))
#define SGLADE_XMLP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GLADE_XML)

#endif

/* GDK Predicates */
#define SGDK_FONTP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GdkFont)
#define SGDK_COLORP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GdkColor)
#define SGDK_EVENTP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GdkEvent)
#define SGDK_WINDOWP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GdkWindow)
#define SGDK_GCP(x)				(SCM_OBJTYPE(x) == SOBJ_T_GdkGC)
#define SGDK_VISUALP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GdkVisual)
#define SGDK_COLORMAPP(x)		(SCM_OBJTYPE(x) == SOBJ_T_GdkColormap)
#define SGDK_DRAGCONTEXTP(x)	(SCM_OBJTYPE(x) == SOBJ_T_GdkDragContext)
#define SGDK_ATOMP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GdkAtom)
#define SGDK_CURSORP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GdkCursor)

/* GTK Predicates */
#define SGTK_OBJECTP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GtkObject)
#define SGTK_ACCELGROUPP(x)		(SCM_OBJTYPE(x) == SOBJ_T_GtkAccelGroup)
#define SGTK_STYLEP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GtkStyle)
#define SGTK_STYLEHELPERP(x)	(SCM_OBJTYPE(x) == SOBJ_T_GtkStyleHelper)
#define SGTK_SELECTIONDATAP(x)	(SCM_OBJTYPE(x) == SOBJ_T_GtkSelectionData)
#define SGTK_CTREE_NODEP(x)		(SCM_OBJTYPE(x) == SOBJ_T_GtkCTreeNode)

#define scm_anystrp(x) (SCM_STRINGP(x)||SCM_ATOMP(x)||SCM_KEYWORDP(x)||SCM_SYMBOLP(x))

void 		sgtk_obj_cache_add(gpointer gtkobj, SOBJ scmobj);
void 		sgtk_func_add(gpointer gtkobj, SOBJ scmobj);
SOBJ        sgtk_aux_compare(SOBJ x, SOBJ y);
int         sgtk_enumget(char *typename, SOBJ member);
SOBJ        sgtk_enum_get(SOBJ type, SOBJ member);
int         sgtk_flagsget(char *typename, SOBJ member);
SOBJ        sgtk_flags_get(SOBJ type, SOBJ member);
SOBJ        sgdk_Font_new(GdkFont *obj);
void        *sgdk_get_Font(SOBJ obj);
SOBJ        sgdk_Color_new(GdkColor *obj);
void        *sgdk_get_Color(SOBJ obj);
SOBJ        sgdk_Window_new(GdkWindow *win);
void        *sgdk_get_Window(SOBJ obj);
SOBJ        sgdk_GC_new(GdkGC *gc);
void        *sgdk_get_GC(SOBJ obj);
SOBJ        sgdk_Colormap_new(GdkColormap *obj);
void        *sgdk_get_Colormap(SOBJ obj);
SOBJ        sgdk_DragContext_new(GdkDragContext *obj);
void        *sgdk_get_DragContext(SOBJ obj);
SOBJ        sgdk_Atom_new(GdkAtom obj);
SOBJ        sgdk_Cursor_new(GdkCursor *obj);
void        *sgdk_get_Cursor(SOBJ obj);
SOBJ        sgdk_Event_new(GdkEvent *obj);
void        *sgdk_get_Event(SOBJ obj);
SOBJ        sgtk_object_new(GtkObject *o);
void        *sgtk_get_Object(SOBJ obj);
SOBJ        sgtk_AccelGroup_new(GtkAccelGroup *obj);
void        *sgtk_get_AccelGroup(SOBJ obj);
SOBJ        sgtk_Style_new(GtkStyle *obj);
void        *sgtk_get_Style(SOBJ obj);
void        *sgtk_get_SelectionData(SOBJ obj);
SOBJ        sgtk_CTreeNode_new(GtkCTreeNode *node);
void        *sgtk_get_CTreeNode(SOBJ obj);
void        sgtk_func_destroy_notify(gpointer data);
void        sgtk_callback_marshal(GtkObject *o, gpointer data, guint nargs, GtkArg *args);
SOBJ        sgtk_signal_connect(SOBJ obj, SOBJ name, SOBJ func, SOBJ data);
SOBJ        sgtk_idle_add(SOBJ proc);
SOBJ        sgtk_timeout_add(SOBJ timeout, SOBJ proc);
SOBJ        sgdk_window_get_pointer(SOBJ win);
SOBJ        sgtk_event_motion_get(GdkEventMotion *event);
SOBJ        sgdk_event_window(SOBJ e);
SOBJ        sgtk_event_motion_hint(SOBJ e);
SOBJ        sgtk_event_motion_x(SOBJ e);
SOBJ        sgtk_event_motion_y(SOBJ e);
SOBJ        sgtk_event_motion_state(SOBJ e);
SOBJ        sgtk_text_get_hadj(SOBJ text);
SOBJ        sgtk_text_get_vadj(SOBJ text);
void        scm_init_sgtk();
