/* -*- tab-width:4; -*- */
/*
 * Bindings for libglade
 */

#include "s.h"
#include "vm2.h"
#include "stack.h"

#include "sgtk.h"
#include "sglade.h"
#include <gtk/gtk.h>
#include <glade/glade.h>

/*** print debugging messages for autoconnect */
/*#define DEBUG_AUTOCONNECT	1 */

int SOBJ_T_GLADE_XML;			/* glade xml */

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
	if (!SCM_CLOSUREP(callback)) err("bad closure", callback);
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
  if (!SGLADE_XMLP(xml))			err("bad glade xml", xml);
  if (!SCM_STRINGP(handlername))	err("bad handlername", handlername);
  if (!SCM_CLOSUREP(func))			err("bad func", func);

  glade_xml_signal_connect_full(SGLADE_XML(xml),
								SCM_STR_VALUE(handlername),
								connect_one,
								(void*)func);
  return(scm_undefined);
}

static void connect_all(const gchar *handler_name, GtkObject *object,
				   const gchar *signal_name,  const gchar *signal_data,
				   GtkObject *connect_object, gboolean after,
				   gpointer user_data)
{
  SOBJ cnxobj;
  SOBJ module = user_data;
  SOBJ func, sym, callback;
  char *handler, *p;
  int sig;

#ifdef DEBUG_AUTOCONNECT
  printf("\nautoconnect: handler_name=%s, signal_name=%s, signal_data=%s\n",
		 handler_name, signal_name, signal_data);
  printf("autoconnect: connect_object=%p after=%d\n",
		 connect_object, after);

  scm_puts("autoconnect: module=");  scm_cprint(module);
#endif

  /*** change the _ to - and try to locate symbol */
  handler = scm_must_strdup((char*)handler_name);
  for (p = handler; *p != 0; p++) {
	if (*p == '_') *p = '-';
  }
  sym = scm_module_find_symbol(module, scm_mkatom(handler), FALSE);
  scm_free(handler);
  
  /*** if not found, try also the unchanged symbol */
  if (sym == NULL) {
	sym = scm_module_find_symbol(module,scm_mkatom((char*)handler_name),FALSE);
  }

#ifdef DEBUG_AUTOCONNECT
  scm_puts("autoconnect: symbol=");    scm_cprint(sym);
#endif
  if (sym == NULL) {
	printf("autoconnect: undefined func %s\n", handler_name);
	return;
  }
  if (!SCM_SYMBOLP(sym)) {
	scm_puts("autoconnect: not a symbol"); scm_cprint(sym);
	return;
  }
  callback = SCM_SYM_VALUE(sym);
  if (!SCM_CLOSUREP(callback)) {
	scm_puts("autoconnect: not a closure"); scm_cprint(callback);
	return;
  }
  
  func = scm_cons(callback, NULL);
  sgtk_func_add(func, func);
  sig = gtk_signal_connect_full(object, signal_name, NULL,
								sgtk_callback_marshal,
								func,
								sgtk_func_destroy_notify, FALSE, after);
#ifdef DEBUG_AUTOCONNECT
  printf("autoconnect: sig=%d\n", sig);
#endif
  
/*    if (connect_object) { */
/*  	cnxobj = sgtk_object_new(connect_object); */
/*  	if (!SCM_CLOSUREP(callback)) err("bad closure", callback); */
/*  	if (SCM_PAIRP(callback)) { */
/*  	} else { */
/*  	} */
/*    } */
/*    func = scm_cons(callback, NULL); */
/*    sgtk_func_add(func, func); */
/*    gtk_signal_connect_full(object, signal_name, NULL, */
/*  						  sgtk_callback_marshal, */
/*  						  func, */
/*  						  sgtk_func_destroy_notify, FALSE, after); */
}

SOBJ sglade_xml_signal_autoconnect(SOBJ xml, SOBJ module)
{
  SOBJ mod = module;
  
  if (!SGLADE_XMLP(xml)) 	err("bad glade xml", xml);

  if (SCM_STRINGP(module)) 	mod = scm_find_module(module);

  if (!SCM_MODULEP(mod)) 	err("bad module", module);

  glade_xml_signal_autoconnect_full(SGLADE_XML(xml), connect_all, mod);

  return(scm_undefined);
}



void scm_init_sglade()
{
  SOBJ_T_GLADE_XML		= scm_add_type(&sglade_xml_type_descr);

  scm_add_cprim("glade-xml-signal-connect",
				sglade_xml_signal_connect, 		3);
  scm_add_cprim("glade-xml-signal-autoconnect",
				sglade_xml_signal_autoconnect, 	2);
}
