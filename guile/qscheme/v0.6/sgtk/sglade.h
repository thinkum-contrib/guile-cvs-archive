/*
 * Bindings for libglade
 */
#include <glade/glade.h>

extern int SOBJ_T_GLADE_XML;

#define SGLADE_XML(x)			((GladeXML*)SCM_AUX(x))
#define SGLADE_XMLP(x)			(SCM_OBJTYPE(x) == SOBJ_T_GLADE_XML)

