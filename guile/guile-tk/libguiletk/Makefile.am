## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

CFLAGS = -DUSE_TK @CFLAGS@

INCLUDES = -I$(srcdir)/../Tcl -I$(srcdir)/../Tk/generic

lib_LTLIBRARIES = libguiletk.la
bin_PROGRAMS = guile-tk

libguiletk_la_SOURCES = \
    gtk-module.c tk-glue.c tk-glue.h tkcmd.c tkcmd.h tcl-lib.c
libguiletk_la_LDFLAGS = -version-info 0:0 -rpath $(libdir)

guile_tk_LDADD = libguiletk.la ../Tk/unix/libtk.a ../Tcl/libtcl.a

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS) $< > $@

COMPILE = $(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -MM -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)
