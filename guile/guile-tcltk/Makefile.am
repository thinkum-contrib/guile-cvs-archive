## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(srcdir)/../libguile -I$(srcdir)/.. -I../

lib_LTLIBRARIES = @GTCLTK@
EXTRA_LTLIBRARIES = libgtcltk.la

libgtcltk_la_SOURCES = guile-tcl.c guile-tk.c guile-tcl.h guile-tk.h \
	gtcltk-module.c
libgtcltk_la_LDFLAGS = -version-info 0:0 -rpath $(libdir)

SUFFIXES = .x
SNARF=@SNARF@
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS) $< > $@

COMPILE = $(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -MM -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)
