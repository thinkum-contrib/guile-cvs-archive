## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = PLUGIN

INCLUDES = -I$(srcdir)/../libguile -I$(srcdir)/.. -I../

lib_PROGRAMS = @GTCLTK@
EXTRA_PROGRAMS = libgtcltk.la

libgtcltk_la_SOURCES = guile-tcl.c guile-tk.c guile-tcl.h guile-tk.h gtcltk-module.c
libgtcltk_la_LDFLAGS = -version-info 0:0 -rpath $(libdir)

SUFFIXES = .x
.c.x:
	../libguile/guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

COMPILE = $(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)
