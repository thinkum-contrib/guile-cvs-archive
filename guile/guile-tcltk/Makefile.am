## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libgtcltk.la
bin_PROGRAMS = guile-tcltk

libgtcltk_la_SOURCES = guile-tcl.c guile-tk.c guile-tcl.h guile-tk.h \
	gtcltk-module.c
libgtcltk_la_LDFLAGS = -version-info 0:0 -rpath $(libdir)

guile_tcltk_LDADD = libgtcltk.la

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS) $< > $@

COMPILE = $(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -MM -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)
