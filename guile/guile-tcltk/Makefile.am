## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = PLUGIN

INCLUDES = -I$(srcdir)/../libguile -I$(srcdir)/.. -I../

lib_LIBRARIES = @GTCLTK@
EXTRA_LIBRARIES = libgtcltk.a

libgtcltk_a_SOURCES = guile-tcl.c guile-tk.c guile-tcl.h guile-tk.h

SUFFIXES = .x
.c.x:
	$(CPP) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) \
	   -DSCM_MAGIC_SNARFER $< | grep "^%%%" | sed -e "s/^%%%//" > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
