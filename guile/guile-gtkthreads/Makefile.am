## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libguilegtkthreads.la
bin_PROGRAMS = guile-gtkthreads

guile_gtkthreads_LDADD = libguilegtkthreads.la

libguilegtkthreads_la_SOURCES = gtkthreads.c gthread-guile.c
libguilegtkthreads_la_LDFLAGS = -version-info 0:0 -export-dynamic
BUILT_SOURCES = gtkthreads.x

include_HEADERS = 

modincludedir = $(includedir)/guile
modinclude_HEADERS = gtkthreads.h

subpkgdatadir = $(datadir)/guile/gtk
subpkgdata_DATA = threads.scm

EXTRA_DIST = $(subpkgdata_DATA) GTKTHREADS-VERSION

# This hook installs dynamically loadable libraries
install-exec-hook:
	$(mkinstalldirs) $(subpkgdatadir)
	(. ./libguilegtkthreads.la; \
	 test -z "$$dlname" \
	 || test ! -r $(libdir)/$$dlname \
	 || (cd $(subpkgdatadir) \
	     && rm -f libguilegtkthreads.la $$dlname \
	     && $(LN_S) $(libdir)/libguilegtkthreads.la \
	     && $(LN_S) $(libdir)/$$dlname))

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
