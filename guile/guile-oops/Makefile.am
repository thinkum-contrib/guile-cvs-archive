## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libgoopscore.la
bin_PROGRAMS = guile-oops

guile_oops_SOURCES = guile-oops.c
guile_oops_LDADD = libgoopscore.la

libgoopscore_la_SOURCES = goops.c
libgoopscore_la_LDFLAGS = -version-info 0:0 -export-dynamic
BUILT_SOURCES = goops.x

subpkgdatadir = $(datadir)/guile
subpkgdata_DATA = goops/goops.scm goops/compat.scm goops/describe.scm

goopsdir = $(subpkgdatadir)/oop

install-exec-hook:
	$(mkinstalldirs) $(goopsdir)
	(test ! -r $(libdir)/libgoopscore.so.0 \
	 || (cd $(goopsdir) \
	     && rm -f libgoopscore.la libgoopscore.so.0 \
	     && $(LN_S) $(libdir)/libgoopscore.la \
	     && $(LN_S) $(libdir)/libgoopscore.so.0))

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
