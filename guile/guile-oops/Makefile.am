## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libgoops.la
bin_PROGRAMS = guile-oops

guile_oops_SOURCES = guile-oops.c
guile_oops_LDADD = libgoops.la

libgoops_la_SOURCES = goops.c
libgoops_la_LDFLAGS = -version-info 0:0 -export-dynamic
BUILT_SOURCES = goops.x

subpkgdatadir = $(datadir)/guile
subpkgdata_DATA = goops/goops.scm goops/compat.scm

goopsdir = $(subpkgdatadir)/goops

install-exec-hook:
	$(mkinstalldirs) $(goopsdir)
	(test ! -r $(libdir)/libgoops.so.0 \
	 || (cd $(goopsdir) \
	     && rm -f libgoops.la libgoops.so.0 \
	     && $(LN_S) $(libdir)/libgoops.la \
	     && $(LN_S) $(libdir)/libgoops.so.0))

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
