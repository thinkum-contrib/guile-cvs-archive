## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libgoopscore.la
bin_PROGRAMS = guile-oops

guile_oops_SOURCES = guile-oops.c
guile_oops_DEPENDENCIES = @LIBLOBJS@
guile_oops_LDADD = @LIBLOBJS@ libgoopscore.la

libgoopscore_la_SOURCES = goops.c goops.h goops-snarf.h
libgoopscore_la_LDFLAGS = -version-info 0:0 -export-dynamic
BUILT_SOURCES = goops.x fdi.x

EXTRA_guile_oops_SOURCES = fdi.c

include_HEADERS = goops.h goops-snarf.h

subpkgdatadir = $(datadir)/guile/oop
subpkgdata_DATA = goops.scm

goopsdatadir = $(datadir)/guile/oop/goops
goopsdata_DATA = describe.scm internal.scm active-slot.scm composite-slot.scm

install-exec-hook:
	$(mkinstalldirs) $(goopsdatadir)
	(test ! -r $(libdir)/libgoopscore.so.0 \
	 || (cd $(goopsdatadir) \
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
