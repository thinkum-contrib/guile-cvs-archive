## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libgoops.la
bin_PROGRAMS = guile-oops

guile_oops_SOURCES = guile-oops.c
guile_oops_LDADD = libgoops.la

libgoops_la_SOURCES = goops.c
BUILT_SOURCES = goops.x

subpkgdatadir = $(datadir)/site/goops
subpkgdata_DATA = 

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
