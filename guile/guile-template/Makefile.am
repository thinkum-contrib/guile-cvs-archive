## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libPACKAGE.la
bin_PROGRAMS = guile-PACKAGE

libPACKAGE_la_SOURCES = 
BUILT_SOURCES = .x

subpkgdatadir = $(datadir)/site/PACKAGE
subpkgdata_DATA = .scm

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
