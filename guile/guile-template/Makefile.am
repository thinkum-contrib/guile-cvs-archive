## Process this file with Automake to create Makefile.in

#
# Replace PACKAGE, CFILE1 ..., and SCMFILE1 ...
#

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libPACKAGE.la
bin_PROGRAMS = guile-PACKAGE

libPACKAGE_la_SOURCES = CFILE1.c ...
libPACKAGE_la_LDFLAGS = -version-info 0:0 -export-dynamic
BUILT_SOURCES = CFILE1.x ...

subpkgdatadir = $(datadir)/guile/PACKAGE
subpkgdata_DATA = SCMFILE.scm ...

# This hook installs dynamically loadable libraries
install-exec-hook:
	$(mkinstalldirs) $(subpkgdatadir)
	(test ! -r $(libdir)/libPACKAGE.so.0 \
	 || (cd $(subpkgdatadir) \
	     && rm -f libPACKAGE.la libPACKAGE.so.0 \
	     && $(LN_S) $(libdir)/libPACKAGE.la \
	     && $(LN_S) $(libdir)/libPACKAGE.so.0))

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
