## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = debian

lib_LTLIBRARIES = libgoopscore.la
bin_PROGRAMS = guile-oops

guile_oops_SOURCES = guile-oops.c
guile_oops_DEPENDENCIES = @LIBLOBJS@
guile_oops_LDADD = @LIBLOBJS@ libgoopscore.la @LIBQTHREADS@

libgoopscore_la_SOURCES = goops.c
libgoopscore_la_LDFLAGS = -version-info @LIBGOOPSCORE_MAJOR_VERSION@:@LIBGOOPSCORE_MINOR_VERSION@:@LIBGOOPSCORE_REVISION_VERSION@ -export-dynamic
BUILT_SOURCES = goops.x fdi.x

EXTRA_guile_oops_SOURCES = fdi.c

include_HEADERS = goops.h goops-snarf.h

modincludedir = $(includedir)/goops
## This file is generated at configure time.  That is why it is DATA
## and not a header -- headers are included in the distribution.
modinclude_DATA = versiondat.h

subpkgdatadir = $(datadir)/guile/@GUILEVERSION@/oop
subpkgdata_DATA = goops.scm

goops_sources = dispatch.scm compile.scm util.scm stklos.scm \
		describe.scm internal.scm active-slot.scm composite-slot.scm \
		save.scm

goopsdatadir = $(datadir)/guile/@GUILEVERSION@/oop/goops
goopsdata_DATA = $(goops_sources)

info_TEXINFOS = goops.texi
goops_TEXINFOS = goops-tutorial.texi hierarchy.eps hierarchy.txt

man_MANS = guile-oops.1

miscdocs = \
	ANON-CVS COPYRIGHTS HACKING SNAPSHOTS TAGS TODO mop.text guile-oops.1

EXTRA_DIST = \
	$(subpkgdata_DATA) $(goops_sources) $(miscdocs) GOOPS-VERSION relpath

RELPATH = guile -e main -s $(srcdir)/relpath

install-exec-hook:
	$(mkinstalldirs) $(goopsdatadir)
	(. ./libgoopscore.la; \
	 relpath=`$(RELPATH) $(goopsdatadir) $(libdir)`; \
	 test -z "$$dlname" \
	 || test ! -r $(libdir)/$$dlname \
	 || (cd $(goopsdatadir) \
	     && rm -f libgoopscore.la $$dlname \
	     && $(LN_S) $$relpath/libgoopscore.la \
	     && $(LN_S) $$relpath/$$dlname))

dist-hook: goops.ps
	cp -p $(top_builddir)/goops.ps $(distdir)

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

DISTCLEANFILES = *.x
