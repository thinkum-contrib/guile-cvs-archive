## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

lib_LTLIBRARIES = libgoopscore.la
bin_PROGRAMS = guile-oops

guile_oops_SOURCES = guile-oops.c
guile_oops_DEPENDENCIES = @LIBLOBJS@
guile_oops_LDADD = @LIBLOBJS@ libgoopscore.la

libgoopscore_la_SOURCES = goops.c
libgoopscore_la_LDFLAGS = -version-info 1:0 -export-dynamic
BUILT_SOURCES = goops.x fdi.x

EXTRA_guile_oops_SOURCES = fdi.c

include_HEADERS = goops.h goops-snarf.h

modincludedir = $(includedir)/goops
## This file is generated at configure time.  That is why it is DATA
## and not a header -- headers are included in the distribution.
modinclude_DATA = versiondat.h

subpkgdatadir = $(datadir)/guile/oop
subpkgdata_DATA = goops.scm

goops_sources = dispatch.scm compile.scm util.scm stklos.scm \
		describe.scm internal.scm active-slot.scm composite-slot.scm

goopsdatadir = $(datadir)/guile/oop/goops
goopsdata_DATA = $(goops_sources)

info_TEXINFOS = goops.texi
goops_TEXINFOS = goops-tutorial.texi hierarchy.eps hierarchy.txt

EXTRA_DIST = ANON-CVS COPYRIGHTS GOOPS-VERSION HACKING SNAPSHOTS TAGS TODO \
             goops.scm $(goops_sources) mop.text
	$(LIBTOOL) --mode=install $(INSTALL) libgoopscore.la \
		$(goopsdatadir)/libgoopscore.la

install-exec-hook:
	$(mkinstalldirs) $(goopsdatadir)
	(. ./libgoopscore.la; \
	 test -z "$$dlname" \
	 || test ! -r $(libdir)/$$dlname \
	 || (cd $(goopsdatadir) \
	     && rm -f libgoopscore.la $$dlname \
	     && $(LN_S) $(libdir)/libgoopscore.la \
	     && $(LN_S) $(libdir)/$$dlname))

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
