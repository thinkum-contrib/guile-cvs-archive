## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

INCLUDES = $(GUILE_CFLAGS)

lib_LTLIBRARIES = libguile-tcltk-gtcltk.la libguiledlopenhelper.la
bin_PROGRAMS = guile-tcltk

libguile_tcltk_gtcltk_la_SOURCES = guile-tcl.c guile-tk.c \
		       guile-tcl.h guile-tk.h gtcltk-module.c
EXTRA_libguile_tcltk_gtcltk_la_SOURCES = tclInt.h tclUnixPort.h tclUnixNotfy.c
BUILT_SOURCES = guile-tcl.x guile-tk.x
libguile_tcltk_gtcltk_la_DEPENDENCIES = @LIBLOBJS@
libguile_tcltk_gtcltk_la_LIBADD = @LIBLOBJS@
libguile_tcltk_gtcltk_la_LDFLAGS = -version-info 1:0 -export-dynamic

guile_tcltk_LDADD = libguile-tcltk-gtcltk.la $(GUILE_LIBS)

libguiledlopenhelper_la_SOURCES = guiledlopenhelper.c
libguiledlopenhelper_la_LDFLAGS = -version-info 0:0:0 -export-dynamic

modincludedir = $(includedir)/guile-tcltk
modinclude_HEADERS = guile-tcl.h guile-tk.h

subpkgdatadir = @TCLTKDIR@
subpkgdata_DATA = tcltk.scm dynlink.scm gtcltk.scm \
	          hand.scm graph.scm breakout.scm

# install-exec-hook:
# 	$(mkinstalldirs) $(subpkgdatadir)
# 	(test ! -r $(libdir)/libgtcltk.so.1 \
# 	 || (cd $(subpkgdatadir) \
# 	     && rm -f libgtcltk.la libgtcltk.so.1 \
# 	     && $(LN_S) $(libdir)/libgtcltk.la \
# 	     && $(LN_S) $(libdir)/libgtcltk.so.1))

EXTRA_DIST = license.terms $(subpkgdata_DATA)

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS) $< > $@

COMPILE = $(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS)
