## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(srcdir)/../libguile -I$(srcdir)/.. -I../

lib_LIBRARIES = librx.a
librx_a_SOURCES = rx.h hashrexp.c inst-rxposix.h rgx.c rgx.h rx.c \
rx.h rxall.h rxanal.c rxanal.h rxbasic.c rxbasic.h rxbitset.c rxbitset.h \
rxcontext.h rxcset.c rxcset.h rxdbug.c rxgnucomp.c rxgnucomp.h rxhash.c \
rxhash.h rxnfa.c rxnfa.h rxnode.c rxnode.h rxposix.c rxposix.h rxproto.h \
rxsimp.c rxsimp.h rxspencer.c rxspencer.h rxstr.c rxstr.h rxsuper.c \
rxsuper.h rxunfa.c rxunfa.h _rx.h

check_PROGRAMS = runtests
runtests_SOURCES = runtests.c testcases.h
runtests_LDADD = librx.a

$(srcdir)/testcases.h: TESTS TESTS2C.sed
	sed -f $(srcdir)/TESTS2C.sed $(srcdir)/TESTS > $(srcdir)/testcases.h

EXTRA_DIST = TESTS TESTS2C.sed W32 PLUGIN/OPT PLUGIN/guile.config \
PLUGIN/guile.inits PLUGIN/guile.libs

SUFFIXES = .x
.c.x:
	../libguile/guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
