SUBDIRS = lightning

INCLUDES = -I ./lightning

lib_LTLIBRARIES = libguile-lightning.la

libguile_lightning_la_SOURCES = lightning.c \
                                disassemble.h \
                                disassemble.c \
                                i386-dis.c

BUILT_SOURCES = assemble-ops.c lightning.x
CLEANFILE = lightning.x

SUFFIXES = .x
.c.x:
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< >$<.tmp && mv $<.tmp $@

assemble-ops.c: rod.scm lightning.ops
	guile -s rod.scm >$@
