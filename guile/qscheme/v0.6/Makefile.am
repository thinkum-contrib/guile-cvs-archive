
# options
# AUTOMAKE_OPTIONS = no-installinfo no-installman no-texinfo.tex

CFLAGS=@CFLAGS@

# Import configure variables
libavcall=@AVCALL_LIB@
libpcre=@PCRE_LIB@
libgmp=@GMP_LIB@

incavcall=@AVCALL_INC@
incpcre=@PCRE_INC@
incgmp=@GMP_INC@

CFLAGS += -I$(incavcall) -I$(incgmp)

# What we need to build libqs.a
libqs_src = array.c asm.c atom.c boolean.c catch.c chr.c code.c cont.c \
	dyn.c  env.c hash.c heap.c list.c lsym.c macro.c misc.c module.c \
	number.c pointer.c port.c proc.c s.c str.c sym.c variable.c vm2.c \
	process.c format.c thread.c file.c vm3.c

libqs_obj = array.o asm.o atom.o boolean.o catch.o chr.o code.o cont.o \
	dyn.o  env.o hash.o heap.o list.o lsym.o macro.o misc.o module.o \
	number.o pointer.o port.o proc.o s.o str.o sym.o variable.o vm2.o \
	process.o format.o thread.o file.o vm3.o

libqs_hdr = s.h sproto.h stack.h vm2.h asm.h cont.h heap.h port.h proc.h vm3.h

vmfiles = prim2.i number.i prim2.x number.x

modobj = regex.so object.so process.so struct.so socket.so unix.so
modsrc = regex.c object.c process.c struct.c socket.c \
		 object.h struct.h unix.c

scmsrc = s.scm macro.scm loop.scm wloop.scm fib.scm cat.scm

# supplementary files to clean
CLEANFILES = $(modobj)

# supplementary files to include in distribution
EXTRA_DIST = mkoptable mkwhatis $(modsrc) $(scmsrc) CHANGES LICENCE_EXCEPTION

# other directories
SUBDIRS = . doc sgtk

noinst_LIBRARIES = libqs.a

shared_dir=$(prefix)/share/qscheme/$(VERSION)

scmobjdir = $(shared_dir)
scmobj_DATA = $(modobj) $(scmsrc)

bin_PROGRAMS = qscheme

qscheme_SOURCES 	= smain.c
qscheme_LDADD   	= libqs.a $(libavcall) $(libgmp)
qscheme_LDFLAGS		= -rdynamic
qscheme_DEPENDENCIES= $(modobj) $(modsrc) $(vmfiles) libqs.a

libqs_a_SOURCES = $(libqs_src) $(libqs_hdr) $(vmfiles)

%.so:%.c
	$(COMPILE) -o $@ -shared -fPIC $<

libqs.so: $(libqs_obj)
	$(LINK) -shared $(libqs_obj) @LIBS@

regex.so: regex.c 
	$(COMPILE) -o $@ -shared -fPIC -I$(incpcre) $(srcdir)/regex.c $(PCRE_LIB)

%.x:%.i
	mkoptable $< > $@

sproto.h: $(libqs_src)
	@cfunctions `echo $(libqs_src)|sed 's|vm3..||'` \
		| sed 's/^#include "c-extensions.h".*$$/#define PROTO(x) x/' > $@.new
	@if diff $@ $@.new >/dev/null ; then rm $@.new; else mv $@.new $@ ; fi

vm3.h: vm3-ops.h

vm3-ops.h: vm3.c
	@echo "creating vm3-ops.h"
	@perl -ne 'while(/DECL\((\w+),/g) {print "\tSCM_OP_\U$$1,\n";}' vm3.c>vm3-ops.h

# force sproto.h to be up to date. Otherwise compilation of
# distribution may fail, because sproto.h can trigger the cfunction
# program which is not included in distribution.

dist-hook:
	touch $(distdir)/sproto.h

# force the x bit on the shared libraries
install-data-local:
	chmod +x $(shared_dir)/*.so

whatis: whatis.qs

whatis.qs: $(libqs_src)
	@perl mkwhatis $(libqs_src) *.i > whatis.qs

prev=$(shell cat PREV_VERSION)

publish: dist
	cp qscheme-@VERSION@.tar.gz arch
	cp CHANGES arch
	(cd arch && \
	 tar xzf qscheme-@VERSION@.tar.gz && \
	 tar xzf qscheme-$(prev).tar.gz && \
	 diff -Naur qscheme-$(prev) qscheme-@VERSION@|gzip> diff-$(prev)-@VERSION@.gz && \
	 rm -r qscheme-@VERSION@ qscheme-$(prev) )
	(cd ~/mpp/raw/qscheme/files && \
	 rm -f CHANGES qscheme-@VERSION@.tar.gz	diff-$(prev)-@VERSION@.gz && \
	 ln -s $(HOME)/devel/qscheme-0.5/arch/qscheme-@VERSION@.tar.gz qscheme-@VERSION@.tar.gz && \
	 ln -s $(HOME)/devel/qscheme-0.5/arch/CHANGES CHANGES && \
	 ln -s $(HOME)/devel/qscheme-0.5/arch/diff-$(prev)-@VERSION@.gz diff-$(prev)-@VERSION@.gz && \
	 touch ../download.mpp)
	
