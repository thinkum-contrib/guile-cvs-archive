DISTCLEANFILES = asm.h core.h funcs.h fp.h

BACKENDS = i386 sparc ppc

if LIGHTNING_MAIN
lightningdir = $(includedir)/lightning

lightning_HEADERS = funcs-common.h core-common.h fp-common.h asm-common.h \
	asm.h core.h funcs.h fp.h
else
noinst_HEADERS = funcs-common.h core-common.h fp-common.h asm-common.h
endif

dist-hook:
	cd $(distdir); rm -f $(DISTCLEANFILES) || true
	for i in $(BACKENDS); do \
	  mkdir $(distdir)/$$i; \
	  for j in $(DISTCLEANFILES); do \
	    cp $(srcdir)/$$i/$$j $(distdir)/$$i; \
	  done; \
	done
