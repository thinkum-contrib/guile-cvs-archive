## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign

info_TEXINFOS = guile.texi r4rs.texi r5rs.texi goops.texi

guile_TEXINFOS = preface.texi intro.texi scheme-intro.texi \
	scheme-ideas.texi scheme-data.texi scheme-procedures.texi \
	scheme-utility.texi scheme-binding.texi scheme-control.texi \
	scheme-io.texi scheme-evaluation.texi scheme-memory.texi \
	scheme-modules.texi scheme-scheduling.texi scheme-options.texi \
	scheme-translation.texi scheme-debug.texi deprecated.texi \
	scheme-reading.texi scheme-indices.texi slib.texi posix.texi \
	expect.texi scsh.texi tcltk.texi scripts.texi gh.texi scm.texi \
	appendices.texi indices.texi

goops_TEXINFOS = goops-tutorial.texi hierarchy.eps hierarchy.txt

# Optionally support building an HTML version of the reference manual.

if HTML

htmldir = $(prefix)/html/guile-$(VERSION)

html_DATA = guile_toc.html $(shell ls guile_*.html 2>/dev/null)

guile_toc.html: guile.texi $(guile_TEXINFOS)
	$(TEXI2HTML) -split_chapter guile.texi

endif
