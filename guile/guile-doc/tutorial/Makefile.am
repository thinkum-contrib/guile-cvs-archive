## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-tut.texi

# Optionally support building an HTML version of the tutorial.

if HTML

htmldir = $(prefix)/html/guile-$(VERSION)

html_DATA = guile-tut_toc.html $(shell ls guile-tut_*.html 2>/dev/null)

guile-tut_toc.html: guile-tut.texi
	$(TEXI2HTML) -split_chapter guile-tut.texi

endif
