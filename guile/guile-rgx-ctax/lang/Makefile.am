## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

subpkgdatadir = $(pkgdatadir)/$(VERSION)/@module@
subpkgdata_DATA = grammar.scm lex.scm lr0.scm lr1.scm pp.scm

EXTRA_DIST = PLUGIN/REQ $(subpkgdata_DATA)
