## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

subpkgdatadir = $(pkgdatadir)/$(VERSION)/@module@
subpkgdata_DATA = c-ops.scm grammar.scm hashtabs.scm lexer.scm macros.scm \
reader.scm scm-ops.scm

EXTRA_DIST = PLUGIN/REQ $(subpkgdata_DATA)
