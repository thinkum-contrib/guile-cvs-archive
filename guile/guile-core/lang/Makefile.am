## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

# These should be installed and distributed.
lang_sources = elisp.scm

subpkgdatadir = $(pkgdatadir)/$(VERSION)/lang
subpkgdata_DATA = $(lang_sources)
ETAGS_ARGS = $(subpkgdata_DATA)

## test.scm is not currently installed.
EXTRA_DIST = $(lang_sources)
