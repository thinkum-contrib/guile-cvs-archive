## Process this file with automake to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign

# These should be installed and distributed.
comp_sources = ghobbit.scm compile.scm

subpkgdatadir = $(pkgdatadir)/$(VERSION)/comp
subpkgdata_DATA = $(comp_sources)
ETAGS_ARGS = $(subpkgdata_DATA)

EXTRA_DIST = $(comp_sources)
