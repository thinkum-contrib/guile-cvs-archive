## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign

## Add our own -I option.
# MAKEINFO = makeinfo -I ../tutorial

info_TEXINFOS = guile.texi r4rs.texi r5rs.texi goops.texi

## Just a convenience var; no special significance.
## Most of these files are outdated and have been moved into a `sources' dir.
#dist_texis = guile.texi intro.texi \
#	ci.texi			libguile-tools.texi	scheme-concepts.texi \
#	gh-ref.texi		scm-ref.texi \
#	new-types.texi		ti.texi 		debug-c.texi \
#	guile-slib.texi		pi.texi			tk.texi \
#	data-rep.texi		unix.texi		version.texi \
#	rx.texi			vi.texi 		debug-scheme.texi \
#	libguile-overview.texi	sample-APIs.texi	using.texi \
#	contributors.texi	slib.texi		strings.texi \
#	unix-other.texi		guile-scheme.texi

dist_texis = guile.texi goops.texi intro.texi scheme.texi posix.texi \
	expect.texi scsh.texi tcltk.texi scripts.texi gh.texi scm.texi \
	goops-tutorial.texi hierarchy.eps hierarchy.txt \
	appendices.texi indices.texi

## FIXME: for now we just omit ../guile-user/more-resources.texi from
## the dependencies.  We don't want to try to put it into the
## distribution.
guile_ref_TEXINFOS = $(dist_texis)

EXTRA_DIST = $(dist_texis)
