#### Makefile.in template for guile-core/build.
#### Jim Blandy <jimb@red-bean.com> --- September 1997

bin_SCRIPTS=build-guile
CLEANFILES=build-guile
EXTRA_DIST=build-guile.in

## We use @-...-@ as the substitution brackets here, instead of the
## usual @...@, so autoconf doesn't go and substitute the values
## directly into the left-hand sides of the sed substitutions.  *sigh*
build-guile: build-guile.in
	rm -f build-guile.tmp
	sed < ${srcdir}/build-guile.in > build-guile.tmp \
	    -e s:@-bindir-@:${bindir}: \
	    -e s:@-GUILE_VERSION-@:${GUILE_VERSION}:
	chmod +x build-guile.tmp
	mv build-guile.tmp build-guile
