scripts =					\
	checkout-guile-cvs-module		\
	nightly					\
	update-anon-cvs-public			\
	dist-guile-www				\
	update-anon-cvs

# Yes, sometimes people will check in files w/o +x -- that's life.

all:
	chmod +x $(scripts)

# Use guile's scripts, too.  Users can't have all the fun.

gscripts = ../core/scripts
gscripts-symlinks:
	rm -f gscripts
	ln -s $(gscripts) gscripts
	eval `find gscripts -follow -type f -perm '+''+x' -printf "ln -sf %p %f; "`
