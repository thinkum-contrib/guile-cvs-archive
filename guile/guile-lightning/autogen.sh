#!/bin/sh
# Run this to generate all the initial makefiles, etc.

# Generate the building cruft for a freshly checked out guile-gtk
# module.

##
## NOTE: before changing anything here, please read README.gnome-guile
##

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="guile-gtk"

(test -f $srcdir/configure.in \
  && test -f $srcdir/gdk-1.2.defs ) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

USE_GNOME2_MACROS=1 . gnome-autogen.sh
