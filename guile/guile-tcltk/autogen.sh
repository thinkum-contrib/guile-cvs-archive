#!/bin/sh

# Generate the building cruft for a freshly checked out guile-gtk
# module.

if test -z "$AUTOMAKE" ; then
 for each in automake-1.7 automake-1.6 automake ; do
   AUTOMAKE=$each
   if test -n `which $each` ; then break ; fi
 done
fi

./guile-tcltk-aclocal &&
libtoolize --copy --automake &&
autoconf && 
AUTOMAKE --add-missing &&
echo Now run configure and make.
