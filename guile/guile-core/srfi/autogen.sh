#!/bin/sh

[ -f srfi-13.scm ] || {
  echo "autogen.sh: run this command only in the srfi directory."
  exit 1
}

../guile-aclocal.sh

libtoolize --copy --automake
autoconf
automake --add-missing
