#!/bin/sh

[ -f ltdl.c ] || {
  echo "autogen.sh: run this command only in the libltdl directory."
  exit 1
}

aclocal
autoheader
autoconf
automake
