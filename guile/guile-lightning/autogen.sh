#!/bin/sh

aclocal
libtoolize --copy --automake
autoconf
automake --add-missing
