#!/bin/sh

if test -z "$ACLOCAL" ; then
 for each in aclocal-1.7 aclocal-1.6 aclocal ; do
   ACLOCAL=$each
   if test -n `which $each` ; then break ; fi
 done
fi

ACDIR=`which $ACLOCAL`
ACDIR=`dirname $ACDIR`
ACDIR=`dirname $ACDIR`/share/aclocal
GUDIR=`guile-config info prefix`/share/aclocal

for each in $GUDIR $ACDIR ; do
  if test -d "$each"  ; then 
    AFLAGS="-I $each $AFLAGS"
  fi
done

echo $ACLOCAL $AFLAGS $@
$ACLOCAL $AFLAGS $@
