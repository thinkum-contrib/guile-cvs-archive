AC_DEFUN(LIGHTNING_CONFIGURE, [
AC_REQUIRE([AC_PROG_LN_S])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AM_CONDITIONAL(LIGHTNING_MAIN, false)

lightning=

case "$host_cpu" in
	i?86)	 cpu_subdir=i386					;;
	sparc)	 cpu_subdir=sparc					;;
	powerpc) cpu_subdir=ppc						;;
	*)	 AC_ERROR(GNU lightning does not support your CPU)	;;
esac

if test -n "$cpu_subdir"; then
    lightning=yes
fi

AC_OUTPUT_COMMANDS([
  if test -n "$cpu_subdir"; then
    for i in asm fp core funcs; do
      echo linking lightning/$cpu_subdir/$i.h to lightning/$i.h 
      (cd lightning && $LN_S -f $cpu_subdir/$i.h $i.h)
    done
  fi
], [
  LN_S='$LN_S'
  cpu_subdir=$cpu_subdir
])

if test -n "$lightning"; then
  lightning=
  ifelse([$1], , :, [$1])
else
  ifelse([$2], , :, [$2])
fi

])dnl
