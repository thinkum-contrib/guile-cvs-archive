/* acconfig.h --- documentation for symbols possibly defined in scmconfig.h
   Jim Blandy <jimb@cyclic.com> --- August 1996 */

/* Define these two if you want support for debugging of Scheme
   programs.  */
#undef DEBUG_EXTENSIONS
#undef READER_EXTENSIONS

/* Define this if your system has a way to set a stdio stream's file
   descriptor.  You should also copy fd.h.in to fd.h, and give the
   macro SET_FILE_FD_FIELD an appropriate definition.  See
   configure.in for more details.  */
#undef HAVE_FD_SETTER

/* Define this if your system has a way to set a stdio stream's file
   descriptor.  You should also copy fd.h.in to fd.h, and give the
   macro SET_FILE_FD_FIELD an appropriate definition.  See
   configure.in for more details.  */
#undef HAVE_FD_SETTER

/* Set this to the name of a field in FILE which contains the number
   of buffered characters waiting to be read.  */
#undef FILE_CNT_FIELD

/* Define this if your stdio has _gptr and _egptr fields which can
   be compared to give the number of buffered characters waiting to
   be read.  */
#undef FILE_CNT_GPTR

/* Define this if your stdio has _IO_read_ptr and _IO_read_end fields
   which can be compared to give the number of buffered characters
   waiting to be read.  */
#undef FILE_CNT_READPTR

/* Define this if your system defines struct linger, for use with the
   getsockopt and setsockopt system calls.  */
#undef HAVE_STRUCT_LINGER

/* Define this if floats are the same size as longs.  */
#undef SCM_SINGLES

/* Define this if a callee's stack frame has a higher address than the
   caller's stack frame.  On most machines, this is not the case.  */
#undef SCM_STACK_GROWS_UP

/* Define this if <utime.h> doesn't define struct utimbuf unless
   _POSIX_SOURCE is #defined.  */
#undef UTIMBUF_NEEDS_POSIX

/* Define these to indicate the current version of Guile.  These
   values are supposed to be supplied by the configuration system.  */
#undef GUILE_MAJOR_VERSION
#undef GUILE_MINOR_VERSION
#undef GUILE_VERSION

/* Define if using cooperative multithreading.  */
#undef USE_COOP_THREADS

/* Define if using "FSU" pthreads.  */
#undef USE_FSU_PTHREADS

/* Define if using MIT pthreads.  */
#undef USE_MIT_PTHREADS

/* Define if using PCthreads pthreads.  */
#undef USE_PCTHREADS_PTHREADS

/* Define if using any sort of threads.  */
#undef USE_THREADS
