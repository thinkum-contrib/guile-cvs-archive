/*
 * Heap allocation and garbage collection
 *
 * $Id$
 */
/*-- heap variables */
extern SOBJ scm_hbase, scm_hptr, scm_hlimit, scm_hfree, scm_hwater;
extern long	scm_hsize;
extern int gcmarked, gcfree;

/*-- proto */
void        *scm_must_alloc(long size);
void        *scm_must_realloc(void *p, long size);
void        scm_gc_protect(SOBJ *location);
void        scm_gc_mark(SOBJ obj);
void        scm_gc();
SOBJ        scm_newcell(int type);
void        scm_heap_init(long size);
void        scm_heap_stat();
