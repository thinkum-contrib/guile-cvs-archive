/* -*- tab-width:4; -*- */
/*
 * Heap allocation and garbage collection
 *
 * $Id$
 */
#include "s.h"
/* #include "gc.h" */
#include <string.h>

/* #define DEBUG_GC_PHASE */
/* #define SUPER_SAFE_GC */

int scm_gc_verbose = FALSE;
int scm_in_gc = 0;				/* true during gc */

struct gc_protected {
  SOBJ *location;				/* location to protect */
  struct gc_protected *next;
};

static struct gc_protected *protected_cells; /* list of protected cell */

/* This is the heap block descriptor.
 *
 * A heap block is a memory block where we store Sobject structure.
 *
 * Note: The mblock pointer points to the block allocated with
 * malloc. Because of odd alignement, the base pointer may point to a
 * different address.
 * */

typedef struct _SCM_HEAP_BLOCK {
  struct _SCM_HEAP_BLOCK 	*next;		/* next heap block */
  int		size;				/* size of this block */
  void		*mblock;			/* memory block obtained with alloc */
  SOBJ 		base;				/* points to base of heap */
  SOBJ 		limit;				/* points to limit of heap */
  SOBJ		ptr;				/* points to next cell to be allocated */
} SCM_HEAP_BLOCK;

/* List of heap block descriptor. The newest heap block are first in
 * list.
 */

static SCM_HEAP_BLOCK *scm_heap_list;

/* List of free cells contained between base and ptr of all heap
 * blocks. after gc(), free nodes from the older heap are on top of
 * the free list, so we try to reuse space from old heap first.
 */

static SOBJ scm_hfree;

/*-- heap variables */
#ifdef OLD
SOBJ 	scm_hbase, scm_hptr, scm_hlimit, scm_hfree, scm_hwater;
long	scm_hsize;
#endif

/* This variable holds statistic of gc run */
int 	gcmarked, gcfree;

/* This keeps track of successive calls to scm_must_alloc / scm_free 
 */
long	scm_cells_allocated;
long	scm_malloc_allocated;
long	scm_global_heap_size;	/* size of all heap block */

/****************************************************************
 * Malloc utilities
 ****************************************************************/

/* Alloc a memory block or exit with an error message.
 */
void *scm_must_alloc(long size)
{
  void *p = malloc(size);
#ifdef DEBUG
  fprintf(stderr, "scm_must_alloc: %ld bytes\n", size);
#endif
  if (p == NULL) {
	fprintf(stderr, "out of memory\n");
	exit(1);
  }
  memset(p, 0, size);
  scm_malloc_allocated++;
  return(p);
}

/* Alloc a fresh block and copy data from the old one.
 */
void *scm_must_realloc(void *mem, long size)
{
  void *p = realloc(mem, size);
  if (p == NULL) {
	fprintf(stderr, "out of memory\n");
	exit(1);
  }
  return(p);
}

/* Alloc a new block and copy a string into it.
 */
char *scm_must_strdup(char *str)
{
  char *p = scm_must_alloc(strlen(str) + 1);
  strcpy(p, str);
  return(p);
}

/* Clear a memory block
 */
void scm_mem_clear(void *mem, long size)
{
  memset(mem, 0, size);
}

/* Release a memory block. Decrements the scm_malloc_allocated counter
 */
void scm_free(void *mem)
{
  if (mem) { scm_malloc_allocated--;  free(mem); }
}

/****************************************************************
 * Heap blocks utilities
 ****************************************************************/

/* Add a new heap block in front of the heap list
 */
static void scm_add_heap_block(int size)
{
  SCM_HEAP_BLOCK *hb = scm_must_alloc(sizeof(SCM_HEAP_BLOCK));

  /* 2 more byte for alignement purpose */
  hb->mblock = scm_must_alloc((size * sizeof(Sobject)) + 2);

  hb->size = size;
  hb->base  = hb->mblock + ((long)hb->mblock & 1);
  hb->limit = hb->base + size;
  hb->ptr   = hb->base;
  hb->next  = scm_heap_list;
  scm_heap_list = hb;
  scm_global_heap_size += size;
}

/* Display some statistics about usage of a block heap
 */
static void scm_heap_block_stats(SCM_HEAP_BLOCK *h)
{
  scm_puts("; heap block @");  scm_putx(h);
  scm_puts(" size=");  scm_putn(h->size);
  scm_puts(" used=");  scm_putn(h->ptr - h->base);
  scm_puts(" free=");  scm_putn(h->limit - h->ptr);
  scm_puts("\n");
}

/* Display stat about all heap block */
void scm_heap_stat()
{
  SCM_HEAP_BLOCK *h;
  for (h = scm_heap_list; h; h = h->next) {
	scm_heap_block_stats(h);
  }
  scm_puts("; malloced "); scm_putn(scm_malloc_allocated);
  scm_puts("\n; gc: marked ");  scm_putn(gcmarked);
  scm_puts(", free ");			scm_putn(gcfree);
  scm_puts("\n");
}

/* Test if a pointer is a valid member of any heap block of the heap
 * block list.
 *
 * The pointer is tested against the heap block limit and also if it
 * points to a valid object. An valid pointer object is correctly
 * aligned and points to a valid Sobject structure.
 */
int scm_is_pointer_to_heap(void *p)
{
  SCM_HEAP_BLOCK *h;

  if ( (long)p & 1)				/* ignore odd pointers */
	return(FALSE);

  for (h = scm_heap_list; h; h = h->next) {
	if ((SOBJ)p >= h->base && (SOBJ)p < h->ptr &&
		((p - (void*)h->base) % sizeof(Sobject)) == 0)
	  return(TRUE);
  }
  return(FALSE);
}

/****************************************************************
 * Garbage collection
 ****************************************************************/

/* Keep track of pointer to objects which are not stored in the heap.
 * Note: reference are not stored twice.
 */
void scm_gc_protect(SOBJ *location)
{
  struct gc_protected *p;

  for (p = protected_cells; p; p = p->next) {
	if (p->location == location) {
#ifdef DEBUG
	  fprintf(stderr, "gc_protect: location %p already registered\n",
			  p->location);
#endif
	  return;
	}
  }

  p = (struct gc_protected *) scm_must_alloc(sizeof(struct gc_protected));
  
  p->location = location;
  p->next = protected_cells;
  protected_cells = p;
}

#ifdef SUPER_SAFE_GC
static int scm_is_protected(SOBJ obj)
{
  struct gc_protected *p;
  printf("scm_is_protected: %p\n", obj);
  for (p = protected_cells; p; p = p->next) {
	if (*(p->location) == obj)
	  return(TRUE);
  }
  return(FALSE);
}
#endif

/*-- mark a cell as used */

void scm_gc_mark(SOBJ obj)
{
  int t;
  
  if (obj == NULL || SCM_INUMP(obj) || SCM_GCBIT(obj))
	return;

  t = SCM_OBJTYPE(obj);
  SCM_GCBIT_SET(obj);
  gcmarked++;
  switch(t) { 
  case SOBJ_T_PAIR:		scm_gc_mark(SCM_CAR(obj)); scm_gc_mark(SCM_CDR(obj));
	return;
  case SOBJ_T_BNUM:		return;
  case SOBJ_T_FNUM:		return;
  case SOBJ_T_LSYMBOL:
	
	/* Do we really need to mark atoms that are anyway referenced by
	 * the atom_hash ? :)
	 */
	/* scm_gc_mark(SCM_LSYM_SYM(obj)); */
	return;
  case SOBJ_T_CPRIM:	return;

  default:
#ifdef SUPER_SAFE_GC
	if (!scm_is_pointer_to_heap(obj) &&
		!((obj >= (SOBJ)scm_sp) && (obj < (SOBJ)scm_stack_limit)) &&
		!scm_is_protected(obj)) {
	  fprintf(stderr, "OOPS: object %p (type=%d) does not point to heap\n",
			  obj, obj->type & 0x3fff);
	}
#endif
	if (t < SOBJ_T_MAX && scm_type_hook[t].mark != NULL)
	  (*scm_type_hook[t].mark)(obj);
  }
}

/* Mark location from protected object list
 */
static void scm_gc_mark_protected()
{
  struct gc_protected *p;
  
  p = protected_cells;
  while(p) {
	scm_gc_mark(*(p->location));
	p = p->next;
  }
}

/* Mark pointers that are stored on the VM stack.
 */
static void scm_gc_mark_stacks()
{
  SOBJ p, *pp;
  SCM_VMD *v = scm_vmd();

  pp = v->reg.sp;
  while(pp < v->stack_limit) {
	p = *pp++;
	if (scm_is_pointer_to_heap(p)) 	scm_gc_mark(p);
  }
}

/* Try to mark pointers that are stored on the system stack and in the
 * CPU registers.
 */
static void scm_gc_mark_cstack()
{
#ifdef SCM_WITH_THREADS
  SOBJ *pp;
  SOBJ node;
  SCM_VMD *v;

  for (node = scm_thread_list; node; node = SCM_CDR(node)) {
	v = SCM_AUX(SCM_CAR(node));
	if ((v->tflags & SCM_THREAD_FINISHED) != 0) continue;
#ifdef DEBUG
	fprintf(stderr, "scm_gc_mark_cstack: tid=%d stackrange=[%p %p]\n",
			v->tid, v->cstack_ptr, v->cstack_limit);
#endif
	
	pp = v->cstack_ptr;
	while(pp < (SOBJ*)v->cstack_limit) {
	  if (scm_is_pointer_to_heap(*pp)) {
#ifdef DEBUG
		fprintf(stderr, "scm_gc_mark_cstack: marking %p\n", *pp);
#endif
		scm_gc_mark(*pp);
	  }
	  pp++;
	}
  }
  
#else /* ! SCM_WITH_THREADS */
  SOBJ *pp;
  jmp_buf regs;

  /* save the registers on the stack and mark registers */
  setjmp(regs);
  pp = (SOBJ*)&regs;
#ifdef DEBUG
  fprintf(scm_stdout, "; marking the registers\n");
#endif
  while((void*)pp < ((void*)regs) + sizeof(regs)) {
	if (scm_is_pointer_to_heap(*pp)) {
#ifdef DEBUG
	  fprintf(stderr, "; found plausible reg=%p\n", *pp);
#endif
	  scm_gc_mark(*pp);
	}
	pp++;
  }
  
  pp = (SOBJ*)&pp;
#ifdef DEBUG
  fprintf(stderr, "; marking the cstack beween %p and %p\n", pp, scm_cstack_start);
#endif
  while((void*)pp < (void*)scm_cstack_limit) {
	if (scm_is_pointer_to_heap(*pp)) {
#ifdef DEBUG
	  fprintf(stderr, "; found plausible ptr: (%p)=%p\n", pp, *pp);
#endif
	  scm_gc_mark(*pp);
	}
	pp++;
  }
#endif /* SCM_WITH_THREADS */
}

/* Prepare mark by clearing the mark bit of each cells. 
 *
 * Note: the mark bit shoud never be set and normaly does not need to
 * be cleared. This routine is more a sanity check than something
 * really needed.
 */
static void scm_gc_clear_gcmark()
{
  SCM_HEAP_BLOCK *h;
  SOBJ p;

#ifdef DEBUG_GC_PHASE
  fprintf(stderr, "  clearing gcmark\n");
#endif
  for (h = scm_heap_list; h; h = h->next) {
	p = h->base;
	while(p < h->ptr) {
	  if (SCM_GCBIT(p)) {
		SCM_GCBIT_CLR(p);
		scm_puts("; gc: cell at ");  scm_putx(p);
		scm_puts(" had gcmark != 0, obj=");
		scm_cprint(p);
	  }
	  p++;
	}
  }
}


/* Add free cells to scm_hfree list.
 * Make some efforts to lower the ptr of the heap block when possible.
 */
static void scm_gc_sweep_block(SCM_HEAP_BLOCK *h)
{
  SOBJ p;
  int t;

#ifdef DEBUG_GC_PHASE
  fprintf(stderr, "  compressing heap\n");
#endif
  p = h->ptr - 1;
  while(p >= h->base) {
	if (SCM_GCBIT(p)) break;
#ifdef DEBUG
	fprintf(stdout, "; gc : removing node at %p: ", p); scm_print(p);
#endif
	t = SCM_OBJTYPE(p);
	if (t < SOBJ_T_MAX) {
	  if (scm_type_hook[t].sweep != NULL)
		(*scm_type_hook[t].sweep)(p);

	  if (scm_type_hook[t].finalize != NULL) {
		printf("; gc : calling finalizer for type '%s'\n",
			   scm_type_hook[t].name);
		scm_apply1(scm_type_hook[t].finalize, p);
	  }
	}
	
	h->ptr = p;
	p--;
  }

  while(p >= h->base) {
	if (SCM_GCBIT(p) == 0) {
#ifdef DEBUG
	  fprintf(scm_stdout, "; gc : removing node at %p: ", p); scm_print(p);
#endif
	  t = p->type;
	  if (t < SOBJ_T_MAX) {
		if (scm_type_hook[t].sweep != NULL)
		  (*scm_type_hook[t].sweep)(p);

		if (scm_type_hook[t].finalize != NULL) {
		  printf("; gc : calling finalizer for type '%s'\n",
				 scm_type_hook[t].name);
		  scm_apply1(scm_type_hook[t].finalize, p);
		}
	  }
	  p->type = SOBJ_T_FREE;
	  p->data.pair.car = scm_hfree;
	  scm_hfree = p;
	  gcfree++;
	}
	SCM_GCBIT_CLR(p);			/* clear gcmark */
	p--;
  }
}

/*-- add free cells to scm_hfree list : try to lower scm_hptr */
static void scm_gc_sweep()
{
  SCM_HEAP_BLOCK *h;
  gcfree = 0;
  scm_hfree = NULL;
  for (h = scm_heap_list; h; h = h->next) {
	scm_gc_sweep_block(h);
  }
}

/*-- garbage collect:
 * ASSUMES THAT HEAP IS LOCKED
 */
void scm_gc()
{
  if (scm_in_gc) {
	fprintf(stderr, "OOPS: recursive gc not allowed\n");
	return;
  }
  scm_in_gc = 1;

#ifdef SCM_WITH_THREADS
  scm_thread_suspend_other();
#endif /* SCM_WITH_THREADS */

  if (scm_gc_verbose > 0 && scm_interractive) {
	scm_puts("; gc ("); scm_putn(scm_cells_allocated);
	scm_puts(" new cells)\n");
  }
  scm_cells_allocated = 0;
  scm_gc_clear_gcmark();

  gcmarked = 0;
  scm_gc_mark_protected();
  scm_gc_mark_chars();
  scm_gc_mark_stacks();
  scm_gc_mark_cstack();
  scm_gc_sweep_chars();
  scm_gc_sweep();

#ifdef SCM_WITH_THREADS
  scm_thread_resume_other();
#endif /* SCM_WITH_THREADS */
  scm_in_gc = 0;

  if (scm_gc_verbose > 1 && scm_interractive) 
	scm_heap_stat();

}

#define scm_clearcell(o) { SCM_CAR(o) = SCM_CDR(o) = 0;}

/* Allocate a new cell to hold an object.
 *
 * If scm_free list is not empty, use one of this cells. Otherwise try
 * to allocate from current heap.
 * 
 * If current heap is full, examine older heap and try to allocate
 * there.
 *
 * If all of this fail, make a gc. After gc, free nodes are collected
 * to the scm_free list. So if scm_free list is not empty use one of
 * this node.
 *
 * If scm_free is empty, this means that gc has not collected any
 * cells. Allocate a new heap block an return a cell from this heap
 * block. 
 *
 */
SOBJ scm_newcell(int type)
{
  SOBJ obj;
  SCM_HEAP_BLOCK *h;
  int try_gc;

  SCM_HEAP_LOCK();

  try_gc = TRUE;

restart_after_gc:

  if (scm_hfree) {				/* have some avail cells */
	obj = scm_hfree;			/* take the first one */
	scm_hfree = SCM_OBJREF(SCM_CAR(obj));
	gcfree--;					/* and decrement counter of free nodes */

  } else {

  restart_after_new_heap:
	h = scm_heap_list;

	/* try to allocate in current block. */
	if ((h->ptr+1) >= h->limit) {

	  /* oops not enough space in current block. may be we can find
	   * one with some more free space */
	  h = h->next;
	  while(h && (h->ptr+1) >= h->limit) h = h->next;
	  
	  if (h == NULL) {
		/* Seems to be desesparate. If we have not try a gc(), try it
		 * and start again all the processus
		 */
		if (try_gc &&
			scm_cells_allocated >= (scm_global_heap_size/5) ) {
		  try_gc = FALSE;
		  scm_gc();
		  goto restart_after_gc;
		}

		/* Bad news, gc was not sucessfull. Last chance is to allocate
		 * a new heap block and to take a cell there.
		 */
		scm_add_heap_block(scm_heap_list->size);
		goto restart_after_new_heap;
	  }
	}

	/* YEAH. We got a heap block with some free cells inside. Eat one
	 * and continue to work. */
	obj = h->ptr++;
  }

  SCM_HEAP_UNLOCK();
  scm_cells_allocated++;
  scm_clearcell(obj);  obj->type = type;
  return(obj);
}

/* Return a cell to free list
 *
 * Usefull to really remove cell now (file closing etc...)
 */
void scm_freecell(SOBJ obj)
{
  int t;

  SCM_HEAP_LOCK();

  t = obj->type;
  if (scm_type_hook[t].sweep != NULL) {
	(*scm_type_hook[t].sweep)(obj);
  }
  if (scm_type_hook[t].finalize != NULL) {
	fprintf(stderr, "scm_freecell: calling finalizer for type '%s'\n",
			scm_type_hook[t].name);
	scm_apply1(scm_type_hook[t].finalize, obj);
  }
  obj->type = SOBJ_T_FREE;
  SCM_CAR(obj) = scm_hfree;
  scm_hfree = obj;
  gcfree++;
  SCM_HEAP_UNLOCK();
}


SOBJ scm_clone(SOBJ obj)
{
  SOBJ new;
  if (SCM_INUMP(obj)) 	return(obj);
  new = scm_newcell(obj->type);
  *new = *obj;
  return(new);
}


/*-- initialize */

void scm_heap_init(long size)
{
  scm_add_heap_block(size);
}

#ifdef OLD
static void scm_pre_gc()
{
  int est_free;
  return;
  est_free = (scm_hlimit - scm_hptr) + gcfree;
  if (est_free < 10240) {
	scm_puts("; pre gc\n");
	scm_gc();
  }
}
#endif
