/* -*- tab-width:4; -*- 
 *
 * Threading library 
 */

#include "s.h"
#include "vm2.h"
#include "stack.h"

#ifdef SCM_WITH_THREADS
/* #define DEBUG_THREADS */

/* Dump system error and abort */
#define SCM_ABORT(x)	{ perror(x); exit(1); }

#ifdef DEBUG_THREADS
#define SCM_DEBUG(x...) { \
  fprintf(stderr, "%s:",__FUNCTION__); fprintf(stderr, ## x); }
#else
#define SCM_DEBUG(x...)
#endif


int SOBJ_T_THREAD;				/* type descriptor for thread objects */
int SOBJ_T_MUTEX;				/* type descriptor for mutex objects */
int SOBJ_T_SEMAPHORE;			/* type descriptor for semaphore objects */

/* Threads list. Need it for gc, because I have to access to the VM
 * stack and to the system stack of all concurent threads to determine
 * which objects are alive. */

pthread_mutex_t scm_thread_locker = PTHREAD_MUTEX_INITIALIZER;
SOBJ scm_thread_list = NULL;	/* (VMD ...) */

/* Heap locker: every body that requests a newcell must lock this
 * mutex */
pthread_mutex_t scm_heap_locker = PTHREAD_MUTEX_INITIALIZER;

/* When a thread receive a SIG_SUSPEND it will sem_post this variable */
sem_t	scm_got_suspend_signal;

/* Key for accessing scm_vm in thread specific area */
pthread_key_t scm_vmd_key;

/* return a plausible value for the system SP */
static void *get_sp()
{
  /* int dummy; */
  return(__builtin_frame_address(0));
  /*  return(&dummy); */
}


/****************************************************************
 * The THREAD type
 ****************************************************************/
SOBJ scm_thread_new(SCM_VMD *vmd)
{
  SOBJ new = scm_newcell(SOBJ_T_THREAD);
  SCM_AUX(new) = vmd;
  return(new);
}

void scm_thread_mark(SOBJ x)
{
  SCM_VMD *vmd = SCM_THREAD(x);

  SCM_DEBUG("marking %p (vmd=%p)\n", x, vmd);
  if (vmd->thunk) 	scm_gc_mark(vmd->thunk);
}

void scm_thread_sweep(SOBJ x)
{
  SCM_VMD *vmd = SCM_THREAD(x);
  if (vmd->tflags & SCM_THREAD_FINISHED) {
	fprintf(stderr, "scm_thread_sweep: sweeping %p (vmd=%p)\n", x, vmd);
	if (vmd->stack_base) scm_free(vmd->stack_base);
	scm_free(vmd);
  }
}

void scm_thread_print(SOBJ x, PORT *p)
{
  scm_puts("#<thread>");
}

void scm_thread_write(SOBJ x, PORT *p)
{
  SCM_VMD *v = SCM_THREAD(x);
  char *str = scm_asprintf("#<thread id=%d flags=%d>", v->tid, v->tflags);
  scm_puts(str);
  scm_free(str);
}

SOBJ_TYPE_DESCR scm_thread_type_descr = {
  0,  "thread",
  scm_thread_mark,	scm_thread_sweep,
  scm_thread_print,	scm_thread_write,
  NULL };

/*E* (thread-dump THR) => #undefined */
/*D* Dump the content of a thread. For debugging purpose. */
SOBJ scm_thread_dump(SOBJ x)
{
  SCM_VMD *v;
  char *str;

  if (!SCM_THREADP(x))	SCM_ERR("bad thread", x);
  
  v = SCM_THREAD(x);
  str = scm_asprintf("#<thread id=%d flag=%02x code=%d "
					 "{sp=%p ip=%p cont=%p env=%p} stk={%p %p %d}>",
					 v->tid, v->tflags, v->code,
					 v->reg.sp,v->reg.ip,v->reg.cont,v->reg.env,
					 v->stack_base,v->stack_limit,v->stack_size);
  scm_puts(str);
  scm_free(str);
  return(scm_undefined);
}

/****************************************************************
 * The MUTEX type
 ****************************************************************/
/*E* (make-mutex) => MUTEX*/
/*D* Create a new MUTEX */
SOBJ scm_mutex_new()
{
  SOBJ new = scm_newcell(SOBJ_T_MUTEX);
  SCM_MUTEX(new) = scm_must_alloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(SCM_MUTEX(new), NULL);
  return(new);
}

void scm_mutex_sweep(SOBJ x)
{
  SCM_DEBUG("mutex %p\n", SCM_MUTEX(x));
  if (SCM_MUTEX(x))	{
	if (pthread_mutex_destroy(SCM_MUTEX(x)) == EBUSY) {
	  fprintf(stderr, "OOPS: mutex %p not destroyed\n", SCM_MUTEX(x));
	} else {
	  scm_free(SCM_MUTEX(x));
	}
	SCM_MUTEX(x) = NULL;
  }
}

void scm_mutex_print(SOBJ x, PORT *p)
{
  scm_puts("#<mutex>");
}

void scm_mutex_write(SOBJ x, PORT *p)
{
  char *str = scm_asprintf("#<mutex %p>", SCM_MUTEX(x));
  scm_puts(str);
  scm_free(str);
}

SOBJ_TYPE_DESCR scm_mutex_type_descr = {
  0,	"mutex",
  NULL,				scm_mutex_sweep,
  scm_mutex_print,	scm_mutex_write,
  NULL };

/*E* (mutex? OBJ) => BOOL */
/*D* Returns #t if OBj is a mutex, #f otherwise */
SOBJ scm_mutex_p(SOBJ x)
{
  return(SCM_MKBOOL(SCM_OBJTYPE(x) == SOBJ_T_MUTEX));
}  

/*E* (mutex-lock MUTEX) => #undef */
/*D* Lock MUTEX. If MUTEX is already locked, thread execution is
 * suspended until MUTEX is unlocked */
SOBJ scm_mutex_lock(SOBJ x)
{
  if (!SCM_MUTEXP(x))	SCM_ERR("bad mutex", x);
  pthread_mutex_lock(SCM_MUTEX(x));
  return(scm_undefined);
}

/*E* (mutex-try-lock MUTEX) => BOOLEAN */
/*D* Try to lock MUTEX. If MUTEX is already locked, #f is returned
 * immediately. otherwise, the MUTEX is locked and #t is returned. */
SOBJ scm_mutex_try_lock(SOBJ x)
{
  if (!SCM_MUTEXP(x))	SCM_ERR("bad mutex", x);
  return(SCM_MKBOOL(pthread_mutex_trylock(SCM_MUTEX(x)) != EBUSY));
}

/*E* (mutex-unlock MUTEX) => #undefined */
/*D* Unlock mutex */
SOBJ scm_mutex_unlock(SOBJ x)
{
  if (!SCM_MUTEXP(x))	SCM_ERR("bad mutex", x);
  pthread_mutex_unlock(SCM_MUTEX(x));
  return(scm_undefined);
}

/****************************************************************
 * The SEMAPHORE type
 ****************************************************************/
/*E* (make-semaphore [VAL]) => SEM */
/*D*  */
SOBJ scm_semaphore_new(int nargs, SOBJ *arg)
{
  int n = 0;
  SOBJ new = scm_newcell(SOBJ_T_SEMAPHORE);
  SCM_SEMAPHORE(new) = scm_must_alloc(sizeof(sem_t));
  if (nargs >= 1 && SCM_INUMP(arg[0])) {
	n = SCM_INUM(n);
  }
  sem_init(SCM_SEMAPHORE(new), 0, n);
  return(new);
}

void scm_semaphore_sweep(SOBJ x)
{
  SCM_DEBUG("semaphore %p\n", SCM_SEMAPHORE(x));
  if (SCM_SEMAPHORE(x)) {
	if (sem_destroy(SCM_SEMAPHORE(x)) != 0) {
	  fprintf(stderr, "OOPS: semaphore %p not destroyed\n", SCM_SEMAPHORE(x));
	} else {
	  scm_free(SCM_SEMAPHORE(x));
	}
	SCM_SEMAPHORE(x) = NULL;
  }
}

void scm_semaphore_print(SOBJ x, PORT *p)
{
  scm_puts("#<semaphore>");
}

void scm_semaphore_write(SOBJ x, PORT *p)
{
  char *str = scm_asprintf("#<semaphore %p>", SCM_SEMAPHORE(x));
  scm_puts(str);
  scm_free(str);
}

SOBJ_TYPE_DESCR scm_semaphore_type_descr = {
  0,	"semaphore",
  NULL,					scm_semaphore_sweep,
  scm_semaphore_print,	scm_semaphore_write,
  NULL };


/*E* (semaphore? OBJ) => BOOL*/
/*D* Returns #t if OBJ is a semaphore, #f otherwise. */
SOBJ scm_semaphore_p(SOBJ x)
{
  return(SCM_MKBOOL(SCM_OBJTYPE(x) == SOBJ_T_SEMAPHORE));
}

/*E* (semaphore-wait SEM) => #undefined */
/*D* Suspend the calling thread until SEM has non-zero count, then
 * decrease the semaphore count. */
SOBJ scm_semaphore_wait(SOBJ x)
{
  if (!SCM_SEMAPHOREP(x))	SCM_ERR("bad semaphore", x);
  sem_wait(SCM_SEMAPHORE(x));
  return(scm_undefined);
}

/*E* (semaphore-try-wait SEM) => BOOL */
/*D* Non blocking variant of semaphore-wait. If SEM has non-zero
 * count, the count is atomically decreased and #t is returned. If SEM count is zero, #f is returned. */
SOBJ scm_semaphore_try_wait(SOBJ x)
{
  if (!SCM_SEMAPHOREP(x))	SCM_ERR("bad semaphore", x);
  return(SCM_MKBOOL(sem_trywait(SCM_SEMAPHORE(x)) == 0));
}

/*E* (semaphore-post SEM) => #undefined*/
/*D* Atomically increases the count of SEM.*/
SOBJ scm_semaphore_post(SOBJ x)
{
  if (!SCM_SEMAPHOREP(x))	SCM_ERR("bad semaphore", x);
  sem_post(SCM_SEMAPHORE(x));
  return(scm_undefined);
  
}

/*E* (semaphore-get-value SEM) => NUM */
/*D* Return the count of the semaphore SEM. */
SOBJ scm_semaphore_get_value(SOBJ x)
{
  int val;
  if (!SCM_SEMAPHOREP(x))	SCM_ERR("bad semaphore", x);
  sem_getvalue(SCM_SEMAPHORE(x), &val);
  return(scm_int2num(val));
}


/****************************************************************
 * Thread suspend and resume
 ****************************************************************/

/* Thread suspend: 
 *
 * save my own context in my SCM_VMD structure, send SCM_SIG_SUSPEND
 * to all other alive threads and wait for all threads to be
 * suspended.
 *
 * After that, all thread will have the same kind of context. SCM_VMD
 * will contain the current system SP and a valid pointer to the top
 * of the system stack. The contents of the registers for each threads
 * are saved on the system stack (jmp_buf regs).
 *
 * Mark phase of GC is identical for all threads, and that's good.
 * 
 */
void scm_thread_suspend_other()
{
  SOBJ node;
  SCM_VMD *v;
  pthread_t my_tid = pthread_self();
  int nthreads, r, i;
  jmp_buf regs;
  
  SCM_DEBUG("enter\n");

  /* save contents of my registers on the stack */
  setjmp(regs);					/* save registers to stack */

  nthreads = 0;
  pthread_mutex_lock(&scm_thread_locker);
  for (node = scm_thread_list; node; node = SCM_CDR(node)) {
	v = SCM_THREAD(SCM_CAR(node));
	if (v->tid == my_tid) {
	  /* save my sp to the vmd structure */
	  v->cstack_ptr = get_sp();	
	  continue;
	}
	if ((v->tflags & SCM_THREAD_FINISHED) != 0)
	  continue;

	SCM_DEBUG("sending suspend to %d\n", v->tid);
	nthreads++;
	r = pthread_kill(v->tid, SCM_SIG_SUSPEND);
	switch(r) {
	case 0:
	  break;
	case ESRCH:	/* thread does not exist */
	  SCM_DEBUG("can't send suspend to thread %d, does not exist\n", v->tid);
	  nthreads--;
	  break;
	default:
	  SCM_ABORT("pthread_kill failed\n");
	}
  }
  pthread_mutex_unlock(&scm_thread_locker);
  SCM_DEBUG("suspend sended to %d threads, waiting response\n", nthreads);
  for (i = 0; i < nthreads; i++) {
	sem_wait(&scm_got_suspend_signal);
  }
  SCM_DEBUG("all thread stopped (except me)\n");
  SCM_DEBUG("leaving\n");
}

/* Thread resume */
void scm_thread_resume_other()
{
  SOBJ node;
  SCM_VMD *v;
  pthread_t my_tid = pthread_self();
  int nthreads, r;

  SCM_DEBUG("enter\n");
  nthreads = 0;
  pthread_mutex_lock(&scm_thread_locker);
  for (node = scm_thread_list; node; node = SCM_CDR(node)) {
	v = SCM_THREAD(SCM_CAR(node));
	if (v->tid != my_tid && (v->tflags & SCM_THREAD_FINISHED) == 0) {
	  SCM_DEBUG("sending resume to %d\n", v->tid);
	  nthreads++;
	  r = pthread_kill(v->tid, SCM_SIG_RESUME);
	  switch(r) {
	  case 0:
		break;
	  case ESRCH:	/* thread does not exist */
		SCM_DEBUG("can't send suspend to thread %d, does not exist\n", v->tid);
		nthreads--;
		break;
	  default:
		SCM_ABORT("pthread_kill failed\n");
	  }
	}
  }
  pthread_mutex_unlock(&scm_thread_locker);
  SCM_DEBUG("thread resumed\n");
  SCM_DEBUG("leaving\n");
}

/* search for tid in thread-list, returns the scheme object matching
   or NULL */
SOBJ scm_thread_lookup(pthread_t tid)
{
  SOBJ node;
  SCM_VMD *v;

  for (node = scm_thread_list; node != NULL; node = SCM_CDR(node)) {
	v = SCM_THREAD(SCM_CAR(node));
	if (v->tid == tid)
	  return(SCM_CAR(node));
  }
  return(NULL);
}

void scm_thread_suspend_handler(int sig)
{
  SOBJ node;
  SCM_VMD *v;
  pthread_t my_tid = pthread_self();
  sigset_t mask;
  jmp_buf regs;					/* Buffer to save current registers on
                                   the stack. Because the gc will scan
                                   the stack, contents of the
                                   registers will be examined */
  
  SCM_DEBUG("enter\n");
  if (sig != SCM_SIG_SUSPEND)	SCM_ABORT("bad signal");

  SCM_DEBUG("searching myself (%d) in thread list\n", my_tid);

  /* search my tid in the scm_thread_list */
  node = scm_thread_lookup(my_tid);
  if (node == NULL) SCM_ABORT("no thread found");
  v = SCM_THREAD(node);

  SCM_DEBUG("found: node=%p v=%p tid=%d\n", node, v, v->tid);
  v->cstack_ptr = get_sp();		/* save possible sp to vmd */
  setjmp(regs);					/* save registers to stack */

  /* tell the stopper that we are ready: sem_post can be used in
     interrupt context */
  sem_post(&scm_got_suspend_signal);

  /* wait for a SCM_SIG_RESUME */
  SCM_DEBUG("thread %d waiting for restart\n", v->tid);
  sigfillset(&mask);
  sigdelset(&mask, SCM_SIG_RESUME);
  do {
	v->signal = 0;
	sigsuspend(&mask);
	SCM_DEBUG("thread %d: got signal %d\n", v->tid, v->signal);
  } while(v->signal != SCM_SIG_RESUME);

  SCM_DEBUG("thread %d: restarted\n", v->tid);
  SCM_DEBUG("leaving\n");
}

void scm_thread_resume_handler(int sig)
{
  SOBJ node;

  SCM_DEBUG("enter\n");

  node = scm_thread_lookup(pthread_self());
  SCM_DEBUG("found node %p, tid=%d\n", node, SCM_THREAD(node)->tid);
  if (node == NULL)	SCM_ABORT("no thread found");

  SCM_THREAD(node)->signal = SCM_SIG_RESUME;

  SCM_DEBUG("leaving\n");
}

/* cleanup handler for thread. Pushed by pthread_cleanup_push */
static void scm_thread_cleanup(SOBJ vm)
{
  SOBJ node, prev;
  SCM_VMD *vmd = SCM_THREAD(vm);
  
  /* remove myself from vm_list */
  SCM_DEBUG("removing thread from thread-list\n");
  SCM_DEBUG("vm=%p, vmd=%p\n", vm, vmd);
  prev = NULL;
  pthread_mutex_lock(&scm_thread_locker);
  for (node = scm_thread_list; node; node = SCM_CDR(node)) {
	if (SCM_CAR(node) == vm) {
	  SCM_DEBUG("found node at %p\n", node);
	  if (prev) {
		SCM_CDR(prev) = SCM_CDR(node);
	  } else {
		scm_thread_list = SCM_CDR(node);
	  }
	  break;
	}
	prev = node;
  }
  vmd->tflags |= SCM_THREAD_FINISHED;
  pthread_mutex_unlock(&scm_thread_locker);
  SCM_DEBUG("thread terminating\n");
}

/* real thread executor */
static void scm_thread_exec(SOBJ vm)
{
  int k;
  SCM_VMD *vmd = SCM_THREAD(vm);
  
  SOBJ vmcode[] = {
	SCM_OPCODE(SCM_OP_MARK), SCM_OPCODE(SCM_OP_PUSH), vmd->thunk,
	SCM_OPCODE(SCM_OP_CALL), SCM_OPCODE(SCM_OP_END)  };

  pthread_setspecific(scm_vmd_key, vmd);

  vmd->tid 			= pthread_self();
  vmd->code 		= SCM_VM_DO_EXECUTE;
  vmd->reg.ip 		= vmcode;
  vmd->cstack_limit = __builtin_frame_address(0);

  SCM_HEAP_UNLOCK();
  SCM_DEBUG("thread %d: ready to exec\n", vmd->tid);
  SCM_DEBUG("vm=%p, vmd=%p\n", vm, vmd);

  pthread_cleanup_push((void*)scm_thread_cleanup, (void*)vm);

  if ((k = setjmp(scm_errjmp)) == 0) {
	SCM_DEBUG("running engine\n");
	scm_vm(vmd);
	SCM_DEBUG("engine finished\n");
  } else {
	SCM_DEBUG("error caught in thread %d\n", vmd->tid);
  }
  pthread_cleanup_pop(1);
}

/*E* (thread THUNK) => THREAD */
/*D* Invoke THUNK in a new thread. Returns THREAD, a thread descriptor */

SOBJ scm_thread(SOBJ thunk)
{
  SCM_VMD *vmd;
  SOBJ vm;
  
  if (!SCM_CLOSUREP(thunk)) SCM_ERR("bad proc", thunk);

  /* prepare the environment for the new thread */
  vmd = scm_vmd_new();
  vm  = scm_thread_new(vmd);
  scm_vmd_stack_alloc(vmd, scm_default_ds * 1024);

  vmd->thunk = thunk;
  pthread_mutex_lock(&scm_thread_locker);
  scm_thread_list = scm_cons(vm, scm_thread_list);
  pthread_mutex_unlock(&scm_thread_locker);

  SCM_HEAP_LOCK();
  if (pthread_create(&vmd->tid,NULL,(void*)scm_thread_exec,vm) != 0)
	SCM_ERR("thread creation failed",NULL);

  return(vm);
}

/*E* (thread? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a thread descriptor, false otherwise */
SOBJ scm_threadp(SOBJ x)
{
  return(SCM_MKBOOL(SCM_OBJTYPE(x) == SOBJ_T_THREAD));
}

/*E* (thread-id THREAD) => NUMBER */
/*D* Returns the pthread id associated with THREAD. */
SOBJ scm_thread_id(SOBJ x)
{
  if (!SCM_THREADP(x))	SCM_ERR("bad thread", x);
  return(SCM_MKINUM(SCM_THREAD(x)->tid));
}

/*E* (current-thread) => THREAD */
/*D* Returns the THREAD for current thread */
SOBJ scm_current_thread()
{
  return(scm_thread_lookup(pthread_self()));
}

/*E* (thread-running? THREAD) => BOOL */
/*D* Returns #t if THREAD is running, #f otherwise */
SOBJ scm_thread_runningp(SOBJ x)
{
  if (!SCM_THREADP(x))	SCM_ERR("bad thread", x);
  return(SCM_MKBOOL( (SCM_THREAD(x)->tflags & SCM_THREAD_FINISHED) == 0));
}

/*E* (thread-wait THREAD) => #undefined */
/*D* Wait until thread THREAD finishes */
SOBJ scm_thread_wait(SOBJ x)
{
  if (!SCM_THREADP(x))	SCM_ERR("bad thread", x);

  if ((SCM_THREAD(x)->tflags & SCM_THREAD_FINISHED) == 0) 
	pthread_join(SCM_THREAD(x)->tid, NULL);

  return(scm_undefined);
}

/*E* (thread-kill THREAD) => #undefined */
/*D* Terminates THREAD */
SOBJ scm_thread_kill(SOBJ x)
{
  if (!SCM_THREADP(x))	SCM_ERR("bad thread", x);

  if ((SCM_THREAD(x)->tflags & SCM_THREAD_FINISHED) == 0)
	pthread_cancel(SCM_THREAD(x)->tid);

  return(scm_undefined);
}

/*E* (thread-exit) => ??? (never returns) */
/*D* Terminates current thread. */
SOBJ scm_thread_exit()
{
  pthread_exit(NULL);
}

/*E* (thread-suspend THREAD) => #undefined */
/*D* Temporary suspend the THREAD */
SOBJ scm_thread_suspend(SOBJ x)
{
  if (!SCM_THREADP(x))	SCM_ERR("bad thread", x);
  return(scm_undefined);
}

/*E* (thread-resume THREAD) => #undefined */
/*D* Restart the thread at point where it was stopped */
SOBJ scm_thread_resume(SOBJ x)
{
  if (!SCM_THREADP(x))	SCM_ERR("bad thread", x);
  return(scm_undefined);
}

/****************************************************************
 * Thread early initialization
 ****************************************************************/

/* Setup for a newly create thread.
 *
 * Creates a new vm node, add it to the scm_thread_list. Setup interrupt
 * handler for SCM_SIG_SUSPEND and SCM_SIG_RESUME.
 *
 * Returns the VM object created
 */
static void scm_thread_setup(void *cstack_limit)
{
  SCM_VMD *vmd;
  struct sigaction sig;
  
  SCM_DEBUG("enter\n");

  /* create a key for thread specific pointer. This pointer will point
   * to a SCM_VMD structure. Each thread will have it's own SCM_VMD.
   */
  vmd = scm_vmd_new();
  vmd->tid = pthread_self();

  SCM_DEBUG("vmd=%p\n", vmd);
  pthread_key_create(&scm_vmd_key, NULL);
  pthread_setspecific(scm_vmd_key, vmd);

  SCM_DEBUG("setting signal handler for signal %d and %d\n",
			SCM_SIG_SUSPEND, SCM_SIG_RESUME);

  sig.sa_flags = SA_RESTART;
  sigfillset(&sig.sa_mask);

  sig.sa_handler = scm_thread_suspend_handler;
  if (sigaction(SCM_SIG_SUSPEND, &sig, NULL) != 0)	SCM_ABORT("sigaction()");
  
  sig.sa_handler = scm_thread_resume_handler;
  if (sigaction(SCM_SIG_RESUME, &sig, NULL) != 0)	SCM_ABORT("sigaction()");

  vmd->cstack_limit = cstack_limit;
  vmd->cstack_ptr = NULL;
  SCM_DEBUG("return\n");
}

/* Number of invocation of scm_init_main_thread() */
static int scm_init_main_thread_invocations = 0;

/* Initialize the main thread. This must be done only once.
 *
 * Note: This func is called very early during the boot of scheme. At
 * this point, we only have heap initialized, so that calling new_cell
 * will not crash :)
 */

void scm_init_main_thread()
{
  SCM_DEBUG("enter\n");

  if (scm_init_main_thread_invocations != 0) {
	SCM_DEBUG("allready initialized\n");
	fprintf(stderr, "ERROR:scm_init_main_thread() must only be called once\n");
	exit(1);
  }

  /* setup new thread context */
  scm_thread_setup(scm_cstack_limit);

  /* initialize the semaphore where the threads signal they have go
   * the suspend message */
  if (sem_init(&scm_got_suspend_signal,0,0) != 0)  	SCM_ABORT("sem_init()");

  SCM_DEBUG("return\n");
}

/****************************************************************
 * Thread late initialization
 *
 * Initialize thread module.
 ****************************************************************/

/* Initialize the thread type. When called the scheme environment must
 * be more or less ready */
void scm_init_thread()
{
  SOBJ vm;

  SOBJ_T_THREAD 	= scm_add_type(&scm_thread_type_descr);
  SOBJ_T_MUTEX		= scm_add_type(&scm_mutex_type_descr);
  SOBJ_T_SEMAPHORE 	= scm_add_type(&scm_semaphore_type_descr);

  vm = scm_thread_new(scm_vmd());
  scm_thread_list = scm_cons(vm, NULL);
  
  scm_add_cvar("thread-list", &scm_thread_list);

  scm_add_cprim("thread", 			scm_thread,				1);
  scm_add_cprim("thread?",			scm_threadp,			1);
  scm_add_cprim("thread-id",		scm_thread_id,			1);
  scm_add_cprim("current-thread",	scm_current_thread,		0);
  scm_add_cprim("thread-running?",	scm_thread_runningp,	1);
  scm_add_cprim("thread-wait",		scm_thread_wait,		1);
  scm_add_cprim("thread-kill",		scm_thread_kill,		1);
  scm_add_cprim("thread-suspend",	scm_thread_suspend,		1);
  scm_add_cprim("thread-resume",	scm_thread_resume,		1);
  scm_add_cprim("thread-exit",		scm_thread_exit,		0);

  scm_add_cprim("make-mutex",		scm_mutex_new,			0);
  scm_add_cprim("mutex?",			scm_mutex_p,			0);
  scm_add_cprim("mutex-lock",		scm_mutex_lock,			1);
  scm_add_cprim("mutex-try-lock",	scm_mutex_try_lock,		1);
  scm_add_cprim("mutex-unlock",		scm_mutex_unlock,		1);

  scm_add_cprim("make-semaphore",		scm_semaphore_new,			-1);
  scm_add_cprim("semaphore?",			scm_semaphore_p,			1);
  scm_add_cprim("semaphore-wait",		scm_semaphore_wait,			1);
  scm_add_cprim("semaphore-try-wait",	scm_semaphore_try_wait,		1);
  scm_add_cprim("semaphore-post",		scm_semaphore_post,			1);
  scm_add_cprim("semaphore-get-value",	scm_semaphore_get_value,	1);
}

#endif /* SCM_WITH_THREADS */

