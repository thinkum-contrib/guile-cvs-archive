/* -*- tab-width:4; -*- */
/*
 * Process interface
 */
#include "s.h"
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

/* define this to use execv. If USE_EXECV is not defined, we will use execvp */

static int use_execv = FALSE;	/* TRUE=>execv, FALSE=>execvp */

int SOBJ_T_PROCESS;				/* process type */

static SOBJ atom_pipe, atom_null;

/*E* process-list => LIST */
/*D* The list of processe created with make-process */
SOBJ scm_process_list;			/* list of active processes */

/*NOTE: we need the process list so that we can note status of
  processes that get waited */

typedef struct
{
  int pid;
  SOBJ port[3];					/* stdin stdout and stderr */
  int status;					/* status of process after exit */
  int exited;					/* TRUE when process has exited This
								 * flag is set only when process has 
								 * been waited for */
} SCM_ProcessAux;

/* NOTE: if exited is not set when collecting, it's time to wait for next */

#define SCM_PROCESS(x)		((SCM_ProcessAux*)SCM_AUX(x))
#define SCM_PROCESSP(x)		(SCM_OBJTYPE(x) == SOBJ_T_PROCESS)

#define SCM_PROCESS_PID(x)		(SCM_PROCESS(x)->pid)
#define SCM_PROCESS_PORT(x,i) 	(SCM_PROCESS(x)->port[i])


/* forward declaration for type descriptor functions */
static void scm_process_mark(SOBJ obj);
static void scm_process_sweep(SOBJ obj);
static void scm_process_print(SOBJ obj, PORT *p);
static void scm_process_write(SOBJ obj, PORT *p);


/****************************************************************
 * Type descrptor entry
 ****************************************************************/
SOBJ_TYPE_DESCR scm_process_type = {
  0,
  "process",
  scm_process_mark,				/* mark */
  scm_process_sweep,			/* sweep */
  scm_process_print,			/* print */
  scm_process_write,			/* write */
  NULL,							/* creconize */
  NULL,							/* cparse */
  NULL,							/* wreconize */
  NULL,							/* wparse */
  NULL							/* compare */
};

/****************************************************************
 * Type functions
 ****************************************************************/
static SOBJ proc_remove_by_pid(int pid)
{
  SOBJ l, last, proc;

  last = NULL;
  for (l = scm_process_list; l; l = SCM_CDR(l)) {
    if (!SCM_PAIRP(l)) {
      scm_process_list = NULL;
      SCM_ERR("bad process-list: reseted", NULL);
    }
    proc = SCM_CAR(l);
    if (SCM_PROCESS_PID(proc) == pid) {
      if (last) {		/* have a node to link */
		SCM_CDR(last) = SCM_CDR(l);
      } else {			/* was first in the process_list */
		scm_process_list = SCM_CDR(l);
      }
      return(proc);
    }
    last = l;
  }
  return(NULL);
}

static void scm_process_mark(SOBJ obj)
{
  int i;

  if (SCM_PROCESS(obj)->exited) {
    fprintf(stderr, "process_mark: proc %d @%p terminated with status %d\n",
			SCM_PROCESS_PID(obj), obj, SCM_PROCESS(obj)->status);
    fprintf(stderr, "process_mark: closing ports\n");
    for (i = 0; i < 3; i++) {
      if (SCM_PROCESS_PORT(obj, i) != NULL) {
		scm_close_port(SCM_PROCESS_PORT(obj, i));
		SCM_PROCESS_PORT(obj, i) = NULL;
      }
    }
    return;
  }
  for (i = 0; i < 3; i++) {
    if (SCM_PROCESS_PORT(obj, i))
      scm_gc_mark(SCM_PROCESS_PORT(obj, i));
  }
}

static void scm_process_sweep(SOBJ obj)
{
  if (SCM_PROCESS(obj))
    scm_free(SCM_PROCESS(obj));
}

static void scm_process_print(SOBJ obj, PORT *p)
{
  port_puts(p, "#<process>");
}

static void scm_process_write(SOBJ obj, PORT *p)
{
  port_puts(p, "#<process pid=");
  port_putn(p, SCM_PROCESS_PID(obj));
  port_puts(p, " in=");
  scm_write_obj(SCM_PROCESS_PORT(obj, 0), p, 1);
  port_puts(p, " out=");
  scm_write_obj(SCM_PROCESS_PORT(obj, 1), p, 1);
  port_puts(p, " err=");
  scm_write_obj(SCM_PROCESS_PORT(obj, 2), p, 1);
  port_puts(p, " status=");
  scm_putn(SCM_PROCESS(obj)->status);
  port_puts(p, " exited=");
  scm_putn(SCM_PROCESS(obj)->exited);
  port_putc(p, '>');
}

SCM_STRBUF *scm_process2str(SCM_STRBUF *sb, SOBJ obj, int raw)
{
  if (raw) {
	return(scm_strbuf_concat_str(sb, "#<process>"));
  }
  sb = scm_strbuf_concat_sprintf(sb, "#<process pid=%d",
								 SCM_PROCESS_PID(obj));
  sb = scm_strbuf_concat_str(sb, " in=");
  sb = scm_iobj2str(sb, SCM_PROCESS_PORT(obj, 0), 1);
  sb = scm_strbuf_concat_str(sb, " out=");
  sb = scm_iobj2str(sb, SCM_PROCESS_PORT(obj, 1), 1);
  sb = scm_strbuf_concat_str(sb, " err=");
  sb = scm_iobj2str(sb, SCM_PROCESS_PORT(obj, 2), 1);

  sb = scm_strbuf_concat_sprintf(sb, " status=%d exited=%d>",
								 SCM_PROCESS(obj)->status,
								 SCM_PROCESS(obj)->exited);
  return(sb);
}


/****************************************************************
 * Helper functions
 ****************************************************************/

static SOBJ scm_process_alloc()
{
  int i;
  SOBJ new = scm_newcell(SOBJ_T_PROCESS);

  SCM_PROCESS(new) = scm_must_alloc(sizeof(SCM_ProcessAux));
  SCM_PROCESS_PID(new) = 0;
  for (i = 0; i < 3; i++)
    SCM_PROCESS_PORT(new, i) = NULL;
  SCM_PROCESS(new)->status = -1;
  SCM_PROCESS(new)->exited = 0;
  return (new);
}

static SOBJ scm_process_add()
{
  SOBJ new = scm_process_alloc();

  scm_process_list = scm_cons(new, scm_process_list);
  return (new);
}

#ifdef OLD
static SOBJ scm_process_remove(SOBJ obj)
{
  SOBJ before, l;

  before = NULL;
  for (l = scm_process_list; l; l = SCM_CDR(l)) {
    if (SCM_CAR(l) == obj)
      break;
    before = l;
  }
  if (l == NULL)
    SCM_ERR("process-remove: process not found", obj);

  if (before) {			/* middle of list */
    SCM_CDR(before) = SCM_CDR(l);	/* jump over current */
  } else {			/* first of list */
    scm_process_list = SCM_CDR(l);
  }
  return (obj);
}
#endif

/****************************************************************
 * public functions
 ****************************************************************/

/*E* (process? OBJ) => BOOLEAN */
/*D* Returns #t if OBJ is a process, #f otherwise. */
SOBJ scm_processp(SOBJ obj)
{
  return (SCM_TYPEP(obj, SOBJ_T_PROCESS) ? scm_true : scm_false);
}

/*E* (process-pid PROCESS) => PID */
/*D* Returns the PID of the process PROCESS */
SOBJ scm_process_pid(SOBJ obj)
{
  if (!SCM_PROCESSP(obj))
    SCM_ERR("process-id: bad process", obj);
  return (SCM_MKINUM(SCM_PROCESS_PID(obj)));
}

/*E* (process-input PROCESS) => PORT */
/*D* Returns the process input port  */
SOBJ scm_process_input(SOBJ obj)
{
  if (!SCM_PROCESSP(obj))
    SCM_ERR("process-input: bad process", obj);
  return (SCM_PROCESS_PORT(obj, 0));
}

/*E* (process-output PROCESS) => PORT */
/*D* Returns the process output port */
SOBJ scm_process_output(SOBJ obj)
{
  if (!SCM_PROCESSP(obj))
    SCM_ERR("process-output: bad process", obj);
  return (SCM_PROCESS_PORT(obj, 1));
}

/*E* (process-error PROCESS) => PORT */
/*D* return the process error port */
SOBJ scm_process_error(SOBJ obj)
{
  if (!SCM_PROCESSP(obj))
    SCM_ERR("process-error: bad process", obj);
  return (SCM_PROCESS_PORT(obj, 2));
}

/*E* (process-status PROCESS) => STATUS */
/*D* Returns the process status of the process PROCESS. */
SOBJ scm_process_status(SOBJ obj)
{
  if (!SCM_PROCESSP(obj))
    SCM_ERR("process-error: bad process", obj);
  return (SCM_MKINUM(SCM_PROCESS(obj)->status));
}

/* IN OUT ERR possible values:
 *     :null | '()  : not opened
 *     :pipe | "-"  : open to pipe
 *     STR          : open to file
 *     NUM			: redirect to same descriptor
 *
 * (make-process :null :pipe 1 "ls" "-al")
 * (make-process :null "/tmp/ake" 1 "ls -al")
 *
 */
enum IO_TYPE
{
  IO_T_NULL = 0,
  IO_T_PIPE,					/* pipe */
  IO_T_FILE,					/* redir to file */
  IO_T_PORT,					/* redir to open port */
  IO_T_REDIR,					/* redir to another descriptor */
  IO_T_MAX
};

static int get_io_type(SOBJ obj, int enable_redir)
{
  switch (SCM_OBJTYPE(obj)) {
  case SOBJ_T_KEYWORD:
    if (SCM_KEYW_NAME(obj) == atom_pipe)
      return (IO_T_PIPE);
    if (SCM_KEYW_NAME(obj) == atom_null)
      return (IO_T_NULL);
    break;

  case SOBJ_T_STRING:
    return (IO_T_FILE);
  case SOBJ_T_PORT:
    return (IO_T_PORT);
  case SOBJ_T_INUM:
    if (enable_redir)
      return (IO_T_REDIR);
    break;
  }
  SCM_ERR("make-process: bad io type", obj);
  return (-1);
}

/*-- output an error message after the fork and exit with status 111 */
static void child_err(char *msg)
{
  static char head[] = "make-process child: ";
  static char tail[] = ", dying\n";

  write(2, head, strlen(head));
  write(2, msg, strlen(msg));
  write(2, tail, strlen(tail));
  exit(111);
}

/*E* (make-process IN OUT ERR [ARG...|LIST|ARRAY]) => PROCESS */
/*D* Create a new process. IN, OUT or ERR indicates the type of port
  to open for the new process. :null or '() means that no file is
  opened, :pipe or "-" means open to a pipe, a string means open to a
  file, NUMBER means redirect to descriptor. */
/*X* (make-process :null :pipe 1 "ls" "-al") */

SOBJ scm_make_process(int argc, SOBJ *arg)
{
  SOBJ proc, arg_array;
  static int io_rd[3] = { TRUE, FALSE, FALSE };
  int io_type[3], pfd[3][2];
  int i, pid;

  if (argc < 4)
    SCM_ERR("make-process: not enough arguments", SCM_MKINUM(argc));

  io_type[0] = get_io_type(arg[0], FALSE);
  io_type[1] = get_io_type(arg[1], TRUE);
  io_type[2] = get_io_type(arg[2], TRUE);

  if (SCM_STRINGP(arg[3])) {
    for (i = 3; i < argc; i++) {
      if (!SCM_STRINGP(arg[i]))
		SCM_ERR("make-process: bad arg type", arg[i]);
    }
  } else if (!SCM_ARRAYP(arg[3]) && !SCM_PAIRP(arg[3])) {
    SCM_ERR("make-process: bad arg type", arg[3]);
  }

  for (i = 0; i < 3; i++) {
    switch (io_type[i]) {
    case IO_T_NULL:
      pfd[i][0] = -1;
      pfd[i][1] = -1;
      break;

    case IO_T_PIPE:
      if (pipe(pfd[i]) != 0)
		SCM_ERR("make-process: cannot create pipe for io ", SCM_MKINUM(i));
      break;

    case IO_T_FILE:
      {
		int flags;

		if (io_rd[i]) {		/* read */
		  flags = O_RDONLY;
		} else {		/* write */
		  flags = O_WRONLY | O_CREAT | O_TRUNC;
		}
		pfd[i][0] = open(SCM_STR_VALUE(arg[i]), flags, 0666);
		if (pfd[i][0] < 0)
		  SCM_ERR("make-process: cannot redirect from file", arg[i]);
      }
      break;

    case IO_T_PORT:
    case IO_T_REDIR:
    default:
      SCM_ERR("make-process: illegal io_type", NULL);
    }
  }

  if ((pid = fork()) == -1)
    SCM_ERR("make-process: fork failed", NULL);
  if (pid == 0) {		/* child process */
    for (i = 0; i < 3; i++) {
      switch (io_type[i]) {
      case IO_T_NULL:
		break;
      case IO_T_PIPE:
		if (close(i) != 0)
		  child_err("close failed");
		if (dup(pfd[i][io_rd[i] ? 0 : 1]) != i)
		  child_err("dup failed");
		if (close(pfd[i][0]) != 0 || close(pfd[i][1]) != 0)
		  child_err("pipe close failed");
		break;
      case IO_T_FILE:
		if (close(i) != 0)
		  child_err("close failed");
		if (dup(pfd[i][0]) != i)
		  child_err("dup failed");
		if (close(pfd[i][0]) != 0)
		  child_err("file close failed");
		break;

      case IO_T_PORT:
      case IO_T_REDIR:
      default:
		SCM_ERR("make-process: io type not supported", SCM_MKINUM(io_type[i]));
      }
    }

    if (SCM_STRINGP(arg[3])) {	/* inlined list of string */
      /* reuse the arg[] to put string for the exec */
      for (i = 3; i < argc; i++)
		arg[i - 3] = (void *) SCM_STR_VALUE(arg[i]);
      arg[i - 3] = NULL;
      if (use_execv) {
		execv((void *) arg[0], (char **) arg);
      } else {
		execvp((void *) arg[0], (char **) arg);
      }
      child_err("exec failed");
    }

    if (SCM_PAIRP(arg[3])) {
      arg_array = scm_list_to_vector(arg[3]);
    } else {			/* !list ==> array, we checked this before! */
      arg_array = arg[3];
    }
    scm_vector_append(arg_array, NULL);
    for (i = 0; i < (SCM_ASIZE(arg_array) - 1); i++) {
      if (!SCM_STRINGP(SCM_AREF(arg_array, i)))
		child_err("not a string arg");
      SCM_AREF(arg_array, i) = (void *) SCM_STR_VALUE(SCM_AREF(arg_array, i));
    }
    if (use_execv) {
      execv((void *) SCM_AREF(arg_array, 0), (char **) SCM_ARRAY(arg_array));
    } else {
      execvp((void *) SCM_AREF(arg_array, 0), (char **) SCM_ARRAY(arg_array));
    }
    child_err("exec failed");
  }

  /* parent process */
  proc = scm_process_add();
  SCM_PROCESS_PID(proc) = pid;

  for (i = 0; i < 3; i++) {
    switch (io_type[i]) {
    case IO_T_NULL:
      break;
    case IO_T_PIPE:
      if (close(pfd[i][io_rd[i] ? 0 : 1]) != 0)
		SCM_ERR("make-process: close pipe", NULL);

      SCM_PROCESS_PORT(proc, i) =
		scm_mk_fn_port(pfd[i][io_rd[i] ? 1 : 0], !io_rd[i]);
      break;

    case IO_T_FILE:
      if (close(pfd[i][0]) != 0)
		SCM_ERR("make-process: close file failed", NULL);

      break;

    case IO_T_PORT:
    case IO_T_REDIR:
    default:
      SCM_ERR("make-process: bad io type", SCM_MKINUM(io_type[i]));
    }
  }
  return (proc);
}

/*E* (process-wait PROC) => STATUS */
/*D* Wait for process to terminate and return the exit code of the
  process. */

SOBJ scm_process_wait(SOBJ proc)
{
  int status, result, pid;

  if (!SCM_PROCESSP(proc) && proc != scm_true)
    SCM_ERR("process-wait: bad process", proc);

  if (scm_process_list == NULL)
    SCM_ERR("process-wait: process list is empty", scm_process_list);

  if (SCM_PROCESSP(proc)) {		/* process is specified */
    pid = SCM_PROCESS_PID(proc);
    if (SCM_PROCESS(proc)->exited)  return (scm_false);
  } else {						/* wait for any process */
    pid = -1;
  }
  result = waitpid(pid, &status, 0);
  if (result == -1) 	return(scm_false);
  proc = proc_remove_by_pid(result);
  if (proc) {
    SCM_PROCESS(proc)->status = status;
    SCM_PROCESS(proc)->exited = TRUE;
  }
  return (SCM_MKINUM(status));
}

/*E* (process-use-execv FLAG) => OLD */
/*D* Determine if next make-process will use execv() or execvp(). If
  FLAG is #t, execv() will be used. Returns the previous mode */
SOBJ scm_process_use_execv(SOBJ flag)
{
  SOBJ ret = SCM_MKBOOL(use_execv);

  use_execv = (flag != scm_false) ? TRUE : FALSE;
  return (ret);
}

void scm_init_process()
{
  SOBJ_T_PROCESS = scm_add_type(&scm_process_type);

  atom_pipe = scm_mkatom("pipe");
  atom_null = scm_mkatom("null");

  scm_add_cvar("process-list", &scm_process_list);

  scm_add_cprim("process?", 		scm_processp, 				1);
  scm_add_cprim("process-pid", 		scm_process_pid, 			1);
  scm_add_cprim("process-input", 	scm_process_input, 			1);
  scm_add_cprim("process-output", 	scm_process_output, 		1);
  scm_add_cprim("process-error", 	scm_process_error, 			1);
  scm_add_cprim("process-status", 	scm_process_status, 		1);

  scm_add_cprim("make-process", 	scm_make_process, 			-1);
  scm_add_cprim("process-wait", 	scm_process_wait, 			1);
  scm_add_cprim("process-use-execv",scm_process_use_execv,		1);
}
