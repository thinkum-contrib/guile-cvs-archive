/* -*- tab-width:4; -*-
 *
 * Unix interface for QScheme
 */
#include "s.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <pwd.h>
#include <grp.h>
#include <unistd.h>
#include <dirent.h>

#define SCM_SYSERR(msg) \
	SCM_ERR(msg, (errno >=0 && errno < sys_nerr) ? \
			scm_mkstring((void*)strerror(errno)):NULL)

/*E* (system CMD) => STATUS */
/*D* Execute an external command. CMD is either a string or a list of
 * string. If CMD is a string and the first word of CMD specifies a
 * path and there is no shell meta character, the execv system call is
 * used. If CMD contains shell meta char, the cmd string is passed to
 * /bin/sh. system returns the status of the command.
 */

SOBJ scm_system(SOBJ x)
{
  int pid, listlen, status;
  char *cmd;

  if (!SCM_STRINGP(x) && !SCM_PAIRP(x))	SCM_ERR("bad argument", x);

  if (SCM_STRINGP(x)) {
	cmd = SCM_STR_VALUE(x);
	if ((*cmd != '.' && *cmd != '/') || strpbrk(cmd, "*?[]$'<>") != NULL) {
	  /* no path OR shell meta chars */
	  x = SCM_LIST3(scm_mkstring("/bin/sh"), scm_mkstring("-c"), x);
	} else {
	  x = scm_string_split(scm_mkstring(" \t"), x);
	}
  }
  listlen = scm_list_length(x);
  if (listlen < 0)		SCM_ERR("bad argument", x);

  switch( (pid = fork()) ) {
  case -1:
	SCM_SYSERR("fork failed");
  case 0:
	{
	  char **argv = alloca((listlen + 2) * sizeof(char *));
	  char **p;
	  SOBJ l;

	  p = argv;
	  for (l = x; l; l = SCM_CDR(l)) {
		if (!SCM_STRINGP(SCM_CAR(l)))	SCM_ERR("bad string", SCM_CAR(l));
		/*	fprintf(stderr, "child: argv=%s\n", SCM_STR_VALUE(SCM_CAR(l))); */
		*p++ = SCM_STR_VALUE(SCM_CAR(l));
	  }
	  *p++ = NULL;
	  if (strchr(SCM_STR_VALUE(SCM_CAR(x)), '/') != NULL) {
		/* absolute path given */
		execv(SCM_STR_VALUE(SCM_CAR(x)), argv);
	  } else {					/* no abs path */
		execvp(SCM_STR_VALUE(SCM_CAR(x)), argv);
	  }
	  SCM_SYSERR("exec failed");
	}
  }
  if (wait(&status) == -1) {
	SCM_SYSERR("wait failed");
  }
  return(SCM_MKINUM(status >> 8));
}

/*E* (getuid) => uid */
/*D* Return the current user id */
SOBJ scm_getuid()
{
  return(scm_int2num(getuid()));
}

/*E* (getgid) => gid */
/*D* Return the current group id */
SOBJ scm_getgid()
{
  return(scm_int2num(getgid()));
}

/*E* (getcwd) => STR */
/*D* Returns the current working directory */
SOBJ scm_getcwd()
{
  char path[PATH_MAX+1];
  
  if (getcwd(path, sizeof(path)) == NULL) SCM_SYSERR("getcwd");
  return(scm_mkstring(path));
}

/*E* (chdir STR) => NUM */
/*D* Changes the current directory. */
SOBJ scm_chdir(SOBJ str)
{
  if (!SCM_STRINGP(str))	SCM_ERR("bad director", str);
  return(SCM_MKINUM(chdir(SCM_STR_VALUE(str))));
}

/*E* (access FILE MODE) => BOOL */
/*D* Return #t if access is enabled, #f otherwise */
SOBJ scm_access(SOBJ file, SOBJ mode)
{
  char *p;
  int m;
  if (!SCM_STRINGP(file))	SCM_ERR("bad file", file);
  if (!SCM_STRINGP(mode))	SCM_ERR("bad mode", mode);
  m = 0;
  for (p = SCM_STR_VALUE(mode); *p; p++) {
	switch(*p) {
	case 'r':	m |= R_OK; 	break;
	case 'w':	m |= W_OK; 	break;
	case 'x':	m |= X_OK; 	break;
	case 'f':	m |= F_OK; 	break;
	}
  }
  return(SCM_MKBOOL(access(SCM_STR_VALUE(file), m) == 0));
}

/*E* (symlink OLDPATH NEWPATH) => BOOL */
/*D* Create a symlink named NEWPATH pointing to OLDPATH */
SOBJ scm_symlink(SOBJ old, SOBJ new)
{
  if (!SCM_STRINGP(old))	SCM_ERR("bad path", old);
  if (!SCM_STRINGP(new))	SCM_ERR("bad path", new);
  return(SCM_MKBOOL(symlink(SCM_STR_VALUE(old),	SCM_STR_VALUE(new)) == 0));
}

/*E* (readlink LINK) => BOOL | STR */
/*D* Read content of LINK */
SOBJ scm_readlink(SOBJ file)
{
  char path[PATH_MAX+1];
  if (!SCM_STRINGP(file))	SCM_ERR("bad link", file);
  if (readlink(SCM_STR_VALUE(file),path, sizeof(path)) < 0)
	return(scm_false);
  return(scm_mkstring(path));
}

/*E* (link OLDPATH NEWPATH) => BOOL */
/*D* Create a link named NEWPATH pointing to OLDPATH */
SOBJ scm_link(SOBJ old, SOBJ new)
{
  if (!SCM_STRINGP(old))	SCM_ERR("bad path", old);
  if (!SCM_STRINGP(new))	SCM_ERR("bad path", new);
  return(SCM_MKBOOL(link(SCM_STR_VALUE(old), SCM_STR_VALUE(new)) == 0));
}

/*E* (unlink FILE) => BOOL */
/*D* Remove FILE from filesystem */
SOBJ scm_unlink(SOBJ file)
{
  if (!SCM_STRINGP(file))	SCM_ERR("bad file", file);
  return(SCM_MKBOOL(unlink(SCM_STR_VALUE(file)) == 0));
}

/*E* (rmdir DIR) => BOOL */
/*D* Remove DIR from filesystem */
SOBJ scm_rmdir(SOBJ dir)
{
  if (!SCM_STRINGP(dir))	SCM_ERR("bad dir", dir);
  return(SCM_MKBOOL(rmdir(SCM_STR_VALUE(dir)) == 0));
}

/*E* (mkdir DIR MODE) => BOOL */
SOBJ scm_mkdir(SOBJ dir, SOBJ mode)
{
  if (!SCM_STRINGP(dir))	SCM_ERR("bad dir", dir);
  if (!SCM_INUMP(mode))		SCM_ERR("bad mode", mode);
  return(SCM_MKBOOL(mkdir(SCM_STR_VALUE(dir), SCM_INUM(mode)) == 0));
}

/*E* (rename OLD NEW) => BOOL */
SOBJ scm_rename(SOBJ old, SOBJ new)
{
  if (!SCM_STRINGP(old))	SCM_ERR("bad path", old);
  if (!SCM_STRINGP(new))	SCM_ERR("bad path", new);
  return(SCM_MKBOOL(rename(SCM_STR_VALUE(old), SCM_STR_VALUE(new)) == 0));
}

/*E* (fork) => PID */
SOBJ scm_fork()
{
  return(SCM_MKINUM(fork()));
}

/* helper for exec */
static char **list2arg(SOBJ l)
{
  char **a, **ap;
  SOBJ s;
  int len;

  len = scm_list_length(l);
  if (len < 0)	return(NULL);
  a = scm_must_alloc( (len + 1) * sizeof(char *));
  ap = a;
  while(l) {
	s = SCM_CAR(l);
	if (!SCM_STRINGP(s)) { free(a); return(NULL); }
	*ap++ = SCM_STR_VALUE(s);
	l = SCM_CDR(l);
  }
  *ap = NULL;
  return(a);
}

/*E* (exec PATH ARG ENV) => ? */
SOBJ scm_exec(SOBJ path, SOBJ arg, SOBJ env)
{
  char **argv = NULL;
  char **envp = NULL;

  if (!SCM_STRINGP(path))	SCM_ERR("bad path", path);
  if (!SCM_PAIRP(arg))		SCM_ERR("bad arg",	arg);
  if (env && !SCM_PAIRP(env))	SCM_ERR("bad env",	env);
  
  if ((argv = list2arg(arg)) == NULL)	SCM_ERR("bad argument list", arg);
  if (env &&
	  (envp = list2arg(arg)) == NULL)	SCM_ERR("bad environment list", env);
  
  if (envp) {
	execve(SCM_STR_VALUE(path), argv, envp);
  } else {
	execv(SCM_STR_VALUE(path), argv);
  }
  SCM_SYSERR("exec failed");
  return(NULL);
}

/*E* (wait) => (PID . STATUS) */
SOBJ scm_wait(int narg, SOBJ *arg)
{
  int status = 0;
  int ret = wait(&status);
  return(scm_cons(SCM_MKINUM(ret), SCM_MKINUM(status)));
}

/*E* (nice NUM) => NUM */
SOBJ scm_nice(SOBJ x)
{
  if (!SCM_NUMBERP(x))		SCM_ERR("bad nice value", x);
  return(SCM_MKINUM(nice(scm_number2long(x))));
}

/*E* (kill PID SIG) => NUM */
SOBJ scm_kill(SOBJ pid, SOBJ sig)
{
  if (!SCM_INUMP(pid))		SCM_ERR("bad pid", pid);
  if (!SCM_INUMP(sig))		SCM_ERR("bad sig", sig);

  return(SCM_MKINUM(kill(SCM_INUM(pid), SCM_INUM(sig))));
}

/*E* (getpid) => NUM */
SOBJ scm_getpid()
{
  return(scm_int2num(getpid()));
}

/*E* (getpgrp) => NUM */
SOBJ scm_getpgrp()
{
  return(scm_int2num(getpgrp()));
}

/*E* (exit NUMBER) => never returns */
SOBJ scm_exit(SOBJ x)
{
  if (!SCM_INUMP(x))	SCM_ERR("bad exit status", x);
  exit(SCM_INUM(x));
}

/* DIR type here */
int SOBJ_T_DIR;

#define SCM_DIRP(x)		(SCM_OBJTYPE(x) == SOBJ_T_DIR)
#define SCM_DIR(x)		(DIR *)SCM_AUX(x)

void scm_dir_sweep(SOBJ x)
{
  if (SCM_DIR(x) != NULL) {
	closedir(SCM_DIR(x));
	SCM_DIR(x) = NULL;
  }
}

SOBJ scm_opendir(SOBJ name)
{
  DIR *dir;
  SOBJ new;
  
  if (!SCM_STRINGP(name))	SCM_ERR("bad dir name", name);
  if ((dir = opendir(SCM_STR_VALUE(name))) != NULL) {
	new = scm_newcell(SOBJ_T_DIR);
	SCM_DIR(new) = dir;
	return(new);
  }
  return(scm_false);
}

SOBJ scm_closedir(SOBJ dir)
{
  int r;
  if (!SCM_DIRP(dir))	SCM_ERR("bad dir", dir);
  if (SCM_DIR(dir) == NULL) SCM_ERR("already closed", dir);
  r = closedir(SCM_DIR(dir));
  SCM_DIR(dir) = NULL;
  return(SCM_MKBOOL(r == 0));
}

SOBJ scm_readdir(SOBJ dir)
{
  struct dirent *r;
  if (!SCM_DIRP(dir))	SCM_ERR("bad dir", dir);
  r = readdir(SCM_DIR(dir));
  if (r == NULL) {
	return(scm_false);
  }
  return(scm_mkstring(r->d_name));
}

SOBJ_TYPE_DESCR scm_dir_type_descr = {
  0, "dir",
  NULL,			scm_dir_sweep,
  NULL };


/*E* (stat FILE) => ALIST */

typedef struct {
  char *name;
  SOBJ atom;
} SCM_AMAKER;

static void scm_amaker_fill(SCM_AMAKER *a)
{
  if (a->atom)	return;
  while(a->name) {
	a->atom = scm_mkatom(a->name);
	a++;
  }
}

static SOBJ scm_stat2hash(struct stat *st)
{
  SOBJ h;
  static SCM_AMAKER a[] = {
	{"dev"}, {"ino"}, {"mode"}, {"nlink"},
	{"uid"}, {"gid"}, {"rdev"}, {"size"}, {"blksize"}, {"blocks"},
	{"atime"}, {"mtime"}, {"ctime"}, {NULL}};

  scm_amaker_fill(a);
  h = scm_mkhash(SCM_HASH_T_GEN);
  scm_hash_set(h, a[0].atom,		scm_int2num(st->st_dev));
  scm_hash_set(h, a[1].atom,		scm_int2num(st->st_ino));
  scm_hash_set(h, a[2].atom,		scm_int2num(st->st_mode));
  scm_hash_set(h, a[3].atom,		scm_int2num(st->st_nlink));
  scm_hash_set(h, a[4].atom,		scm_int2num(st->st_uid));
  scm_hash_set(h, a[5].atom,		scm_int2num(st->st_gid));
  scm_hash_set(h, a[6].atom,		scm_int2num(st->st_rdev));
  scm_hash_set(h, a[7].atom,		scm_int2num(st->st_size));
  scm_hash_set(h, a[8].atom,		scm_int2num(st->st_blksize));
  scm_hash_set(h, a[9].atom,		scm_int2num(st->st_blocks));
  scm_hash_set(h, a[10].atom,		scm_int2num(st->st_atime));
  scm_hash_set(h, a[11].atom,		scm_int2num(st->st_mtime));
  scm_hash_set(h, a[12].atom,		scm_int2num(st->st_ctime));
  return(h);
}

SOBJ scm_stat2array(struct stat *st)
{
  SOBJ a = scm_mkarray(13,NULL);
  SCM_AREF(a, 0 ) = scm_int2num(st->st_dev);
  SCM_AREF(a, 1 ) = scm_int2num(st->st_ino);
  SCM_AREF(a, 2 ) = scm_int2num(st->st_mode);
  SCM_AREF(a, 3 ) = scm_int2num(st->st_nlink);
  SCM_AREF(a, 4 ) = scm_int2num(st->st_uid);
  SCM_AREF(a, 5 ) = scm_int2num(st->st_gid);
  SCM_AREF(a, 6 ) = scm_int2num(st->st_rdev);
  SCM_AREF(a, 7 ) = scm_int2num(st->st_size);
  SCM_AREF(a, 8 ) = scm_int2num(st->st_blksize);
  SCM_AREF(a, 9 ) = scm_int2num(st->st_blocks);
  SCM_AREF(a, 10) = scm_int2num(st->st_atime);
  SCM_AREF(a, 11) = scm_int2num(st->st_mtime);
  SCM_AREF(a, 12) = scm_int2num(st->st_ctime);
  return(a);
}

SOBJ scm_stat2list(struct stat *st)
{
  SOBJ l, *lp;
  lp = &l;
  *lp = scm_cons(scm_int2num(st->st_dev),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_ino),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_mode),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_nlink),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_uid),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_gid),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_rdev),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_size),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_blksize),	NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_blocks),	NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_atime),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_mtime),		NULL);  lp = &SCM_CDR(*lp);
  *lp = scm_cons(scm_int2num(st->st_ctime),		NULL);  lp = &SCM_CDR(*lp);
  return(l);
}

SOBJ (*scm_stat2scheme)(struct stat *) = scm_stat2hash;

/*E* (stat-as TYPE) => #undefined */
SOBJ scm_stat_as(SOBJ type)
{
  if (SCM_KEYWORDP(type))  type = SCM_KEYW_NAME(type);
  if (!SCM_ATOMP(type))	SCM_ERR("bad type", type);
  if (SCM_EQ(type, scm_mkatom("hash"))) {
	scm_stat2scheme = scm_stat2hash;
	return(scm_undefined);
  }
  if (SCM_EQ(type, scm_mkatom("vector"))) {
	scm_stat2scheme = scm_stat2array;
	return(scm_undefined);
  }
  if (SCM_EQ(type, scm_mkatom("list"))) {
	scm_stat2scheme = scm_stat2list;
	return(scm_undefined);
  }
  SCM_ERR("unreconized type (should be 'hash or 'vector or 'list)", type);
  return(scm_undefined);
}

/*E* (stat FILE) => STAT */
SOBJ scm_stat(SOBJ x)
{
  struct stat st;
  
  if (!SCM_STRINGP(x))	SCM_ERR("bad file", x);
  
  if (stat(SCM_STR_VALUE(x), &st) != 0) 
	return(scm_false);

  return((*scm_stat2scheme)(&st));
}

SOBJ scm_lstat(SOBJ x)
{
  struct stat st;
  
  if (!SCM_STRINGP(x))	SCM_ERR("bad file", x);
  
  if (lstat(SCM_STR_VALUE(x), &st) != 0) 
	return(scm_false);

  return((*scm_stat2scheme)(&st));
}

/*E* (file-truncate PATH LEN) => BOOL */
SOBJ scm_file_truncate(SOBJ path, SOBJ len)
{
  if (!SCM_STRINGP(path))	SCM_ERR("bad file", path);
  if (!SCM_NUMBERP(len))	SCM_ERR("bad len", len);
  return(SCM_MKBOOL(truncate(SCM_STR_VALUE(path), scm_number2long(len)) == 0));
}

/*E* (chmod PATH MODE) => BOOL */
SOBJ scm_chmod(SOBJ path, SOBJ mode)
{
  if (!SCM_STRINGP(path))	SCM_ERR("bad file", path);
  if (!SCM_INUMP(mode))		SCM_ERR("bad mode", mode);
  return(SCM_MKBOOL(chmod(SCM_STR_VALUE(path), SCM_INUM(mode)) == 0));
}

/*E* (chown PATH UID GID) => BOOL */
SOBJ scm_chown(SOBJ path, SOBJ uid, SOBJ gid)
{
  if (!SCM_STRINGP(path))	SCM_ERR("bad file", path);
  if (!SCM_INUMP(uid))		SCM_ERR("bad uid", uid);
  if (!SCM_INUMP(gid))		SCM_ERR("bad gid", gid);
  return(SCM_MKBOOL(chown(SCM_STR_VALUE(path),
						  SCM_INUM(uid),
						  SCM_INUM(gid)) == 0));
}

/*E* (umask MASK) => PREV */
SOBJ scm_umask(SOBJ mask)
{
  if (!SCM_INUMP(mask))		SCM_ERR("bad mask",	mask);
  return(SCM_MKINUM(umask(SCM_INUM(mask))));
}

/*E* (time) => TIME */
SOBJ scm_time()
{
  time_t t;
  time(&t);
  return(scm_int2num(t));
}

static SOBJ scm_tm2hash(struct tm *t)
{
  SOBJ h;
  static SCM_AMAKER a[] = {
	{"sec"}, {"min"}, {"hour"}, 
	{"mday"}, {"month"}, {"year"}, {"wday"}, {"yday"},
	{"isdst"}, {NULL}};

  scm_amaker_fill(a);
  h = scm_mkhash(SCM_HASH_T_GEN);
  scm_hash_set(h, a[0].atom,	SCM_MKINUM(t->tm_sec));
  scm_hash_set(h, a[1].atom,	SCM_MKINUM(t->tm_min));
  scm_hash_set(h, a[2].atom,	SCM_MKINUM(t->tm_hour));
  scm_hash_set(h, a[3].atom,	SCM_MKINUM(t->tm_mday));
  scm_hash_set(h, a[4].atom,	SCM_MKINUM(t->tm_mon));
  scm_hash_set(h, a[5].atom,	SCM_MKINUM(t->tm_year));
  scm_hash_set(h, a[6].atom,	SCM_MKINUM(t->tm_wday));
  scm_hash_set(h, a[7].atom,	SCM_MKINUM(t->tm_yday));
  scm_hash_set(h, a[8].atom,	SCM_MKINUM(t->tm_isdst));
  return(h);
}

static SOBJ scm_tm2array(struct tm *t)
{
  SOBJ a = scm_mkarray(10, NULL);

  SCM_AREF(a, 0) = SCM_MKINUM(t->tm_sec);
  SCM_AREF(a, 1) = SCM_MKINUM(t->tm_min);
  SCM_AREF(a, 2) = SCM_MKINUM(t->tm_hour);
  SCM_AREF(a, 3) = SCM_MKINUM(t->tm_mday);
  SCM_AREF(a, 4) = SCM_MKINUM(t->tm_mon);
  SCM_AREF(a, 5) = SCM_MKINUM(t->tm_year);
  SCM_AREF(a, 6) = SCM_MKINUM(t->tm_wday);
  SCM_AREF(a, 7) = SCM_MKINUM(t->tm_yday);
  SCM_AREF(a, 8) = SCM_MKINUM(t->tm_isdst);
  return(a);
}

static SOBJ scm_tm2list(struct tm *t)
{
  SOBJ l, *lp;

  lp = &l;
  *lp = scm_cons(SCM_MKINUM(t->tm_sec	), NULL);	lp = &SCM_CDR(*lp);
  *lp = scm_cons(SCM_MKINUM(t->tm_min	), NULL);	lp = &SCM_CDR(*lp);
  *lp = scm_cons(SCM_MKINUM(t->tm_hour	), NULL);	lp = &SCM_CDR(*lp);
  *lp = scm_cons(SCM_MKINUM(t->tm_mday	), NULL);	lp = &SCM_CDR(*lp);
  *lp = scm_cons(SCM_MKINUM(t->tm_mon	), NULL);	lp = &SCM_CDR(*lp);
  *lp = scm_cons(SCM_MKINUM(t->tm_year	), NULL);	lp = &SCM_CDR(*lp);
  *lp = scm_cons(SCM_MKINUM(t->tm_wday	), NULL);	lp = &SCM_CDR(*lp);
  *lp = scm_cons(SCM_MKINUM(t->tm_yday	), NULL);	lp = &SCM_CDR(*lp);
  *lp = scm_cons(SCM_MKINUM(t->tm_isdst	), NULL);	lp = &SCM_CDR(*lp);
  return(l);
}

SOBJ (*scm_tm2scheme)(struct tm *) = scm_tm2hash;

/*E* (tm-as TYPE) => #undefined */
/*D* Specify how the tm structure is converted to scheme: if TYPE is
 * 'hash, tm is converted to a hash where key is the field name. if
 * TYPE is 'vector, each item of the tm are stored in an array, if
 * TYPE is 'list, successive element of list will contain the element
 * in the tm structure, in order. Elements are: (sec min hour mday mon
 * year wday yday isdst). */
SOBJ scm_tm_as(SOBJ type)
{
  if (SCM_KEYWORDP(type))  type = SCM_KEYW_NAME(type);
  if (!SCM_ATOMP(type))	SCM_ERR("bad type", type);
  if (SCM_EQ(type, scm_mkatom("hash"))) {
	scm_tm2scheme = scm_tm2hash;
	return(scm_undefined);
  }
  if (SCM_EQ(type, scm_mkatom("vector"))) {
	scm_tm2scheme = scm_tm2array;
	return(scm_undefined);
  }
  if (SCM_EQ(type, scm_mkatom("list"))) {
	scm_tm2scheme = scm_tm2list;
	return(scm_undefined);
  }
  SCM_ERR("unreconized type (should be 'hash or 'vector or 'list)", type);
  return(scm_undefined);
}

/*E* (localtime NUM) => HASH */
SOBJ scm_localtime(SOBJ time)
{
  time_t t;
  if (!SCM_NUMBERP(time))	SCM_ERR("bad time", time);
  t = scm_number2long(time);
  return( (*scm_tm2scheme)(localtime(&t)));
}

/*E* (gmtime NUM) => HASH */
SOBJ scm_gmtime(SOBJ time)
{
  time_t t;
  if (!SCM_NUMBERP(time))	SCM_ERR("bad time", time);
  t = scm_number2long(time);
  return( (*scm_tm2scheme)(gmtime(&t)));
}

/*E* (mktime LIST) => NUM */
/*D* Return the unix time for the given list. The list should contain:
 * (sec min hour mday mon year) */
SOBJ scm_mktime(SOBJ x)
{
  struct tm t;
  t.tm_sec = t.tm_min = t.tm_hour = t.tm_mday =
	t.tm_mon = t.tm_year =t.tm_wday = t.tm_yday = t.tm_isdst = 0;
  
  if (x && SCM_INUMP(SCM_CAR(x))) {
	t.tm_sec = SCM_INUM(SCM_CAR(x));	x = SCM_CDR(x);
  }
  if (x && SCM_INUMP(SCM_CAR(x))) {
	t.tm_min = SCM_INUM(SCM_CAR(x));	x = SCM_CDR(x);
  }
  if (x && SCM_INUMP(SCM_CAR(x))) {
	t.tm_hour =SCM_INUM(SCM_CAR(x));	x = SCM_CDR(x);
  }
  if (x && SCM_INUMP(SCM_CAR(x))) {
	t.tm_mday =SCM_INUM(SCM_CAR(x));	x = SCM_CDR(x);
  }
  if (x && SCM_INUMP(SCM_CAR(x))) {
	t.tm_mon = SCM_INUM(SCM_CAR(x));	x = SCM_CDR(x);
  }
  if (x && SCM_INUMP(SCM_CAR(x))) {
	t.tm_year =SCM_INUM(SCM_CAR(x));	x = SCM_CDR(x);
  }
  if (x && SCM_INUMP(SCM_CAR(x))) SCM_ERR("bad list", x);

  return(scm_int2num(mktime(&t)));
}

/* split pw entry fields to an array */
static SOBJ split_pwent(struct passwd *p)
{
  SOBJ x;

  if (p == NULL) return(NULL);
  x = scm_mkarray(7, NULL);
  SCM_AREF(x,0) = scm_mkstring(p->pw_name);
  SCM_AREF(x,1) = scm_mkstring(p->pw_passwd);
  SCM_AREF(x,2) = SCM_MKINUM(p->pw_uid);
  SCM_AREF(x,3) = SCM_MKINUM(p->pw_gid);
  SCM_AREF(x,4) = scm_mkstring(p->pw_gecos);
  SCM_AREF(x,5) = scm_mkstring(p->pw_dir);
  SCM_AREF(x,6) = scm_mkstring(p->pw_shell);
  return(x);
}
/* returns: #(name passwd gid (member ...)) */
static SOBJ split_grent(struct group *p)
{
  SOBJ x, l, *lp;
  char **pp;
  if (p == NULL)	return(NULL);
  x = scm_mkarray(4, NULL);
  SCM_AREF(x,0) = scm_mkstring(p->gr_name);
  SCM_AREF(x,1) = scm_mkstring(p->gr_passwd);
  SCM_AREF(x,2) = SCM_MKINUM(p->gr_gid);

  l = NULL; 
  lp = &l;  
  pp = p->gr_mem;
  while(*pp) {
	*lp = scm_cons(scm_mkstring(*pp++), NULL);
	lp = &SCM_CDR(*lp);
  }
  SCM_AREF(x,3) = l;
  return(x);
}

/*E* (getpw*) => #(name pwd uid gid quota comment gcos dir shell expire) */

/*E* (getpwent) => #(name pwd uid gid quota comment gcos dir shell expire) */
/*D* Return the next pwentry */
SOBJ scm_getpwent()
{
  return(split_pwent(getpwent()));
}

/*E* (setpwent) => #undefined */
SOBJ scm_setpwent()
{
  setpwent();  	return(scm_undefined);
}

/*E* (endpwent) => #undefined */
SOBJ scm_endpwent()
{
  endpwent();	return(scm_undefined);
}

/*E* (getpwnam STR) => #(name pwd uid gid quota comment gcos dir shell expire) */
SOBJ scm_getpwnam(SOBJ name)
{
  if (!SCM_STRINGP(name))	SCM_ERR("bad name", name);
  return(split_pwent(getpwnam(SCM_STR_VALUE(name))));
}

/*E* (getpwuid UID) => #(name pwd uid gid quota comment gcos dir shell expire) */
SOBJ scm_getpwuid(SOBJ uid)
{
  if (!SCM_NUMBERP(uid))	SCM_ERR("bad user id", uid);
  return(split_pwent(getpwuid(scm_number2long(uid))));
}
/*E* (getgrent) => #(name passwd gid (member ...)) */
SOBJ scm_getgrent()
{
  return(split_grent(getgrent()));
}
/*E* (setgrent) => #undefined */
SOBJ scm_setgrent()
{
  setgrent();  	return(scm_undefined);
}
/*E* (endgrent) => #undefined */
SOBJ scm_endgrent()
{
  endgrent();  	return(scm_undefined);
}

/*E* (getgrnam NAME) => #(name passwd gid (member ...)) */
SOBJ scm_getgrnam(SOBJ name)
{
  if (!SCM_STRINGP(name))	SCM_ERR("bad name", name);
  return(split_grent(getgrnam(SCM_STR_VALUE(name))));
}

/*E* (getgrgid GID) => #(name passwd gid (member ...)) */
SOBJ scm_getgrgid(SOBJ gid)
{
  if (!SCM_INUMP(gid))		SCM_ERR("bad gid", gid);
  return(split_grent(getgrgid(SCM_INUM(gid))));
}

void scm_init_unix()
{
  scm_add_cprim("system", 		scm_system,			1);
  scm_add_cprim("getuid",		scm_getuid,			0);
  scm_add_cprim("getgid",		scm_getgid,			0);
  scm_add_cprim("getcwd",		scm_getcwd,			0);
  scm_add_cprim("access",		scm_access,			2);
  scm_add_cprim("symlink",		scm_symlink,		2);
  scm_add_cprim("readlink",		scm_readlink,		1);
  scm_add_cprim("link",			scm_link,			2);
  scm_add_cprim("unlink",		scm_unlink,			1);
  scm_add_cprim("rmdir",		scm_rmdir,			1);
  scm_add_cprim("mkdir",		scm_mkdir,			2);
  scm_add_cprim("rename",		scm_rename,			2);

  scm_add_cprim("fork",			scm_fork,			0);
  scm_add_cprim("exec",			scm_exec,			3);
  scm_add_cprim("wait",			scm_wait,			0);

  scm_add_cprim("nice",			scm_nice,			1);
  scm_add_cprim("kill",			scm_kill,			2);

  scm_add_cprim("getpid",		scm_getpid,			0);
  scm_add_cprim("getpgrp",		scm_getpgrp,		0);
  scm_add_cprim("exit",			scm_exit,			1);

  SOBJ_T_DIR = scm_add_type(&scm_dir_type_descr);
  scm_add_cprim("opendir",		scm_opendir,		1);
  scm_add_cprim("closedir",		scm_closedir,		1);
  scm_add_cprim("readdir",		scm_readdir,		1);

  scm_add_cprim("stat-as",		scm_stat_as,		1);
  scm_add_cprim("stat",			scm_stat,			1);
  scm_add_cprim("lstat",		scm_lstat,			1);
  scm_add_cprim("file-truncate",scm_file_truncate,	2);
  scm_add_cprim("chmod",		scm_chmod,			2);
  scm_add_cprim("chown",		scm_chown,			3);
  scm_add_cprim("umask",		scm_umask,			1);

  scm_add_cprim("tm-as",		scm_tm_as,			1);
  scm_add_cprim("time",			scm_time,			0);
  scm_add_cprim("localtime",	scm_localtime,		1);
  scm_add_cprim("gmtime",		scm_gmtime,			1);
  scm_add_cprim("mktime",		scm_mktime,			1);

  scm_add_cprim("getpwent",		scm_getpwent,		0);
  scm_add_cprim("setpwent",		scm_setpwent,		0);
  scm_add_cprim("endpwent",		scm_endpwent,		0);
  scm_add_cprim("getpwnam",		scm_getpwnam,		1);
  scm_add_cprim("getpwuid",		scm_getpwuid,		1);

  scm_add_cprim("getgrent",		scm_getgrent,		0);
  scm_add_cprim("setgrent",		scm_setgrent,		0);
  scm_add_cprim("endgrent",		scm_endgrent,		0);
  scm_add_cprim("getgrnam",		scm_getgrnam,		1);
  scm_add_cprim("getgrgid",		scm_getgrgid,		1);


/*scm_add_cprim("*/
}

