/* -*- tab-width:4; -*- */
/* FILE interface: test
 */
#include "s.h"

int SOBJ_T_FILE;

#define SCM_FILE(x)		(FILE *)(SCM_AUX(x))
#define SCM_FILEP(x)	(SCM_OBJTYPE(x) == SOBJ_T_FILE)

SOBJ scm_file_new(FILE *fp)
{
  SOBJ new = scm_newcell(SOBJ_T_FILE);
  SCM_FILE(new) = fp;
  return(new);
}

SOBJ scm_file2obj(int type, void *p)
{
	SOBJ new = scm_newcell(SOBJ_T_FILE);
	SCM_AUX(new) = p;
	return(new);
}

void *scm_obj2file(SOBJ x)
{
	return(SCM_AUX(x));
}

void scm_file_sweep(SOBJ x)
{
  printf("scm_file_sweep: type=%d, FILE=%p\n", x->type, SCM_AUX(x));
  if (SCM_AUX(x) != NULL)
	fclose(SCM_AUX(x));
  SCM_AUX(x)=NULL;
}

SOBJ scm_file_compare(SOBJ x, SOBJ y)
{
  return(SCM_AUX(x) == SCM_AUX(y) ? scm_true : scm_false);
}

SOBJ_TYPE_DESCR scm_file_type = {
  0,							/* nothing here */
  "file",						/* type name */
  NULL,							/* code to mark */
  scm_file_sweep,				/* code to sweep */
  NULL,							/* howto print */
  NULL,							/* howto write */
  NULL,
  NULL,							/* char reconizer */
  NULL,							/* char parser */
  NULL,							/* word reconizer */
  NULL,							/* word parser */
  scm_file_compare,				/* type value comparison */
  scm_file2obj,					/* cast to obj */
  scm_obj2file,					/* cast to file */
};  

/****************************************************************
 * Interface
 ****************************************************************/
SOBJ scm_fopen(SOBJ fname, SOBJ fmod)
{
  int type, stringp;

  type = SCM_OBJTYPE(fname);
  stringp = SCM_STRINGP(fname);

  if (!SCM_STRINGP(fname)) 	SCM_ERR("bad file name", fname);
  if (!SCM_STRINGP(fmod))	SCM_ERR("bad file mod", fmod);
  return(scm_file_new(fopen(SCM_STR_VALUE(fname),
							SCM_STR_VALUE(fmod))));
}

SOBJ scm_fclose(SOBJ x)
{
  int r;
  if (!SCM_FILEP(x))		SCM_ERR("bad file", x);
  r = fclose(SCM_FILE(x));
  SCM_FILE(x) = NULL;
  return(SCM_MKINUM(r));
}

SOBJ scm_fread(SOBJ str, SOBJ len, SOBJ file)
{
  int r;
  
  if (!SCM_STRINGP(str))	SCM_ERR("bad string", 	str);
  if (!SCM_INUMP(len))		SCM_ERR("bad length", 	len);
  if (!SCM_FILEP(file))		SCM_ERR("bad file",		file);
  scm_str_resize(str, SCM_INUM(len));
  r = fread(SCM_STR_VALUE(str), 1, SCM_INUM(len), SCM_FILE(file));
  return(SCM_MKINUM(r));
}

SOBJ scm_fwrite(SOBJ str, SOBJ file)
{
  int r;
  
  if (!SCM_STRINGP(str))	SCM_ERR("bad string", 	str);
  if (!SCM_FILEP(file))		SCM_ERR("bad file",		file);
  r = fwrite(SCM_STR_VALUE(str), 1, SCM_STR_LEN(str), SCM_FILE(file));
  return(SCM_MKINUM(r));
}

SOBJ scm_fgetline(SOBJ str, SOBJ file)
{
  int r;
  char *buf = NULL;
  int  len = 0;

  if (!SCM_STRINGP(str))	SCM_ERR("bad string", 	str);
  if (!SCM_FILEP(file))		SCM_ERR("bad file",		file);
  
  r = getline(&buf, &len, SCM_FILE(file));
  if (r < 0) {
	if (buf) free(buf);
  } else {
	scm_str_resize(str, r);
	strncpy(SCM_STR_VALUE(str), buf, r);
  }
  return(SCM_MKINUM(r));
}

SOBJ scm_fputs(SOBJ str, SOBJ file)
{
  if (!SCM_STRINGP(str))	SCM_ERR("bad string", 	str);
  if (!SCM_FILEP(file))		SCM_ERR("bad file",		file);
  return(SCM_MKINUM(fputs(SCM_STR_VALUE(str), SCM_FILE(file))));
}

void scm_init_file()
{
  static SOBJ scm_stdin, scm_stdout, scm_stderr;
  
  SOBJ_T_FILE = scm_add_type(&scm_file_type);
  scm_stdin = scm_file_new(stdin);
  scm_stdout = scm_file_new(stdout);
  scm_stderr = scm_file_new(stderr);

  scm_add_cvar("stdin",		&scm_stdin);
  scm_add_cvar("stdout",	&scm_stdout);
  scm_add_cvar("stderr",	&scm_stderr);
  
  scm_add_cprim("fopen",		scm_fopen,		2);
  scm_add_cprim("fclose",		scm_fclose,		1);
  scm_add_cprim("fread",		scm_fread,		3);
  scm_add_cprim("fwrite",		scm_fwrite,		2);
  scm_add_cprim("fgetline",		scm_fgetline,	2);
  scm_add_cprim("fputs",		scm_fputs,		2);
}

