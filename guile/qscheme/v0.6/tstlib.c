/* -*- tab-width:4; -*- */
#include <stdio.h>
#include <stdlib.h>
#include "s.h"

char 	tvar_b = 'a';
short	tvar_w = 10;
long	tvar_l = 20;
float	tvar_f = 30.3;
double	tvar_d = 40.4;
char *	tvar_str;
char 	tvar_strbuf[200];
void *  tvar_ptr = &tvar_str;

typedef struct {
  int x;
  int y;
} TestStruct;

TestStruct test_struct;

int SOBJ_T_TEST;

SOBJ scm_mktest(int x)
{
  return(SCM_MKINUM(x << 1));
}

void scm_test_print(SOBJ x, PORT *p)
{
  scm_puts("#<test ");	scm_putn(SCM_INUM(x)>> 1); scm_puts(">");
}

static SOBJ scm_test2obj(int type, void *x)
{
  return(scm_mktest((long)x));
}

static void *scm_obj2test(SOBJ obj)
{
  void *p = (void*)(SCM_INUM(obj) >> 1);
  return(p);
}

SOBJ_TYPE_DESCR scm_test_type = {
  0,
  "test-type",
  NULL,					NULL,				/* mark / sweep */
  scm_test_print,		NULL,				/* print / write */
  NULL,	NULL,			NULL,	NULL,		/* cparse / wparse */
  NULL,									   	/* compare */
  scm_test2obj,			scm_obj2test, 		/* convert test */
};


void test_func()
{
  printf("Hello world\n");

  printf("tvar_b = '%c'\n", tvar_b);
  printf("tvar_w = %d\n",   tvar_w);
  printf("tvar_l = %ld\n",   tvar_l);
  printf("tvar_f = %g\n",   tvar_f);
  printf("tvar_d = %g\n",   tvar_d);
  printf("tvar_str    = '%s'\n", tvar_str);
  printf("tvar_strbuf = '%s'\n", tvar_strbuf);
  printf("tvar_ptr    = %p\n",   tvar_ptr);

  printf("Do you like it ?\n");
}

TestStruct *get_test_struct()
{
  return(&test_struct);
}

void set_test_struct(int x, int y)
{
  test_struct.x = x;
  test_struct.y = y;
}

void print_test_struct()
{
  printf("test_struct { x=%d y=%d };\n",
		 test_struct.x, test_struct.y);
}

void scm_init_tstlib()
{
  printf("initializing dtest\n");

  SOBJ_T_TEST = scm_add_type(&scm_test_type);
  
  tvar_str = strdup("Hello from the C side");
  strcpy(tvar_strbuf, "A static hello");

  /*-- try to callback some scheme function */
  scm_evalstr("(print \"Hello world from scheme\\n\")");

  test_struct.x = 10;
  test_struct.y = 10;
}
  

