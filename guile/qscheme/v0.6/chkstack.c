/* -*- tab-width:4; -*- */
#include <stdio.h>
#include <stddef.h>

void *cstack_top;

void check_stack()
{
  void *mark[1];

  if ((void*)mark < (void*)cstack_top) {
	printf("#define STACK_GROWS_DOWN\n");
	/*	printf("stack grows down: mark=%p top=%p\n", mark, cstack_top); */
  } else {
	printf("#undef STACK_GROWS_DOWN\n");
	/* printf("stack grows up: mark=%p top=%p\n", mark, cstack_top); */
  }
}


void main()
{
  void *mark;

  cstack_top = &mark;
  check_stack();
  exit(0);
}
