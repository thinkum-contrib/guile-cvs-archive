/*
 * Stack handling macros. To be used outside the vm
 */

#ifndef __SCM_STACK_DEFINITIONS__
#define __SCM_STACK_DEFINITIONS__

#ifndef __SP__
#define __SP__			scm_sp
#endif

#define SCM_OPCODE(x)		scm_get_addr(x)->address
#ifdef OLD
#define sclear()		{__SP__ = scm_stack_limit; }
#define sdepth()		scm_stack_limit - __SP__
#define spush(x) 		*(--__SP__) = (x)
#define spush_opc(x)	*(--__SP__) = SCM_OPCODE(x)
#define spop()			*__SP__++
#define sdrop()			__SP__++

#define scm_vmr(v,r)		((v)->r)
#define scm_vm_push(v,x)	{ --scm_vmr(v,sp); *scm_vmr(v,sp) = (SOBJ)(x); }
#define scm_vm_pop(v,x)		{ x = (SOBJ)(*scm_vmr(v,sp));  scm_vmr(v,sp)++; }

#define scm_vm_push_cont(v) \
{ scm_vm_push(v,0); scm_vm_push(v,0); \
  scm_vm_push(v, scm_vmr(v,cont));  scm_vmr(v,cont)=(void*)scm_vmr(v,sp); \
}
#endif /* OLD */


#endif
