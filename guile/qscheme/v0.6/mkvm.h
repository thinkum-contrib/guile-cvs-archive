#define INUM_OPTIMIZATION 1

#define NEXT goto __end__

#define VRETURN(value) 									\
{														\
  TOS=value;											\
  sp=(void*)cont+(sizeof(SCM_ContFrame)-sizeof(SOBJ));	\
  cont=cont->next;										\
  NEXT;													\
}

#define RETURN(value) { TOS=(value);  NEXT; }
