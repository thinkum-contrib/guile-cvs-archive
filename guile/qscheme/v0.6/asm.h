#define SCM_ASM_INSTR 	struct _SCM_ASM_INSTR
#define SCM_ASM_CODE 	struct _SCM_ASM_CODE

SCM_ASM_INSTR {
  char *name;
  SOBJ 	*atom;
  void  (*func)(SOBJ code, SOBJ argl);
};

