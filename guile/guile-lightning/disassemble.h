#ifndef DISASSEMBLE_H
#define DISASSEMBLE_H

#include <stdio.h>

typedef unsigned char bfd_byte;
typedef unsigned long bfd_vma;

typedef int (*fprintf_ftype) (FILE *, const char*, ...);

struct disassemble_info {
  fprintf_ftype fprintf_func;
  void (*print_address_func) (bfd_vma addr, struct disassemble_info *info);
  FILE *stream;
  
  bfd_byte *start;
  bfd_byte *end;    // exclusive

  void *private;
};

typedef struct disassemble_info disassemble_info;

void disassemble (FILE *stream, bfd_byte *start, bfd_byte *end);

#endif
