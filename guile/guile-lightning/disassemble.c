#include "disassemble.h"

static void
print_address (bfd_vma adr, disassemble_info *info)
{
  info->fprintf_func (info->stream, "%p", adr);
}

void
disassemble (FILE *stream, bfd_byte *start, bfd_byte *end)
{
  struct disassemble_info info;
  bfd_byte *pc;

  info.fprintf_func = fprintf;
  info.print_address_func = print_address;
  info.stream = stream;
  info.start = start;
  info.end = end;

  pc = info.start;
  while (pc < info.end)
    {
      info.fprintf_func (info.stream, "%08x    ", pc);
      pc += print_insn_i386_intel (pc, &info);
      info.fprintf_func (info.stream, "\n");
    }
}
