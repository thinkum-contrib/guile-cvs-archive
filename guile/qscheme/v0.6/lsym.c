/* -*- tab-width:4; -*- */
#include "s.h"

/*-- make a local symbol node */
SOBJ scm_mklsymbol(SOBJ atom, int ofs)
{
  SOBJ new;

  if (!SCM_ATOMP(atom))	SCM_ERR("mklsymbol: not an atom: ", atom);
  new = scm_newcell(SOBJ_T_LSYMBOL);
  SCM_LSYM_NAME(new) = atom;
  SCM_LSYM_OFS(new) = ofs;
  return(new);
}






