/* -*- tab-width:4; -*- */
#include "s.h"

/*-- make runtime env slot */
SOBJ scm_mkenv(int nslots)
{
  SOBJ new = scm_newcell(SOBJ_T_ENV);
  SCM_ENV_FRAME(new) = scm_must_alloc(offsetof(SCM_EnvFrame, binding[nslots]));
  SCM_ENV_NEXT(new) = NULL;
  SCM_ENV_FRAME(new)->nslots = SCM_MKINUM(nslots);
  return(new);
}

/*-- add symbol to compile time environment */

SOBJ scm_env_add(SOBJ env, SOBJ lsym)
{
  SCM_CAR(env) = scm_cons(lsym, SCM_CAR(env));
  return(env);
}

/*-- add a new level to compile-time environment */
SOBJ scm_env_add_level(SOBJ env)
{
  return(scm_cons(NULL, env));
}

/*-- search env
 * return the lsymbol node if found and set the distance to the depth
 * from the first searched level to the found level...
 * IE if found in first level, distance will be 0
 * if found in 2nd level, distance will be 1 etc...
 */
SOBJ scm_env_search(SOBJ env, SOBJ atom, int *distance)
{
  SOBJ p, s;
  int  l;

  /* level loop */
  for (l=0 ; env ; env = SCM_CDR(env), l++) { 
	if (!SCM_PAIRP(env)) 	SCM_ERR("env: not a pair", env);

	/* loop for vars inside current level */
	for (p = SCM_CAR(env); p; p = SCM_CDR(p)) {
	  if (!SCM_PAIRP(p)) 	SCM_ERR("env: not a pair", p);
	  s = SCM_CAR(p);
	  if (SCM_ENVP(s)) 	continue;
	  if (!SCM_LSYMBOLP(s))	SCM_ERR("env: bad lsymbol or env", s);
	  if (SCM_EQ(SCM_LSYM_NAME(s), atom)) {
		if (distance) { *distance = l; }
		return(s);
	  }
	}
  }
  return(NULL);
}
