/* -*- c -*- */

#define PRIM(lab,name,narg) l_##lab##:
#define PRIMV(lab,name)		l_##lab##:

#define END_PRIM(lab)		e_##lab##:  NEXT
#define RETURN(x)			TOS = (x);
#define VRETURN(x)			{ sp=(void*)env; env=env->next; TOS=(x); }

#define NARGS				(sp - (env->slot))

#define requires(x,t,msg) \
   if(x == NULL||SCM_INUMP(x)||((SOBJ)x)->type!=t){SCM_ERR(msg,x);}

/****************************************************************
 * cell && list
 ****************************************************************/
PRIM(pairp, "pair?", 1)   	/* obj -- bool */
{
  RETURN(SCM_MKBOOL(TOS != NULL && SCM_PAIRP(TOS)));
}
END_PRIM(pairp);

PRIM(cons, "cons", 2) 		/* n2 n1 -- obj */
{
  SOBJ new;
  sresync();
  new = scm_newcell(SOBJ_T_PAIR);
  spop(SCM_CDR(new));
  SCM_CAR(new) = TOS;
  RETURN(new);
}
END_PRIM(cons);

/*E* (list2 OBJ1 OBJ2) => LIST */
/*D* Returns a newly allocated list of 2 elements. */
PRIM(list2, "list2", 2)
{
  SOBJ obj1, obj2;
  spop(obj2);
  obj1 = TOS;
  RETURN(SCM_LIST2(obj1, obj2));
}
END_PRIM(list2);

/*S* (set-car! PAIR OBJ) => #undefined */
/*D* Change the car field of the PAIR to OBJ. */
PRIM(setcar, "set-car!", 2) 	/* value obj -- undefined */
{
  SOBJ value;
  spop(value);
  requires(TOS, SOBJ_T_PAIR, "set-car! wrong arg type");
  SCM_CAR(TOS) = value;
  RETURN(scm_undefined);
}
END_PRIM(setcar);

/*S* (set-cdr! PAIR OBJ) => #undefined */
/*D* Change the cdr field of the PAIR to OBJ. */
PRIM(setcdr, "set-cdr!", 2) 	/* value obj -- undefined */
{
  SOBJ value;
  spop(value);
  requires(TOS, SOBJ_T_PAIR, "set-cdr! wrong arg type");
  SCM_CDR(TOS) = value;
  RETURN(scm_undefined);
}
END_PRIM(setcdr);

PRIM(nullp, "null?", 1) /* obj -- #t | #f */
{
  RETURN(SCM_MKBOOL(SCM_NULLP(TOS)));
}
END_PRIM(nullp);

PRIM(listp, "list?", 1)		/* obj -- len */
{
  RETURN(SCM_MKBOOL(scm_list_length(TOS) >= 0));
}
END_PRIM(listp);

PRIMV(list, "list")	/* objn .. obj2 obj1 -- (obj1 obj2 .. objn) */
{
  SOBJ list, node;
  list = NULL;
  while(sp > env->slot) {
	spop(node);  list = scm_cons(node, list);
  }
  VRETURN(list);
}
END_PRIM(list);


PRIM(length, "length", 1) 		/* obj -- n */
{
  int len;
  if ((len = scm_list_length(TOS)) < 0)
	SCM_ERR("length: cannot calculate", TOS);
  RETURN(SCM_MKINUM(len));
}
END_PRIM(length);

/*E* (nth ELT LIST) => OBJ */
PRIM(nth, "nth", 2)				/* l n -- obj */
{
  SOBJ list;
  int n;
  spop(list);
  if (!SCM_INUMP(TOS))	SCM_ERR("nth: bad number", TOS);
  n = SCM_INUM(TOS);
  while(--n >= 0 && list && SCM_PAIRP(list))
	list = SCM_CDR(list);
  if (!SCM_PAIRP(list)) SCM_ERR("nth: bad list", list);
  RETURN(SCM_CAR(list));
}
END_PRIM(nth);

/*-- need by macro */
/*E* (qq-append2 L1 L2) => (L1 L2) */
PRIM(qq_append2, "qq-append2", 2)	/* l2 l1 -- (l1 l2) */
{
  SOBJ l1, l2, list, *lp, new;
  spop(l2);
  l1 = TOS;
  list = NULL; 	lp = &list;
  if (l1 != NULL && l2 != NULL) {
	while(l1 && SCM_PAIRP(l1)) {
	  new = scm_newcell(SOBJ_T_PAIR);
	  SCM_CAR(new) = SCM_CAR(l1);
	  *lp = new;
	  lp = &SCM_CDR(new);
	}
  }
  *lp = l2;
  RETURN(list);
}
END_PRIM(qq_append2);

/*-- IO */
PRIMV(display, "display") 	/* [ port ] string -- undef */
{
  VRETURN(scm_display2(env->slot[0], (NARGS >= 2) ? env->slot[1] : NULL));
}
END_PRIM(display);

PRIMV(print, "print") 	/* [ port ] string -- undef */
{
  VRETURN(scm_print2(env->slot[0], (NARGS >= 2) ? env->slot[1] : NULL));
}
END_PRIM(print);

PRIMV(write, "write") 	/* [ port ] string -- undef */
{
  VRETURN(scm_write2(env->slot[0], (NARGS >= 2) ? env->slot[1] : NULL));
}
END_PRIM(write);

/*S* (newline [PORT]) => #undefined */
/*D* Output a newline char on port PORT. If no PORT argument is given,
 * newline is sended to current-output-port */
PRIMV(newline, "newline") 	/* [ port ] -- undef */
{
  VRETURN(scm_newline1( NARGS >= 1 ? env->slot[0] : NULL));
}
END_PRIM(newline);

/****************************************************************
 * boolean
 ****************************************************************/
/*S* (not OBJ) => BOOL */
/*D* Returns #t if OBJ is #f, #f otherwise */
PRIM(not, "not", 1)
{
  RETURN(SCM_MKBOOL(TOS == scm_false));
}
END_PRIM(not);

/*S* (boolean? OBJ) => BOOL */
/*D* Returns #t if OBJ is either #t or #f. Otherwise #f is returned. */
PRIM(booleanp, "boolean?", 1)
{
  RETURN(SCM_MKBOOL(SCM_BOOLEANP(TOS)));
}
END_PRIM(booleanp);

/*S* (eq? OBJ1 OBJ2) => BOOL */
/*D* Returns #t if OBJ1 and OBJ2 refer to same scheme object. */
PRIM(eqp, "eq?", 2)
{
  SOBJ n;  spop(n);
  RETURN(SCM_MKBOOL(n == TOS));
}
END_PRIM(eqp);

/****************************************************************
 * control
 ****************************************************************/
/*PRIMV(compile, "compile")
{
  sresync();
  VRETURN(scm_compile(TOS, (NARGS > 1) ? sp[1] : NULL));
}
*/

/*PRIMV(eval, "eval")
{
  sresync();
  TOS = scm_compile(TOS, (NARGS > 1) ? sp[1] : NULL);
  cont->ip = ip; cont->env = env;
  ip = SCM_CODE_CODE(TOS);
  NEXT;
}
*/

/*PRIMV(neval, "neval")
{
  sresync();
#ifdef OLD_COMPILER
  TOS = scm_compile2(TOS, (NARGS > 1) ? sp[1] : NULL);
#else
  TOS = scm_compile(TOS, (NARGS > 1) ? sp[1] : NULL);
#endif
  cont->ip = ip; cont->env = env;
  ip = SCM_CODE_CODE(TOS);
  NEXT;
}
*/

/*PRIM(execute, "execute", 1)
{
  SOBJ code;
  spop(code);
  if (!SCM_CODEP(code)) SCM_ERR("execute: bad code", code);
  
  PUSH_CONT();
  cont->ip = ip;
  ip = (SOBJ*)SCM_CODE_CODE(code);
  NEXT;
}
*/
/*E* (load FILE) => OBJ */
/*D* Interpret the content of the file which name is given in
 * STR. Returns the value of the last evaluated expression */
PRIM(load, "load", 1)
{
  sresync();
  RETURN(scm_load(TOS));
}
END_PRIM(load);

#ifdef OLD
/*S* (apply PROC ARG1 ARG2 ... ARGS) => VALUE*/
/*D* Build an argument list such as (append (list arg1 arg2 ...) args)
 * and call proc with this list as argument */
/*PRIMV(apply, "apply") */
{
  SOBJ list;
  int len, nargs;
  
  spop(proc);

  if (SCM_MACROP(proc)) {
	if (SCM_MACRO_FUNC(proc) == NULL)
	  SCM_ERR("apply: cannot apply to macro", proc);
	proc = SCM_MACRO_FUNC(proc);
  }

  if (NARGS < 1) SCM_ERR("apply: needs at least 2 arguments", proc);

  nargs = NARGS-1;
  
  list = sp[nargs]; /* list should be last */
  if (list) { 
	len = scm_list_length(list);
	if (len < 0)  SCM_ERR("apply: bad argument list", list);

	if (len == 1) {				/* only one element : replace list by elt */
	  sp[nargs] = SCM_CAR(list);
	} else {					/* more than one element */
	  scm_vm_move_stack(sp - (len - 1), sp, nargs);
	  sp -= (len - 1);
	  while(list) {
		sp[nargs++] = SCM_CAR(list);
		list = SCM_CDR(list);
	  }
	}
  } else {						/* pop list */
	spop(list);
  }
  TOS = *sp;
  spush(proc);
  goto l_call;
}

/*PRIM(engine, "engine", 1) */
{
  scm_cprint(TOS);
  RETURN(TOS);
}
#endif
/****************************************************************
 * Symbol
 ****************************************************************/
/*S* (symbol? OBJ) => BOOL */
/*D* Returns #t if OBJ is a symbol, #f otherwise */
PRIM(symbolp, "symbol?", 1)
{
  RETURN(SCM_MKBOOL((SCM_SYMBOLP(TOS) || SCM_ATOMP(TOS))));
}
END_PRIM(symbolp);

/*E* (pure-symbol? OBJ) => BOOL */
/*D* Returns #t if OBJ is a pure symbol, #f otherwise. Pure symbols
 * are binding a name with a value. Quoted symbols are not pure
 * symbols, they are atoms. */
PRIM(pure_symbolp, "pure-symbol?", 1)
{
  RETURN(SCM_MKBOOL(SCM_SYMBOLP(TOS)));
}
END_PRIM(pure_symbolp);

/****************************************************************
 * Keyword
 ****************************************************************/
/*E* (keyword? OBJ) => BOOL */
/*D* Returns #t if OBJ is a keyword, #f otherwise */
PRIM(keywordp, "keyword?", 1)
{
  RETURN(SCM_MKBOOL(SCM_KEYWORDP(TOS)));
}
END_PRIM(keywordp);

/*E* (keyword->string KEYW) => STR */
/*D* Convert a keyword to a string representation */
PRIM(keyw2str, "keyword->string", 1)
{
  RETURN(scm_keyword_to_string(TOS));
}
END_PRIM(keyw2str);

/*E* (string->keyword STR) => KEYWORD */
/*D* Returns the keyword corresponding to the string STR. */

PRIM(str2keyw, "string->keyword", 1)
{
  RETURN(scm_string_to_keyword(TOS));
}
END_PRIM(str2keyw);

/*E* (get-keyword KEYW LIST DEFAULT) => VALUE */
/*D* Search KEYW in the LIST. Returns the value following the keyword
 * or DEFAULT if not found. */
PRIMV(getkeyw, "get-keyword")
{
  if (NARGS < 2)	SCM_ERR("get-keyword: bad number of args", NULL);
  VRETURN(scm_get_keyword(env->slot[0], env->slot[1],
						  (NARGS >= 3) ? env->slot[2]: scm_false));
}
END_PRIM(getkeyw);

/****************************************************************
 * Misc predicates
 ****************************************************************/

/*E* (atom? OBJ) => BOOL */
/*D* Returns #t if OBJ is an atom, #f otherwise */
PRIM(atomp, "atom?", 1)
{
  RETURN(SCM_MKBOOL(SCM_ATOMP(TOS)));
}
END_PRIM(atomp);

/*E* (undefined-object? OBJ) => BOOL */
/*D* Returns #t if OBJ is undefined, #f otherwise */
PRIM(undefinedp, "undefined-object?", 1)
{
  RETURN(SCM_MKBOOL(SCM_OBJTYPE(TOS) == SOBJ_T_UNDEFINED));
}
END_PRIM(undefinedp);

/*E* (unbound-object? OBJ) => BOOL */
/*D* Returns #t if OBJ is unbound, #f otherwise */
PRIM(unboundp, "unbound-object?", 1)
{
  RETURN(SCM_MKBOOL(SCM_OBJTYPE(TOS) == SOBJ_T_UNBOUND));
}
END_PRIM(unboundp);

/*E* (macro? OBJ) => BOOL */
/*D* Returns #t if OBJ is a macro, #f otherwise */
PRIM(macrop, "macro?", 1)
{
  RETURN(SCM_MKBOOL(SCM_MACROP(TOS)));
}
END_PRIM(macrop);

/*E* (add2 n1 n2) => n */
PRIM(add2, "add2", 2)
{
  long r;
  SOBJ n1, n2;
  spop(n2); spop(n1);
  if (SCM_INUMP( (long)n1 & (long)n2 )) {
	r = SCM_INUM(n1) + SCM_INUM(n2);
	n2 = SCM_INUM_RANGE(r) ? SCM_MKINUM(r) : scm_int2bnum(r);
  } else {
	n2 = scm_add2(n1, n2);
  }
  spush(n2);
}
END_PRIM(add2);

