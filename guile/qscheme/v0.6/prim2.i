/* -*- c -*- */
#define requires(x,t,msg) \
   if(x == NULL||SCM_INUMP(x)||((SOBJ)x)->type!=t){SCM_ERR(msg,x);}

/****************************************************************
 * cell && list
 ****************************************************************/
Prim(pairp, "pair?", 1)   	/* obj -- bool */
{
  TOS = SCM_MKBOOL(TOS != NULL && SCM_PAIRP(TOS));
  NEXT;
}

Prim(cons, "cons", 2) 		/* n2 n1 -- obj */
{
  SOBJ new;
  sresync(); /* need to resync in case of gc */
  new = scm_newcell(SOBJ_T_PAIR);
  spop(SCM_CAR(new));
  SCM_CDR(new) = TOS;
  TOS = new;
  NEXT;
}

/*E* (list2 OBJ1 OBJ2) => LIST */
/*D* Returns a newly allocated list of 2 elements. */
Prim(list2, "list2", 2) 	/* l2 l1 -- new */
{
  SOBJ new, l1;
  sresync();
  spop(l1);
  new = SCM_LIST2(l1, TOS);
  RETURN(new);
}

Prim(car, "car", 1)		/* obj -- obj */
{
  requires(TOS, SOBJ_T_PAIR, "car: wrong arg type");
  TOS = SCM_CAR((SOBJ)TOS);
  NEXT;
}

Prim(cdr, "cdr", 1)		/* obj -- obj */
{
  requires(TOS, SOBJ_T_PAIR, "cdr: wrong arg type");
  TOS = SCM_CDR((SOBJ)TOS);
  NEXT;
}

Prim(setcar, "set-car!", 2) 	/* value obj -- undefined */
{
  SOBJ sym;
  requires(TOS, SOBJ_T_PAIR, "set-car!: wrong arg type");
  spop(sym);
  SCM_CAR(sym) = TOS;
  TOS = scm_undefined;
  NEXT;
}

/*S* (set-cdr! PAIR OBJ) => #undefined */
/*D* Change the cdr field of the PAIR to OBJ. */
Prim(setcdr, "set-cdr!", 2) 	/* value obj -- undefined */
{
  SOBJ sym;
  requires(TOS, SOBJ_T_PAIR, "set-cdr!: wrong arg type");
  spop(sym);
  SCM_CDR(sym) = TOS;
  TOS = scm_undefined;
  NEXT;
}

Prim(nullp, "null?", 1) /* obj -- #t | #f */
{
  TOS = SCM_NULLP(TOS) ? scm_true : scm_false;
  NEXT;
}

Prim(listp, "list?", 1)		/* obj -- len */
{
  TOS = (scm_list_length(TOS) >= 0) ? scm_true : scm_false;
  NEXT;
}

PrimVarargs(list, "list")	/* objn .. obj2 obj1 -- (obj1 obj2 .. objn) */
{
  SOBJ list = NULL;
  SOBJ *l   = (SOBJ*)cont;
  
  sp[0] = TOS;
  while(--l >= sp) list = scm_cons(*l, list);
  VRETURN(list);
}

Prim(length, "length", 1) 		/* obj -- n */
{
  int len = scm_list_length(TOS);
  if (len >= 0) {
	TOS = SCM_MKINUM(len);
	NEXT;
  }
  SCM_ERR("length: cannot calculate", TOS);
}

Prim(nth, "nth", 2)				/* l n -- obj */
{
  SOBJ n;
  int i, limit;
  spop(n);
  if (!SCM_INUMP(n)) SCM_ERR("bad nth index", n);
  for (i=0, limit=SCM_INUM(n);
	   (i<limit) && SCM_PAIRP(TOS);
	   i++, TOS=SCM_CDR(TOS));
  if (!SCM_PAIRP(TOS)) SCM_ERR("bad nth list", TOS);
  RETURN(SCM_CAR(TOS));
}

/*-- need by macro */
Prim(qq_append2, "qq-append2", 2)	/* l2 l1 -- (l1 l2) */
{
  SOBJ l1, new, last, list;
  spop(l1);

  if (l1 == NULL) {                   /* l2 is TOS*/
         NEXT;
  }
  if (!SCM_PAIRP(l1)) {  SCM_ERR("append: argument is not a list", l1); }

  sresync(); /* need to resync in case of gc */
  list = scm_newcell(SOBJ_T_PAIR);
  new = list;
  last = NULL;
  while(1) {
      SCM_CAR(new) = SCM_CAR(l1);
      if (last) { SCM_CDR(last) = new; }
      last = new;
      l1 = SCM_CDR(l1);
      if (l1 == NULL || ! SCM_PAIRP(l1)) break;
      new = scm_newcell(SOBJ_T_PAIR);
  }
  SCM_CDR(new) = TOS;
  TOS = list;
  NEXT;
}

/*-- IO */
PrimVarargs(display, "display") 	/* [ port ] string -- undef */
{
  if (NARGS >= 1) {
	scm_display2(TOS, (NARGS >= 2) ? sp[1] : NULL);
  }
  VRETURN(scm_undefined);
}

PrimVarargs(print, "print") 	/* [ port ] string -- undef */
{
  if (NARGS >= 1) {
	scm_print2(TOS, (NARGS >= 2) ? sp[1] : NULL);
  }
  VRETURN(scm_undefined);
}

PrimVarargs(write, "write") 	/* [ port ] string -- undef */
{
  if (NARGS >= 1) {
	scm_write2(TOS, (NARGS >= 2) ? sp[1] : NULL);
  }
  VRETURN(scm_undefined);
}

/*S* (newline [PORT]) => #undefined */
/*D* Output a newline char on port PORT. If no PORT argument is given,
 * newline is sended to current-output-port */
PrimVarargs(newline, "newline") 	/* [ port ] -- undef */
{
  VRETURN(scm_newline1( NARGS >= 1 ? TOS : NULL));
}

/****************************************************************
 * boolean
 ****************************************************************/
/*S* (not OBJ) => BOOL */
/*D* Returns #t if OBJ is #f, #f otherwise */
Prim(not, "not", 1)
{
  TOS = (TOS == scm_false) ? scm_true : scm_false;  NEXT;
}

/*S* (boolean? OBJ) => BOOL */
/*D* Returns #t if OBJ is either #t or #f. Otherwise #f is returned. */
Prim(booleanp, "boolean?", 1)
{
  TOS = SCM_BOOLEANP( (SOBJ) TOS) ? scm_true : scm_false;  NEXT;
}

/*S* (eq? OBJ1 OBJ2) => BOOL */
/*D* Returns #t if OBJ1 and OBJ2 refer to same scheme object. */
Prim(eqp, "eq?", 2)
{
  SOBJ n;  spop(n);
  TOS = (TOS == n) ? scm_true : scm_false;  NEXT;
}

/****************************************************************
 * control
 ****************************************************************/
PrimVarargs(compile, "compile")
{
  sresync();
  VRETURN(scm_compile(TOS, (NARGS > 1) ? sp[1] : NULL));
}

PrimVarargs(eval, "eval")
{
  sresync();
  TOS = scm_compile(TOS, (NARGS > 1) ? sp[1] : NULL);
  cont->ip = ip; cont->env = env;
  ip = SCM_CODE_CODE(TOS);
  NEXT;
}

PrimVarargs(neval, "neval")
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

Prim(execute, "execute", 1)
{
  SOBJ code;
  spop(code);
  if (!SCM_CODEP(code)) SCM_ERR("execute: bad code", code);
  
  PUSH_CONT();
  cont->ip = ip;
  ip = (SOBJ*)SCM_CODE_CODE(code);
  NEXT;
}

/*E* (load FILE) => OBJ */
/*D* Interpret the content of the file which name is given in
 * STR. Returns the value of the last evaluated expression */
Prim(load, "load", 1)
{
  sresync();
  TOS = scm_load(TOS);
  NEXT;
}

/*S* (apply PROC ARG1 ARG2 ... ARGS) => VALUE*/
/*D* Build an argument list such as (append (list arg1 arg2 ...) args)
 * and call proc with this list as argument */
PrimVarargs(apply, "apply")
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

Prim(engine, "engine", 1)
{
  scm_cprint(TOS);
  RETURN(TOS);
}


/****************************************************************
 * Symbol
 ****************************************************************/
/*S* (symbol? OBJ) => BOOL */
/*D* Returns #t if OBJ is a symbol, #f otherwise */
Prim(symbolp, "symbol?", 1)
{
  RETURN(SCM_MKBOOL((SCM_SYMBOLP(TOS) || SCM_ATOMP(TOS))));
}

/*E* (pure-symbol? OBJ) => BOOL */
/*D* Returns #t if OBJ is a pure symbol, #f otherwise. Pure symbols
 * are binding a name with a value. Quoted symbols are not pure
 * symbols, they are atoms. */
Prim(pure_symbolp, "pure-symbol?", 1)
{
  RETURN(SCM_MKBOOL(SCM_SYMBOLP(TOS)));
}

/****************************************************************
 * Keyword
 ****************************************************************/
/*E* (keyword? OBJ) => BOOL */
/*D* Returns #t if OBJ is a keyword, #f otherwise */
Prim(keywordp, "keyword?", 1)
{
  RETURN(SCM_MKBOOL(SCM_KEYWORDP(TOS)));
}

/*E* (keyword->string KEYW) => STR */
/*D* Convert a keyword to a string representation */
Prim(keyw2str, "keyword->string", 1)
{
  RETURN(scm_keyword_to_string(TOS));
}

/*E* (string->keyword STR) => KEYWORD */
/*D* Returns the keyword corresponding to the string STR. */

Prim(str2keyw, "string->keyword", 1)
{
  RETURN(scm_string_to_keyword(TOS));
}

/*E* (get-keyword KEYW LIST DEFAULT) => VALUE */
/*D* Search KEYW in the LIST. Returns the value following the keyword
 * or DEFAULT if not found. */
PrimVarargs(getkeyw, "get-keyword")
{
  if (NARGS < 2)	SCM_ERR("get-keyword: bad number of args", NULL);
  VRETURN(scm_get_keyword(TOS, sp[1], (NARGS >= 3) ? sp[2]: scm_false));
}

/****************************************************************
 * Misc predicates
 ****************************************************************/

/*E* (atom? OBJ) => BOOL */
/*D* Returns #t if OBJ is an atom, #f otherwise */
Prim(atomp, "atom?", 1)
{
  RETURN(SCM_MKBOOL(SCM_ATOMP(TOS)));
}


/*E* (undefined-object? OBJ) => BOOL */
/*D* Returns #t if OBJ is undefined, #f otherwise */
Prim(undefinedp, "undefined-object?", 1)
{
  RETURN(SCM_MKBOOL(SCM_OBJTYPE(TOS) == SOBJ_T_UNDEFINED));
}

/*E* (unbound-object? OBJ) => BOOL */
/*D* Returns #t if OBJ is unbound, #f otherwise */
Prim(unboundp, "unbound-object?", 1)
{
  RETURN(SCM_MKBOOL(SCM_OBJTYPE(TOS) == SOBJ_T_UNBOUND));
}

/*E* (macro? OBJ) => BOOL */
/*D* Returns #t if OBJ is a macro, #f otherwise */
Prim(macrop, "macro?", 1)
{
  RETURN(SCM_MKBOOL(SCM_MACROP(TOS)));
}
