#include "_scm.h"
#include "environments.h"
#include "symbols.h"
#include "eval.h"
#include "smob.h"
#include "numbers.h"
#include "alist.h"
#include "weaks.h"
#include "gh.h"

#include <stdio.h>
/* 
 * This file implements first class top-level-environments.  An
 * environment is a datatype that maps `symbols' to `values'.
 *
 * For example:  
 * SCM A = scm_make_leaf_environment();
 * SCM B = scm_make_leaf_environment();
 * scm_environment_define (A, gh_symbol2scm("a"), gh_int2scm(1))
 * scm_environment_define (A, gh_symbol2scm("b"), gh_int2scm(2))
 * scm_environment_define (B, gh_symbol2scm("b"), gh_int2scm(3))
 *
 *                     a           b
 *-----+-------------------------------
 *  A  |               1           2
 *  B  |                           3  
 *  
 *   
 * It has an obarray (hash table) and a list of observers.
 * The obarray's elements are vcells e.g. (a . 1) in environment A.
 *
 * An environment is generic and has the type `scm_tc16_environment'.
 * It responds to the messages: 
 *
 * ref -> return `value' (cdr of vcell)
 * fold -> iterate over all bindings (vcells) 
 * define -> create new binding
 * undefine -> remove binding 
 * set -> set `value' 
 * cell -> return vcell 
 * bound -> #t if binding exists, #f if not
 *
 *
 * This file defines 3 concrete environment types: leaf, eval and
 * interface environments.
 *  */

/*
 * Data structures
 * 
 * cell: (((symbol . value) . (property alist)) . location_tag)
 * CAR (cell) -> location
 * CDR (cell) -> a location tag, either "immutable-location" or
 * SCM_EOL ("mutable-location")
 *
 * vcell: (symbol . value) where value is either a scheme object or a
 * cached cell
 *
 *
 * Cache
 *
 * The following environments cache values (vcells) from other
 * environments: 
 *
 * interface-environment caches values from an eval-environment or
 * interface-environment.
 *
 * interface-environment caches values from $n$ other environments
 *
 * eval-environment caches values from the interface-environment and from
 * its local environment.
 *
 *
 * Cache implementation
 *
 * All caching environments use the following structure: (symbol
 * . value) where value is the cached cell from another environment.
 * Given the eval-environment E which imports bindings from A (see
 * above) and has B as its local environment, E holds the following
 * vcell: (b . ((((b . 3)) . property-list) . mutable-location)).
 * Whenever the location of the binding changes, it will take the new
 * location and store it in the cdr of the vcell. 
 *
 *
 * Update protocol
 *
 * Every environment implements an update procedure with four
 * parameters:
 * 1. the receiver environment `self'
 * 2. the calling environment `caller'
 * 3. a symbol
 * 4. the old cell
 * 5. the new cell
 * 6. the name of the current operation
 *
 * When some environment has changed a binding, it sends out an
 * environment-update message (see broadcast and environment-update)
 * to all observers.  The environment which receives this
 * environment-update message must:
 *
 * 1. find the appropriate cell in its environment
 * 2. check if the cell it found has changed (by comparing with old cell)
 * 3. update the cell.
 * If the environment was not able to update the cell it must:
 * a. re-construct the old state
 * b. send out an update broadcast to all observers
 * c. throw an error
 *
 *
 * Whenever some environment has changed all bindings, it sends out an
 * environment update message with symbol, old cell and new cell set
 * to #f.  Every environment receiving such a message can either:
 *
 * a) update all bindings in its environment by looking up
 *    bindings in caller's environment, or 
 * b) simply clear it's cache.
 *
 * An environment observing an eval environment (or any other
 * user-contributed environments in general) however doesn't have this
 * choice; it must implement algorithm a) if it still wants to receive
 * update requests for specific bindings afterwards.  The
 * eval-environment (or user-contributed environments) need to know
 * for which bindings they have to send out update broadcasts; if they
 * find a vcell in their cache they know that someone cares about this
 * binding and announce a change.
 * 
 * Unmemoization protocoll: Guile's evaluator evals
 * expressions in two steps: 
 *
 * 1. compile code into memoized tree code
 * 2. evaluate the compiled code
 *
 * An eval-environment receiving an update request
 * for a specifig binding must:
 *
 * 1. update all expressions pointing to the old binding
 *
 * An eval environment receiving an update request
 * for all bindings must:
 *
 * 1. unmemoize all expressions in its cache.
 *
 *
 * All standard environments will send an update message iff:
 * a) a binding has been removed from an environment (undefine ...) or
 * b) a new binding in eval-environment's local environment shadows a
 *    binding from its import-environment (define ...) or
 * c) the signature of an interface-environment has changed or
 * d) the imported environment of a eval-environment has changed.
 *   
 */


#define SWAP(a, b) {SCM tmp = a; a = b; b = tmp;}
#define UPDATE_EMPTY_OBARRAY (SCM_VELTS(protect_vec)[0])

#define SCM_OBSERVERP(x) ((SCM_CAR (x)) == scm_tc16_observer)
#define SCM_OBSERVER_STRUCT(observer) ((struct environment_observer*) SCM_CDR(observer))

/* cell structure: (((sym . val) . symbol-property-alist) . (home-environment . tag)) */
#define SCM_CELL_PROPERTY_ALIST SCM_CDAR
#define SCM_CELL_TAG SCM_CDR	/* either immutable or mutable_location */
#define SCM_CELL_VAL SCM_CAAR	/* return the (sym . val) of a cell */
#define SCM_IMMUTABLE_LOCATIONP(tag) (scm_sym_immutable_location==(tag)) 
#define SCM_MUTABLE_LOCATIONP(tag) (!(scm_sym_immutable_location==(tag))) /* cells don't carry `scm_sym_mutable_location' tags */

SCM_SYMBOL (scm_sym_immutable_location, "immutable-location");
SCM_SYMBOL (scm_sym_mutable_location, "mutable-location");
SCM_SYMBOL (scm_sym_alias, "alias");
SCM_SYMBOL (scm_sym_syntax, "syntax");

long scm_tc16_environment;
long scm_tc16_observer;
static SCM protect_vec;

/* the observer list */
struct environment_observer {
  SCM next;
  SCM synapse;

  short weak_p;			/* is scheme weak? */

  /* c is either a default_observer or a user supplied c function. */
  scm_environment_observer c;

  SCM scheme; 
};    
 
/* leaf environments */
struct leaf_environment {
  struct environment environment;
  SCM synapse;
  void (*update) (SCM env, SCM caller, SCM sym, SCM old_cell, SCM new_cell, char *name);
};
  
/* eval environments */
struct eval_environment {
  struct leaf_environment leaf;

  SCM imported;			/* interface-environment */
  SCM local;			/* leaf-environment */
  SCM memoized;			/* weak key hash table */
};

/* interface_environments */
struct interface_environment {
  struct leaf_environment leaf;

  scm_environment_conflict_proc c_conflict_proc;
  short aquisition;

  SCM interface;		/* list of environments with interface symbols */
  SCM conflict_proc;

  short empty;
};



/* manipulate obarrays */

/*
 * Copy symbol to obarray.  The symbol must not already exist in obarray.
 */
static SCM 
scm_symbol_create_handle (obarray,  symbol, scm_hash, init)
     SCM symbol;
     SCM obarray;
     scm_sizet scm_hash;
     SCM init;
{
  SCM sym;

  sym = scm_acons (symbol, init, 
		   SCM_VELTS (obarray)[scm_hash]);
  SCM_VELTS (obarray)[scm_hash] = sym;

  return SCM_CAR (sym);
}

/*
 * look up symbol in obarray
 */
static SCM 
scm_symbol_get_handle (obarray, sym, scm_hash)
     SCM sym;
     SCM obarray;
     scm_sizet scm_hash;
{
  SCM lsym;
  SCM z;

  for (lsym = SCM_VELTS (obarray)[scm_hash]; SCM_NIMP (lsym); lsym = SCM_CDR (lsym))
    {
      z = SCM_CAR (lsym);
      if (SCM_CAR (z) == sym)
	{
	  return z;
	}
    }
  return SCM_BOOL_F;
}


/*
 * remove handle from obarray
 */
static SCM 
scm_symbol_remove_handle (obarray, sym, scm_hash)
     SCM sym;
     SCM obarray;
     scm_sizet scm_hash;
{
  SCM lsym;
  SCM *lsymp;
  SCM z;

  for (lsym = *(lsymp = &SCM_VELTS (obarray)[scm_hash]);
	   SCM_NIMP (lsym);
	   lsym = *(lsymp = SCM_CDRLOC (lsym)))
	{
	  z = SCM_CAR (lsym);
	  if (SCM_CAR (z) == sym)
	    {
	      *lsymp = SCM_CDR (lsym);
	      return z;
	    }
	}
  return SCM_BOOL_F;
}

static void
scm_symbol_remove_all_handles (obarray, size)
     SCM obarray;
     scm_sizet size;
{
  scm_sizet i;

  for (i=0; i<size; i++) 
    {
      SCM_VELTS (obarray)[i] = SCM_EOL;
    }
}



/* error conditions */

/*
 * Throw an error if symbol is not bound in environment func
 */
static void
scm_error_environment_unbound(func, message, args, env, symbol)
     char *func;
     char *message;
     SCM args;
     SCM env;
     SCM symbol;
{
  char error[255] = "Symbol not bound in environment `%s' (symbol: `%s').";
  SCM arguments = scm_cons(env, scm_cons(symbol, args));
  scm_misc_error (func, strcat(error, message), arguments);
}

/*
 * Throw an error if two imported symbols have the same name
 */
static void
scm_error_environment_name_conflict(func, message, args, env, symbol)
     char *func;
     char *message;
     SCM args;
     SCM env;
     SCM symbol;
{
  char error[255] =  "Symbol `%s' imported from `%s' shadows another symbol with the same name.";
  SCM arguments = scm_cons(symbol, scm_cons(env, args));
  scm_misc_error (func, strcat(error, message), arguments);
}

/*
 * Throw an error if symbol has conflicting location tags 
 */
static void
scm_error_environment_conflicting_tags(func, message, args, env, symbol)
     char *func;
     char *message;
     SCM args;
     SCM env;
     SCM symbol;
{
  char error[255] = "In environment `%s':\nThe cell for symbol `%s' can either be imported immutable or mutable, but not both.";
  SCM arguments = scm_cons(env, scm_cons(symbol, args));
  scm_misc_error (func, strcat(error, message), arguments);
}

/*
 * Throw an error if func tried to create (define) or remove
 * (undefine) a new binding for symbol in env 
 */
static void
scm_error_environment_immutable_bindings(func, message, args, env, symbol)
     char *func;
     char *message;
     SCM args;
     SCM env;
     SCM symbol;
{
  char error[255] = "Immutable bindings in environment %s (symbol: `%s').";
  SCM arguments = scm_cons(env, scm_cons(symbol, args));
  scm_misc_error (func, strcat(error, message), arguments);
}

/*
 * Throw an error if func tried to change (set_cdr) a immutable vcell.
 */
static void
scm_error_environment_immutable_location(func, message, args, env, symbol)
     char *func;
     char *message;
     SCM args;
     SCM env;
     SCM symbol;
{
  char error[255] = "Immutable location in environment `%s' (symbol: `%s').";
  SCM arguments = scm_cons(env, scm_cons(symbol, args));
  scm_misc_error (func, strcat(error, message), arguments);
}



/* generic environments */

SCM
scm_c_environment_ref (env, sym)
     SCM env;
     SCM sym;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, "scm_c_environment_ref");
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, "scm_c_environment_ref");

  return SCM_ENVIRONMENT_REF(env, sym);
}

SCM_PROC(s_environment_ref, "environment-ref", 2, 0, 0, scm_environment_ref);
SCM
scm_environment_ref (env, sym)
     SCM env;
     SCM sym;
{
  SCM val;
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_ref);
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, s_environment_ref);
 

 val = SCM_ENVIRONMENT_REF (env, sym);

  if(val == SCM_UNDEFINED)
    scm_error_environment_unbound(s_environment_ref, "", SCM_EOL, env, sym);

  return val;
}


/*
 * the default env_folder
 */
static SCM
scm_default_environment_folder (data, symbol, value, tail)
     SCM data;
     SCM symbol;
     SCM value;
     SCM tail;
{
  SCM answer;
				/* don't pass these values to scheme level */
  if ((value == SCM_UNDEFINED) || (value == SCM_UNSPECIFIED))
     return tail;
  
  answer = gh_call3 (data, symbol, value, tail);
  
  return answer;
}

SCM 
scm_c_environment_fold (env, proc, data, init)
     SCM env;
     scm_environment_folder proc;
     SCM data;
     SCM init;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, "scm_c_environment_fold");

  return SCM_ENVIRONMENT_FOLD(env, proc, data, init);
}
  
SCM_PROC(s_environment_fold, "environment-fold", 3, 0, 0, scm_environment_fold);
SCM
scm_environment_fold(env, proc, init)
     SCM env;
     SCM proc;
     SCM init;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_fold);
  SCM_ASSERT(SCM_NIMP(proc) && (SCM_BOOL_T == scm_procedure_p(proc)), proc, SCM_ARG2, s_environment_fold);
  
  return SCM_ENVIRONMENT_FOLD (env, scm_default_environment_folder, proc, init); 
}

SCM_PROC(s_environment_define, "environment-define", 3, 0, 0, scm_environment_define);
SCM
scm_environment_define(env, sym, val)
     SCM env;
     SCM sym;
     SCM val;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_define);
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, s_environment_define);
 
  SCM_ENVIRONMENT_DEFINE(env, sym, val);

  return SCM_UNSPECIFIED;
}

SCM_PROC(s_environment_undefine, "environment-undefine", 2, 0, 0, scm_environment_undefine);
SCM
scm_environment_undefine(env, sym)
     SCM env;
     SCM sym;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_undefine);
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, s_environment_undefine);

  SCM_ENVIRONMENT_UNDEFINE(env, sym);

  return SCM_UNSPECIFIED;
}

SCM_PROC(s_environment_set_x, "environment-set!", 3, 0, 0, scm_environment_set_x);
SCM
scm_environment_set_x(env,sym,val)
     SCM env;
     SCM sym;
     SCM val;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_set_x);
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, s_environment_set_x);

  SCM_ENVIRONMENT_SET(env, sym, val);

  return SCM_UNSPECIFIED;
}  

SCM
scm_c_environment_cell(env, sym, for_write)
     SCM env;
     SCM sym;
     int for_write;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, "scm_c_environment_cell");
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, "scm_c_environment_cell");

  return SCM_ENVIRONMENT_CELL (env, sym, for_write);
}


SCM_PROC(s_environment_cell, "environment-cell", 3, 0, 0, scm_environment_cell);
SCM
scm_environment_cell(env, sym, for_write)
     SCM env;
     SCM sym;
     SCM for_write;
{
  SCM vcell;

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_cell);
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, s_environment_cell);

  vcell = SCM_ENVIRONMENT_CELL (env, sym, for_write == SCM_BOOL_T);

  if (SCM_IMP (vcell))
    scm_error_environment_unbound(s_environment_cell, "", SCM_EOL, env, sym);

  return vcell;
}

SCM_PROC(s_environment_bound_p, "environment-bound?", 2, 0, 0, scm_environment_bound_p);
SCM
scm_environment_bound_p (env, sym)
     SCM env;
     SCM sym;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_bound_p);
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, s_environment_bound_p);

  return SCM_ENVIRONMENT_BOUND (env, sym);
}

/*
 * call scheme level observer
 * data is observer proc
 */
static void
default_observer (env, data)
     SCM env;
     SCM data;
{
  gh_call1 (data, env);
}

/* 
 * the internal update protocoll uses this function as a tag.  Instead
 * of calling this function it will call environment_update()
 * directly. */
static void
environment_update_dummy_observer (env, data)
     SCM env;
     SCM data;
{
}

static SCM
scm_environment_observe_internal (env, proc, data, weak_p)
     SCM env;
     scm_environment_observer proc;
     SCM data;
     int weak_p;
{
  static SCM scm_make_leaf_observer ();

  return scm_make_leaf_observer(SCM_LEAF_ENVIRONMENT_STRUCT(env), proc, data, weak_p);
}

SCM
scm_c_environment_observe (env, proc, data, weak_p)
     SCM env;
     scm_environment_observer proc;
     SCM data;
     int weak_p;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, "scm_c_environment_observe");

  return SCM_ENVIRONMENT_OBSERVE(env, proc, data, weak_p);
}
  
SCM_PROC(s_environment_observe, "environment-observe", 2, 0, 0, scm_environment_observe);
SCM
scm_environment_observe(env, proc)
     SCM env;
     SCM proc;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_observe);

  return SCM_ENVIRONMENT_OBSERVE (env, default_observer, proc, 0);
}

SCM_PROC(s_environment_observe_weak, "environment-observe-weak", 2, 0, 0, scm_environment_observe_weak);
SCM
scm_environment_observe_weak(env, proc)
     SCM env;
     SCM proc;
{
  static SCM scm_make_leaf_observer ();

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_observe_weak);

  return scm_make_leaf_observer(SCM_LEAF_ENVIRONMENT_STRUCT(env), default_observer, proc, 1);
}


/*
 * Remove observer from list
 */
static void 
environment_unobserve (observer)
     struct environment_observer *observer;
{
  SCM leaf_environment;
  SCM smob;
  struct environment_observer *node, *prev;

				/* does it observe an environment? */
  if (SCM_IMP(observer->synapse)) return;

				/* is the environment still available? */
  leaf_environment = SCM_VELTS (observer->synapse)[0];
  if(SCM_IMP(leaf_environment)) return;

				/* does the environment have an observer-list? */
  smob = SCM_VELTS(SCM_LEAF_ENVIRONMENT_STRUCT(leaf_environment)->synapse)[1];
  if(SCM_IMP(smob)) return;

				/* remove first element */
  node = SCM_OBSERVER_STRUCT(smob);
  if(node == observer) 
    {
      SCM_VELTS(SCM_LEAF_ENVIRONMENT_STRUCT (leaf_environment)->synapse)[1] = node->next;
      node->synapse = SCM_BOOL_F;
    }

				/* remove element from list */
  prev = node;
  smob = node->next;

  while (SCM_NIMP(smob))
    {
      node = SCM_OBSERVER_STRUCT(smob);
      smob = node->next;

      if (node == observer) 
	{
	  prev->next = smob;
	  node->synapse = SCM_BOOL_F;
	  return;
	}
      prev = node;
    }
}

static void
scm_environment_unobserve_internal (dummy, observer_smob)
     SCM dummy;
     SCM observer_smob;
{
  environment_unobserve (SCM_OBSERVER_STRUCT(observer_smob));
}
  
SCM_PROC(s_environment_unobserve, "environment-unobserve", 1, 0, 0, scm_environment_unobserve);
SCM
scm_environment_unobserve (observer_smob)
     SCM observer_smob;
{
  struct environment_observer *observer;
  SCM_ASSERT(SCM_NIMP(observer_smob) && SCM_OBSERVERP(observer_smob),
	     observer_smob, SCM_ARG1, s_environment_unobserve);

  observer = SCM_OBSERVER_STRUCT(observer_smob);
  if(SCM_NIMP (observer->synapse) && SCM_NIMP(SCM_VELTS(observer->synapse)[0]))
    {
      SCM_ENVIRONMENT_UNOBSERVE(SCM_VELTS(observer->synapse)[0], observer_smob);
    }

  return SCM_UNSPECIFIED;
}

/* 
 * tell all obervers to update their bindings 
 */
static void 
scm_environment_broadcast (leaf_env, sym, old_cell, new_cell, name)
     SCM leaf_env;
     SCM sym;			/* #f -> remove all bindings */
     SCM old_cell;		
     SCM new_cell;		/* #f -> remove binding */
     char *name;
{
  struct leaf_environment *leaf_environment = SCM_LEAF_ENVIRONMENT_STRUCT(leaf_env);
  SCM node;

				/* call all observers */
  node = SCM_VELTS(leaf_environment->synapse)[1];
  while (SCM_NIMP (node))
    {
      struct environment_observer *observer =  SCM_OBSERVER_STRUCT(node);
      node = observer->next;

      (observer->c) (leaf_env, observer->scheme);
    }


  /* now the enhanced version for all environments that support it
   (leaf, export, protect, eval and import environment).  Environments
   which do not support the enhanced update protocol (i.e. do not
   share the common structure `leaf_environment') must not call
   environment_observe with argument
   `environment_update_dummy_observer'. */

  node = SCM_VELTS(leaf_environment->synapse)[1];
  while (SCM_NIMP (node))
    {
      struct environment_observer *observer =  SCM_OBSERVER_STRUCT(node);
      node = observer->next;
      
      if(observer->c == environment_update_dummy_observer)
	{
	  (*SCM_LEAF_ENVIRONMENT_STRUCT(observer->scheme)->update)
	    (observer->scheme, leaf_env, sym, old_cell, new_cell, name);
	}
    }
}

static SCM
mark_environment (SCM environment_smob)
{
  struct environment *environment = SCM_ENVIRONMENT_STRUCT(environment_smob);

  scm_gc_mark (environment->obarray);

				/* subtype */
  if(environment->environment_funcs->mark) 
    {
      return (*environment->environment_funcs->mark) (environment_smob);
    }

  return  SCM_BOOL_F;
}

static scm_sizet
free_environment (SCM environment_smob)
{
  struct environment *environment = SCM_ENVIRONMENT_STRUCT(environment_smob);
  scm_sizet size;

  /* free the concrete but not the generic environment */
  size = (*environment->environment_funcs->free) (environment_smob);

  return size;
}


static int
print_environment (SCM environment_smob, SCM port, scm_print_state *pstate)
{
  struct environment_funcs *environment_funcs = 
    SCM_ENVIRONMENT_FUNCS(environment_smob);

  if(environment_funcs->print) 
    {
      return (*environment_funcs->print) (environment_smob, port, pstate);
    }
  else 
    {
      abort(); /* instance of a generic environment!?! */
    }

  return 1;
}

static scm_smobfuns environment_funs = {
  mark_environment, free_environment, print_environment, 0
};

/*
 * Create a concrete environment for type `handle'
 * handle is the parameter for the generic type struct environment. Together
 * they build a concrete (eval-/export- etc) environment.
 *
 * `handle' is a pointer to both struct environment and
 * struct_export/eval/..._environment.  struct environment_funcs
 * is the class signature and has been initialized by the caller.
 */
SCM
scm_make_environment (void *handle, scm_sizet size)
{
  struct environment *environment = (struct environment*) handle;
  SCM environment_smob;
  
  environment->obarray = SCM_BOOL_F;

  SCM_NEWCELL (environment_smob);
  SCM_SETCDR (environment_smob, environment);
  SCM_SETCAR (environment_smob, scm_tc16_environment);

  environment->obarray = 
    scm_make_vector ((SCM) SCM_MAKINUM (size), SCM_EOL);  

  return environment_smob;
}

SCM_PROC(s_environment_p, "environment?", 1, 0, 0, scm_environment_p);
SCM
scm_environment_p(env)
     SCM env;
{
  return (SCM_NIMP(env) && SCM_ENVIRONMENTP(env)) ? SCM_BOOL_T : SCM_BOOL_F;
}




/* observers */

/* after the sweep phase every weak observer will be checked if its
   weak `scheme' data is still valid.  If not, the observer will be
   removed from the observer-list and from guardians  */
struct guardian {
  struct environment_observer observer;
  struct guardian *next;
};

static struct guardian *guardians = 0;

/*
 * create and return a new observer smob observing any of the
 * standard environments (leaf, eval, export, protect and import).
 * If data is weak, create a guard for it.
 */
static SCM
scm_make_leaf_observer (leaf_environment, proc, data, weak_p)
     struct leaf_environment *leaf_environment;
     scm_environment_observer proc;
     SCM data;
     int weak_p;
{
  SCM observer_smob;
  struct environment_observer *observer;

  observer = scm_must_malloc (weak_p ? 
			      sizeof (struct guardian) : 
			      sizeof (struct environment_observer), 
			      "scm_make_leaf_observer");
  
  observer->weak_p = weak_p;
  observer->c = proc;
  observer->scheme = data;
  observer->next = SCM_EOL;

  SCM_NEWCELL (observer_smob);
  SCM_SETCDR (observer_smob, observer);
  SCM_SETCAR (observer_smob, scm_tc16_observer);

  observer->next = SCM_VELTS(leaf_environment->synapse)[1];
  SCM_VELTS(leaf_environment->synapse)[1] = observer_smob;
  observer->synapse = leaf_environment->synapse;

  if(weak_p) /* needs guard */
    {
      SCM_REDEFER_INTS;
      ((struct guardian*) observer)->next = guardians;
      guardians = (struct guardian*) observer;
      SCM_REALLOW_INTS;
    }

  return observer_smob;
}


/*
 * remove the observer_env watching env from env's observer-list
 */
static void 
scm_drop_internal_observer(env, observer_env)
     SCM env;
     SCM observer_env;
{
  struct leaf_environment *leaf_environment = SCM_LEAF_ENVIRONMENT_STRUCT(env);
  SCM node;

  node = SCM_VELTS(leaf_environment->synapse)[1];
  while (SCM_NIMP(node))
    {
      struct environment_observer *observer = SCM_OBSERVER_STRUCT(node);

      if (observer->c == environment_update_dummy_observer && 
	  observer->scheme == observer_env)
	{
	  environment_unobserve(observer);
	  return;
	}
      node = observer->next;
    }
}
  
/*
 * remove guard from guardians
 */
static void
scm_remove_guard (guard)
     struct guardian *guard;
{
  struct guardian *node, *prev;

  if (guardians == guard)
    {
      guardians = guardians->next;
      return;
    }

  prev = guardians; 
  node = guardians->next;
  while (node)
    {
      if (node == guard)
	{
	  prev->next = node->next;
	  return;
	}
      prev = node;
      node = node->next;
    }

  /* may happen when removed by finalize_weaks and then
     gc'ed. */
}

/*
 * remove all observers that contain gc'ed `scheme' data
 * called directly after the sweep phase 
 */
void 
scm_observer_finalize_weaks()
{
  struct guardian *node, *prev;

  while (guardians && SCM_FREEP (guardians->observer.scheme))
    {
      environment_unobserve ((struct environment_observer*)guardians);
      guardians = guardians->next;
    }
  if (!guardians) return;


  prev = guardians;
  node = guardians->next;
  while (node)
    {
      if (SCM_FREEP (node->observer.scheme))
	{
	  environment_unobserve ((struct environment_observer*)node);
	  prev->next = node->next;
	}
      prev = node;
      node = node ->next;
    }
}

static SCM
mark_observer (SCM observer_smob)
{
  struct environment_observer *observer = SCM_OBSERVER_STRUCT(observer_smob);
  
  if (!observer->weak_p)
    {
      scm_gc_mark(observer->scheme);
    }
  
  scm_gc_mark(observer->synapse);

  return SCM_BOOL_F;
}

static scm_sizet
free_observer (SCM observer_smob)
{
  struct environment_observer *observer = SCM_OBSERVER_STRUCT(observer_smob);
  
  if (observer->weak_p)		/* observer has guard */
    {
				/* remove from guarded list */
      scm_remove_guard ((struct guard*) observer);
    }

  free(observer);

  return sizeof observer_smob;
}


static int
print_observer (SCM type, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<observer ", port);
  scm_puts (SCM_CHARS(scm_number_to_string(scm_ulong2num((unsigned long)type), SCM_MAKINUM (16))), port);
  scm_puts (">", port);

  return 1;
}

static scm_smobfuns observer_funs = {
  mark_observer, free_observer, print_observer, 0
};



/* leaf environments */

static SCM
leaf_environment_get_vcell(env, sym)
     SCM env;
     scm_sizet sym;
{
  scm_sizet scm_hash = SCM_HASHCODE (sym);
  return scm_symbol_get_handle (SCM_ENVIRONMENT_OBARRAY(env), sym, scm_hash);
}  

static SCM 
scm_leaf_environment_fold (env, proc, data, init)
     SCM env;
     scm_environment_folder proc;
     SCM data;
     SCM init;
{
  scm_sizet i;
  SCM retcode;
  SCM obarray;

  retcode = init;
  
  obarray = SCM_ENVIRONMENT_OBARRAY(env);

  for (i=0; i<scm_symhash_dim; i++) 
    {
      SCM lsym;
      
      for (lsym = SCM_VELTS (obarray)[i]; SCM_NIMP (lsym); 
	   lsym = SCM_CDR (lsym))
	{
	  SCM vcell;
	  vcell = SCM_CAR (lsym);
	  vcell = SCM_CELL_VAL (SCM_CDR (vcell));
	  retcode = (*proc)(data, SCM_CAR(vcell), SCM_CDR(vcell), retcode);
	} 
    }
  return retcode;
}

static SCM
scm_leaf_environment_define(env, sym, val)
     SCM env;
     SCM sym;
     SCM val;
{
  SCM vcell;
  scm_sizet scm_hash;

  scm_hash = SCM_HASHCODE (sym);
  vcell = scm_symbol_get_handle (SCM_ENVIRONMENT_OBARRAY(env), sym, scm_hash);

  if (SCM_NIMP (vcell))
    {
      SCM_SETCDR (SCM_CELL_VAL (SCM_CDR (vcell)), val);
    }
  else 
    {				/* create a new cell */
      SCM cell = scm_cons (scm_cons (scm_cons (sym, val), SCM_EOL), SCM_EOL);
      vcell = scm_symbol_create_handle (SCM_ENVIRONMENT_OBARRAY(env), sym, scm_hash, cell);
      
    }

  return SCM_CELL_VAL (SCM_CDR (vcell)); /* environment_define returns "real" vcell */
}

static void
scm_leaf_environment_undefine(env, sym)
     SCM env;
     SCM sym;
{
  SCM obarray = SCM_ENVIRONMENT_OBARRAY(env);
  scm_sizet scm_hash = SCM_HASHCODE (sym);
  SCM vcell = scm_symbol_get_handle(obarray, sym, scm_hash);

  if (SCM_IMP (vcell)) return;

  scm_environment_broadcast (env, sym, SCM_CDR(vcell), SCM_BOOL_F, s_environment_undefine);

				/* if succeded ... */
  scm_symbol_remove_handle(obarray, sym, scm_hash);
}

static void
scm_leaf_environment_set_x(env, sym, val)
     SCM env;
     SCM sym;
     SCM val;
{
  SCM vcell = leaf_environment_get_vcell(env, sym);
  
  if (SCM_NIMP (vcell))
    {
      SCM_SETCDR( SCM_CELL_VAL (SCM_CDR (vcell)), val);
    }
  else 
    {
      scm_error_environment_unbound(s_environment_set_x, "", SCM_EOL, env, sym);
    }
}

static SCM 
scm_leaf_environment_ref (env, sym)
     SCM env;
     SCM sym;
{
  SCM vcell = leaf_environment_get_vcell(env, sym);

  if (SCM_NIMP(vcell))
    {
      return SCM_CDR (SCM_CELL_VAL (SCM_CDR (vcell)));
    }
  return SCM_UNDEFINED;
}

static SCM
scm_leaf_environment_cell(env, sym, for_write)
     SCM env;
     SCM sym;
     int for_write;
{
  SCM vcell = leaf_environment_get_vcell(env, sym);

  if (SCM_NIMP(vcell))
    {
      return SCM_CDR (vcell);
    }
  return SCM_BOOL_F;
}

static void 
mark_all_observer_smobs(node)
     SCM node;
{
  while (SCM_NIMP (node))
    {
      struct environment_observer *observer =  SCM_OBSERVER_STRUCT(node);

      scm_gc_mark(node);

      node = observer->next;
    }
}
  
static SCM
mark_leaf_environment (env)
     SCM env;
{
  struct leaf_environment *leaf_environment = SCM_LEAF_ENVIRONMENT_STRUCT(env);

  if(SCM_NIMP(leaf_environment->synapse))
    {
      mark_all_observer_smobs(SCM_VELTS(leaf_environment->synapse)[1]);
      scm_gc_mark(leaf_environment->synapse);
    }

  return  SCM_BOOL_F;
}

static scm_sizet
free_leaf_environment (env)
     SCM env;
{
  struct leaf_environment *leaf_environment = SCM_LEAF_ENVIRONMENT_STRUCT(env);

  scm_sizet size;

  size = sizeof (struct leaf_environment);

  free (leaf_environment);

  return size;
}

static int
print_leaf_environment (SCM type, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<leaf environment ", port);
  scm_puts (SCM_CHARS(scm_number_to_string(scm_ulong2num((unsigned long)type), SCM_MAKINUM (16))), port);
  scm_puts (">", port);
  return 1;
}

struct environment_funcs leaf_environment_funcs = {
  scm_leaf_environment_ref,
  scm_leaf_environment_fold,
  scm_leaf_environment_define,
  scm_leaf_environment_undefine,
  scm_leaf_environment_set_x,
  scm_leaf_environment_cell,
  scm_environment_observe_internal,
  scm_environment_unobserve_internal,
  mark_leaf_environment,
  free_leaf_environment,
  print_leaf_environment
};

void *scm_type_leaf_environment = &leaf_environment_funcs;

SCM_PROC(s_make_leaf_environment, "make-leaf-environment", 0, 0, 0, scm_make_leaf_environment);
SCM
scm_make_leaf_environment ()
{
  SCM leaf_environment_smob;
  struct leaf_environment *leaf_environment = 
    scm_must_malloc (sizeof *leaf_environment, s_make_leaf_environment);

  leaf_environment->environment.environment_funcs = &leaf_environment_funcs;
  leaf_environment->synapse = SCM_EOL;
  leaf_environment->update = 0;

  leaf_environment_smob = scm_make_environment(leaf_environment, scm_symhash_dim);

  leaf_environment->synapse = scm_make_weak_vector (SCM_MAKINUM(2), SCM_EOL);
  SCM_VELTS(leaf_environment->synapse)[0] = leaf_environment_smob;


  return leaf_environment_smob;
}

SCM_PROC(s_leaf_environment_p, "leaf-environment?", 1, 0, 0, scm_leaf_environment_p);
static SCM
scm_leaf_environment_p (env)
     SCM env;
{
  return SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && 
    SCM_LEAF_ENVIRONMENTP(env) ? SCM_BOOL_T : SCM_BOOL_F;
}
 


/* eval environments */ 

static SCM
eval_environment_get_cell(env, sym, for_write, name)
     SCM env;
     SCM sym;
     int for_write;
     char *name;
{
  struct eval_environment *eval_environment =
    SCM_EVAL_ENVIRONMENT_STRUCT(env);

  SCM cell;
  SCM vcell;
  SCM obarray;
  SCM local_obarray;
  scm_sizet scm_hash;
  SCM import;

  obarray = SCM_ENVIRONMENT_OBARRAY (env);
  local_obarray = SCM_ENVIRONMENT_OBARRAY (eval_environment->local);

  scm_hash = SCM_HASHCODE (sym);

				/* in cache? */
  vcell =  scm_symbol_get_handle (obarray, sym, scm_hash);
  if (SCM_NIMP (vcell))
    {
      if (!for_write || 
	 SCM_MUTABLE_LOCATIONP (SCM_CELL_TAG (SCM_CDR(vcell))))
	{
	  return SCM_CDR(vcell); 
	}
      else
	{
	  scm_error_environment_immutable_location(name, "", SCM_EOL, env, sym);
	}
    }
				/* in local? */
  vcell = scm_symbol_get_handle (local_obarray, sym, scm_hash);
  if (SCM_NIMP (vcell))
    {				/* copy to cache */
      vcell = scm_symbol_create_handle(obarray, sym, scm_hash, SCM_CDR(vcell));

      return SCM_CDR(vcell);
    }
      

  import = SCM_EVAL_ENVIRONMENT_IMPORTED(env);
  cell = SCM_ENVIRONMENT_CELL (import, sym, 0); 

  if (SCM_NIMP (cell))
    {
      if(!for_write || 
	 SCM_MUTABLE_LOCATIONP(SCM_CELL_TAG (cell)))
	{			
				/* copy to cache */
	  vcell = scm_symbol_create_handle(obarray, sym, scm_hash, cell);
	  return SCM_CDR(vcell);
	}
      else
	{
	  scm_error_environment_immutable_location(name, "", SCM_EOL, env, sym);
	}
    }
  return SCM_BOOL_F;
}

static SCM
scm_eval_environment_cell(env, sym, for_write)
     SCM env;
     SCM sym;
     int for_write;
{
  return eval_environment_get_cell (env, sym, for_write, s_environment_cell);
}

static SCM 
scm_eval_environment_ref (env, sym)
     SCM env;
     SCM sym;
{
  SCM cell = eval_environment_get_cell(env, sym, 0, s_environment_ref);
    
  if (SCM_NIMP (cell))
    {			    
      return SCM_CDR (SCM_CELL_VAL (cell));
    }

  return SCM_UNDEFINED;
}

static SCM 
scm_eval_environment_fold (env, proc, data, init)
     SCM env;
     scm_environment_folder proc;
     SCM data;
     SCM init;
{
  struct eval_environment *environment = SCM_EVAL_ENVIRONMENT_STRUCT(env);
  return scm_leaf_environment_fold (environment->local, proc, data, init);
}

/*
 * update all memoized expressions to new_vcell+1 or unmemoize them
 * if new_vcell is SCM_UNDEFINED
 */
void
eval_environment_update_memoized (env, old_vcell, new_vcell)
     SCM env;
     SCM old_vcell;
     SCM new_vcell; /* SCM_UNDEFINED -> unmemoize */
{
  SCM lsym;
  SCM *lsymp;
  SCM vcell;
  SCM obarray;
  unsigned int k;

  obarray = SCM_EVAL_ENVIRONMENT_MEMOIZED(env);
  k = scm_ihashq (old_vcell, SCM_LENGTH (obarray));

  for (lsym = *(lsymp = &SCM_VELTS (obarray)[k]); SCM_NIMP (lsym);)
	{
	  vcell = SCM_CAR (lsym);
	  if (SCM_CAR (vcell) == old_vcell)
	    {			/* (vcell sym . expr) */
	      SCM expr = SCM_CDDR (vcell);
	      if (SCM_IMP (new_vcell)) /* undefined: unmemoize expr */
		{
		  SCM sym = SCM_CADR(vcell);

		  SCM_SETCAR(expr, sym);
		  lsym = (*lsymp = SCM_CDR (lsym)); /* remove unmemoized */
		}
	      else		/* re-memoize */
		{
		  SCM_SETCAR (expr, new_vcell + 1);
		  lsym = *(lsymp = SCM_CDRLOC (lsym)); /* next cell */
		}
	    }
	  else
	    {
	      lsym = *(lsymp = SCM_CDRLOC (lsym)); /* next cell */
	    }
	}
}

static void
eval_environment_unmemoize_all(env)
     SCM env;
{
  scm_sizet i;
  SCM obarray;
  SCM lsym;
  SCM *lsymp;

  obarray = SCM_EVAL_ENVIRONMENT_MEMOIZED (env);

  for (i=0; i<scm_symhash_dim; i++) 
    {
      for (lsym = *(lsymp = &SCM_VELTS (obarray)[i]); 
	   SCM_NIMP (lsym); 
	   lsym = (*lsymp = SCM_CDR (lsym))) /* remove unmemoized */
	{
				/* (vcell sym . expr) */
	  SCM vcell = SCM_CAR(lsym);
	  SCM sym = SCM_CADR(vcell);
	  SCM expr = SCM_CDDR(vcell);

	  SCM_SETCAR(expr, sym); /* unmemoize */
	}
    }
}

/*
 * Update the value of old_cell to the value of new_cell.
 * - Do not update a value that has not been cached
 * - old_cell == #f: remove and unmemoize old binding
 * - new_cell == #f: remove and unmemoize all bindings
 * - copy location tags
 */
static void
scm_eval_environment_update (env, caller, sym, old_cell, new_cell, name)
     SCM env;
     SCM caller;
     SCM sym;
     SCM old_cell;
     SCM new_cell;
     char *name;
{
  SCM obarray;
  scm_sizet scm_hash;
  SCM vcell;
  obarray = SCM_ENVIRONMENT_OBARRAY(env);

  if (SCM_NIMP(old_cell))
    {
      scm_hash = SCM_HASHCODE (sym);
      vcell = scm_symbol_get_handle (obarray, sym, scm_hash);
      if (!((SCM_IMP(vcell)) || (SCM_CELL_VAL (SCM_CDR (vcell)) != SCM_CELL_VAL (old_cell))))
	{
	  old_cell = SCM_CDR(vcell);
	  
	  if (SCM_NIMP (new_cell)) 
	    {
	      SCM_SETCDR (vcell, new_cell);
	    }
	  else
	    {
	      vcell = scm_symbol_remove_handle (obarray, sym, scm_hash);
	    }
      
	  scm_environment_broadcast (env, sym, old_cell, new_cell, name);
				/* if succeded ... */
	}

				/* unmemoize all expressions which
                                   capture old_cell in the current
                                   environment */
      eval_environment_update_memoized(env, SCM_CELL_VAL(old_cell),
				       SCM_NIMP (new_cell)? 
				       SCM_CELL_VAL(new_cell) :
				       new_cell);
    }
  else 
    {
      scm_symbol_remove_all_handles(obarray, scm_symhash_dim);
      scm_environment_broadcast (env, sym, old_cell, new_cell, name);
				/* if succeded ... */

      eval_environment_unmemoize_all(env);
    }
}
/*
 * create a new binding in env and send out an update broadcast
 * if we shadow a binding from the uses list.
 *
 * Do not send out an update broadcast if
 * 1. vcell was not found in local
 * 2. environment has no observers
 * 3. vcell was not found in cache, which means that
 * binding was never requested by env's observers.
 */
static SCM
scm_eval_environment_define(env, sym, val)
     SCM env;
     SCM sym;
     SCM val;
{
  struct eval_environment *eval_environment = SCM_EVAL_ENVIRONMENT_STRUCT(env);
  SCM obarray;
  SCM vcell;
  SCM local_environment;
  SCM local_obarray;
  SCM local_vcell;
  scm_sizet scm_hash;

  obarray = eval_environment->leaf.environment.obarray;
  scm_hash = SCM_HASHCODE (sym);
  vcell = scm_symbol_get_handle(obarray, sym, scm_hash);

  local_environment = eval_environment->local;
  local_obarray = SCM_ENVIRONMENT_OBARRAY (local_environment);
  local_vcell = scm_symbol_get_handle (local_obarray, sym, scm_hash);
  

  if (SCM_IMP(local_vcell))
    {
				/* create a cell and copy into local and
                                   into cache */
      SCM local_cell = scm_cons (scm_cons (scm_cons (sym, val), SCM_EOL), SCM_EOL);

      local_vcell = scm_symbol_create_handle (local_obarray, sym, scm_hash, local_cell);
      scm_symbol_create_handle(obarray, sym, scm_hash, local_cell);

      if (SCM_NIMP (vcell))	/* we've shadowed a binding from
                                   useslist */
	{
	  SCM shadowed_cell = SCM_CDR(vcell);

	  scm_environment_broadcast (env, sym, shadowed_cell, local_cell, s_environment_define);

				/* re-memoize */
	  eval_environment_update_memoized(env, SCM_CELL_VAL(shadowed_cell), SCM_CELL_VAL (local_cell));
	}
    }
  else
    {
				/* replace value of existing local cell */
      SCM_SETCDR (SCM_CELL_VAL(SCM_CDR (local_vcell)), val);
    }

  return SCM_CELL_VAL (SCM_CDR (local_vcell));
}

static void
scm_eval_environment_undefine(env, sym)
     SCM env;
     SCM sym;
{
  struct eval_environment *eval_environment = SCM_EVAL_ENVIRONMENT_STRUCT(env);
  SCM shadowed_cell;
  SCM local_environment;
  SCM local_obarray;
  SCM local_vcell;
  SCM local_cell;
  scm_sizet scm_hash;

  scm_hash = SCM_HASHCODE (sym);
  local_environment = eval_environment->local;
  local_obarray = SCM_ENVIRONMENT_OBARRAY (local_environment);
  local_vcell = scm_symbol_get_handle (local_obarray, sym, scm_hash);
  
				/* does it exist? */
  if (SCM_IMP (local_vcell)) return;

  local_cell = SCM_CDR(local_vcell);

				/* did it shadow a binding? */
  shadowed_cell = SCM_ENVIRONMENT_CELL (eval_environment->imported, sym, 0); 


  scm_environment_broadcast (env, sym, local_cell, shadowed_cell, s_environment_undefine);
  
				/* if succeded ... */
  eval_environment_update_memoized(env, SCM_CELL_VAL(local_cell), 
				   SCM_NIMP(shadowed_cell) ? 
				   SCM_CELL_VAL (shadowed_cell) :
				   shadowed_cell);
  

				/* remove from cache and from local */
  scm_symbol_remove_handle (eval_environment->leaf.environment.obarray, sym, scm_hash);
  scm_symbol_remove_handle (local_obarray, sym, scm_hash);
}

static void
scm_eval_environment_set_x(env, sym, val)
     SCM env;
     SCM sym;
     SCM val;
{
  SCM cell = eval_environment_get_cell(env, sym, 1, s_environment_set_x);
    
  if (SCM_NIMP (cell))
    {
      SCM_SETCDR(SCM_CELL_VAL (cell), val);
    }
  else
    {
      scm_error_environment_unbound(s_environment_set_x, "", SCM_EOL, env, sym);
    }
}


static SCM
mark_eval_environment (env)
     SCM env;
{
  struct eval_environment *eval_environment = SCM_EVAL_ENVIRONMENT_STRUCT(env);

  if(SCM_NIMP(eval_environment->leaf.synapse))
    {
      mark_all_observer_smobs(SCM_VELTS(eval_environment->leaf.synapse)[1]);
      scm_gc_mark(eval_environment->leaf.synapse);
    }

  scm_gc_mark (eval_environment->imported);
  scm_gc_mark (eval_environment->local);
  scm_gc_mark (eval_environment->memoized);

  return  SCM_BOOL_F;
}

static scm_sizet
free_eval_environment (env)
     SCM env;
{
  scm_sizet size;
  struct eval_environment *eval_environment = SCM_EVAL_ENVIRONMENT_STRUCT(env);

  size = sizeof (struct eval_environment);

  free (eval_environment);

  return size;
}

static int
print_eval_environment (SCM type, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<eval environment ", port);
  scm_puts (SCM_CHARS(scm_number_to_string(scm_ulong2num((unsigned long)type), SCM_MAKINUM (16))), port);
  scm_puts (">", port);

  return 1;
}


static struct environment_funcs eval_environment_funcs = {
    scm_eval_environment_ref,
    scm_eval_environment_fold,
    scm_eval_environment_define,
    scm_eval_environment_undefine,
    scm_eval_environment_set_x,
    scm_eval_environment_cell,
    scm_environment_observe_internal,
    scm_environment_unobserve_internal,
    mark_eval_environment,
    free_eval_environment,
    print_eval_environment
 };
void *scm_type_eval_environment = &eval_environment_funcs;


SCM_PROC(s_make_eval_environment, "make-eval-environment", 2, 0, 0, scm_make_eval_environment);
SCM
scm_make_eval_environment (local, imported)
     SCM local;
     SCM imported;
{
  struct eval_environment *eval_environment;
  SCM eval_environment_smob;

  SCM_ASSERT(SCM_NIMP(local) && SCM_ENVIRONMENTP(local) && SCM_LEAF_ENVIRONMENTP(local), local, SCM_ARG1, s_make_eval_environment);

  SCM_ASSERT(SCM_NIMP(imported) && SCM_ENVIRONMENTP(imported), imported, SCM_ARG2, s_make_eval_environment);
  
  eval_environment = 
    scm_must_malloc (sizeof *eval_environment, s_make_eval_environment);

  eval_environment->leaf.environment.environment_funcs = &eval_environment_funcs;
  eval_environment->local = local;
  eval_environment->imported = imported;
  eval_environment->memoized = SCM_BOOL_F;

  eval_environment->leaf.synapse = SCM_BOOL_F;
  eval_environment->leaf.update = scm_eval_environment_update;

  eval_environment_smob = scm_make_environment(eval_environment, scm_symhash_dim);

  eval_environment->leaf.synapse = scm_make_weak_vector(SCM_MAKINUM(2), SCM_EOL);
  SCM_VELTS(eval_environment->leaf.synapse)[0] = eval_environment_smob;

  eval_environment->memoized =
    scm_make_weak_key_hash_table ((SCM) SCM_MAKINUM (scm_symhash_dim));

  if (SCM_NIMP (imported))
  {
				/* connect to imported.  Will be dropped when
				   environment_smob disappears */
    SCM_ENVIRONMENT_OBSERVE (imported, 
				      environment_update_dummy_observer,
				      eval_environment_smob, 1);
  }

  /* not necessary to observe local because local cells are changed
     through eval_environment_define/undefine */

  return eval_environment_smob;
}

SCM_PROC(s_eval_environment_imported, "eval-environment-imported", 1, 0, 0, scm_eval_environment_imported);

SCM
scm_eval_environment_imported (env)
     SCM env;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_EVAL_ENVIRONMENTP(env), env, SCM_ARG1, s_eval_environment_imported);

  return SCM_EVAL_ENVIRONMENT_IMPORTED(env);
}

SCM_PROC(s_eval_environment_local, "eval-environment-local", 1, 0, 0, scm_eval_environment_local);

SCM
scm_eval_environment_local (env)
     SCM env;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_EVAL_ENVIRONMENTP(env), env, SCM_ARG1, s_eval_environment_local);

  return SCM_EVAL_ENVIRONMENT_LOCAL(env);
}

/*
 * append expr to the list of memoized cells
 */
void
scm_eval_environment_memoize_cell_internal(env, expr, vcell, sym)
     SCM env;
     SCM expr;
     SCM vcell;
     SCM sym;
{
  SCM obarray;
  unsigned int k;

  obarray = SCM_EVAL_ENVIRONMENT_MEMOIZED (env);
  k = scm_ihashq (vcell, SCM_LENGTH (obarray));

  SCM_VELTS(obarray)[k] = scm_acons(vcell, scm_cons(sym, expr), SCM_VELTS(obarray)[k]);
  SCM_SETCAR (expr, vcell + 1);

}

SCM_PROC(s_eval_environment_set_imported_x, "eval-environment-set-imported!", 2, 0, 0, scm_eval_environment_set_imported_x);
SCM
scm_eval_environment_set_imported_x (env, imported)
     SCM env;
     SCM imported;
{
  struct eval_environment *eval_environment;
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_EVAL_ENVIRONMENTP(env), env, SCM_ARG1, s_eval_environment_set_imported_x);
  SCM_ASSERT(SCM_NIMP(imported) && SCM_ENVIRONMENTP(imported), imported, SCM_ARG2, s_eval_environment_set_imported_x);

  eval_environment = SCM_EVAL_ENVIRONMENT_STRUCT(env);  

  eval_environment_unmemoize_all(env);
  scm_symbol_remove_all_handles(eval_environment->leaf.environment.obarray, scm_symhash_dim);

				/* ask old import-env to remove my old
                                   observer */
  if (SCM_NIMP(eval_environment->imported))
    {
      scm_drop_internal_observer(eval_environment->imported, env);
    }

  eval_environment->imported = imported;

				/* a new observer */
  SCM_ENVIRONMENT_OBSERVE (imported, 
			   environment_update_dummy_observer,
			   env, 1);

				/* environments should update their
                                   caches */
  scm_environment_broadcast (env, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, s_eval_environment_set_imported_x);

  return SCM_UNSPECIFIED;
}

SCM_PROC(s_eval_environment_p, "eval-environment?", 1, 0, 0, scm_eval_environment_p);
SCM
scm_eval_environment_p (env)
     SCM env;
{
  return SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && 
    SCM_EVAL_ENVIRONMENTP(env) ? SCM_BOOL_T : SCM_BOOL_F;
}



/* interface environments */

/*
 * Throw an error if two imported symbols with the same name are bound
 * to two different locations 
 */
static SCM
scm_interface_environment_conflict_proc (data, sym, env1, env2)
     SCM data;
     SCM sym;
     SCM env1;
     SCM env2;
{
				/* whamm! */
  scm_error_environment_name_conflict(SCM_CHARS (data), "", SCM_EOL, env1, sym);

  return 0;
}

static SCM
interface_environment_get_vcell(env, sym, for_write, name)
     SCM env;
     SCM sym;
     int for_write;
     char *name;
{
  struct interface_environment *interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);
  scm_sizet scm_hash = SCM_HASHCODE (sym);
  SCM obarray = interface_environment->leaf.environment.obarray;
  SCM vcell = scm_symbol_get_handle (obarray, sym, scm_hash);

  if (SCM_IMP (vcell))
    {
      /* Special case: aquisition.  Assume that all symbols are
         visible */

      if (interface_environment->aquisition)
	{
	  SCM cell = SCM_ENVIRONMENT_CELL (SCM_CAAR (interface_environment->interface), sym, for_write);

	  if (SCM_NIMP (cell)) 
	    {
				/* copy to cache */
	      vcell = scm_symbol_create_handle(obarray, sym, scm_hash, cell);
	    }
	}
    }
  else				/* found in cache */
    {
      if (for_write && SCM_IMMUTABLE_LOCATIONP (SCM_CELL_TAG (SCM_CDR(vcell))))
	{
	  scm_error_environment_immutable_location(name, "", SCM_EOL, env, sym);
	}
    }
  
  return vcell;
}

static SCM interface_environment_fix_interface(SCM env, struct interface_environment *interface_environment, SCM imports, char *name);

static SCM 
scm_interface_environment_ref_fixed (env, sym)
     SCM env;
     SCM sym;
{
  SCM vcell = interface_environment_get_vcell(env, sym, 0, s_environment_ref);

  if (SCM_NIMP (vcell))
    {				
      return SCM_CDR (SCM_CELL_VAL (SCM_CDR (vcell)));
    }

  return SCM_UNDEFINED;
}

static SCM 
scm_interface_environment_ref(env, sym)
     SCM env;
     SCM sym;
{
  struct interface_environment *interface = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);
  interface_environment_fix_interface(env, interface, interface->interface, s_environment_ref);

  return scm_interface_environment_ref_fixed(env, sym);
}

static void 
no_interface_error(env, imported_environment, name)
     SCM env;
     SCM imported_environment;
     char *name;
{
  scm_misc_error(name, "The export environment `%s' for `%s' doesn't have a signature.", 
		 scm_listify(env, imported_environment,SCM_UNDEFINED));
}

/* 
 * iterate over all symbols mentioned in our interface 
 */
static SCM 
scm_interface_environment_fold_fixed (env, proc, data, init)
     SCM env;
     scm_environment_folder proc;
     SCM data;
     SCM init;
{
  SCM ret_val;

  struct interface_environment *interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);

  if (interface_environment->aquisition)
    {				
      no_interface_error(env, interface_environment->interface, s_environment_fold);
    }
  else
    {
      ret_val = scm_leaf_environment_fold (env, proc, data, init);
    }

  return ret_val;
}
static SCM 
scm_interface_environment_fold(env, proc, data, init)
     SCM env;
     scm_environment_folder proc;
     SCM data;
     SCM init;
{
  struct interface_environment *interface = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);
  interface_environment_fix_interface(env, interface, interface->interface, s_environment_fold);

  return scm_interface_environment_fold_fixed(env, proc, data, init);
}

static SCM
scm_interface_environment_define_fixed(env, sym, val)
     SCM env;
     SCM sym;
     SCM val;
{
  scm_error_environment_immutable_bindings(s_environment_define, "", SCM_EOL, env, sym);
  return 0;
}

static void
scm_interface_environment_undefine_fixed(env, sym)
     SCM env;
     SCM sym;
{
  scm_error_environment_immutable_bindings(s_environment_undefine, "", SCM_EOL, env, sym);
}

static void
scm_interface_environment_set_x_fixed (env, sym, val)
     SCM env;
     SCM sym;
     SCM val;
{
  SCM vcell;

  vcell = interface_environment_get_vcell(env, sym, 1, s_environment_set_x);

  if (SCM_NIMP (vcell))
    {
      vcell = SCM_CELL_VAL (SCM_CDR (vcell));
      SCM_SETCDR(vcell, val);
    }
  else
    {
      scm_error_environment_unbound(s_environment_set_x, "", SCM_EOL, env, sym);
    }
}
static void 
scm_interface_environment_set_x(env, sym, val)
     SCM env;
     SCM sym;
     SCM val;
{
  struct interface_environment *interface = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);
  interface_environment_fix_interface(env, interface, interface->interface, s_environment_set_x);

  scm_interface_environment_set_x_fixed(env, sym, val);
}

static SCM
scm_interface_environment_cell_fixed(env, sym, for_write)
     SCM env;
     SCM sym;
     int for_write;
{
  SCM vcell = interface_environment_get_vcell(env, sym, for_write, s_environment_cell);

  if (SCM_NIMP (vcell))
    {
      return SCM_CDR (vcell); /* (vcell . (list-of-tags)) */
    }

  return SCM_BOOL_F;  
}
static SCM 
scm_interface_environment_cell(env, sym, for_write)
     SCM env;
     SCM sym;
     int for_write;
{
  struct interface_environment *interface = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);
  interface_environment_fix_interface(env, interface, interface->interface, s_environment_cell);

  return scm_interface_environment_cell_fixed(env, sym, for_write);
}

/*
 * Do not respond to update requests until the environment
 * is fixed.
 */
static void
scm_interface_environment_update (env, caller, sym, old_cell, new_cell, name)
     SCM env;
     SCM caller;
     SCM old_cell;
     SCM new_cell;
     char *name;
{
  abort();
}

/*
 * Update the value of old_cell to the value of new_cell.
 * - Do not update a value that has not been cached
 * - old_cell == #f (remove all cells): re-build cache
 * - new_cell == #f (remove cell): throw immutable-binding error
 * - do not copy location tags (the location tag always stays the same)
 * - throw an error on a request to change the location tag from
 *   mutable-location to immutable-location.
 * - Always extract the interface and the exported name from the sig
 */
static void
update_cell(env, sig, sym, obarray, old_cell, new_cell, name)
     SCM env;
     SCM sig;
     SCM sym;
     SCM old_cell;
     SCM new_cell;
     char *name;
{
  SCM vcell;
  SCM tags = scm_assq(sym, SCM_CDR(sig));
  short export_writeable;
  scm_sizet scm_hash;
  short new_cell_is_writeable;
  
  if(SCM_IMP(tags)) return;	/* nothing to update */

  scm_hash = SCM_HASHCODE(sym);     
  tags = SCM_CDR(tags);
     
  export_writeable = SCM_NIMP(scm_sloppy_memq (scm_sym_mutable_location, tags));
  vcell = scm_symbol_get_handle (obarray, sym, scm_hash);

				/* cache inconsistency? */
  if (SCM_IMP(vcell)) abort();

				/* if we receive $n$ requests through
				   different paths it may happen that
				   the cell is already updated */
  if  (SCM_CELL_VAL (SCM_CDR (vcell)) == SCM_CELL_VAL (new_cell)) return;

				/* cache inconsistency? */
  if (SCM_CELL_VAL (SCM_CDR (vcell)) != SCM_CELL_VAL (old_cell)) abort();

  old_cell = SCM_CDR(vcell);
  
  new_cell_is_writeable = SCM_MUTABLE_LOCATIONP (SCM_CELL_TAG (new_cell));
  if (new_cell_is_writeable != export_writeable)
    {
      if (export_writeable)
	{
	  /* the user decided to export a mutable location but
	     we received an immutable location.  Throw an error. */
		  
	  scm_error_environment_immutable_bindings(name, "", SCM_EOL, env, sym);
	}
      else
	{
				/* reset to old tag by constructing a new cell */
	  new_cell = scm_cons (SCM_CAR (new_cell), export_writeable ? scm_sym_mutable_location : scm_sym_immutable_location);
	}
    }
  SCM_SETCDR (vcell, new_cell);
}
static void
update_all_cells(interface, obarray, name)
  SCM interface;
 {
   SCM l;
   SCM s;
		 
   for(l = interface; SCM_NIMP(l); l = SCM_CDR(l))
     {
       SCM spec = SCM_CAR(l);
       SCM env = SCM_CAR(spec);
       SCM sig = SCM_CDR(spec);
       
       for(s = sig; SCM_NIMP(s); s = SCM_CDR(s))
	 {
	   short new_cell_is_writeable;
	   SCM sym = SCM_CAAR(s);
	   SCM tags = SCM_CDAR(s);
	   scm_sizet scm_hash;
	   short export_writeable = SCM_NIMP(scm_sloppy_memq (scm_sym_mutable_location,tags));
	   SCM alias;
	   SCM new_cell;
	  
	   alias = scm_sloppy_assq (scm_sym_alias, tags);
	   if(SCM_NIMP(alias))
	     {
	       alias = SCM_CADR(alias);
	     }
	   else
	     {
	       alias = sym;
	     }

	   new_cell = SCM_ENVIRONMENT_CELL(env, sym, 0);
	   if (SCM_IMP(new_cell))
	     {
	       scm_error_environment_unbound(name, "", SCM_EOL, env, sym);
	     }

	   new_cell_is_writeable = SCM_MUTABLE_LOCATIONP (SCM_CELL_TAG (new_cell));
	   if (new_cell_is_writeable != export_writeable)
	     {
	       if (export_writeable)
		 {
		   /* the user decided to export a mutable location but
		      we received an immutable location.  Throw an error. */
			  
		   scm_error_environment_immutable_bindings(name, "", SCM_EOL, env, sym);
		 }
		      
				/* reset to old tag by constructing a new cell */
	       new_cell = scm_cons (SCM_CAR (new_cell), export_writeable ? scm_sym_mutable_location : scm_sym_immutable_location);
	     }
				/* copy to cache */
	   scm_hash = SCM_HASHCODE (alias);
	   scm_symbol_create_handle (obarray, alias, scm_hash, new_cell);
	 }
     }
 }
static void
scm_interface_environment_update_fixed (env, caller, sym, old_cell, new_cell, name)
     SCM env;
     SCM caller;
     SCM old_cell;
     SCM new_cell;
     char *name;
{
  struct interface_environment *interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);
  SCM obarray;

  obarray = interface_environment->leaf.environment.obarray;

  if(interface_environment->aquisition)
    {
      scm_symbol_remove_all_handles(obarray, scm_symhash_dim);
    }
  else
    {
      if (SCM_NIMP (old_cell)) 
	{
	  SCM sig;

	  if (SCM_IMP(new_cell))
	    {
	      scm_error_environment_immutable_bindings(name, "", SCM_EOL, env, sym);
	    }

	  sig = scm_assq(caller, interface_environment->interface);
	  if (SCM_IMP (sig)) 
	    {
	      abort();
	    }
	  update_cell(env, sig, sym, obarray, old_cell, new_cell, name);
	}
      else
	{
	  scm_symbol_remove_all_handles(obarray, scm_symhash_dim);
	  update_all_cells(interface_environment->interface, obarray, name);
	}
    }
				/* environments should update their
                                   caches */
  scm_environment_broadcast (env, sym, old_cell, new_cell, name);
}

static SCM
mark_interface_environment (env)
     SCM env;
{
  struct interface_environment *interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);

  if(SCM_NIMP(interface_environment->leaf.synapse))
    {
      mark_all_observer_smobs(SCM_VELTS(interface_environment->leaf.synapse)[1]);
      scm_gc_mark(interface_environment->leaf.synapse);
    }

  scm_gc_mark (interface_environment->interface);
  scm_gc_mark (interface_environment->conflict_proc);

  return  SCM_BOOL_F;
}

static scm_sizet
free_interface_environment (env)
     SCM env;
{
  struct interface_environment *interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);

  scm_sizet size;
  
  size = sizeof (struct interface_environment);

  free (interface_environment);

  return size;
}

static int
print_interface_environment (SCM type, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<incomplete interface environment ", port);
  scm_puts (SCM_CHARS(scm_number_to_string(scm_ulong2num((unsigned long)type), SCM_MAKINUM (16))), port);
  scm_puts (">", port);

  return 1;
}
static int
print_interface_environment_fixed (SCM type, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<interface environment ", port);
  scm_puts (SCM_CHARS(scm_number_to_string(scm_ulong2num((unsigned long)type), SCM_MAKINUM (16))), port);
  scm_puts (">", port);

  return 1;
}

static struct environment_funcs interface_environment_funcs_fixed = {
    scm_interface_environment_ref_fixed,
    scm_interface_environment_fold_fixed,
    scm_interface_environment_define_fixed, /* throw error */
    scm_interface_environment_undefine_fixed, /* throw error */
    scm_interface_environment_set_x_fixed,
    scm_interface_environment_cell_fixed,
    scm_environment_observe_internal,
    scm_environment_unobserve_internal,
    mark_interface_environment,		
    free_interface_environment,		
    print_interface_environment_fixed		
};
void *scm_type_interface_environment_fixed = &interface_environment_funcs_fixed;
static struct environment_funcs interface_environment_funcs = {
  scm_interface_environment_ref, /* fix it */
  scm_interface_environment_fold, /* fix it */
  scm_interface_environment_define_fixed, /* throw error */
  scm_interface_environment_undefine_fixed, /* throw error */
  scm_interface_environment_set_x, /* fix it */
  scm_interface_environment_cell, /* fix it */
  scm_environment_observe_internal,
  scm_environment_unobserve_internal,
  mark_interface_environment,		
  free_interface_environment,		
  print_interface_environment		
};
void *scm_type_interface_environment = &interface_environment_funcs;

/*
 * parse something like this: (sym (alias a) mutable-location)
 */
static 
SCM syntax_parse_location_tags(tags_list, name, sym_ret, tags_ret)
     SCM tags_list;
     char *name;
     SCM *sym_ret;
     SCM *tags_ret;
{
  SCM alias_sym;
  SCM tags = tags_list;

  SCM_ASSERT(SCM_NIMP(tags), tags_list, SCM_ARG2, name);
	  
  if (SCM_SYMBOLP(tags))
    {
      alias_sym = *sym_ret = tags_list;
      *tags_ret = SCM_EOL;

      return alias_sym;
    }
  else
    {
      SCM_ASSERT(SCM_CONSP(tags), tags_list, SCM_ARG2, name);

      alias_sym = *sym_ret = SCM_CAR(tags);
      SCM_ASSERT(SCM_NIMP(alias_sym) && SCM_SYMBOLP(alias_sym), tags_list, SCM_ARG2, name);

      *tags_ret = tags = SCM_CDR (tags);
    }

  for(; SCM_NIMP(tags); tags = SCM_CDR(tags))
    {
      SCM tag = SCM_CAR(tags);
      if(SCM_NIMP(tag) && SCM_SYMBOLP(tag)) 
	{
	  SCM_ASSERT((scm_sym_immutable_location == tag) 
		     || (scm_sym_mutable_location == tag) 
		     || (scm_sym_syntax == tag), tags_list, SCM_ARG2, name);
	}
      else
	{
	  SCM sym_alias;	/* the name "alias" */

	  SCM_ASSERT(SCM_NIMP(tag) && 
		     SCM_CONSP(tag) && 
		     SCM_NNULLP(SCM_CDR(tag)) &&
		     SCM_NULLP(SCM_CDDR(tag)), tags_list, SCM_ARG2, name);

	  sym_alias = SCM_CAR(tag);
	  SCM_ASSERT(scm_sym_alias == sym_alias, tags_list, SCM_ARG2, name);
	  
	  alias_sym = SCM_CADR(tag);
	  SCM_ASSERT(SCM_NIMP(alias_sym) && SCM_SYMBOLP(alias_sym), tags_list, SCM_ARG2, name);
	}
    }

  return alias_sym;
}	  

/* 
 * Copy all symbols and their values from parent.
 */
static SCM
copy_from_parent(env, obarray, imported_obarray, imported_environment, conflict_proc, name)
     SCM env;
     SCM obarray;
     SCM imported_environment;
     scm_environment_conflict_proc conflict_proc;
     char *name;
{
  scm_sizet i;
  SCM signature_ret = SCM_EOL;

				/* Can't use environment-fold because
                                   we need the cell */
  for (i=0; i<scm_symhash_dim; i++) 
    {
      SCM lsym;
      
      for (lsym = SCM_VELTS (imported_obarray)[i]; SCM_NIMP (lsym); 
	   lsym = SCM_CDR (lsym))
	{
	  SCM old_vcell;
	  SCM vcell = SCM_CAR (lsym);
	  SCM cell = SCM_CDR (vcell);
	  SCM sym = SCM_CAR(vcell);
	  SCM scm_hash = SCM_HASHCODE(sym);

	  signature_ret = scm_cons(SCM_LIST1(sym), signature_ret);

				/* check for name clashes */
	  old_vcell = scm_symbol_get_handle (obarray, sym, scm_hash);
	  if (SCM_NIMP(old_vcell))
	    {
	      SCM old_cell = SCM_CDR(old_vcell);
	      if (SCM_CELL_VAL (old_cell) != SCM_CELL_VAL (cell))
		{
		  (*conflict_proc) (scm_makfrom0str (name), sym, imported_environment, imported_environment);

		}
	      if(SCM_CELL_TAG(old_cell) != SCM_CELL_TAG(cell))
		{
		  scm_error_environment_conflicting_tags(name, "", SCM_EOL, env, sym);
		}
	    }
	  else 
	    {
				/* copy to cache */
	      scm_symbol_create_handle (obarray, sym, scm_hash, cell);
	    }
	}
    }

  return signature_ret;
}
  
/*
 * parse the interface `imports' into obarray and return the
 * parsed interface.  
 */
static SCM
interface_environment_parse_interface_internal (env, imports, obarray, conflict_proc, name)
     SCM env;
     SCM imports;
     SCM obarray;
     scm_environment_conflict_proc conflict_proc;
     char *name;
{
  SCM signature_ret;
  SCM interface_ret;
  SCM sig;
  SCM cell;
  SCM imported_cell;
  SCM import_list;
  SCM old_vcell;

  interface_ret = SCM_EOL;
				/* traverse import list */
  for (import_list = imports; SCM_NIMP (import_list); import_list = SCM_CDR (import_list))
    {
      scm_sizet scm_hash;
      SCM imported_interface_env;
      SCM spec;
      SCM signature;

		
      spec = SCM_CAR(import_list); /* (env ...) or (env . #f) */

      imported_interface_env = SCM_CAR(spec);
      SCM_ASSERT(SCM_NIMP(imported_interface_env) && SCM_ENVIRONMENTP(imported_interface_env), 
		 imported_interface_env, SCM_ARG2, name);

      /* 
       * get the signature 
       */
      signature = SCM_CDR(spec);/* (sym ...) or #f */

      if(SCM_BOOL_F == signature)
	{		
	  SCM signature_ret;
	  if(SCM_INTERFACE_ENVIRONMENTP(imported_interface_env) ||
	     (SCM_ENVIRONMENT_FUNCS(imported_interface_env) == scm_type_export_environment) ||
	     (SCM_ENVIRONMENT_FUNCS(imported_interface_env) == scm_type_import_environment))
	    {
				/* delegate to parent */
	      struct interface_environment *interface_environment = 
		SCM_INTERFACE_ENVIRONMENT_STRUCT(imported_interface_env);
	      
				/* if it not fixed, fix it now! */
	      if(SCM_ENVIRONMENT_FUNCS(imported_interface_env) == scm_type_interface_environment)
		{
		  interface_environment_fix_interface(imported_interface_env,
						      interface_environment, 
						      interface_environment->interface, 
						      name);
		}

	      if(interface_environment->aquisition)
		{
		  no_interface_error(env, imported_interface_env, name);
		}

	      signature_ret = copy_from_parent(env,
					       obarray,
					       interface_environment->leaf.environment.obarray, 
					       imported_interface_env, 
					       conflict_proc, name);
	      
	      interface_ret = scm_cons(scm_cons(imported_interface_env, signature_ret), interface_ret);
	      continue;
	    }
	  else
	    {		
				/* no signature: throw error */
	      no_interface_error(env, imported_interface_env, name);
	    }
	}
      else 
	{			/* signature must be a list */
	  SCM_ASSERT((SCM_NIMP(signature) && SCM_CONSP(signature)) || SCM_NULLP(signature), signature, SCM_ARG2, name);
	}

      /*
       * parse it
       */
      signature_ret = SCM_EOL;
      for(sig = signature; SCM_NIMP(sig); sig = SCM_CDR(sig))
	{
	  SCM alias;
	  SCM sym;
	  SCM tags;

	  alias = syntax_parse_location_tags(SCM_CAR(sig), name, &sym, &tags);
	  signature_ret = scm_cons(scm_cons(sym, tags), signature_ret);

				/* find cell in environment */
	  cell = imported_cell = SCM_ENVIRONMENT_CELL(imported_interface_env, sym, 0);
      
	  if (SCM_NIMP (cell))
	    {			
	      SCM old_tag;
	      int export_writeable;

				/* check the location tag */
	      old_tag = SCM_CELL_TAG (cell);
	      export_writeable = SCM_NIMP(scm_sloppy_memq (scm_sym_mutable_location, tags));

	      if (export_writeable)
		{
				/* check if only one tag has been specified */
		  if (SCM_NIMP(scm_sloppy_memq (scm_sym_immutable_location, tags)))
		    {
		      scm_error_environment_conflicting_tags(name, "", SCM_EOL, env, sym);
		    }
	      
				/* throw error if old tag was immutable */
		  if (old_tag == scm_sym_immutable_location)
		    {
		      scm_error_environment_immutable_location(name, "", SCM_EOL, env, sym);
		    }
		}
	      else
		{
				/* set immutable tag by constructing a new cell */
		  if (old_tag != scm_sym_immutable_location) /* SCM_EOL means: writable! */
		    {
		      cell = scm_cons (SCM_CAR (cell), scm_sym_immutable_location);

		    }
		}
	    }
	  else
	    {
	      scm_error_environment_unbound(name, "", SCM_EOL, imported_interface_env, sym);
	    }

	  /*
	   * copy to cache
	   */
	  scm_hash = SCM_HASHCODE (alias);

				/* check for name clashes */
	  old_vcell = scm_symbol_get_handle (obarray, alias, scm_hash);
	  if (SCM_NIMP(old_vcell))
	    {
	      SCM old_cell = SCM_CDR(old_vcell);
	      if (SCM_CELL_VAL (old_cell) != SCM_CELL_VAL (cell))
		{
		  (*conflict_proc) (scm_makfrom0str (name), alias, imported_interface_env, imported_interface_env);

		}
	      if(SCM_CELL_TAG(old_cell) != SCM_CELL_TAG(cell))
		{
		  scm_error_environment_conflicting_tags(name, "", SCM_EOL, env, alias);
		}
	    }
	  else 
	    {
				/* copy to cache */
	      scm_symbol_create_handle (obarray, alias, scm_hash, cell);
	    }
	}
      interface_ret = scm_cons(scm_cons(imported_interface_env, signature_ret), interface_ret);
    }
  return interface_ret;
}

/*
 * same as parse_interface but also handles aquisition
 */
static SCM
interface_environment_parse_interface (env, interface_environment, imports, obarray, name)
     SCM env;
     struct interface_environment *interface_environment;
     SCM imports;
     SCM obarray;
     char *name;
{
  SCM parsed_interface;

  if(!interface_environment->aquisition)
    {
      if (interface_environment->aquisition == -1) abort(); /* Unknown!?! */

       parsed_interface = 
	 interface_environment_parse_interface_internal(env, 
							imports, 
							obarray,
							interface_environment->c_conflict_proc, 
							name);
    }
  else
    {
				/* nothing to parse */
      parsed_interface = imports;
    }

  return parsed_interface;
}

/*
 * fixit_args, fixit, fixit_rewind, fixit_failed,
 * interface_environment_fix_interface, scm_interface_environment_ref,
 * scm_interface_environment_fold, scm_interface_environment_set_x,
 * scm_interface_environment_cell: Initially lookup bindings, resolve
 * name clashes and make sure that during interface-env's lifetime these
 * functions will never be called again.
 *
 */


static short update_running = 0;	/* sanity check */
struct fixit_args 
{
  SCM env; 
  struct interface_environment *interface_environment;
  SCM interface;
  SCM old_interface;
  struct environment_funcs *old_environment_funcs;
  char *name;
};

/*
 * Observe all environments mentioned in interface
 */
static void
interface_environment_observe_interface(env, interface, name)
     SCM env;
     SCM interface;
     char *name;
{
  SCM node;
				/* connect to all imported_env.  Will be
				   dropped when env disappears. */
  for (node = interface; SCM_NIMP (node); node = SCM_CDR (node))
    {
      SCM spec;
      SCM imported_environment; 

      				/* (env ...) or (env . #f) */
      spec = SCM_CAR(node);
      SCM_ASSERT(SCM_NIMP(spec) && SCM_CONSP(spec), spec, SCM_ARG2, name);

      imported_environment = SCM_CAR(spec);
      SCM_ASSERT(SCM_NIMP(imported_environment) && SCM_ENVIRONMENTP(imported_environment), imported_environment, SCM_ARG2, name);


      SCM_ENVIRONMENT_OBSERVE (imported_environment,
			       environment_update_dummy_observer,
			       env, 1);
    }
}

/*
 * Drop env's old observers from interface_environment's observer list
 * */
static void
interface_environment_drop_old_observers(env, interface_environment)
     SCM env;
     struct interface_environment *interface_environment;
{
  SCM node;
       
				/* remove my old observers */
  for (node = interface_environment->interface; SCM_NIMP(node); node = SCM_CDR(node)) 
    {
				/* tell environment to remove me from
                                   its list */
      scm_drop_internal_observer(SCM_CAAR(node), env);
    }
  
}     

/* 
 * check for aquisition
 */
static short
aquisition(interface, name)
     SCM interface;
     char *name;
{
  SCM l;
  long length = scm_ilength(interface);
  SCM_ASSERT(length >=0, interface, SCM_ARG1, name);

  for (l = interface; SCM_NIMP(l); l = SCM_CDR(l))
    {
      SCM spec;
      SCM environment;
      
      spec = SCM_CAR(l);
      SCM_ASSERT(SCM_NIMP(spec) && SCM_CONSP(spec), interface, SCM_ARG1, name);
      
      environment = SCM_CAR(spec);
      SCM_ASSERT(SCM_NIMP(environment) && SCM_ENVIRONMENTP(environment), interface, SCM_ARG1, name);

      if(SCM_BOOL_T == SCM_CDR(spec)) 
	{
	  SCM_ASSERT(length = 1, interface, SCM_ARG1, name);

	  return 1;
	}
    }
  return 0;
}

/*
 * try
 */
static SCM
fixit(void *p)
{
  SCM parsed_interface;
  struct fixit_args *u = (struct fixit_args*)p;

  if(u->interface_environment->empty != 1) abort();

  update_running++;

  u->interface_environment->aquisition = aquisition(u->interface, u->name);
  parsed_interface = interface_environment_parse_interface(u->env, 
							   u->interface_environment,
							   u->interface, 
							   u->interface_environment->leaf.environment.obarray,
							   u->name);

  u->interface_environment->interface = parsed_interface;
  u->interface_environment->leaf.environment.environment_funcs = &interface_environment_funcs_fixed;
  u->interface_environment->leaf.update = scm_interface_environment_update_fixed;

  /* Because we've been an incomplete interface environment we don't
     have listeners yet. So don't call environment-update() at this point! */
     
				/* now we're ready to receive update
                                   requests */
  interface_environment_observe_interface(u->env, u->interface, u->name);

  update_running--;

  return SCM_UNSPECIFIED;
}

/*
 * re-construct old state after either parse_interface or
 * scm_environment_broadcast failed 
 */
static SCM
fixit_rewind(void *p)
{
  struct fixit_args *u = (struct fixit_args*)p;

  scm_symbol_remove_all_handles(u->interface_environment->leaf.environment.obarray, scm_symhash_dim);

  u->interface_environment->interface = u->old_interface;
  u->interface_environment->aquisition = -1; /* Unknown */

  u->interface_environment->leaf.environment.environment_funcs = u->old_environment_funcs;
  u->interface_environment->leaf.update = scm_interface_environment_update;

  update_running --;

  return SCM_UNSPECIFIED;
}

/*
 * back to old state or abort()
 */
static SCM
fixit_failed(void *p, SCM tag, SCM args)
{
  scm_internal_catch(SCM_BOOL_T, fixit_rewind, p, (scm_catch_handler_t)abort, 0);
  
  scm_ithrow(tag, args, 1);

  abort();

  return 0;
}

/*
 * interface_environment_fix_interface: fix env's interface, once and
 * for all 
 */
static SCM
interface_environment_fix_interface(env, interface_environment, imports, name)
     SCM env;
     struct interface_environment *interface_environment;
     SCM imports;
     char *name;
{
  struct fixit_args fixit_args;

				/* abort if fixed */
  if(interface_environment->leaf.environment.environment_funcs != scm_type_interface_environment)
   {
     abort();
   }

  fixit_args.env = env;
  fixit_args.interface_environment = interface_environment;
  fixit_args.interface = imports;
  fixit_args.old_interface = interface_environment->interface;
  fixit_args.old_environment_funcs = interface_environment->leaf.environment.environment_funcs;
  fixit_args.name = name;

  scm_internal_catch(SCM_BOOL_T, fixit, &fixit_args, fixit_failed, &fixit_args);

  interface_environment->empty = 0;
  return interface_environment->interface;
}

static SCM
make_interface_environment(interface_environment, interface, name)
     struct interface_environment *interface_environment;
     SCM interface;
     char *name;
{
  SCM interface_environment_smob;

  interface_environment->leaf.environment.environment_funcs = scm_type_interface_environment;

  interface_environment->aquisition = -1; /* Unknown */
  interface_environment->interface = interface;

  interface_environment->leaf.synapse = SCM_BOOL_F;
  interface_environment->leaf.update = scm_interface_environment_update;

  interface_environment_smob = scm_make_environment(interface_environment, scm_symhash_dim);

  interface_environment->empty = 1;

  interface_environment->leaf.synapse = scm_make_weak_vector(SCM_MAKINUM(2), SCM_EOL);
  SCM_VELTS(interface_environment->leaf.synapse)[0] = interface_environment_smob;

  return interface_environment_smob;
}

SCM_PROC(s_make_interface_environment, "make-interface-environment", 1, 0, 0, scm_make_interface_environment);
SCM
scm_make_interface_environment (interface)
     SCM interface;
{
  SCM interface_environment_smob;
  struct interface_environment *interface_environment;

  SCM_ASSERT(SCM_NULLP(interface) || SCM_NIMP(interface), interface, SCM_ARG1, s_make_interface_environment);

  interface_environment = 
    scm_must_malloc (sizeof *interface_environment, s_make_interface_environment);

  interface_environment->c_conflict_proc = scm_interface_environment_conflict_proc;
  interface_environment->conflict_proc = SCM_EOL;
  
  interface_environment_smob = make_interface_environment (interface_environment, interface, s_make_interface_environment);

  return interface_environment_smob;
}

SCM_PROC(s_interface_environment_p, "interface-environment?", 1, 0, 0, scm_interface_environment_p);
SCM
scm_interface_environment_p (env)
     SCM env;
{
  return SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && 
    SCM_INTERFACE_ENVIRONMENTP(env) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_interface_environment_interface, "interface-environment-interface", 1, 0, 0, scm_interface_environment_interface);
SCM
scm_interface_environment_interface (env)
     SCM env;
{
  struct interface_environment *interface_environment;
  SCM computed_interface;

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_INTERFACE_ENVIRONMENTP(env), env, SCM_ARG1, s_interface_environment_interface);

  interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);

				/* if not fixed, fix it */
  computed_interface = (interface_environment->leaf.environment.environment_funcs != scm_type_interface_environment) ?
    interface_environment->interface : 
    interface_environment_fix_interface(env, interface_environment,
					interface_environment->interface, 
					s_interface_environment_interface);
  
  return computed_interface;
}

/*
 * update_args, update, update_rewind, update_failed,
 * interface_environment_set_interface,
 * scm_interface_environment_set_interface: Set a new interface for
 * environment `env'. We must send an update broadcast to all
 * observers if `env's interface has been fixed.  This code will
 * call fix_interface through environment_update_internal's
 * lookup method.
 */
SCM_PROC(s_interface_environment_set_interface_x, "interface-environment-set-interface!", 2, 0, 0, scm_interface_environment_set_interface_x);

struct update_args 
{
  SCM env; 
  SCM old_interface; 
  SCM old_aquisition; 
  SCM interface; 
  SCM aquisition; 
  struct interface_environment *interface_environment;
};

/*
 * try
 */
static SCM
update(void *p)
{
  SCM parsed_interface;
  struct update_args *u = (struct update_args*)p;

  u->interface_environment->aquisition = u->aquisition;

				/* abort if incomplete */
  if(u->interface_environment->leaf.environment.environment_funcs == scm_type_interface_environment) abort();

  if(update_running != 0) abort();

  update_running = 1;
     
  SWAP(u->interface_environment->leaf.environment.obarray, 
       UPDATE_EMPTY_OBARRAY);
				/* parse the signature */
  parsed_interface = 
    interface_environment_parse_interface(u->env, 
					  u->interface_environment,
					  u->interface, 
					  u->interface_environment->leaf.environment.obarray,
					  s_interface_environment_set_interface_x);

				/* environments should update their
                                   caches: "hey, look at my new cool
                                   interface" */
  u->interface_environment->interface = parsed_interface;
  scm_environment_broadcast (u->env, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, s_interface_environment_set_interface_x);

  scm_symbol_remove_all_handles(UPDATE_EMPTY_OBARRAY, scm_symhash_dim);

				/* now we're ready to receive update
                                   requests for the new interface */
  interface_environment_drop_old_observers(u->env, u->interface_environment);
  interface_environment_observe_interface(u->env, u->interface, s_interface_environment_set_interface_x);

  update_running --;


  return SCM_UNSPECIFIED;
}

/*
 * re-construct old state after either parse_interface or 
 */
static SCM
update_rewind(void *p)
{
  struct update_args *u = (struct update_args*)p;

  SWAP(u->interface_environment->leaf.environment.obarray, 
       UPDATE_EMPTY_OBARRAY);

  u->interface_environment->interface = u->old_interface;
  u->interface_environment->aquisition = u->old_aquisition;

  scm_symbol_remove_all_handles(UPDATE_EMPTY_OBARRAY, scm_symhash_dim);

				/* back to the old observer list */
  interface_environment_drop_old_observers(u->env, u->interface_environment);
  interface_environment_observe_interface(u->env, u->old_interface, s_interface_environment_set_interface_x);

				/* environments should update their
                                   caches */
  scm_environment_broadcast (u->env, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, s_interface_environment_set_interface_x);

  update_running --;

  return SCM_UNSPECIFIED;
}

/*
 * back to old state or abort()
 */
static SCM
update_failed(void *p, SCM tag, SCM args)
{
  scm_internal_catch(SCM_BOOL_T, update_rewind, p, (scm_catch_handler_t)abort, 0);
  
  if(update_running != 0) abort();
 
  scm_ithrow(tag, args, 1);

  abort();

  return 0;
}

/*
 * Set a new interface for env/interface_environment
 */
static void
interface_environment_set_interface(env, interface_environment, interface)
     SCM env;
     struct interface_environment *interface_environment;
     SCM interface;
{

  if(interface_environment->leaf.environment.environment_funcs != scm_type_interface_environment)
    {				/* fixed interface, either interface-, export- or import-environment */
      struct update_args update_args;
      
      update_args.env = env;	
      update_args.interface = interface;
      update_args.aquisition = aquisition(interface);
      update_args.interface_environment = interface_environment;
      
      update_args.old_interface = interface_environment->interface;
      update_args.old_aquisition = interface_environment->aquisition;
      
      scm_internal_catch(SCM_BOOL_T, update, &update_args, update_failed, &update_args);
    }
  else
    {
      interface_environment->interface = interface;
    }
}

SCM
scm_interface_environment_set_interface_x (env, interface)
     SCM env;
     SCM interface;
{
  struct interface_environment *interface_environment;

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_INTERFACE_ENVIRONMENTP(env), env, SCM_ARG1, s_interface_environment_interface);
  SCM_ASSERT((SCM_NIMP(interface) && SCM_CONSP(interface)) || SCM_NULLP(interface), interface, SCM_ARG2, s_interface_environment_set_interface_x);

  interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);

  interface_environment_set_interface(env, interface_environment, interface);
  
  return interface_environment->interface;
}


/* export environments */

static int
print_export_environment (SCM type, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<export environment ", port);
  scm_puts (SCM_CHARS(scm_number_to_string(scm_ulong2num((unsigned long)type), SCM_MAKINUM (16))), port);
  scm_puts (">", port);

  return 1;
}

static struct environment_funcs export_environment_funcs = {
    scm_interface_environment_ref_fixed,
    scm_interface_environment_fold_fixed,
    scm_interface_environment_define_fixed,
    scm_interface_environment_undefine_fixed,
    scm_interface_environment_set_x_fixed,
    scm_interface_environment_cell_fixed,
    scm_environment_observe_internal,
    scm_environment_unobserve_internal,
    mark_interface_environment,		
    free_interface_environment,		
    print_export_environment		
};
void *scm_type_export_environment = &export_environment_funcs;

SCM_PROC(s_make_export_environment, "make-export-environment", 2, 0, 0, scm_make_export_environment);
SCM
scm_make_export_environment (private, signature)
     SCM private;
     SCM signature;
{
  SCM interface_environment_smob;
  struct interface_environment *interface_environment;

  SCM_ASSERT(SCM_NIMP(private) && SCM_ENVIRONMENTP(private), private, SCM_ARG1, s_make_export_environment);
  SCM_ASSERT(SCM_NIMP(signature) && (SCM_CONSP(signature) || SCM_NULLP(signature)), signature, SCM_ARG2, s_make_export_environment);

  interface_environment = 
    scm_must_malloc (sizeof *interface_environment, s_make_export_environment);

  interface_environment->c_conflict_proc = scm_interface_environment_conflict_proc;
  interface_environment->conflict_proc = SCM_EOL;

  interface_environment_smob = 
    make_interface_environment (interface_environment,
				SCM_LIST1(scm_cons(private, signature)), 
				s_make_export_environment);

				/* now fix it */
  interface_environment_fix_interface(interface_environment_smob, 
				      interface_environment,
				      interface_environment->interface,
				      s_make_export_environment);

  interface_environment->leaf.environment.environment_funcs = scm_type_export_environment;

  return interface_environment_smob;
}

SCM_PROC(s_export_environment_private, "export-environment-private", 1, 0, 0, scm_export_environment_private);

SCM
scm_export_environment_private (env)
     SCM env;
{
  struct interface_environment *interface_environment;
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_EXPORT_ENVIRONMENTP(env), env, SCM_ARG1, s_export_environment_private);
  
  interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);
  
  return SCM_CAAR(interface_environment->interface);
}

SCM_PROC(s_export_environment_set_private_x, "export-environment-set-private!", 2, 0, 0, scm_export_environment_set_private_x);

SCM
scm_export_environment_set_private_x (env, private)
     SCM env;
     SCM private;
{
  struct interface_environment *interface_environment;
  SCM old_signature_list;

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_export_environment_set_private_x);
  SCM_ASSERT(SCM_NIMP(private) && SCM_ENVIRONMENTP(private), private, SCM_ARG2, s_export_environment_set_private_x);

  interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);
  old_signature_list = SCM_CDR(interface_environment->interface);
  
  interface_environment_set_interface(env, scm_cons(private, old_signature_list), interface_environment);

  return SCM_UNSPECIFIED;
}

SCM_PROC(s_export_environment_signature, "export-environment-signature", 1, 0, 0, scm_export_environment_signature);

SCM
scm_export_environment_signature (env)
     SCM env;
{

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_EXPORT_ENVIRONMENTP(env), env, SCM_ARG1, s_export_environment_signature);

  return SCM_INTERFACE_ENVIRONMENT_INTERFACE(env);
}

SCM_PROC(s_export_environment_set_signature_x, "export-environment-set-signature!", 2, 0, 0, scm_export_environment_set_signature_x);

SCM
scm_export_environment_set_signature_x (env, interface)
     SCM env;
     SCM interface;
{
  struct interface_environment *interface_environment;
  SCM old_private;

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_EXPORT_ENVIRONMENTP(env), env, SCM_ARG1, s_export_environment_set_signature_x);
  SCM_ASSERT(SCM_NIMP(interface) && (SCM_CONSP(interface) || SCM_NULLP(interface)), interface, SCM_ARG2, s_export_environment_set_signature_x);
  
  interface_environment = SCM_INTERFACE_ENVIRONMENT_STRUCT(env);
  old_private = SCM_CAAR(interface_environment->interface);

  interface_environment_set_interface(env, scm_cons(scm_cons(old_private, interface), SCM_EOL), interface_environment);

  return SCM_UNSPECIFIED;
}

SCM_PROC(s_export_environment_p, "export-environment?", 1, 0, 0, scm_export_environment_p);
SCM
scm_export_environment_p (env)
     SCM env;
{
  return SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && 
    SCM_EXPORT_ENVIRONMENTP(env) ? SCM_BOOL_T : SCM_BOOL_F;
}



/* import environments */ 

static int
print_import_environment (SCM type, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<import environment ", port);
  scm_puts (SCM_CHARS(scm_number_to_string(scm_ulong2num((unsigned long)type), SCM_MAKINUM (16))), port);
  scm_puts (">", port);

  return 1;
}

static struct environment_funcs import_environment_funcs = {
    scm_interface_environment_ref_fixed,
    scm_interface_environment_fold_fixed,
    scm_interface_environment_define_fixed,  
    scm_interface_environment_undefine_fixed,
    scm_interface_environment_set_x_fixed,
    scm_interface_environment_cell_fixed,
    scm_environment_observe_internal,
    scm_environment_unobserve_internal,
    mark_interface_environment,
    free_interface_environment,
    print_import_environment
};
void *scm_type_import_environment = &import_environment_funcs;

SCM_PROC(s_make_import_environment, "make-import-environment", 2, 0, 0, scm_make_import_environment);
SCM
scm_make_import_environment (imports, conflict_proc)
     SCM imports;
     SCM conflict_proc;
{
  SCM list;
  SCM interface_imports;
  SCM interface_environment_smob;
  struct interface_environment *interface_environment;

  SCM_ASSERT(SCM_NIMP(imports) && (SCM_CONSP(imports) || SCM_NULLP(imports)), imports, SCM_ARG2, s_make_import_environment);

  interface_imports = SCM_EOL;
  for (list = imports; SCM_NIMP(list); list = SCM_CDR(list)) 
    {
      SCM interface_env = SCM_CAR(list);
      SCM_ASSERT(SCM_NIMP(interface_env) && SCM_ENVIRONMENTP(interface_env), interface_env, SCM_ARG1, s_make_import_environment);
      interface_imports = scm_cons(scm_cons(interface_env, SCM_BOOL_F), interface_imports);
    }

  interface_environment = 
    scm_must_malloc (sizeof *interface_environment, s_make_import_environment);

  interface_environment->c_conflict_proc = gh_call3;
  interface_environment->conflict_proc = conflict_proc;

  interface_environment_smob = 
    make_interface_environment (interface_environment,
				interface_imports,
				s_make_import_environment);

				/* now fix it */
  interface_environment_fix_interface(interface_environment_smob, 
				      interface_environment,
				      interface_imports,
				      s_make_import_environment);

  interface_environment->leaf.environment.environment_funcs = scm_type_import_environment;

  return interface_environment_smob;
}

SCM_PROC(s_import_environment_p, "import-environment?", 1, 0, 0, scm_import_environment_p);
SCM
scm_import_environment_p (env)
     SCM env;
{
  return SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && 
    SCM_IMPORT_ENVIRONMENTP(env) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_import_environment_imports, "import-environment-imports", 1, 0, 0, scm_import_environment_imports);
SCM
scm_import_environment_imports (env)
     SCM env;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_IMPORT_ENVIRONMENTP(env), env, SCM_ARG1, s_import_environment_imports);

  return SCM_INTERFACE_ENVIRONMENT_INTERFACE(env);
}

SCM_PROC(s_import_environment_set_imports_x, "import-environment-set-imports!", 2, 0, 0, scm_import_environment_set_imports_x);
SCM
scm_import_environment_set_imports_x (env, imports)
     SCM env;
     SCM imports;
{
  SCM list;
  SCM interface_imports;
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_EXPORT_ENVIRONMENTP(env), env, SCM_ARG1, s_import_environment_set_imports_x);
  SCM_ASSERT(SCM_NIMP(imports) && (SCM_CONSP(imports) || SCM_NULLP(imports)), imports, SCM_ARG2, s_import_environment_set_imports_x);


  interface_imports = SCM_EOL;
  for (list = imports; SCM_NIMP(list); list = SCM_CDR(list)) 
    {
      SCM interface_env = SCM_CAR(list);
      SCM_ASSERT(SCM_NIMP(interface_env) && SCM_ENVIRONMENTP(interface_env), env, SCM_ARG1, s_make_import_environment);
      interface_imports = scm_cons(scm_cons(interface_env, SCM_BOOL_F), interface_imports);
    }

  interface_environment_set_interface(env, SCM_INTERFACE_ENVIRONMENT_STRUCT(env), interface_imports);

  return SCM_UNSPECIFIED;
}



/*
 * intern a symbol in environment env
 */
SCM 
scm_environment_intern (env, name, val)
     SCM env;
     char *name;
     SCM val;
{
  SCM vcell;
  SCM sym;


  vcell = scm_intern (name);
  sym = SCM_CAR(vcell);

  return SCM_ENVIRONMENT_DEFINE(env, sym, val);
}


/* Miscellaneous */

/* according to r5rs `load' evaluates expressions in the interaction
   environment */
SCM_PROC(s_set_interaction_environment_x, "set-interaction-environment!", 1, 0, 0, scm_set_interaction_environment_x);
SCM
scm_set_interaction_environment_x (env)
     SCM env;
{
  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env) && SCM_EVAL_ENVIRONMENTP(env), env, SCM_ARG1, s_set_interaction_environment_x);

  SCM_DEFER_INTS;
  scm_interaction_environment = env;
  SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

SCM_PROC(s_interaction_environment, "interaction-environment", 0, 0, 0, scm_interaction_environment_proc);
SCM
scm_interaction_environment_proc ()
{
  return scm_interaction_environment;
}

SCM_PROC(s_scheme_guile_environment, "scheme-guile-environment", 0, 1, 0, scm_scheme_guile_environment_proc);
SCM
scm_scheme_guile_environment_proc (version)
     SCM version;
{
  SCM_ASSERT(SCM_INUMP(version), version, SCM_ARG1, s_scheme_guile_environment);
  
  return scm_scheme_guile_environment;
}

SCM_PROC(s_null_environment, "null-environment", 0, 1, 0, scm_null_environment_proc);
SCM
scm_null_environment_proc (version)
     SCM version;
{
  SCM_ASSERT(SCM_INUMP(version), version, SCM_ARG1, s_null_environment);
  
  return scm_scheme_guile_environment;
}

SCM_PROC(s_guile_user_environment, "guile-user-environment", 0, 1, 0, scm_guile_user_environment_proc);
SCM
scm_guile_user_environment_proc (version)
     SCM version;
{
  SCM_ASSERT(SCM_INUMP(version), version, SCM_ARG1, s_guile_user_environment);
  
  return scm_guile_user_environment;
}

SCM_PROC(s_module_registry, "module-registry", 0, 0, 0, scm_module_registry_proc);
SCM
scm_module_registry_proc ()
{
  return scm_module_registry;
}

SCM_PROC(s_c_module_registry, "c-module-registry", 0, 0, 0, scm_c_module_registry_proc);
SCM
scm_c_module_registry_proc ()
{
  return scm_c_module_registry;
}

SCM_PROC(s_environment_module_hash, "environment-module-hash", 0, 0, 0, scm_environment_module_hash_proc);
SCM
scm_environment_module_hash_proc ()
{
  return scm_environment_module_hash;
}



SCM_PROC(s_environment_set_symbol_property_x, "environment-set-symbol-property!", 4, 0, 0, scm_environment_set_symbol_property_x);
SCM
scm_environment_set_symbol_property_x(env, sym, prop, val)
     SCM env;
     SCM sym;
     SCM prop;
     SCM val;
{
  SCM list;
  SCM pair;
  SCM cell;

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_set_symbol_property_x);
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, s_environment_set_symbol_property_x);
  SCM_ASSERT(SCM_NIMP(prop) && SCM_SYMBOLP(prop), prop, SCM_ARG3, s_environment_set_symbol_property_x);


  cell = SCM_ENVIRONMENT_CELL (env, sym, 0);

  if(SCM_IMP(cell))
    {
      scm_error_environment_unbound (s_environment_set_symbol_property_x, "", SCM_EOL, env, sym);
    }

  list = SCM_CELL_PROPERTY_ALIST (cell);
  pair = scm_sloppy_assq (prop, list);

  if (SCM_NIMP (pair))
    {
      if (SCM_CONSP (pair))
	{
	  SCM_SETCDR (pair, val);
	}
    }
  else
    {
      SCM_SETCDR (SCM_CAR (cell), scm_acons (prop, val, list));
    }

  return SCM_UNSPECIFIED;
}
  
SCM_PROC(s_environment_symbol_property, "environment-symbol-property", 3, 0, 0, scm_environment_symbol_property); 
SCM
scm_environment_symbol_property(env, sym, prop)
     SCM env;
     SCM sym;
     SCM prop;
{
  SCM list;
  SCM pair;
  SCM cell;

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_symbol_property);
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, s_environment_symbol_property);
  SCM_ASSERT(SCM_NIMP(prop) && SCM_SYMBOLP(prop), prop, SCM_ARG3, s_environment_symbol_property);

  cell = SCM_ENVIRONMENT_CELL (env, sym, 0);

  if(SCM_IMP(cell))
    {
      scm_error_environment_unbound (s_environment_symbol_property, "", SCM_EOL, env, sym);
    }

  list = SCM_CELL_PROPERTY_ALIST (cell);
  pair = scm_sloppy_assq (prop, list);

  return pair;
}
  
SCM_PROC(s_environment_remove_symbol_property_x, "environment-remove-symbol-property!", 3, 0, 0, scm_environment_remove_symbol_property_x);
SCM
scm_environment_remove_symbol_property_x(env, sym, prop)
     SCM env;
     SCM sym;
     SCM prop;
{
  SCM list;
  SCM cell;

  SCM_ASSERT(SCM_NIMP(env) && SCM_ENVIRONMENTP(env), env, SCM_ARG1, s_environment_remove_symbol_property_x);
  SCM_ASSERT(SCM_NIMP(sym) && SCM_SYMBOLP(sym), sym, SCM_ARG2, s_environment_remove_symbol_property_x);
  SCM_ASSERT(SCM_NIMP(prop) && SCM_SYMBOLP(prop), prop, SCM_ARG3, s_environment_remove_symbol_property_x);

  cell = SCM_ENVIRONMENT_CELL (env, sym, 0);

  if(SCM_IMP(cell))
    {
      scm_error_environment_unbound (s_environment_remove_symbol_property_x, "", SCM_EOL, env, sym);
    }

  list = SCM_CELL_PROPERTY_ALIST (cell);

  SCM_SETCDR (SCM_CAR (cell), scm_assq_remove_x (list, prop));


  return SCM_UNSPECIFIED;
}

void
scm_environments_prehistory()
{
  /* create observer smob */
  scm_tc16_observer = scm_newsmob(&observer_funs);

  /* create default environment */
  scm_tc16_environment = scm_newsmob (&environment_funs);
  scm_interaction_environment = scm_scheme_guile_environment = 
    scm_make_eval_environment (scm_make_leaf_environment(), scm_make_interface_environment(SCM_EOL));

  scm_c_module_registry = scm_make_leaf_environment();

  scm_module_registry = scm_make_leaf_environment();
  scm_environment_module_hash = scm_make_doubly_weak_hash_table(SCM_MAKINUM (32));

  /* create the user environment */
  scm_guile_user_environment = 
    scm_make_eval_environment (scm_make_leaf_environment(), 
			       scm_make_interface_environment(SCM_LIST1 (scm_cons (scm_scheme_guile_environment, SCM_BOOL_T))));

  /* create supporting data structures */
  protect_vec = scm_make_vector(SCM_MAKINUM(1), SCM_UNDEFINED);
  scm_permanent_object(protect_vec);

  UPDATE_EMPTY_OBARRAY = scm_make_vector((SCM) SCM_MAKINUM (scm_symhash_dim), SCM_EOL);
}
  
SCM
scm_init_environments (env)
     SCM env;
{
#include "environments.x"

  return SCM_UNSPECIFIED;
}
