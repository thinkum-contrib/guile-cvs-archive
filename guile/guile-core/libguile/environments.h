#ifndef ENVIRONMENTSH
#define ENVIRONMENTSH

#include "__scm.h"
#include "tags.h"



typedef void (*scm_environment_observer) (SCM env, SCM data);
typedef SCM (*scm_environment_folder) (SCM data, SCM symbol, SCM value, SCM tail);
typedef SCM (*scm_environment_conflict_proc) (SCM data, SCM sym, SCM env1, SCM env2);

struct environment_funcs {
  SCM  (*ref) (SCM self, SCM symbol);
  SCM  (*fold) (SCM self, scm_environment_folder proc, SCM data, SCM init);
  SCM  (*define) (SCM self, SCM symbol, SCM value);
  void  (*undefine) (SCM self, SCM symbol);
  void (*set) (SCM self, SCM symbol, SCM value);
  SCM (*cell) (SCM self, SCM symbol, int for_write);
  SCM (*observe) (SCM self, scm_environment_observer proc, SCM data, int weak_p);
  void (*unobserve) (SCM dummy, SCM token);
 
  SCM  (*mark) (SCM self);
  scm_sizet (*free) (SCM self);
  int  (*print) (SCM self, SCM port, scm_print_state *pstate);
};

struct environment {
  struct environment_funcs *environment_funcs; /* class signature */
  SCM obarray;
};



/* create the root environment */
extern void scm_environments_prehistory (void);

/* intern environment functions in env */
extern SCM scm_init_environments (SCM env);


/* generic environment functions */

#define SCM_ENVIRONMENT_FUNCS(env) ( *((struct environment_funcs **) SCM_CDR (env)))
#define SCM_ENVIRONMENT_OBARRAY(env)  (((struct environment*)(SCM_CDR(env)))->obarray)
#define SCM_ENVIRONMENT_STRUCT(env) ((struct environment*) SCM_CDR (env))
#define SCM_ENVIRONMENTP(x) ((SCM_CAR (x)) == scm_tc16_environment)
#define SCM_TOP_LEVEL_ENVP(x) ((SCM_CONSP(x) && (SCM_EOL==SCM_CDR(x))))
#define SCM_ENVIRONMENT_REF(env, symbol) ((*(SCM_ENVIRONMENT_FUNCS(env)->ref)) (env, symbol))
#define SCM_ENVIRONMENT_FOLD(env, proc, data, init) ((*(SCM_ENVIRONMENT_FUNCS(env)->fold)) (env, proc, data, init))
#define SCM_ENVIRONMENT_DEFINE(env, symbol, value) ((*(SCM_ENVIRONMENT_FUNCS(env)->define)) (env, symbol, value))
#define SCM_ENVIRONMENT_UNDEFINE(env, symbol) ((*(SCM_ENVIRONMENT_FUNCS(env)->undefine)) (env, symbol))
#define SCM_ENVIRONMENT_SET(env, symbol, value) ((*(SCM_ENVIRONMENT_FUNCS(env)->set)) (env, symbol, value))
#define SCM_ENVIRONMENT_CELL(env, symbol, for_write) ((*(SCM_ENVIRONMENT_FUNCS(env)->cell)) (env, symbol, for_write))
#define SCM_ENVIRONMENT_OBSERVE(env, proc, data, weak_p) ((*(SCM_ENVIRONMENT_FUNCS(env)->observe)) (env, proc, data, weak_p))
#define SCM_ENVIRONMENT_UNOBSERVE(env, token) ((*(SCM_ENVIRONMENT_FUNCS(env)->unobserve)) (env, token))
#define SCM_ENVIRONMENT_BOUND(env, symbol) (SCM_UNDEFINED==SCM_ENVIRONMENT_REF(env, symbol) ? SCM_BOOL_F : SCM_BOOL_T)

extern long scm_tc16_environment;

extern SCM scm_environment_observe_weak(SCM env, SCM proc);
extern SCM scm_environment_unobserve (SCM token);
extern SCM scm_c_environment_ref (SCM env, SCM sym);
extern SCM scm_environment_ref (SCM env, SCM sym);
extern SCM scm_c_environment_fold (SCM env, scm_environment_folder proc, SCM data, SCM init);
extern SCM scm_environment_fold(SCM env, SCM proc, SCM init);
extern SCM scm_environment_undefine(SCM env, SCM sym);
extern SCM scm_environment_define(SCM env, SCM sym, SCM val);
extern SCM scm_environment_set_x(SCM env, SCM sym, SCM val);
extern SCM scm_c_environment_cell(SCM env, SCM sym, int for_write);
extern SCM scm_environment_cell(SCM env, SCM sym, SCM for_write);
extern SCM scm_environment_bound_p (SCM env, SCM sym);
extern SCM scm_c_environment_observe (SCM env, scm_environment_observer proc, SCM data, int weak_p);
extern SCM scm_environment_observe(SCM env, SCM proc);
extern SCM scm_environment_p(SCM env);
extern SCM scm_make_environment (void *handle, scm_sizet size);



/* leaf environments */
#define SCM_LEAF_ENVIRONMENTP(env) ((void*)SCM_ENVIRONMENT_FUNCS(env) == scm_type_leaf_environment)
#define SCM_LEAF_ENVIRONMENT_STRUCT(env) ((struct leaf_environment*) SCM_CDR (env))

extern void *scm_type_leaf_environment;
extern SCM scm_make_leaf_environment (void);

extern void *scm_type_immutable_environment;
extern SCM scm_make_immutable_environment (void);



/* eval environments */
#define SCM_EVAL_ENVIRONMENTP(env) (SCM_ENVIRONMENT_FUNCS(env) == scm_type_eval_environment)
#define SCM_EVAL_ENVIRONMENT_STRUCT(env) ((struct eval_environment*) SCM_CDR (env))
#define SCM_EVAL_ENVIRONMENT_IMPORTED(eval_env) (SCM_EVAL_ENVIRONMENT_STRUCT(eval_env)->imported)
#define SCM_EVAL_ENVIRONMENT_LOCAL(eval_env)  (SCM_EVAL_ENVIRONMENT_STRUCT(eval_env)->local)
#define SCM_EVAL_ENVIRONMENT_MEMOIZED(eval_env) (SCM_EVAL_ENVIRONMENT_STRUCT(eval_env)->memoized)

extern void *scm_type_eval_environment;

extern void scm_eval_environment_memoize_cell_internal(SCM env, SCM expr, SCM val, SCM sym);
extern SCM scm_make_eval_environment (SCM local, SCM imported);
extern SCM scm_eval_environment_imported (SCM env);
extern SCM scm_eval_environment_local(SCM env);
extern SCM scm_eval_environment_set_imported_x (SCM env, SCM imported);
extern SCM scm_eval_environment_p (SCM env);



/* interface environments */
#define SCM_INTERFACE_ENVIRONMENTP(env) ((SCM_ENVIRONMENT_FUNCS(env) == scm_type_interface_environment)||(SCM_ENVIRONMENT_FUNCS(env) == scm_type_interface_environment_fixed))
#define SCM_INTERFACE_ENVIRONMENT_STRUCT(env) ((struct interface_environment*) SCM_CDR (env))
#define SCM_INTERFACE_ENVIRONMENT_PRIVATE(interface_env) (SCM_INTERFACE_ENVIRONMENT_STRUCT(interface_env)->private)
#define SCM_INTERFACE_ENVIRONMENT_INTERFACE(interface_env) (SCM_INTERFACE_ENVIRONMENT_STRUCT(interface_env)->interface)

extern void *scm_type_interface_environment;

extern SCM scm_make_interface_environment (SCM interface);
extern SCM scm_interface_environment_interface(SCM env);
extern SCM scm_interface_environment_set_interface_x(SCM env, SCM interface);
extern SCM scm_interface_environment_p (SCM env);



/* export environments */
#define SCM_EXPORT_ENVIRONMENTP(env) (SCM_ENVIRONMENT_FUNCS(env) == scm_type_export_environment)
#define SCM_EXPORT_ENVIRONMENT_STRUCT(env) ((struct interface_environment*) SCM_CDR (env))

extern void *scm_type_export_environment;

extern SCM scm_make_export_environment (SCM private, SCM signature);
extern SCM scm_export_environment_signature(SCM env);
extern SCM scm_export_environment_set_signature_x(SCM env, SCM signature);
extern SCM scm_export_environment_set_private_x (SCM env, SCM private);
extern SCM scm_export_environment_private (SCM env);
extern SCM scm_export_environment_p (SCM env);



/* import environments */
#define SCM_IMPORT_ENVIRONMENTP(env) (SCM_ENVIRONMENT_FUNCS(env) == scm_type_import_environment)
#define SCM_IMPORT_ENVIRONMENT_STRUCT(env) ((struct interface_environment*) SCM_CDR (env))
#define SCM_IMPORT_ENVIRONMENT_CONFLICT_PROC(import_env) (SCM_IMPORT_ENVIRONMENT_STRUCT(import_env)->conflict_proc)

extern void *scm_type_import_environment;

extern SCM scm_make_import_environment (SCM imports, SCM conflict_proc);
extern SCM scm_import_environment_imports (SCM env);
extern SCM scm_import_environment_set_imports_x (SCM env, SCM imports);
extern SCM scm_import_environment_p (SCM env);



extern SCM scm_set_interaction_environment_x (SCM env);
extern SCM scm_interaction_environment_proc (void);
extern SCM scm_c_module_registry_proc (void);
extern SCM scm_module_registry_proc (void);
extern SCM scm_environment_module_hash_proc (void);
extern SCM scm_null_environment_proc(SCM version);
extern SCM scm_scheme_guile_environment_proc(SCM version);
extern SCM scm_guile_user_environment_proc(SCM version);
extern SCM scm_report_environment_proc(SCM version);



extern SCM scm_environment_intern (SCM env, char* name, SCM val);



/* environment-symbol-properties */
SCM scm_environment_symbol_property(SCM env, SCM sym, SCM prop);
SCM scm_environment_remove_symbol_property_x(SCM env, SCM sym, SCM prop);
SCM scm_environment_set_symbol_property_x(SCM env, SCM sym, SCM prop, SCM  val);


#endif
