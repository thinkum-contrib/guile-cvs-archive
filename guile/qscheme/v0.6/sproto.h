/* From `array.c': */
void scm_array_mark (SOBJ arr );
void scm_array_sweep (SOBJ arr );
int scm_array_reconize (PORT *port , int c );
SOBJ scm_array_parse (PORT *port , int start_char );
SOBJ scm_mkarray (int size , SOBJ fill );
void scm_vector_print (SOBJ arr , PORT *p );
void scm_vector_write (SOBJ arr , PORT *p );
SCM_STRBUF * scm_array2str (SCM_STRBUF *sb , SOBJ arr , int raw );
SOBJ scm_array_compare (SOBJ a1 , SOBJ a2 );
SOBJ scm_vectorp (SOBJ obj );
SOBJ scm_make_vector (int n , SOBJ *p );
SOBJ scm_vector (int n , SOBJ *p );
SOBJ scm_vector_length (SOBJ obj );
SOBJ scm_vector_ref (SOBJ vector , SOBJ index );
SOBJ scm_vector_set (SOBJ vector , SOBJ index , SOBJ value );
SOBJ scm_vector_to_list (SOBJ vector );
SOBJ scm_list_to_vector (SOBJ list );
SOBJ scm_vector_fill (SOBJ vector , SOBJ fill );
SOBJ scm_vector_resize (SOBJ vector , SOBJ size );
SOBJ scm_vector_append (SOBJ vector , SOBJ item );
SOBJ scm_vector_sort (SOBJ vector , SOBJ comp );
SOBJ scm_vector_qsort (SOBJ vector );
SOBJ scm_vector_remove (SOBJ vector , SOBJ elt );
#define PROTO(x) x
void scm_init_array PROTO ((void));
/* From `asm.c': */
extern int scm_push_seq_max;
SOBJ scm_array_resize (SOBJ array , int newsize );
SOBJ scm_asm_instr (SOBJ c , SOBJ expr );
SOBJ scm_syntax_asm (SOBJ expr );
SOBJ scm_compile_begin (SOBJ icode , SOBJ argl , SOBJ env );
SOBJ backquotify (SOBJ ic , SOBJ l , SOBJ env , int level );
SOBJ scm_compile (SOBJ form , SOBJ env );
SOBJ scm_compile2 (SOBJ form , SOBJ env );
void scm_init_asm PROTO ((void));
/* From `atom.c': */
extern SOBJ scm_atom_hash;
void scm_atom_mark (SOBJ obj );
void scm_atom_sweep (SOBJ obj );
void scm_atom_print (SOBJ obj , PORT *p );
SCM_STRBUF * scm_atom2str (SCM_STRBUF *sb , SOBJ obj , int raw );
SOBJ scm_mkatom (char *str );
SOBJ scm_string_to_atom (SOBJ obj );
SOBJ scm_atom_to_string (SOBJ obj );
SOBJ scm_atom_get_hash PROTO ((void));
void scm_init_atom PROTO ((void));
/* From `boolean.c': */
SOBJ scm_not (SOBJ x );
SOBJ scm_booleanp (SOBJ x );
SOBJ scm_eq (SOBJ x , SOBJ y );
SOBJ scm_eqv (SOBJ x , SOBJ y );
SOBJ scm_equal (SOBJ x , SOBJ y );
void scm_init_boolean PROTO ((void));
/* From `catch.c': */
extern SOBJ scm_catch_list, scm_thrown_tag, scm_thrown_msg;
void scm_ccntxt_mark (SOBJ obj );
void scm_ccntxt_sweep (SOBJ obj );
SOBJ scm_mkcatch PROTO ((void));
SOBJ scm_throw (SOBJ tag , SOBJ msg );
SOBJ scm_segv PROTO ((void));
void scm_init_catch PROTO ((void));
/* From `chr.c': */
extern struct CHR_SYM csym[];
extern Sobject scm_chr_array[];
int scm_is_pointer_to_chr_array (SOBJ obj );
SOBJ scm_mkchar (int c );
void scm_gc_mark_chars PROTO ((void));
void scm_gc_sweep_chars PROTO ((void));
void scm_char_print (SOBJ c , PORT *p );
void scm_char_write (SOBJ obj , PORT *p );
SCM_STRBUF * scm_char2str (SCM_STRBUF *sb , SOBJ obj , int raw );
int scm_char_reconize (PORT *port , int c );
SOBJ scm_char_parse (PORT *port , int c );
SOBJ scm_charp (SOBJ obj );
SOBJ scm_charlt (SOBJ x , SOBJ y );
SOBJ scm_charle (SOBJ x , SOBJ y );
SOBJ scm_chareq (SOBJ x , SOBJ y );
SOBJ scm_charge (SOBJ x , SOBJ y );
SOBJ scm_chargt (SOBJ x , SOBJ y );
SOBJ scm_charlti (SOBJ x , SOBJ y );
SOBJ scm_charlei (SOBJ x , SOBJ y );
SOBJ scm_chareqi (SOBJ x , SOBJ y );
SOBJ scm_chargei (SOBJ x , SOBJ y );
SOBJ scm_chargti (SOBJ x , SOBJ y );
SOBJ scm_char_alphap (SOBJ x );
SOBJ scm_char_nump (SOBJ x );
SOBJ scm_char_whitep (SOBJ x );
SOBJ scm_char_upperp (SOBJ x );
SOBJ scm_char_lowerp (SOBJ x );
SOBJ scm_char2int (SOBJ x );
SOBJ scm_int2char (SOBJ x );
SOBJ scm_charupc (SOBJ x );
SOBJ scm_charlwc (SOBJ x );
void scm_init_chr PROTO ((void));
/* From `code.c': */
void scm_code_mark (SOBJ obj );
void scm_code_sweep (SOBJ obj );
void scm_code_print (SOBJ obj , PORT *p );
SCM_STRBUF * scm_code2str (SCM_STRBUF *sb , SOBJ obj , int raw );
SOBJ scm_mkcode (SOBJ *ptr , long size );
/* From `cont.c': */
void scm_cont_mark (SOBJ cont );
void scm_cont_sweep (SOBJ cont );
SOBJ scm_mkcont (SOBJ proc );
SOBJ scm_call_cc (SOBJ proc );
void scm_cc_throw (SOBJ cont , SOBJ val );
/* From `dyn.c': */
void * scm_find_extsym (char *path , char *sym_name , int must );
SOBJ scm_mk_static_pointer (void *p );
SOBJ scm_mk_dynamic_pointer (void *p );
SOBJ scm_load_library (SOBJ x );
void scm_extfunc_mark (SOBJ extfunc );
void scm_extfunc_sweep (SOBJ extfunc );
SOBJ scm_mkextfunc (SCM_ExtFunc *f );
void scm_extfunc_print (SOBJ x , PORT *p );
void scm_extfunc_write (SOBJ x , PORT *p );
SCM_STRBUF * scm_extfunc2str (SCM_STRBUF *sb , SOBJ obj , int raw );
SOBJ scm_make_extfunc (SOBJ lib , SOBJ ret , SOBJ name , SOBJ argl );
SOBJ scm_extfunc_call (SOBJ proc , int nargs , SOBJ *arg );
SOBJ scm_external_existsp (SOBJ lib , SOBJ entry );
SOBJ scm_external_call (SOBJ lib , SOBJ ret , SOBJ entry , SOBJ argtype , SOBJ argval );
SOBJ scm_make_extptr (SOBJ lib , SOBJ name );
void scm_init_dyn PROTO ((void));
/* From `env.c': */
SOBJ scm_mkenv (int nslots );
SOBJ scm_env_add (SOBJ env , SOBJ lsym );
SOBJ scm_env_add_level (SOBJ env );
SOBJ scm_env_search (SOBJ env , SOBJ atom , int *distance );
/* From `hash.c': */
void scm_hash_mark (SOBJ obj );
void scm_hash_sweep (SOBJ obj );
int scm_hash_reconize (PORT *port , int c );
SOBJ scm_hash_parse (PORT *port , int c );
Uint scm_hash_string (char *str );
Uint scm_hash_string1 (char *str );
Uint scm_hash_pointer (void *ptr );
Uint scm_hash_code (SCM_Hash *h , SOBJ key );
void scm_hash_print (SOBJ obj , PORT *p );
void scm_hash_write (SOBJ obj , PORT *p );
SCM_STRBUF * scm_hash2str (SCM_STRBUF *sb , SOBJ obj , int raw );
SOBJ scm_mkhash (int type );
SOBJ scm_make_hash (int nargs , SOBJ *arg );
SOBJ scm_make_generic_hash PROTO ((void));
SOBJ scm_make_symbol_hash PROTO ((void));
SOBJ scm_make_atom_hash PROTO ((void));
void scm_rebuild_hash (SOBJ hash );
SOBJ scm_hash_search_key (SOBJ hash , SOBJ key , int *hcode , char **key_str );
SOBJ scm_hash_set (SOBJ hash , SOBJ key , SOBJ value );
SOBJ scm_hash_ref (SOBJ hash , SOBJ key );
SOBJ scm_hash_search (SOBJ hash , SOBJ key );
SOBJ scm_hash_remove (SOBJ hash , SOBJ key );
SOBJ scm_hash_to_list (SOBJ hash );
SOBJ scm_list_to_hash (SOBJ l );
SOBJ scm_hash_stat (SOBJ hash );
SOBJ scm_hashp (SOBJ x );
SOBJ scm_atom_hashp (SOBJ x );
SOBJ scm_symbol_hashp (SOBJ x );
SOBJ scm_gen_hashp (SOBJ x );
void scm_init_hash PROTO ((void));
/* From `heap.c': */
extern int scm_gc_verbose;
extern int scm_in_gc;

#ifdef OLD
extern SOBJ scm_hbase, scm_hptr, scm_hlimit, scm_hfree, scm_hwater;
extern long scm_hsize;

#endif
extern int gcmarked, gcfree;
extern long scm_cells_allocated;
extern long scm_malloc_allocated;
extern long scm_global_heap_size;
void * scm_must_alloc (long size );
void * scm_must_realloc (void *mem , long size );
char * scm_must_strdup (char *str );
void scm_mem_clear (void *mem , long size );
void scm_free (void *mem );
void scm_heap_stat PROTO ((void));
int scm_is_pointer_to_heap (void *p );
void scm_gc_protect (SOBJ *location );
void scm_gc_mark (SOBJ obj );
void scm_gc PROTO ((void));
SOBJ scm_newcell (int type );
void scm_freecell (SOBJ obj );
SOBJ scm_clone (SOBJ obj );
void scm_heap_init (long size );
/* From `list.c': */
SOBJ scm_pairp (SOBJ x );
SOBJ scm_cons (SOBJ car , SOBJ cdr );
SOBJ scm_cons2 (SOBJ car , SOBJ cdr , SOBJ cddr );
SOBJ scm_car (SOBJ obj );
SOBJ scm_cdr (SOBJ obj );
SOBJ scm_setcar (SOBJ obj , SOBJ val );
SOBJ scm_setcdr (SOBJ obj , SOBJ val );
SOBJ scm_caar (SOBJ l );
SOBJ scm_cdar (SOBJ l );
SOBJ scm_cadr (SOBJ l );
SOBJ scm_cddr (SOBJ l );
SOBJ scm_caaar (SOBJ l );
SOBJ scm_cdaar (SOBJ l );
SOBJ scm_cadar (SOBJ l );
SOBJ scm_cddar (SOBJ l );
SOBJ scm_caadr (SOBJ l );
SOBJ scm_cdadr (SOBJ l );
SOBJ scm_caddr (SOBJ l );
SOBJ scm_cdddr (SOBJ l );
SOBJ scm_caaaar (SOBJ l );
SOBJ scm_cdaaar (SOBJ l );
SOBJ scm_cadaar (SOBJ l );
SOBJ scm_cddaar (SOBJ l );
SOBJ scm_caadar (SOBJ l );
SOBJ scm_cdadar (SOBJ l );
SOBJ scm_caddar (SOBJ l );
SOBJ scm_cdddar (SOBJ l );
SOBJ scm_caaadr (SOBJ l );
SOBJ scm_cdaadr (SOBJ l );
SOBJ scm_cadadr (SOBJ l );
SOBJ scm_cddadr (SOBJ l );
SOBJ scm_caaddr (SOBJ l );
SOBJ scm_cdaddr (SOBJ l );
SOBJ scm_cadddr (SOBJ l );
SOBJ scm_cddddr (SOBJ l );
SOBJ scm_nullp (SOBJ obj );
long scm_list_length (SOBJ sx );
SOBJ scm_listp (SOBJ obj );
SOBJ scm_list (int n , SOBJ *obja );
SOBJ scm_length (SOBJ obj );
SOBJ scm_append (int len , SOBJ *l );
SOBJ scm_append2 (SOBJ l1 , SOBJ l2 );
SOBJ scm_reverse (SOBJ l );
SOBJ scm_list_tail (SOBJ list , SOBJ k );
SOBJ scm_list_ref (SOBJ list , SOBJ k );
SOBJ scm_memq (SOBJ obj , SOBJ list );
SOBJ scm_memv (SOBJ obj , SOBJ list );
SOBJ scm_member (SOBJ obj , SOBJ list );
SOBJ scm_assq (SOBJ obj , SOBJ alist );
SOBJ scm_assv (SOBJ obj , SOBJ alist );
SOBJ scm_assoc (SOBJ obj , SOBJ alist );
SOBJ scm_map2 (SOBJ func , int argc , SOBJ *argv , int map );
SOBJ scm_map (int argc , SOBJ *argv );
SOBJ scm_foreach (int argc , SOBJ *argv );
SOBJ scm_nth (SOBJ n , SOBJ l );
SOBJ scm_list_remove (SOBJ list , SOBJ n );
SOBJ scm_list_replace (SOBJ list , SOBJ item , SOBJ new );
void scm_init_list PROTO ((void));
/* From `lsym.c': */
SOBJ scm_mklsymbol (SOBJ atom , int ofs );
/* From `macro.c': */
SOBJ scm_mkmacro (SOBJ obj );
void scm_macro_mark (SOBJ obj );
SOBJ scm_macro_to_lambda (SOBJ obj );
SOBJ scm_macro_func (SOBJ macro );
SOBJ scm_macro_set_func (SOBJ macro , SOBJ value );
SOBJ scm_macro_code (SOBJ macro );
SOBJ scm_macro_set_code (SOBJ macro , SOBJ value );
void scm_init_macro PROTO ((void));
/* From `misc.c': */
char * scm_getstr (SOBJ obj );
SOBJ scm_whatis (SOBJ obj );
SOBJ scm_set_prompt (SOBJ x );
SOBJ scm_version PROTO ((void));
void scm_init_misc PROTO ((void));
/* From `module.c': */
extern SOBJ scm_global_module;
extern SOBJ scm_current_module;
extern SOBJ scm_module_hash;
void scm_module_mark (SOBJ obj );
void scm_module_sweep (SOBJ obj );
void scm_module_print (SOBJ obj , PORT *p );
void scm_module_write (SOBJ obj , PORT *p );
SCM_STRBUF * scm_module2str (SCM_STRBUF *sb , SOBJ obj , int raw );
SOBJ scm_module_symbol_search (SOBJ mod , SOBJ atom );
SOBJ scm_module_symbol_lookup (SOBJ mod , SOBJ atom );
SOBJ scm_module_find_exported_symbol (SCM_Module *m , SOBJ atom );
SOBJ scm_module_find_symbol (SOBJ mod , SOBJ atom , int create_it );
SOBJ scm_make_module (SOBJ name );
SOBJ scm_modulep (SOBJ obj );
SOBJ scm_get_current_module PROTO ((void));
SOBJ scm_set_current_module (SOBJ mod );
SOBJ scm_import (int argc , SOBJ *argv );
SOBJ scm_export (int argc , SOBJ *argv );
SOBJ scm_module_exports (SOBJ mod );
SOBJ scm_module_imports (SOBJ mod );
SOBJ scm_module_name (SOBJ mod );
SOBJ scm_module_symbols (SOBJ mod );
SOBJ scm_find_module (SOBJ name );
SOBJ scm_syntax_module (SOBJ icode , SOBJ expr , SOBJ env );
SOBJ scm_syntax_export (SOBJ icode , SOBJ expr , SOBJ env );
SOBJ scm_syntax_import (SOBJ icode , SOBJ expr , SOBJ env );
int scm_module_wreconize (PORT *port , char *s );
SOBJ scm_module_wparse (PORT *port , char *str );
void scm_init_module PROTO ((void));
/* From `number.c': */
SOBJ scm_newbnum PROTO ((void));
void scm_bnum_sweep (SOBJ obj );
SOBJ scm_mkinum (long x );
SOBJ scm_mkfnum (double x );
SOBJ scm_int2bnum (long n );
SOBJ scm_uint2bnum (unsigned long n );
SOBJ scm_flt2num (double n );
SOBJ scm_int2num (long n );
SOBJ scm_uint2num (unsigned long n );
double scm_number2double (SOBJ x );
long scm_number2long (SOBJ x );
SOBJ scm_compact_number (SOBJ n );
SOBJ scm_add2 (SOBJ x , SOBJ y );
SOBJ scm_mul2 (SOBJ x , SOBJ y );
SOBJ scm_sub2 (SOBJ x , SOBJ y );
SOBJ scm_div2 (SOBJ x , SOBJ y );
int scm_cmpnum (SOBJ x , SOBJ y );
SOBJ scm_lt2 (SOBJ x , SOBJ y );
SOBJ scm_le2 (SOBJ x , SOBJ y );
SOBJ scm_ge2 (SOBJ x , SOBJ y );
SOBJ scm_gt2 (SOBJ x , SOBJ y );
SOBJ scm_eq2 (SOBJ x , SOBJ y );
SOBJ scm_zerop (SOBJ x );
SOBJ scm_positivep (SOBJ x );
SOBJ scm_negativep (SOBJ x );
SOBJ scm_oddp (SOBJ x );
SOBJ scm_evenp (SOBJ x );
SOBJ scm_abs (SOBJ x );
SOBJ scm_do_int_div (SOBJ x , SOBJ y , int quotient );
SOBJ scm_quotient (SOBJ x , SOBJ y );
SOBJ scm_remainder (SOBJ x , SOBJ y );
SOBJ scm_modulo (SOBJ x , SOBJ y );
SOBJ scm_gcd (int nargs , SOBJ *obj );
SOBJ scm_lcm (int nargs , SOBJ *obj );
SOBJ scm_floor (SOBJ x );
SOBJ scm_ceil (SOBJ x );
SOBJ scm_truncate (SOBJ x );
SOBJ scm_round (SOBJ x );
SOBJ scm_atan (SOBJ x , SOBJ y );
SOBJ scm_sqrt (SOBJ x );
SOBJ scm_exact_expt (SOBJ x , SOBJ y );
SOBJ scm_expt (SOBJ x , SOBJ y );
SOBJ scm_exact_to_inexact (SOBJ x );
SOBJ scm_inexact_to_exact (SOBJ x );
SCM_STRBUF * scm_number2cstr (SOBJ n , long base );
SOBJ scm_cstr2number (char *s , int base );
SOBJ scm_number_to_string (SOBJ x , SOBJ y );
SOBJ scm_string_to_number (SOBJ x , SOBJ y );
SOBJ scm_ashift (SOBJ val , SOBJ n );
SOBJ scm_bit_and (int narg , SOBJ arg[] );
SOBJ scm_bit_or (int narg , SOBJ arg[] );
SOBJ scm_bit_xor (int narg , SOBJ arg[] );
SOBJ scm_bit_not (SOBJ n );
void scm_init_number PROTO ((void));
/* From `pointer.c': */
void scm_pointer_mark (SOBJ obj );
void scm_pointer_sweep (SOBJ obj );
void scm_pointer_print (SOBJ obj , PORT *p );
void scm_pointer_write (SOBJ obj , PORT *p );
SCM_STRBUF * scm_pointer2str (SCM_STRBUF *sb , SOBJ obj , int raw );
SOBJ scm_mkpointer (void *p );
SOBJ scm_pointer_compare (SOBJ p1 , SOBJ p2 );
SOBJ scm_pointerp (SOBJ ptr );
SOBJ scm_null_pointerp (SOBJ ptr );
void scm_init_pointer PROTO ((void));
/* From `port.c': */
extern SOBJ scm_in_port;
extern SOBJ scm_out_port;
extern SOBJ scm_err_port;
extern SOBJ scm_eof;
char * port_string_output_string (PORT *p );
extern PORT_DESCR port_driver[];
void port_close (PORT *p );
int port_getc (PORT *p );
int port_peekc (PORT *p );
void port_putc (PORT *p , char c );
void port_seek (PORT *p , int pos );
int port_read (PORT *p , SOBJ str , int len );
int port_write (PORT *p , SOBJ str , int len );
int port_getline (PORT *p , SOBJ str );
int port_putline (PORT *p , SOBJ str );
PORT * port_open_input_file (char *fname );
PORT * port_open_output_file (char *fname );
PORT * port_open_input_string (char *string );
PORT * port_open_output_string PROTO ((void));
void port_puts (PORT *p , char *str );
void port_putn (PORT *p , long n );
void port_putx (PORT *p , void *ptr );
void port_putd (PORT *p , double n );
SOBJ scm_mkport (PORT *port );
SOBJ scm_mk_fn_port (int fn , int is_read );
void scm_port_sweep (SOBJ port );
void scm_port_print (SOBJ port , PORT *p );
SCM_STRBUF * scm_port2str (SCM_STRBUF *sb , SOBJ obj , int raw );
void scm_eof_print (SOBJ port , PORT *p );
void scm_eof_write (SOBJ port , PORT *p );
SCM_STRBUF * scm_eof2str (SCM_STRBUF *sb , SOBJ obj , int raw );
SCM_STRBUF * scm_list2str (SCM_STRBUF *sb , SOBJ l , int raw );
SCM_STRBUF * scm_iobj2str (SCM_STRBUF *sb , SOBJ obj , int raw );
void scm_write_obj (SOBJ obj , PORT *port , int raw );
SOBJ scm_display2 (SOBJ obj , SOBJ port );
SOBJ scm_write2 (SOBJ obj , SOBJ port );
SOBJ scm_newline1 (SOBJ port );
SOBJ scm_print2 (SOBJ obj , SOBJ port );
SOBJ scm_putc (int c );
SOBJ scm_puts (char *s );
SOBJ scm_putn (int n );
SOBJ scm_putx (void *ptr );
SOBJ scm_cdisplay (SOBJ obj );
SOBJ scm_cwrite (SOBJ obj );
SOBJ scm_cprint (SOBJ obj );
SOBJ scm_portp (SOBJ x );
SOBJ scm_input_portp (SOBJ x );
SOBJ scm_output_portp (SOBJ x );
SOBJ scm_current_input_port PROTO ((void));
SOBJ scm_current_output_port PROTO ((void));
SOBJ scm_current_error_port PROTO ((void));
SOBJ scm_with_input_from_file (SOBJ filename , SOBJ thunk );
SOBJ scm_with_output_to_file (SOBJ filename , SOBJ thunk );
SOBJ scm_with_input_from_string (SOBJ str , SOBJ thunk );
SOBJ scm_with_output_to_string (SOBJ thunk );
SOBJ scm_open_input_file (SOBJ filename );
SOBJ scm_open_output_file (SOBJ filename );
SOBJ scm_open_input_string (SOBJ string );
SOBJ scm_open_output_string PROTO ((void));
SOBJ scm_get_output_string (SOBJ port );
SOBJ scm_close_port (SOBJ port );
SOBJ scm_close_input_port (SOBJ port );
SOBJ scm_close_output_port (SOBJ port );
SOBJ scm_read (int argc , SOBJ *arg );
SOBJ scm_read_char (int argc , SOBJ *arg );
SOBJ scm_peek_char (int argc , SOBJ *arg );
SOBJ scm_eof_objectp (SOBJ obj );
SOBJ scm_read_line (SOBJ str , SOBJ port );
SOBJ scm_char_readyp (int argc , SOBJ *arg );
SOBJ scm_write_char (int argc , SOBJ *arg );
SOBJ scm_flush_output (int argc , SOBJ *arg );
SOBJ scm_file_position (int argc , SOBJ *arg );
void scm_port_init_default_files PROTO ((void));
SOBJ scm_float_precision (SOBJ x );
SOBJ scm_obj2str (SOBJ obj , SOBJ flag );
void scm_init_port PROTO ((void));
/* From `proc.c': */
void scm_env_mark (SOBJ env );
void scm_proc_mark (SOBJ proc );
void scm_clos_mark (SOBJ obj );
void scm_env_sweep (SOBJ env );
void scm_proc_sweep (SOBJ proc );
void scm_env_print (SOBJ obj , PORT *p );
SCM_STRBUF * scm_env2str (SCM_STRBUF *sb , SOBJ obj , int raw );
void scm_proc_print (SOBJ obj , PORT *p );
SCM_STRBUF * scm_proc2str (SCM_STRBUF *sb , SOBJ obj , int raw );
void scm_clos_print (SOBJ obj , PORT *p );
SCM_STRBUF * scm_clos2str (SCM_STRBUF *sb , SOBJ obj , int raw );
SOBJ scm_procedurep (SOBJ x );
SOBJ scm_environmentp (SOBJ x );
SOBJ scm_closurep (SOBJ x );
SOBJ scm_primitivep (SOBJ x );
SOBJ scm_cprimitivep (SOBJ x );
SOBJ scm_syntaxp (SOBJ x );
SOBJ scm_cprimitive_arity (SOBJ x );
SOBJ scm_primitive_address (SOBJ x );
SOBJ scm_primitive_arity (SOBJ x );
void scm_init_proc PROTO ((void));
/* From `s.c': */
extern long scm_default_hs;
extern long scm_default_ds;
extern int scm_no_init;
extern int scm_interractive;
extern int scm_force_interractive;
extern char * scm_progname;
extern char * scm_prompt_str;
extern char * scm_qs_lib;
extern void * scm_cstack_limit;
extern SOBJ scm_last_read_object;
extern SOBJ_TYPE_DESCR scm_type_hook[];
extern int scm_type_next_descr;
extern SOBJ scm_true;
extern SOBJ scm_false;
extern SOBJ scm_unbound;
extern SOBJ scm_undefined;
extern SOBJ scm_env, scm_sym_quote, scm_sym_qquote, scm_sym_unquote, scm_sym_unquote_splicing, scm_sym_set, scm_sym_if, scm_sym_lambda, scm_sym_begin, scm_sym_define, scm_sym_code, scm_sym_let, scm_sym_letstar, scm_sym_letrec, scm_sym_env, scm_sym_immediate, scm_sym_and, scm_sym_or, scm_sym_cond, scm_sym_else, scm_sym_while, scm_sym_until, scm_sym_do, scm_sym_catch;
SOBJ scm_disasm (SOBJ obj );
int scm_skip_blanks (PORT *port );
int scm_get_word (PORT *port , int c );
int scm_read_token (PORT *port );
SOBJ scm_read_list (PORT *port );
SOBJ scm_read_port (PORT *port );
void scm_internal_err (char *cfunc , char *msg , SOBJ obj );

#ifdef OLD
void SCM_ERR (char *msg , SOBJ obj );

#endif
SOBJ scm_error (SOBJ string , SOBJ object );
SOBJ scm_execute (SOBJ code );
SOBJ scm_eval (SOBJ expr , SOBJ env );
SOBJ scm_apply (SOBJ func , SOBJ argl );
SOBJ scm_apply_v (SOBJ func , int argc , SOBJ *argv );
SOBJ scm_apply0 (SOBJ func );
SOBJ scm_apply1 (SOBJ func , SOBJ a1 );
SOBJ scm_apply2 (SOBJ func , SOBJ a1 , SOBJ a2 );
SOBJ scm_apply3 (SOBJ func , SOBJ a1 , SOBJ a2 , SOBJ a3 );
PORT * scm_load_try_open (SOBJ filename );
SOBJ scm_load (SOBJ filename );
SOBJ scm_loadstr (char *str );
SOBJ scm_eval_string (SOBJ str );
SOBJ scm_evalstr (char *str );
void scm_add_cprim (char *name , SCM_CPRIM f , int n );
void scm_add_vmfunc (char *name , void ( *f ) ( SCM_vmRegisters * ) );
void scm_add_prim (char *name , SCM_PRIM_TABLE *entry );
SOBJ scm_add_syntax (char *name , SCM_CPRIM f );
void scm_syntax_print (SOBJ obj , PORT *p );
int scm_add_type (SOBJ_TYPE_DESCR *type );
SOBJ_TYPE_DESCR * scm_lookup_type (char *name );
SOBJ scm_type_list PROTO ((void));
SOBJ scm_mktype (SOBJ name );
SOBJ scm_add_type_finalizer (SOBJ type , SOBJ proc );
SOBJ scm_null_auxp (SOBJ obj );
SOBJ scm_set_aux (SOBJ obj , SOBJ val );
SOBJ scm_get_aux (SOBJ obj );
SOBJ scm_clear_aux (SOBJ obj );
void scm_init_type PROTO ((void));
SOBJ scm_unbound_func PROTO ((void));
void scm_set_argv (int argc , char **argv );
void scm_set_environ PROTO ((void));
void scm_init (int argc , char **argv );
void scm_main_loop PROTO ((void));
void scm_announce PROTO ((void));
void scm_usage PROTO ((void));
void scm_boot (int argc , char **argv );
/* From `str.c': */
SCM_STRBUF * scm_strbuf_new (int size );
SCM_STRBUF * scm_strbuf_resize (SCM_STRBUF *sb , int newlen );
SCM_STRBUF * scm_strbuf_clear (SCM_STRBUF *sb );
SCM_STRBUF * scm_strbuf_set (SCM_STRBUF *sb , char *str );
SCM_STRBUF * scm_strbuf_concat_buf (SCM_STRBUF *sb , char *buf , int len );
SCM_STRBUF * scm_strbuf_concat_str (SCM_STRBUF *sb , char *buf );
SCM_STRBUF * scm_strbuf_concat_chr (SCM_STRBUF *sb , char c );
SCM_STRBUF * scm_strbuf_sprintf (SCM_STRBUF *sb , char *fmt , ... );
SCM_STRBUF * scm_strbuf_concat_sprintf (SCM_STRBUF *sb , char *fmt , ... );
SCM_STRBUF * scm_strbuf_print (SCM_STRBUF *sb );
SCM_STRBUF * scm_strbuf_fprint (SCM_STRBUF *sb , FILE *fd );
void scm_strbuf_stats (SCM_STRBUF *sb );
SOBJ scm_str_alloc (int len );
SOBJ scm_str_resize (SOBJ str , int newlen );
SOBJ scm_mkstring (char *str );
int scm_strlen (SOBJ str );
SOBJ scm_strcat_chr (SOBJ str , int c );
SOBJ scm_strcat_buf (SOBJ str , char *buf , int len );
SOBJ scm_string_append_char (SOBJ str , SOBJ chr );
void scm_string_sweep (SOBJ str );
void scm_string_print (SOBJ str , PORT *p );
void scm_string_write (SOBJ str , PORT *p );
SCM_STRBUF * scm_string2str (SCM_STRBUF *sb , SOBJ str , int raw );
int scm_string_reconize (PORT *port , int c );
SOBJ scm_string_parse (PORT *port , int start_char );
SOBJ scm_string_compare (SOBJ s1 , SOBJ s2 );
SOBJ scm_stringp (SOBJ x );
SOBJ scm_make_string2 (SOBJ l , SOBJ c );
SOBJ scm_make_string (int nargs , SOBJ *a );
SOBJ scm_string (int nargs , SOBJ *a );
SOBJ scm_string_length (SOBJ str );
SOBJ scm_string_ref (SOBJ str , SOBJ index );
SOBJ scm_string_set (SOBJ str , SOBJ index , SOBJ chr );
SOBJ scm_string_lt (SOBJ s1 , SOBJ s2 );
SOBJ scm_string_le (SOBJ s1 , SOBJ s2 );
SOBJ scm_string_eq (SOBJ s1 , SOBJ s2 );
SOBJ scm_string_ge (SOBJ s1 , SOBJ s2 );
SOBJ scm_string_gt (SOBJ s1 , SOBJ s2 );
SOBJ scm_string_ci_lt (SOBJ s1 , SOBJ s2 );
SOBJ scm_string_ci_le (SOBJ s1 , SOBJ s2 );
SOBJ scm_string_ci_eq (SOBJ s1 , SOBJ s2 );
SOBJ scm_string_ci_ge (SOBJ s1 , SOBJ s2 );
SOBJ scm_string_ci_gt (SOBJ s1 , SOBJ s2 );
SOBJ scm_substring (SOBJ string , SOBJ start , SOBJ end );
SOBJ scm_string_append (int nargs , SOBJ *s );
SOBJ scm_string_append2 (SOBJ str1 , SOBJ str2 );
SOBJ scm_string_concat (SOBJ str1 , SOBJ str2 );
SOBJ scm_string_to_list (SOBJ str );
SOBJ scm_string_to_symbol (SOBJ str );
SOBJ scm_symbol_to_string (SOBJ sym );
SOBJ scm_list_to_string (SOBJ list );
SOBJ scm_string_copy (SOBJ str );
SOBJ scm_string_fill (SOBJ str , SOBJ chr );
SOBJ scm_string_index (SOBJ instr , SOBJ ssearch );
SOBJ scm_string_chop (SOBJ str );
SOBJ scm_string_split (SOBJ delim , SOBJ str );
SOBJ scm_string_join (SOBJ sep , SOBJ list );
SOBJ scm_string_lower (SOBJ str );
SOBJ scm_string_upper (SOBJ str );
SOBJ scm_string_translate (SOBJ str , SOBJ fr , SOBJ to );
SOBJ scm_string_pack (int nargs , SOBJ *arg );
SOBJ scm_string_unpack (SOBJ tmpl , SOBJ string );
SOBJ scm_string_resize (SOBJ str , SOBJ len );
void scm_init_str PROTO ((void));
/* From `sym.c': */
extern SOBJ scm_symbol_hash;
extern SOBJ scm_keyword_hash;
extern int scm_keyword_write_mode;
extern char * scm_keyword_write_prefix;
extern char * scm_keyword_write_suffix;
SOBJ scm_mksymbol2 (SOBJ atom , SOBJ value );
SOBJ scm_mksymbol (char *str );
SOBJ scm_mkkeyword2 (SOBJ atom , SOBJ dummy );
SOBJ scm_mkkeyword (char *str );
void scm_symbol_mark (SOBJ obj );
void scm_symbol_sweep (SOBJ obj );
SOBJ scm_symsearch (char *str );
SOBJ scm_atom_hash_complete (SOBJ hash , SOBJ atom , SOBJ ( *create ) ( ) );
SOBJ scm_symadd (char *str , SOBJ value );
SOBJ scm_sym_clone (SOBJ sym );
SOBJ scm_keyword_add (char *str );
SOBJ scm_keywordp (SOBJ obj );
SOBJ scm_keyword_to_string (SOBJ obj );
SOBJ scm_string_to_keyword (SOBJ obj );
SOBJ scm_get_keyword (SOBJ keyw , SOBJ list , SOBJ default_value );
void scm_init_symbol_hash PROTO ((void));
SOBJ scm_keyword_display_type (SOBJ x );
SOBJ scm_gensym (int nargs , SOBJ *arg );
SOBJ scm_make_symbol (SOBJ name );
SOBJ scm_symbol_name (SOBJ x );
SOBJ scm_symbol_value (SOBJ x );
void scm_init_symbol PROTO ((void));
/* From `variable.c': */
extern SCM_VarAux scm_var_hook[];
SOBJ scm_var_get (SOBJ var , void *ptr );
SOBJ scm_var_set (SOBJ var , void *ptr , SOBJ value );
SCM_VarAux * scm_var_type_lookup (SOBJ atom );
void scm_var_mark (SOBJ var );
void scm_var_sweep (SOBJ var );
void scm_var_print (SOBJ x , PORT *p );
void scm_var_write (SOBJ x , PORT *p );
SOBJ scm_mkvar (SOBJ type , void *addr );
SOBJ scm_external_variablep (SOBJ x );
SOBJ scm_make_extern_var (SOBJ lib , SOBJ type , SOBJ name );
void scm_add_cvar (char *name , SOBJ *addr );
void scm_init_variable PROTO ((void));
/* From `vm2.c': */
extern int scm_op_max;
extern void * scm_op_low_addr;
extern void * scm_op_high_addr;

#ifndef SCM_WITH_THREADS
extern SCM_VMD scm_vmdata;

#endif
void scm_vmd_regs_reset (SCM_VMD *vm );
void scm_vmd_dump (SCM_VMD *vm );
void scm_vmd_stack_alloc (SCM_VMD *vm , int size );
void scm_vmd_stack_free (SCM_VMD *vm );
SCM_VMD * scm_vmd_new PROTO ((void));
void sdump (SCM_VMD *vm );
void scm_dump_cont (SCM_ContFrame *c );
void scm_dump_env (SOBJ e );
void scm_vmfunc_mark (SOBJ obj );
void scm_vmfunc_sweep (SOBJ obj );
inline void scm_vm_move_stack (SOBJ *dst , SOBJ *src , int nitems );
void scm_vm (SCM_VMD *vm );
void scm_engine_init PROTO ((void));
SOBJ scm_run_engine (SOBJ *ip );
SCM_PRIM_TABLE * scm_get_addr (int opc );
int scm_is_opcode_address (void *p );
SCM_PRIM_TABLE * scm_search_opcode_address (char *name );
char * scm_search_opcode_name (void *p );
SOBJ scm_disassemble (SOBJ *code , int nslots );
/* From `process.c': */
extern int SOBJ_T_PROCESS;
extern SOBJ scm_process_list;
extern SOBJ_TYPE_DESCR scm_process_type;
SCM_STRBUF * scm_process2str (SCM_STRBUF *sb , SOBJ obj , int raw );
SOBJ scm_processp (SOBJ obj );
SOBJ scm_process_pid (SOBJ obj );
SOBJ scm_process_input (SOBJ obj );
SOBJ scm_process_output (SOBJ obj );
SOBJ scm_process_error (SOBJ obj );
SOBJ scm_process_status (SOBJ obj );
SOBJ scm_make_process (int argc , SOBJ *arg );
SOBJ scm_process_wait (SOBJ proc );
SOBJ scm_process_use_execv (SOBJ flag );
void scm_init_process PROTO ((void));
/* From `format.c': */
char * scm_asprintf (char *fmt , ... );
SOBJ scm_sprintf (char *fmt , ... );
SOBJ scm_cformat (int nargs , SOBJ *arg );
void scm_init_format PROTO ((void));
/* From `thread.c': */

#ifdef SCM_WITH_THREADS
extern int SOBJ_T_THREAD;
extern int SOBJ_T_MUTEX;
extern int SOBJ_T_SEMAPHORE;
extern pthread_mutex_t scm_thread_locker;
extern SOBJ scm_thread_list;
extern pthread_mutex_t scm_heap_locker;
extern sem_t scm_got_suspend_signal;
extern pthread_key_t scm_vmd_key;
SOBJ scm_thread_new (SCM_VMD *vmd );
void scm_thread_mark (SOBJ x );
void scm_thread_sweep (SOBJ x );
void scm_thread_print (SOBJ x , PORT *p );
void scm_thread_write (SOBJ x , PORT *p );
extern SOBJ_TYPE_DESCR scm_thread_type_descr;
SOBJ scm_thread_dump (SOBJ x );
SOBJ scm_mutex_new PROTO ((void));
void scm_mutex_sweep (SOBJ x );
void scm_mutex_print (SOBJ x , PORT *p );
void scm_mutex_write (SOBJ x , PORT *p );
extern SOBJ_TYPE_DESCR scm_mutex_type_descr;
SOBJ scm_mutex_p (SOBJ x );
SOBJ scm_mutex_lock (SOBJ x );
SOBJ scm_mutex_try_lock (SOBJ x );
SOBJ scm_mutex_unlock (SOBJ x );
SOBJ scm_semaphore_new (int nargs , SOBJ *arg );
void scm_semaphore_sweep (SOBJ x );
void scm_semaphore_print (SOBJ x , PORT *p );
void scm_semaphore_write (SOBJ x , PORT *p );
extern SOBJ_TYPE_DESCR scm_semaphore_type_descr;
SOBJ scm_semaphore_p (SOBJ x );
SOBJ scm_semaphore_wait (SOBJ x );
SOBJ scm_semaphore_try_wait (SOBJ x );
SOBJ scm_semaphore_post (SOBJ x );
SOBJ scm_semaphore_get_value (SOBJ x );
void scm_thread_suspend_other PROTO ((void));
void scm_thread_resume_other PROTO ((void));
SOBJ scm_thread_lookup (pthread_t tid );
void scm_thread_suspend_handler (int sig );
void scm_thread_resume_handler (int sig );
SOBJ scm_thread (SOBJ thunk );
SOBJ scm_threadp (SOBJ x );
SOBJ scm_thread_id (SOBJ x );
SOBJ scm_current_thread PROTO ((void));
SOBJ scm_thread_runningp (SOBJ x );
SOBJ scm_thread_wait (SOBJ x );
SOBJ scm_thread_kill (SOBJ x );
SOBJ scm_thread_exit PROTO ((void));
SOBJ scm_thread_suspend (SOBJ x );
SOBJ scm_thread_resume (SOBJ x );
void scm_init_main_thread PROTO ((void));
void scm_init_thread PROTO ((void));

#endif /* SCM_WITH_THREADS */
/* From `file.c': */
extern int SOBJ_T_FILE;
SOBJ scm_file_new (FILE *fp );
SOBJ scm_file2obj (int type , void *p );
void * scm_obj2file (SOBJ x );
void scm_file_sweep (SOBJ x );
SOBJ scm_file_compare (SOBJ x , SOBJ y );
extern SOBJ_TYPE_DESCR scm_file_type;
SOBJ scm_fopen (SOBJ fname , SOBJ fmod );
SOBJ scm_fclose (SOBJ x );
SOBJ scm_fread (SOBJ str , SOBJ len , SOBJ file );
SOBJ scm_fwrite (SOBJ str , SOBJ file );
SOBJ scm_fgetline (SOBJ str , SOBJ file );
SOBJ scm_fputs (SOBJ str , SOBJ file );
void scm_init_file PROTO ((void));
