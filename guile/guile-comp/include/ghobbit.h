#include <libguile.h>
#if defined(SCM_FLOATS) 
#include <math.h>
#endif

/* To avoid name clashes when linking with libc.a or libm.a */
#define ftell ftell_notC
#define fseek fseek_notC
#define freopen freopen_notC
#define log10 log10_notC
#define getenv getenv_notC
#define exit exit_notC
#define getpwnam getpwnam_notC
#define getpwuid getpwuid_notC
#define endpwent endpwent_notC
#define getpwent getpwent_notC
#define setpwent setpwent_notC
#define getpw getpw_notC

/* Specifique a hobbit */
#define GLOBAL(x) (*(x))
#define VECTOR_SET(v,k,o) (SCM_VELTS(v)[((long)SCM_INUM(k))] = o)
#define VECTOR_REF(v,k) (SCM_VELTS(v)[((long)SCM_INUM(k))])
#define VECTOR_LENGTH(v) SCM_MAKINUM(SCM_LENGTH(v))

/* Pourrait etre mis dans le noyau de hobbit */
/*#define vector_fill_excl_ scm_vector_fill_x*/

/* Passage scm --> guile */
#define EOL SCM_EOL
#define CAR SCM_CAR
#define CDR SCM_CDR
#define NFALSEP SCM_NFALSEP
#define BOOL_F SCM_BOOL_F
#define BOOL_T SCM_BOOL_T
#define MAKINUM SCM_MAKINUM
#define listofnull scm_listofnull
#define tc7_subr_1 scm_tc7_subr_1
#define tc7_subr_2 scm_tc7_subr_2
#define tc7_subr_3 scm_tc7_subr_3
#define tc7_lsubr scm_tc7_lsubr
#define apply scm_apply
#define cons scm_cons
#define divide scm_divide
#define eqp scm_eq_p
#define evenp scm_even_p
#define greaterp scm_gr_p
#define lessp scm_less_p
#define lremainder scm_remainder
#define makrect scm_make_rectangular
#define product scm_product
#define sum scm_sum
#define difference scm_difference
#define intern(x,y) scm_permanent_object(scm_intern((x),(y)))
#define make_subr scm_make_subr
#define make_vector(vec,fill) scm_make_vector((vec),(fill),SCM_BOOL_F)
/* scm_abs does not work for real numbers as required by R4RS */
#define absval scm_magnitude
#define string2number scm_string_to_number
#define makfromstr(x,y) scm_makfromstr((x),(y),0)
#define list_ref scm_list_ref

#define NULL_P SCM_NULLP
#define SBOOL(x) ((x) ? SCM_BOOL_T : SCM_BOOL_F)
#define INUM SCM_INUM
#define cur_output_port scm_current_output_port
#define display scm_display
#define newline scm_newline
#define makcclo scm_makcclo
#define UNSPECIFIED SCM_UNSPECIFIED
#define tc7_subr_0 scm_tc7_subr_0
#define equal scm_equal_p
#define assv scm_assv
#define PAIR_P(x) (!(SCM_IMP(x)) && SCM_CONSP(x))
#define SET_CDR SCM_SETCDR
/*#define eval scm_eval*/

#define close_port scm_close_port
#define lwrite scm_write
#define open_file scm_open_file
#define set_outp scm_set_current_output_port
#define MAKICHR SCM_MAKICHR
#define memq scm_memq

#define cur_input_port scm_current_input_port
#define set_inp scm_set_current_input_port
#define lread scm_read
#define eof_objectp scm_eof_object_p

#define modulo scm_modulo
#define CHAR2INT(chr) scm_ulong2num((unsigned long)SCM_ICHR(chr))
#define ST_LENGTH(str) SCM_MAKINUM (SCM_ROLENGTH (str))
#define ST_REF(str,k) SCM_MAKICHR (SCM_ROUCHARS (str)[SCM_INUM (k)])
#define NOT(x) ((x)==SCM_BOOL_F ? SCM_BOOL_T : SCM_BOOL_F)

#define SYMBOL_P(x) (!(SCM_IMP(x)) && SCM_SYMBOLP(x))
#define STRING_P(x) (!(SCM_IMP(x)) && SCM_STRINGP(x))
#define BOOLEAN_P(obj) ((SCM_BOOL_F==(obj)) || (SCM_BOOL_T==(obj)))
#define NUMBERP SCM_NUMBERP
#define NUMBER_P SCM_INUMP
#define number2string scm_number_to_string
#define symbol2string scm_symbol_to_string

#define append2(lst1,lst2) (scm_append(scm_cons2((lst1),(lst2),SCM_EOL)))
#define st_append scm_string_append

#define string scm_string
#define string2list scm_string_to_list
#define string2symbol scm_string_to_symbol
#define INTEGER_P(x) (scm_integer_p(x) == SCM_BOOL_T)

#define listp scm_list_p
#define assoc scm_assoc

#define assq scm_assq
#define memv scm_memv
#define member scm_member
#define append scm_append

#define CHAR_P SCM_ICHRP
#define CHAR_UPCASE(chr) SCM_MAKICHR(scm_upcase(SCM_ICHR(chr)))
#define CHAR_DOWNCASE(chr) SCM_MAKICHR(scm_downcase(SCM_ICHR(chr)))
#define procedurep(x) (scm_procedure_p(x) == SCM_BOOL_T)
#define VECTOR_P(x) (!(SCM_IMP(x)) && SCM_VECTORP(x))
#define reverse scm_reverse
#define length scm_length
#define input_portp scm_input_port_p
#define output_portp scm_output_port_p

#define substring scm_substring
#define st_set scm_string_set_x
#define make_string scm_make_string
#define zerop scm_zero_p
#define lquotient scm_quotient
#define st_equal scm_string_equal_p
#define peek_char scm_peek_char
#define eqv scm_eqv_p
#define CHAR_NUMP scm_char_numeric_p
#define CHAR_WHITEP scm_char_whitespace_p

#define realp scm_real_p
#define real_part scm_real_part
#define imag_part scm_imag_part
#define makpolar scm_make_polar
#if defined(SCM_FLOATS) 
#define EXP_FUN(x) scm_makdbl(exp(scm_num2dbl((x), "hobbit")), 0.0)
#define LOG_FUN(x) scm_makdbl(log(scm_num2dbl((x), "hobbit")), 0.0)
#define SIN_FUN(x) scm_makdbl(sin(scm_num2dbl((x), "hobbit")), 0.0)
#define COS_FUN(x) scm_makdbl(cos(scm_num2dbl((x), "hobbit")), 0.0)
#define TAN_FUN(x) scm_makdbl(tan(scm_num2dbl((x), "hobbit")), 0.0)
#define ASIN_FUN(x) scm_makdbl(asin(scm_num2dbl((x), "hobbit")), 0.0)
#define ACOS_FUN(x) scm_makdbl(acos(scm_num2dbl((x), "hobbit")), 0.0)
#define ATAN_FUN(x) scm_makdbl(atan(scm_num2dbl((x), "hobbit")), 0.0)
#define SINH_FUN(x) scm_makdbl(sinh(scm_num2dbl((x), "hobbit")), 0.0)
#define COSH_FUN(x) scm_makdbl(cosh(scm_num2dbl((x), "hobbit")), 0.0)
#define TANH_FUN(x) scm_makdbl(tanh(scm_num2dbl((x), "hobbit")), 0.0)
#define ASINH_FUN(x) scm_makdbl(asinh(scm_num2dbl((x), "hobbit")), 0.0)
#define ACOSH_FUN(x) scm_makdbl(acosh(scm_num2dbl((x), "hobbit")), 0.0)
#define ATANH_FUN(x) scm_makdbl(atanh(scm_num2dbl((x), "hobbit")), 0.0)
#define SQRT_FUN(x) scm_makdbl(sqrt(scm_num2dbl((x), "hobbit")), 0.0)
#define EXPT_FUN(x,y) scm_makdbl(pow(scm_num2dbl((x), "hobbit"),scm_num2dbl((y), "hobbit")), 0.0)
#endif
#define intp scm_integer_p
#define CHAR_ALPHAP scm_char_alphabetic_p

#define ZERO_P(x) ((x) == SCM_INUM0)
#define magnitude scm_magnitude
#define angle scm_angle
#define greqp scm_geq_p
#define leqp scm_leq_p
#define exactp scm_exact_p
#define negativep scm_negative_p
#define in2ex scm_inexact_to_exact
#define NEGATIVE_P(x) ((x) < SCM_INUM0)
#define POSITIVE_P(x) ((x) > SCM_INUM0)
#define SET_CAR SCM_SETCAR

#define vector2list scm_vector_to_list
#define vector scm_vector

#if 0
#define INT2CHAR(n) SCM_MAKICHR(SCM_INUM(n))
#endif

