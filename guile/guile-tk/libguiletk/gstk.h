#include <libguile.h>
/*
 * Header <tcl.h> is always included (even if not USE_TK) for the hash table
 * function prototypes. 
 */
#include <tcl.h>

/* ------------------------------------------------------------------------ */

#define ROOT_WINDOW	"*root*"	/* Scheme name of main window */

/*
  ----------------------------------------------------------------------------
  ----
  ---- T K - M A I N . C
  ----
  ----------------------------------------------------------------------------
*/
extern Tcl_Interp *STk_main_interp;	/* Interpreter for this application. */
