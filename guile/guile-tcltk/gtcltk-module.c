#include "libguile.h"

static void
scm_init_gtcltk ()
{
    scm_init_gtcl ();
    scm_init_gtk ();
}

void
scm_init_ice_9_gtcltk_module ()
{
    scm_register_module_xxx ("ice-9 gtcltk", scm_init_gtcltk);
}
