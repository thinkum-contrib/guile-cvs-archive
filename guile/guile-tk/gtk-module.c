#include "libguile.h"

static void
scm_init_gtcltk ()
{
    scm_init_gtcl ();
    scm_init_gtk ();
}

void
scm_init_ice_9_gtk_module ()
{
    scm_register_module_xxx ("ice-9 gtk", scm_init_gtk);
}
