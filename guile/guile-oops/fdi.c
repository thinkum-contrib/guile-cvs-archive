#include <libguile.h>
#include <goops.h>

struct b {
  double y;
  double z;
};

struct a {
  int x;
  struct b *p;
};

static struct a *make_a (SCM initargs);
static size_t free_a (struct a *o);

SCM_CLASS (b, "<b>", SCM_EOL, 0, 0, 0);
SCM_SLOT_DOUBLE (b, struct b, y, "y", "b:y");
SCM_SLOT_DOUBLE (b, struct b, z, "z", "b:z");

SCM_CLASS (a, "<a>", SCM_EOL, 0, make_a, free_a);
SCM_SLOT_INT (a, struct a, x, "x", "a:x");
SCM_SLOT_PTR (a, struct a, p, "p", b, "a:p");

SCM_KEYWORD (k_x, "x");

static struct a *
make_a (SCM initargs)
{
  struct b *p = (struct b *) malloc (sizeof (*p));
  struct a *o = (struct a *) malloc (sizeof (*o));
  o->x = SCM_INUM (scm_get_keyword (k_x, initargs, SCM_INUM0));
  o->p = p;
  scm_done_malloc (sizeof (*p) + sizeof (*o));
  return o;
}

static size_t
free_a (struct a *o)
{
  free (o->p);
  free (o);
  return sizeof (struct b) + sizeof (*o);
}

void scm_init_fdi (void);

void
scm_init_fdi ()
{
#include "fdi.x"
}
