#include "s.h"
#include <gtk/gtk.h>

int main(int argc, char **argv)
{
  gtk_init(&argc, &argv);
  scm_boot(argc, argv);
  exit(0);
}
