; -*- scheme -*-
;
; Gdk

(if (not (member "sgtk.so" library-list))
	(load-library "sgtk.so"))

(load "defextern.scm")
(load "gdkdefs.scm")					; The GDK bindings
(load "gtkdefs.scm")					; The GTK bindings
(load "glib.scm")
