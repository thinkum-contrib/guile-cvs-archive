;
;
;
(needs "define-extern"  		"defextern.scm")
(needs "gtk-signal-connect" 	"sgtk.so")
(needs "gdk-window-ref"		 	"gdkdefs.so")
(needs "gtk-main"				"gtkdefs.so")
(needs "glade-xml-signal-connect" "sglade.so")

(define-extern :void	glade_init)
;(define-extern :void	glade_gnome_init)
(define-extern :void    glade_load_module :string)

(define-extern :GladeXML  glade_xml_new :string :string)
(define-extern :GladeXML  glade_xml_new_with_domain :string :string :string)

(define-extern :GtkObject glade_xml_get_widget :GladeXML :string)
(define-extern :GtkObject glade_xml_get_widget_by_long_name :GladeXML :string)

(define-extern :string    glade_get_widget_name :GtkObject)
(define-extern :string	  glade_get_widget_long_name :GtkObject)
(define-extern :GladeXML  glade_get_widget_tree :GtkObject)

(glade-init)

