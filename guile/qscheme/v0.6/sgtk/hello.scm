#!/home/crad/s/sgtk/sgtk -*- scheme -*-

; (needs "gtk-main" "sgtk.scm")
(needs "gtk-signal-connect"  	"sgtk.so")
(needs "gtk-window-new"    		"gtkdefs.so")
(needs "gdk-window-init"   		"gdkdefs.so")
(needs "g-print"  				"glib.scm")

(define gc-verbose 
  (make-extern-variable "" :int "scm_gc_verbose"))

(define sgtk-func-hash
  (make-extern-variable "" :cscheme "sgtk_func_hash"))

(define sgtk-obj-cache
  (make-extern-variable "" :cscheme "sgtk_obj_cache"))

(define TOPLEVEL 	(gtk-enum-get "GtkWindowType" 	    'toplevel))
(define FILL   		(gtk-enum-get "GtkAttachOptions"    "fill"))
(define EXPAND 		(gtk-enum-get "GtkAttachOptions"    "expand"))

(define gdk-exposure-mask
  (gtk-enum-get "GdkEventMask" "exposure-mask"))

(define gdk-pointer-motion-mask
  (gtk-enum-get "GdkEventMask" "pointer-motion-mask"))

(define gdk-pointer-motion-hint-mask
  (gtk-enum-get "GdkEventMask" "pointer-motion-hint-mask"))

(define gdk-button-press-mask
  (gtk-enum-get "GdkEventMask" "button-press-mask"))

(define gdk-enter-notify-mask
  (gtk-enum-get "GdkEventMask" "enter-notify-mask"))

(define (delete-event widget event data)
  (g-print "delete event occured\n")  
  (g-print "delete-event: widget=%p event=%p data=%p\n" widget event data)
  #f)

(define (destroy widget data)
  (g-print "got destroyed\n")
  (gtk-main-quit)
  #f)

(define (clicked widget data)
  (g-print "%p %p - Hello world\n" widget data)
)

; create a new window

(define (window-create title x y w h border destroy)
  (let ((win (gtk-window-new :toplevel)))
	(gtk-signal-connect win "delete_event" destroy '())
	(if (and (number? x) (number? y))
		(gtk-window-set-uposition win x y))
	(gtk-window-set-title win title)
	(gtk-widget-set-usize win w h)
	(gtk-widget-show win)
	(gtk-container-set-border-width win (if (number? border) border 2))
	win))

(define (adopt parent child)
  (if (gtk-container? parent)
	  (gtk-container-add parent child)
	  (gtk-box-pack-start parent child #t #t 0)))

;;; create a new box

(define (box-create parent dir border)
  (let ((box
		 (if (eq? dir :hori) (gtk-hbox-new #f 0) (gtk-vbox-new #f 0))))
	(adopt parent box)
	(gtk-container-set-border-width box border)
	(gtk-widget-show box)
	box))


;;; create a new entry

(define (entry-create parent text)
  (let ((ientry (gtk-entry-new)))
	(gtk-entry-set-text ientry text)
	(adopt parent ientry)
	(gtk-widget-show ientry)
	ientry))

;;; create a new button

(define (button-create parent text click)
  (let ((but (gtk-button-new-with-label text)))
	(gtk-signal-connect but "clicked" click '())
	(gtk-box-pack-start parent but #t #t 0)
	(gtk-widget-show but)
	but))

;;; create a drawing area

(define (drawing-area-create parent width height)
  (let ((darea (gtk-drawing-area-new)))

	(gtk-widget-set-events darea
	   (bit-or gdk-exposure-mask
			   gdk-pointer-motion-mask
			   gdk-pointer-motion-hint-mask
			   gdk-button-press-mask
			   gdk-enter-notify-mask))

	(gtk-widget-set-usize darea width height)
	(gtk-box-pack-start parent darea #t #t 0)
	(gtk-widget-show darea)
	darea))


(define (click-cb x)
  (display "callback: ") (print x))

(define (entry-cb widget entry)
  (g-print "callback: widget=%p entry=%p\n" widget entry)
  (print (gtk-entry-get-text entry))
  (gtk-entry-set-text entry "")
  )

;;; Callbacks for darea

(define (darea-expose widget event data)
  (g-print "darea exposed\n")
  #f)

(define (darea-motion widget event data)
  (if (gdk-event-motion-hint event)
	  (let ((l (gdk-window-get-pointer (gdk-event-window event))))
		(g-print "mouse %d %d %04lx\n"
				 (nth 0 l) (nth 1 l) (nth 2 l)))

	  (g-print "mouse %d %d %04lx\n"
			   (gdk-event-motion-x event)
			   (gdk-event-motion-y event)
			   (gdk-event-motion-state event)))
  #f)

(define (darea-bpress widget event data)
  (g-print "darea bpress\n")
  #f) 

(define (on-entry-activate widget data)
  (let ((str (gtk-entry-get-text widget))
		(result '()))
	(display "String to interpret: ") (print str)
	(display "Result of interpret: ")
	(print
	 (catch #t
			(lambda () "*** ABORTED ***")
			(eval (with-input-from-string str read) '())))
	(gtk-entry-set-text widget "")))

(define (hello)
  (letrec
	  ((win (window-create "Hello world" #f #f 200 200 #f delete-event))
	   (box (box-create win :verti 4))
	   (but1 (button-create
			  box "Hello world"
			  (lambda (widget data) (print "Hello world")) '()))

	   (but2 (button-create
			  box "Yes"
			  (lambda (widget data) (print "Yes!")) '()))

	   (but3 (button-create
			  box "No"
			  (lambda (widget data) (print "No!")) '()))
	   
	   (entry (entry-create box "Default text"))
	   (darea (drawing-area-create box 120 120))
	   )

	(gtk-signal-connect win "destroy" destroy '())
	(gtk-signal-connect entry "activate" on-entry-activate '())
	
	(gtk-signal-connect darea "expose_event"
						(lambda (w e d) (darea-expose w e d)) '())
	(gtk-signal-connect darea "motion_notify_event" darea-motion '())
	(gtk-signal-connect darea "button_press_event"  darea-bpress '())
	(gtk-main)))


(define (tst1)
  (let* ((win (window-create "Hello world" #f #f 200 300 #f delete-event))
		 (box (box-create win :verti 10))
		 (darea (gtk-drawing-area-new)))

	(gtk-signal-connect win "destroy" destroy '())

	(gtk-widget-set-events darea
	   (bit-or gdk-exposure-mask
			   gdk-pointer-motion-mask
			   gdk-pointer-motion-hint-mask
			   gdk-button-press-mask
			   gdk-enter-notify-mask))

	
	(gtk-box-pack-start box darea #t #t 0)
	(gtk-widget-set-usize darea 200 200)
	(gtk-widget-show darea)
	
	(gtk-signal-connect darea "expose_event"        darea-expose '())
	(gtk-signal-connect darea "motion_notify_event" darea-motion '())
	(gtk-signal-connect darea "button_press_event"  darea-bpress '())

	(gtk-main)))
