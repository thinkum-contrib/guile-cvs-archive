; -*- scheme -*-
;
; Easy Gtk
;

(needs "gtk-main" "sgtk.scm")

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
  (let ((win (gtk-window-new TOPLEVEL)))
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
;; dir = :hori | :verti
;;
(define (box-create parent dir border)
  (let ((box
		 (if (eq? dir :hori) (gtk-hbox-new #f 0) (gtk-vbox-new #f 0))))
	(adopt parent box)
	(gtk-container-set-border-width box border)
	(gtk-widget-show box)
	box))

;;
;; pack a widget to a box.
;; placement = left | :right
;; fill = <boolean>
;;
(define (box-add parent widget placement fill)
  (if fill
	  (if (eq? placement :left)
		  (gtk-box-pack-start parent widget #t #t 0)
		  (gtk-box-pack-end   parent widget #t #t 0))
	  (if (eq? placement :left)
		  (gtk-box-pack-start parent widget #f #f 0)
		  (gtk-box-pack-end   parent widget #f #f 0))))

;; create a box
;;
;; placement = :hori | :verti
(define (box-fixed parent placement border)
  (let ((box (if (eq? placement :hori)
				 (gtk-hbox-new #f 0)
				 (gtk-vbox-new #f 0))))
	(gtk-box-pack-start parent box #f #f 0)
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

(define (entry-get entry)
  (gtk-entry-get-text entry))

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

;; create a text widget
;; editable = <boolean>

(define (text-create parent editable initial text-changed)
  (let ((itable '())
		(itext  '())
		(isb '()))
	(set! itable (gtk-table-new 2 2 #f))
	(gtk-table-set-row-spacing itable 0 2)
	(gtk-table-set-col-spacing itable 0 2)
	(adopt parent itable)
	(gtk-widget-show itable)
	
	(set! itext (gtk-text-new '() '()))
	(gtk-text-set-word-wrap itext #t)
	(gtk-table-attach-defaults itable itext 0 1 0 1)
	(gtk-text-set-editable itext editable)
	(gtk-widget-show itext)

	(set! isb (gtk-vscrollbar-new (gtk-text-get-vadj itext)))
	(gtk-table-attach itable isb
					  1 2 0 1
					  FILL (bit-or FILL EXPAND)
					  0 0)

	(gtk-widget-show isb)
	(if (procedure? text-changed) 
		(gtk-signal-connect itext "changed" text-changed '()))
	(gtk-text-freeze itext)
	(gtk-widget-realize itext)
	(gtk-text-insert itext '() '() '() initial -1)
	(gtk-text-thaw itext)
	itext))

(define (text-insert textarea text)
  (gtk-text-freeze textarea)
  (gtk-text-insert textarea '() '() '() text -1)
  (gtk-text-thaw textarea))

(define (text-insert-file textarea filename)
  (catch
   #t
   (lambda ()
	 (print "File not found") #f)
   (let ((p (open-input-file filename))
		 (line))
	 (gtk-text-freeze textarea)
	 (while (not (eof-object? (set! line (read-line p))))
			(gtk-text-insert textarea '() '() '() line -1)
			(gtk-text-insert textarea '() '() '() "\n" -1))
	 
	 (gtk-text-thaw textarea)
	 (close-port p))))

(define (text-get textarea)
  (gtk-editable-get-chars textarea 0 (gtk-text-get-length textarea)))

(define (text-delete textarea)
  (gtk-text-set-point textarea 0)
  (gtk-text-forward-delete textarea (gtk-text-get-length textarea))
  (gtk-text-get-length textarea))


;; test
(define (tst)
  (letrec
	  ((wmain
		(window-create "Test for egtk.scm" #f #f 640 400 #f
					   (lambda (w e d)
						 (print "Window destroyed")
						 (gtk-main-quit)
						 #f)))
	   
	   (wbox (box-create wmain :verti 4))
	   (hbox (box-fixed  wbox  :hori  0))
	   (clear-button 
		(button-create hbox "Clear"
					   (lambda (w d)
						 (display "clearing ") (print wtxt)
						 (text-delete wtxt)) '()))
	   (load-button
		(button-create hbox "Load"
					   (lambda (w d)
						 (print "loading")
						 (text-delete wtxt)
						 (text-insert-file wtxt (entry-get entry))) '()))
	   (entry
		(entry-create hbox ""))
		
	   (wtxt
		(text-create wbox #t "Initial text" '()))
		)
	(gtk-main)
	(gc)))
