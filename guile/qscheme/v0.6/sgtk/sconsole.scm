; -*- scheme -*-
; Scheme console
;
(needs "glade-xml-new"  "sglade.scm")


(define sconsole-text-point 0)			; insert point

(define (text-insert widget text)
  (gtk-text-insert widget '() '() '() text (string-length text)))

(define (sc-handle-text-input w d)
  (display "changed:")
  (print (gtk-editable-get-chars w sconsole-text-point (gtk-text-get-length w))))
;  (print (gtk-editable-get-chars w 0 (gtk-text-get-length w)))))

(define (on-sctext-activate w d)
  (display "activate: widget: ") (print w)
  (let* ((end-point (gtk-text-get-length w))
		 (text (gtk-editable-get-chars w sconsole-text-point end-point))
		 (result) (output))
	(display "Activate:") (print text)
	(set! result
		  (with-output-to-string
			(lambda ()
			  (catch #t
					 (lambda (t m) (string-append "ERROR: " m))
					 (write (eval (with-input-from-string text read) '()))))))
	 
	(display "Eval:") (print result)
	(text-insert w "\n")
	(text-insert w result)
	(text-insert w "\nQScheme> ")
	(set! sconsole-text-point (gtk-text-get-length w))
	))

(define (make-sconsole)
  (let ((xml (glade-xml-new "sconsole.glade" '())))
	(glade-xml-signal-connect
	 xml "on_wconsole_destroy"
	 (lambda (w d)
	   (print "wconsole destroy")
	   (gtk-main-quit)
	   #f))

	(glade-xml-signal-connect
	 xml "on_exit1_activate"
	 (lambda (w d)
	   (gtk-widget-destroy (glade-xml-get-widget xml "wconsole"))
	   (gtk-main-quit)))

;	(glade-xml-signal-connect
;	 xml "on_sctext_changed"  (lambda (w d) (sc-handle-text-input w d)))

	(glade-xml-signal-connect
	 xml "on_sctext_activate" (lambda (w d) (on-sctext-activate w d)))

	xml))

(define xml)


(define (start)
  (set! xml (make-sconsole))
  (let* ((text-widget (glade-xml-get-widget xml "sctext")))
	(text-insert text-widget
				 "QScheme
Copyright (C) 1998-2000 Daniel Crettol <dan@sof.ch>
QScheme is distributed under the GNU General Public Licence.
See the LICENCE file for more informations.\nQScheme> ")
	(gtk-widget-set-usize (glade-xml-get-widget xml "wconsole") 520 320)
;	(gtk-text-set-line-wrap text-widget 0)
	(set! sconsole-text-point (gtk-text-get-length text-widget))
	(gtk-main)))

;(thread (lambda() (start)))
