(define-module (gtk threads)
  :use-module (gtk guilegtkthreads)
  :use-module (gtk gtk)
  :use-module (ice-9 threads))

(export gtkthreads-handler? gtkthreads-ensure-handler)

(define handler-running? #f)

(define (gtkthreads-handler?)
  handler-running?)

(define (gtkthreads-ensure-handler)
  (if (not handler-running?)
      (dynamic-wind
	  (lambda ()
	    (set! handler-running? #t))
	  (lambda ()
	    (begin-thread
	     (gtk-main)))
	  (lambda ()
	    (set! handler-running? #f)))))
