(define-module (gtk threads)
  :use-module (gtk guilegtkthreads)
  :use-module (gtk gtk)
  :use-module (gtk gdk)
  :use-module (ice-9 threads))

(export gtkthreads-handler? gtkthreads-ensure-handler)

(define handler-running? #f)

(define (gtkthreads-handler?)
  handler-running?)

(define (gtkthreads-ensure-handler)
  (if (not handler-running?)
      (begin-thread
       (dynamic-wind
	   (lambda ()
	     (gdk-threads-enter)
	     (set! handler-running? #t))
	   (lambda ()
	     (gtk-main))
	   (lambda ()
	     (set! handler-running? #f)
	     (gdk-threads-leave))))))
