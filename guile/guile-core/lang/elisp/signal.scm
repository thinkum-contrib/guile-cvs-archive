(define-module (lang elisp signal)
  #:use-module (lang elisp fset)
  #:export (signal))

(define (signal error-symbol data)
  (scm-error 'elisp-signal
	     #f
	     "Signalling ~A with data ~S"
	     (list error-symbol data)
	     #f))

;;; {Elisp Exports}

(fset 'signal signal)
