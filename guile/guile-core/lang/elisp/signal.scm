(define-module (lang elisp signal)
  #:use-module (lang elisp fset)
  #:export (signal wta))

(define (signal error-symbol data)
  (scm-error 'elisp-signal
	     #f
	     "Signalling ~A with data ~S"
	     (list error-symbol data)
	     #f))

(define (wta x pos)
  (error (string-append "Wrong type argument in position "
			(number->string pos)
			":")
	 x))

;;; {Elisp Exports}

(fset 'signal signal)
