(define-module (lang elisp strings)
  #:use-module (lang elisp fset))

(fset 'substring substring)

(fset 'concat
      (lambda args
	(apply string-append
	       (map (lambda (arg)
		      (cond
		       ((string? arg) arg)
		       ((list? arg) (list->string arg))
		       ((vector? arg) (list->string (vector->list arg)))
		       (else (error "Wrong type argument for concat"))))
		    args))))

(fset 'string-to-number string->number)

(fset 'number-to-string number->string)
