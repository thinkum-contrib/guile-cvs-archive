(define-module (lang elisp symprop)
  #:use-module (lang elisp fset))

;;; {Elisp Exports}

(fset 'put set-symbol-property!)

(fset 'get symbol-property)

(fset 'set
      (lambda (sym val)
	(local-define (list sym) val)))

(fset 'fboundp
      (lambda (sym)
	(variable? (symbol-fref sym))))

(fset 'symbol-function
      (lambda (sym)
	(let ((var (symbol-fref sym)))
	  (if (variable? var)
	      (variable-ref var)
	      (error "Symbol's function definition is void:" sym)))))

(fset 'boundp defined?)

(fset 'symbol-value
      (lambda (sym)
	(or (local-ref (list sym))
	    (error "Symbol's value as variable is void:" sym))))

(fset 'symbolp symbol?)
