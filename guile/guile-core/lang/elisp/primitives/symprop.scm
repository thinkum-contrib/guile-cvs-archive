(define-module (lang elisp primitives symprop)
  #:use-module (lang elisp internals fset))

;;; {Elisp Exports}

(fset 'put set-symbol-property!)

(fset 'get symbol-property)

(fset 'set
      (lambda (sym val)
	(local-define (list sym) val)))

(fset 'boundp defined?)

(fset 'symbol-value
      (lambda (sym)
	(or (local-ref (list sym))
	    (error "Symbol's value as variable is void:" sym))))

(fset 'symbolp symbol?)
