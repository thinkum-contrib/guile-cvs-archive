(define-module (lang elisp calling)
  #:use-module (lang elisp fset))

(fset 'apply
      (lambda (sym . args)
	(apply apply (@fop symbol-function sym) args)))

(fset 'funcall
      (lambda (sym . args)
	(apply (@fop symbol-function sym) args)))
