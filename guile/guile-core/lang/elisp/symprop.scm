(define-module (lang elisp symprop)
  #:use-module (lang elisp fset))

;;; {Elisp Exports}

(fset 'put set-symbol-property!)
(fset 'get (lambda (sym prop)
	     (nil-ify (symbol-property sym prop))))
