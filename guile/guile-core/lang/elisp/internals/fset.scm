(define-module (lang elisp internals fset)
  #:use-module (lang elisp internals signal)
  #:use-module (lang elisp internals evaluation)
  #:export (fset
	    fref
	    fref/error-if-void
	    elisp-apply
	    interactive-specification
	    not-subr?))

;; By default, Guile GC's unreachable symbols.  So we need to make
;; sure they stay reachable!
(define syms '())

;; Store the procedure, macro or alias symbol PROC in SYM's function
;; slot.
(define (fset sym proc)
  (or (memq sym syms)
      (set! syms (cons sym syms)))
  (let ((vcell (symbol-fref sym)))
    (if (variable? vcell)
	(variable-set! vcell proc)
	(symbol-fset! sym (make-variable proc)))
    (if (procedure? proc)
	(or (procedure-property proc 'name)
	    (set-procedure-property! proc
				     'name
				     (symbol-append '<elisp-subr:
						    sym
						    '>))))))

;; Retrieve the procedure or macro stored in SYM's function slot.
;; Note the asymmetry w.r.t. fset: if fref finds an alias symbol, it
;; recursively calls fref on that symbol.  Returns #f if SYM's
;; function slot doesn't contain a valid definition.
(define (fref sym)
  (let ((var (symbol-fref sym)))
    (if (and var (variable? var))
	(let ((proc (variable-ref var)))
	  (cond ((symbol? proc)
		 (fref proc))
		(else
		 proc)))
	#f)))

;; Same as fref, but signals an Elisp error if SYM's function
;; definition is void.
(define (fref/error-if-void sym)
  (or (fref sym)
      (signal 'void-function (list sym))))

;; Maps a procedure to its (interactive ...) spec.
(define interactive-specification (make-object-property))

;; Maps a procedure to #t if it is NOT a built-in.
(define not-subr? (make-object-property))

(define (elisp-apply function . args)
  (apply apply
	 (cond ((symbol? function)
		(fref/error-if-void function))
	       ((procedure? function)
		function)
	       ((and (pair? function)
		     (eq? (car function) 'lambda))
		(eval function the-elisp-module))
	       (else
		(signal 'invalid-function (list function))))
	 args))
