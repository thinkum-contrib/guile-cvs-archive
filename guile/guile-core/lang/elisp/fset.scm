(define-module (lang elisp fset)
  #:export (fset))

;;; By default, Guile GC's unreachable symbols.  So we need to make
;;; sure they are reachable!
(define syms '())

;;; fset SYM PROC
;;;
;;; Store the Scheme procedure PROC in SYM's function slot.
(define (fset sym proc)
  (or (memq sym syms)
      (set! syms (cons sym syms)))
  (let ((vcell (symbol-fref sym)))
    (if (variable? vcell)
	(variable-set! vcell proc)
	(symbol-fset! sym (make-variable proc)))))

(define (interactive . args)
  #t)

(fset 'fset fset)
(fset 'defalias fset)
(fset 'interactive interactive) ;FIXME
