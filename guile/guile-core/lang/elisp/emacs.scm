(define-module (lang elisp emacs)
  #:use-module (ice-9 optargs)
  #:use-module (lang elisp transform))


;;;; {Elisp Evaluation}

;;;; All elisp evaluation happens within the same module.  This is
;;;; necessary both because elisp itself has no concept of different
;;;; modules - reflected for example in its single argument `eval'
;;;; function - and because Guile's current implementation of elisp
;;;; stores elisp function definitions in slots in global symbol
;;;; objects.

(define the-elisp-module (current-module))


;;;; {Elisp Primitives}
;;;;
;;;; In other words, Scheme definitions of elisp primitives.  This
;;;; should include everything that Emacs defines in C.

(use-modules (lang elisp fset)
	     (lang elisp signal)
	     (lang elisp lists)
	     (lang elisp load)
	     (lang elisp features)
	     (lang elisp format)
	     (lang elisp buffers)
	     (lang elisp symprop)
	     (lang elisp keymaps))

(define-macro (fset-procs-with-same-name . syms)
  `(begin ,@(map (lambda (sym)
		   `(fset ',sym ,sym))
		 syms)))

(fset 'set
      (lambda (sym val)
	(local-define (list sym) val)))

(fset 'fboundp
      (lambda (sym)
	(t-ify (variable? (symbol-fref sym)))))

(fset 'symbol-function
      (lambda (sym)
	(let ((var (symbol-fref sym)))
	  (if (variable? var)
	      (variable-ref var)
	      (error "Symbol's function definition is void:" sym)))))

(fset 'boundp
      (lambda (sym)
	(t-ify (defined? sym))))

(fset 'symbol-value
      (lambda (sym)
	(or (local-ref (list sym))
	    (error "Symbol's value as variable is void:" sym))))

(fset 'apply
      (lambda (sym . args)
	(apply apply (@fop symbol-function sym) args)))

(fset 'funcall
      (lambda (sym . args)
	(apply (@fop symbol-function sym) args)))

(define (wta x pos)
  (error (string-append "Wrong type argument in position "
			(number->string pos)
			":")
	 x))

(fset 'length
      (lambda (x)
	(cond ((null x) 0)
	      ((pair? x) (length x))
	      ((vector? x) (vector-length x))
	      ((string? x) (string-length x))
	      (else (wta x 1)))))

(fset 'elt
      (lambda (obj i)
	(cond ((pair? obj) (list-ref obj i))
	      ((vector? obj) (vector-ref obj i))
	      ((string? obj) (char->integer (string-ref obj i))))))

(fset 'logior logior)
(fset 'list list)

(for-each (lambda (sym+proc)
	    (fset (car sym+proc)
		  (lambda (x) (t-ify ((cdr sym+proc) x)))))

	  `((integerp . ,integer?)
	    (symbolp  . ,symbol?)))

(for-each (lambda (sym+proc)
	    (fset (car sym+proc)
		  (lambda (x y) (t-ify ((cdr sym+proc) x y)))))

	  `((equal    . ,equal?)
	    (=        . ,=)
	    (<        . ,<)
	    (>        . ,>)
	    (<=       . ,<=)
	    (>=       . ,>=)))

(for-each (lambda (sym+proc)
	    (fset (car sym+proc)
		  (lambda (x y) (nil-ify ((cdr sym+proc) x y)))))

	  `((memq     . ,memq)
	    (member   . ,member)
	    (assq     . ,assq)
	    (assoc    . ,assoc)))

;;; More elisp primitives

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

(fset 'number-to-string number->string)

;;; {Functions}

(if #f
    (let ((accessible-procedures
	   (apropos-fold (lambda (module name var data)
			   (cons (cons name var) data))
			 '()
			 ""
			 (apropos-fold-accessible (current-module)))))
      (for-each (lambda (name var)
		  (if (procedure? var)
		      (fset name var)))
		(map car accessible-procedures)
		(map cdr accessible-procedures))))

;;; MEGA HACK!!!!

(fset 'read (lambda (str)
	      (cond ((string=? str "?\\M-\\^@")
		     -134217728)
		    (else
		     (with-input-from-string str read)))))

;;; Now switch into Emacs Lisp syntax and continue by loading
;;; `loadup.el', which is what the `bare' undumped Emacs does to load
;;; in all its standard elisp code before dumping.

(read-set! keywords 'prefix)
(read-set! language 'elisp)

(set-module-transformer! (current-module) transformer)

(load "loadup.el")
