(define-module (lang elisp)
  )

(define-public the-elisp-module %module-public-interface)

;;; {Exporting names}
;;;

(define (fset fun val)
  (let ((vcell (symbol-fref fun)))
    (if (pair? vcell)
	(set-cdr! vcell val)
	(symbol-fset! fun (cons fun val)))))

(define (binder converter)
  (lambda (names data)
    (for-each (lambda (name data)
		(fset name (converter data)))
	      names
	      data)))

(define export-functions (binder (lambda (x) x)))
(define export-mmacros
  (binder (lambda (x)
	    (if (macro? x)
		x
		(procedure->memoizing-macro x)))))

;;; {S-expressions}
;;;

(define (syntax-error x)
  (error "Syntax error in expression" x))

(define (wta x pos)
  (error (string-append "Wrong type argument in position "
			(number->string pos)
			":")
	 x))

;; Should be made mutating instead of constructing
;;
(define (transform x)
  (cond ((null? x) nil)
	((not (pair? x)) x)
	((symbol? (car x))
	 (case (car x)
	   ((@fop @bind define-module use-modules use-syntax) x)
	   ; Should be handled in reader
	   ((quote) (cons 'quote (cars->nil (cdr x))))
	   ((nil-cond) (transform-1 x))
	   ((let) (m-let x ()))
	   ((let*) (m-let* x ()))
	   ((if) (m-if x ()))
	   ((and) (m-and x ()))
	   ((or) (m-or x ()))
	   ((while) (m-while x ()))
	   ;((while) (cons macro-while (cdr x)))
	   ((prog1) (m-prog1 x ()))
	   ((prog2) (m-prog2 x ()))
	   ((progn begin) (cons 'begin (map transform (cdr x))))
	   ((cond) (m-cond x ()))
	   ((lambda) (transform-lambda x))
	   ((defun) (m-defun x ()))
	   ((setq) (cons macro-setq (cdr x)))
	   (else (transform-application x))))
	(else (syntax-error x))))

(define (transform-1 x)
  (cons (car x) (map transform (cdr x))))

(define (transform-2 x)
  (cons (car x)
	(cons (cadr x)
	      (map transform (cddr x)))))

(define (transform-3 x)
  (cons (car x)
	(cons (cadr x)
	      (cons (caddr x)
		    (map transform (cdddr x))))))

(define (transform-list x)
  (map transform x))

(define (transform-lambda exp)
  `(lambda ,(repaint (cadr exp))
     (@bind ,(cadr exp) ,@(transform-list (cddr exp)))))

(define (transform-application x)
  (cons '@fop
	(cons (car x)
	      (map transform (cdr x)))))

(define (cars->nil ls)
  (cond ((not (pair? ls)) ls)
	((null? (car ls)) (cons 'nil (cars->nil (cdr ls))))
	(else (cons (cars->nil (car ls))
		    (cars->nil (cdr ls))))))

(define-public elisp transform)

;;; {Special forms}
;;;

(define (m-setq exp env)
  (let* ((binder (car (last-pair env)))
	 (varvals (let loop ((ls (cdr exp)))
		    (if (null? ls)
			'()
			;; Ensure existence only at macro expansion time
			(let ((var (or (binder (car ls) #f)
				       (binder (car ls) #t))))
			  (if (not (variable-bound? var))
			      (variable-set! var #f))
			  (cons (list 'set! (car ls) (transform (cadr ls)))
				(loop (cddr ls))))))))
    (cond ((null? varvals) 'nil)
	  ((null? (cdr varvals)) (car varvals))
	  (else (cons 'begin varvals)))))

;;; *fixme* &rest etc
;;;
(define (repaint names)
  (map (lambda (name) '_)
       names))

(define (m-defun exp env)
  `(begin (@fop fset ',(cadr exp)
		(lambda ,(repaint (caddr exp))
		  (@bind ,(caddr exp) ,@(transform-list (cdddr exp)))))
	  ',(cadr exp)))

(define (m-let exp env)
  (let ((names (map car (cadr exp))))
    `((lambda ,(repaint names)
	(@bind ,names ,@(transform-list (cddr exp))))
      ,@(transform-list (map cadr (cadr exp))))))

(define (m-let* exp env)
  (if (null? (cadr exp))
      (cons 'begin (cddr exp))
      (let loop ((bindings (cadr exp)))
	(if (null? bindings)
	    (cddr exp)
	    (list `(lambda ,(repaint (list (caar bindings)))
		     (@bind (,(caar bindings))
			    ,@(loop (cdr bindings))))
		  (transform (cadar bindings)))))))

(define (m-prog1 exp env)
  `(,let ((%res1 ,(transform (cadr exp))))
	 ,@(transform-list (cddr exp))
	 %res1))

(define (m-prog2 exp env)
  `(begin ,(transform (cadr exp))
	  (,let ((%res2 ,(transform (caddr exp))))
		,@(transform-list (cdddr exp))
		%res2)))

(define <-- *unspecified*)

(define (m-if exp env)
  (let ((else-case (cdddr exp)))
    (cond ((null? else-case)
	   `(nil-cond ,(transform (cadr exp)) ,(transform (caddr exp)) nil))
	  ((null? (cdr else-case))
	   `(nil-cond ,(transform (cadr exp))
		      ,(transform (caddr exp))
		      ,(transform (car else-case))))
	  (else
	   `(nil-cond ,(transform (cadr exp))
		      ,(transform (caddr exp))
		      (begin ,@(transform-list else-case)))))))

(define (m-and exp env)
  (cond ((null? (cdr exp)) 't)
	((null? (cddr exp)) (transform (cadr exp)))
	(else
	 (cons 'nil-cond
	       (let loop ((args (cdr exp)))
		 (if (null? (cdr args))
		     (transform args)
		     (cons (list 'not (transform (car args)))
			   (cons 'nil
				 (loop (cdr args))))))))))

(define (m-or exp env)
  (cond ((null? (cdr exp)) 'nil)
	((null? (cddr exp)) (transform (cadr exp)))
	(else
	 (cons 'nil-cond
	       (let loop ((args (cdr exp)))
		 (if (null? (cdr args))
		     (transform args)
		     (cons (transform (car args))
			   (cons <--
				 (loop (cdr args))))))))))

(define m-cond
  (lambda (exp env)
    (if (null? (cdr exp))
	'nil
	(cons
	 'nil-cond
	 (let loop ((clauses (cdr exp)))
	   (if (null? clauses)
	       '(nil)
	       (let ((clause (car clauses)))
		 (if (eq? (car clause) 't)
		     (cond ((null? (cdr clause)) '(t))
			   ((null? (cddr clause))
			    (list (transform (cadr clause))))
			   (else `((begin ,@(transform-list (cdr clause))))))
		     (cons (transform (car clause))
			   (cons (cond ((null? (cdr clause)) <--)
				       ((null? (cddr clause))
					(transform (cadr clause)))
				       (else
					`(begin ,@(transform-list (cdr clause)))))
				 (loop (cdr clauses))))))))))))

(define (m-while exp env)
  `(,let %while ()
	 (nil-cond ,(transform (cadr exp))
		   (begin ,@(transform-list (cddr exp)) (%while))
		   nil)))

(export-mmacros
 '(setq defun let let* if and or cond while prog1 prog2 progn)
 (list m-setq m-defun m-let m-let* m-if m-and m-or m-cond m-while m-prog1 m-prog2 begin))

(define macro-setq (cdr (symbol-fref 'setq)))
(define macro-while (cdr (symbol-fref 'while)))

;;; {Functions}

;;; Copy all builtins as function bindings
;;;
(for-each
 (lambda (obarray)
   (array-for-each
    (lambda (oblist)
      (for-each
       (lambda (x)
	 (if (procedure? (cdr x))
	     (symbol-fset! (car x) (cons (car x) (cdr x)))))
       oblist))
    obarray))
 (list (builtin-weak-bindings)
       (builtin-bindings)))


(define (set var val)
  (local-define (list var) val))

(define (funcall fun . args)
  (apply fun args))

;(define (symbol-value sym) )

(define (symbol-function sym)
  (let ((vcell (symbol-fref sym)))
    (if (pair? vcell)
	(cdr vcell)
	(error "Symbol's function definition is void:" sym))))

(define (elisp-length x)
  (cond ((null x) 0)
	((pair? x) (length x))
	((vector? x) (vector-length x))
	((string? x) (string-length x))
	(else (wta x 1))))

(define (elt obj i)
  (cond ((pair? obj) (list-ref obj i))
	((vector? obj) (vector-ref obj i))
	((string? obj) (char->integer (string-ref obj i)))))

(export-functions
 '(set fset apply funcall symbol-function cons car cdr length elt)
 (list set fset apply funcall symbol-function nil-cons nil-car nil-cdr elisp-length elt))

;;; {Predicates}
;;;

(define (fboundp fun)
  (t-ify (pair? (symbol-fref fun))))

(define export-unary-predicates
  (binder (lambda (p) (lambda (x) (t-ify (p x))))))

(define export-binary-predicates
  (binder (lambda (p) (lambda (x y) (t-ify (p x y))))))

(export-functions
 '(fboundp eq)
 (list fboundp nil-eq))

(export-unary-predicates
 '(not null integerp symbolp boundp)
 (list null null integer? symbol? defined?))

(export-binary-predicates
 '(equal = < > <= >=)
 (list equal? = < > <= >=))

;;; {Semi-predicates}
;;;

(define export-binary-semi-predicates
  (binder (lambda (p) (lambda (x y) (nil-ify (p x y))))))

(export-binary-semi-predicates
 '(memq member assq assoc)
 (list memq member assq assoc))
