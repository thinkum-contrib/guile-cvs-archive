(module (ice-9 defmacro) 
	(open (ice-9 error) 
	      (ice-9 provide)
	      (ice-9 guile))
	(export defmacro gentemp defmacro:transformer defmacro:syntax-transformer))
	      

;;; {Macros}
;;;

(define (primitive-macro? m)
  (and (macro? m)
       (not (macro-transformer m))))

;;; {Defmacros}
;;;
(define macro-table (make-weak-key-hash-table 523))
(define xformer-table (make-weak-key-hash-table 523))

(define (defmacro? m)  (hashq-ref macro-table m))
(define (assert-defmacro?! m) (hashq-set! macro-table m #t))
(define (defmacro-transformer m) (hashq-ref xformer-table m))
(define (set-defmacro-transformer! m t) (hashq-set! xformer-table m t))

(define defmacro:transformer
  (lambda (f)
    (let* ((xform (lambda (exp env)
		    (copy-tree (apply f (cdr exp)))))
	   (a (procedure->memoizing-macro xform)))
      (assert-defmacro?! a)
      (set-defmacro-transformer! a f)
      a)))


(define defmacro
  (let ((defmacro-transformer
	  (lambda (name parms . body)
	    (let ((transformer `(lambda ,parms ,@body)))
	      `(define ,name
		 (,(lambda (transformer)
		     (defmacro:transformer transformer))
		  ,transformer))))))
    (defmacro:transformer defmacro-transformer)))


; (defmacro defmacro-public args
;   (define (syntax)
;     (error "bad syntax" (list 'defmacro-public args)))
;   (define (defined-name n)
;     (cond
;      ((symbol? n)	n)
;      (else 		(syntax))))
;   (cond
;    ((null? args)	(syntax))

;    (#t
;     (let ((name (defined-name (car args))))
;       `(begin
; 	 (let* (
; 		(module (the-module))
; 		(eval (module-eval-environment module))

; 		;; look up the old value first
; 		(val (if (environment-bound? eval ',name)
; 			 (environment-ref eval ',name)
; 			 (begin
; 			   (environment-define eval ',name #f)
; 			   #f)))
		
; 		(export (module-export-environment module)))
; 	   (if (not (environment? export))
; 	       (let ( ;; create export environment
; 		     (export (make-export-environment eval (list ',name))))
; 		 (module-export-environment-set! module export)
; )
; 	       (environment-define export ',name val)))
			       
; 	 ;; Now (re)define the var normally.
; 	 ;;
; 	 (defmacro ,@ args))))))

(define defmacro:syntax-transformer
  (lambda (f)
    (procedure->syntax
     (lambda (exp env)
       (copy-tree (apply f (cdr exp)))))))


;; XXX - should the definition of the car really be looked up in the
;; current module?

(define (macroexpand-1 e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (local-ref (list a)))))
		(if (defmacro? val)
		    (apply (defmacro-transformer val) (cdr e))
		    e)))
   (#t e)))

(define (macroexpand e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (local-ref (list a)))))
		(if (defmacro? val)
		    (macroexpand (apply (defmacro-transformer val) (cdr e)))
		    e)))
   (#t e)))

(define (gentemp)
  (gensym "scm:G"))

(provide 'defmacro)

