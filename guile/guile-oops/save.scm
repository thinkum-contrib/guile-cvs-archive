;;; installed-scm-file

;;;; 	Copyright (C) 2000 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 


(define-module (oop goops save)
  :use-module (oop goops internal)
  )

(export save-objects load-objects restore make-unbound
	enumerate! enumerate-component!
	write-readably write-component write-circref
	literal? readable make-readable)

;;;
;;; save-objects ALIST PORT
;;;
;;; ALIST ::= ((NAME . OBJECT) ...)
;;;
;;; Save OBJECT ... to PORT so that when the data is read and evaluated
;;; OBJECT ... are re-created under names NAME ... .
;;;
;;; [OK, I just quickly needed this code, and threw it together.
;;;  This is not how I think code should look like.
;;;  I still think it's usefulness motivates its inclusion in GOOPS.
;;;  Hopefully I can clean it up later.  /mdj]
;;;
;;; To add new read syntax, hang methods on `enumerate!' and
;;; `write-readably'.
;;;
;;; enumerate! OBJECT ENV
;;;   Should call `enumerate-component!' (which takes same args) on
;;;   each component object.  Should return #t if the composite object
;;;   can be written as a literal.  (`enumerate-component!' returns #t
;;;   if the component is a literal.
;;;
;;; write-readably OBJECT PORT ENV
;;;   Should write a readable representation of OBJECT to PORT.
;;;   Should call `write-component' (same args) to print each
;;;   component object.  If `write-component' returns #f, printing
;;;   failed dure to a circular reference, and you should call
;;;   `write-circref'.  Use `literal?' to decide if a component
;;;   is a literal.
;;;
;;; Utilities:
;;;
;;; enumerate-component! OBJECT ENV
;;;
;;; write-component OBJECT PORT ENV
;;;
;;; write-circref OBJECT COMPONENT PATCHER PORT ENV
;;;   PATCHER is a procedure taking OBJECT and VALUE as arguments,
;;;   and returning the list representation of a Scheme expression
;;;   which can set the location in OBJECT containing COMPONENT to
;;;   VALUE.
;;;
;;; literal? COMPONENT ENV
;;;

(define-method immediate? ((o <top>)) #f)

(define-method immediate? ((o <null>)) #t)
(define-method immediate? ((o <number>)) #t)
(define-method immediate? ((o <boolean>)) #t)
(define-method immediate? ((o <symbol>)) #t)
(define-method immediate? ((o <char>)) #t)
(define-method immediate? ((o <string>)) #t)
(define-method immediate? ((o <keyword>)) #t)

;;; enumerate! OBJECT ENVIRONMENT
;;;
;;; Return #t if object is a literal.
;;;
(define-method enumerate! ((o <top>) env) #t)

(define-method write-readably ((o <top>) file env)
  ;;(goops-error "No read-syntax defined for object `~S'" o)
  (write o file) ;doesn't catch bugs, but is much more flexible
  )

(define-method write-readably ((o <null>) file env) (write o file))
(define-method write-readably ((o <number>) file env) (write o file))
(define-method write-readably ((o <boolean>) file env) (write o file))
(define-method write-readably ((o <symbol>) file env) (write o file))
(define-method write-readably ((o <char>) file env) (write o file))
(define-method write-readably ((o <string>) file env) (write o file))
(define-method write-readably ((o <keyword>) file env) (write o file))

(if (or (not (defined? 'readables))
	(not readables))
    (define readables (make-weak-key-hash-table 61)))

(define readable
  (procedure->memoizing-macro
    (lambda (exp env)
      `(make-readable ,(cadr exp) ',(cadr exp)))))

(define (make-readable obj expr)
  (hashq-set! readables obj expr)
  obj)

(define (readable-expression obj)
  (hashq-ref readables obj))

(define readable? readable-expression)

;;;
;;; Vectors
;;;

(define-method enumerate! ((o <vector>) env)
  (or (uniform-vector? o)
      (let ((literal? #t))
	(array-for-each (lambda (o)
			  (if (not (enumerate-component! o env))
			      (set! literal? #f)))
			o)
	literal?)))

(define-method write-readably ((o <vector>) file env)
  (if (uniform-vector? o)
      (write o file)
      (let ((n (vector-length o)))
	(if (zero? n)
	    (display "#()" file)
	    (begin
	      (display (if (literal? o env)
			   "#("
			   "(vector ")
		       file)
	      (or (write-component (vector-ref o 0) file env)
		  (write-circref o
				 (vector-ref o 0)
				 (lambda (cont comp)
				   `(vector-set! ,cont 0 ,comp))
				 file
				 env))
	      (do ((i 1 (+ 1 i)))
		  ((= i n))
		(display #\space file)
		(or (write-component (vector-ref o i) file env)
		    (write-circref o
				   (vector-ref o i)
				   (lambda (cont comp)
				     `(vector-set! ,cont ,i ,comp))
				   file
				   env)))
	      (display #\) file))))))

;;;
;;; Pairs
;;;

;;; These methods have more complex structure than is required for
;;; most objects, since they take over some of the logic of
;;; `write-component'.
;;;
;;; `special-reference?' is used to look ahead
;;; `push-ref!' is used to push references onto the reference stack
;;;

(define-method enumerate! ((o <pair>) env)
  (let ((literal? (enumerate-component! (car o) env)))
    (and (enumerate-component! (cdr o) env)
	 literal?)))

(define-method write-readably ((o <pair>) file env)
  (let ((refs (ref-stack env))
	(proper? (list? o))
	(1? (or (not (pair? (cdr o)))
		(special-reference? (cdr o) env)))
	(literal? (literal? o env)))
    (display (cond (literal? #\()
		   (proper? "(list ")
		   (1? "(cons ")
		   (else "(list* "))
	     file)
    (or (write-component (car o) file env)
	(write-circref o (car o)
		       (lambda (o x) `(set-car! ,o ,x))
		       file env))
    (do ((ls (cdr o) (cdr ls)))
	((or (not (pair? ls))
	     (special-reference? ls env))
	 (if (not (null? ls))
	     (begin
	       (if literal?
		   (display " ." file))
	       (display #\space file)
	       (or (write-component ls file env)
		   (write-circref (container env) ls
				  (lambda (o x) `(set-cdr! ,o ,x))
				  file env))))
	 (display #\) file))
      (push-ref! ls env)
      (display #\space file)
      (or (write-component (car ls) file env)
	  (write-circref ls (car ls)
			 (lambda (o x) `(set-car! ,o ,x))
			 file env))
      )
    (set-ref-stack! env refs)))

;;;
;;; Objects
;;;

;;; Doesn't yet handle unbound slots

;; Don't export this function!  This is all very temporary.
;;
(define (get-set-for-each proc class)
  (for-each (lambda (slotdef g-n-s)
	      (let ((g-n-s (cddr g-n-s)))
		(cond ((integer? g-n-s)
		       (proc (standard-get g-n-s) (standard-set g-n-s)))
		      ((not (memq (slot-definition-allocation slotdef)
				  '(#:class #:each-subclass)))
		       (proc (car g-n-s) (cadr g-n-s))))))
	    (class-slots class)
	    (slot-ref class 'getters-n-setters)))

(define (access-for-each proc class)
  (for-each (lambda (slotdef g-n-s)
	      (let ((g-n-s (cddr g-n-s))
		    (a (slot-definition-accessor slotdef)))
		(cond ((integer? g-n-s)
		       (proc (slot-definition-name slotdef)
			     (and a (generic-function-name a))
			     (standard-get g-n-s)
			     (standard-set g-n-s)))
		      ((not (memq (slot-definition-allocation slotdef)
				  '(#:class #:each-subclass)))
		       (proc (slot-definition-name slotdef)
			     (and a (generic-function-name a))
			     (car g-n-s)
			     (cadr g-n-s))))))
	    (class-slots class)
	    (slot-ref class 'getters-n-setters)))

(define (restore class . values)
  (let ((o (%allocate-instance class '())))
    (get-set-for-each (lambda (get set)
			(set o (car values))
			(set! values (cdr values)))
		      class)
    o))

(define-method enumerate! ((o <object>) env)
  (get-set-for-each (lambda (get set)
		      (let ((val (get o)))
			(if (not (unbound? val))
			    (enumerate-component! val env))))
		    (class-of o))
  #f)

(define-method write-readably ((o <object>) file env)
  (let ((class (class-of o)))
    (display "(restore " file)
    (display (class-name class) file)
    (access-for-each (lambda (name aname get set)
		       (display #\space file)
		       (let ((val (get o)))
			 (if (unbound? val)
			     (display '(make-unbound) file)
			     (or (write-component val file env)
				 (write-circref o val
						(lambda (o x)
						  (if aname
						      `(set! (,aname ,o) ,x)
						      `(slot-set! ,o
								  ',name
								  ,x)))
						file env)))))
		     class)
    (display #\) file)))

;;;
;;; Classes
;;;

;;; Currently, we don't support reading in class objects
;;;

(define-method enumerate! ((o <class>) env) #f)

(define-method write-readably ((o <class>) file env)
  (display (class-name o) file))

;;;
;;; Generics
;;;

;;; Currently, we don't support reading in generic functions
;;;

(define-method enumerate! ((o <generic>) env) #f)

(define-method write-readably ((o <generic>) file env)
  (display (generic-function-name o) file))

;;;
;;; Method
;;;

;;; Currently, we don't support reading in methods
;;;

(define-method enumerate! ((o <method>) env) #f)

(define-method write-readably ((o <method>) file env)
  (goops-error "No read-syntax for <method> defined"))

;;;
;;; Environments
;;;
;;; ENVIRONMENT = (HASH-TABLE REFSTACK TOP-LEVEL BINDING-WRITTEN . BINDINGS)
;;;
;;; HASH-KEY = OBJECT
;;; HASH-DATA = (#t/NAME . LITERAL?)
;;;
;;; TOP-LEVEL-KEY = OBJECT
;;; TOP-LEVEL-DATA = (NAME SETEXPn SETEXPn-1 ...)
;;;
;;; BINDING-KEY = NAME
;;; BINDING-DATA = OBJECT
;;;

(define (make-env)
  (list (make-hash-table 61) '() '() #f))

(define hash-table car)

;;; The ref-stack is obsolete and should be removed
;;; root and cointainer should be new fields
(define ref-stack cadr)

(define (set-ref-stack! env refs)
  (set-car! (cdr env) refs))

(define (clear-ref-stack! env)
  (set-car! (cdr env) '()))

(define (push-ref! o env)
  (set-car! (cdr env) (cons o (ref-stack env))))

(define (pop-ref! env)
  (set-car! (cdr env) (cdr (ref-stack env))))

(define container caadr)

(define container-container cadadr)

(define (root env)
  (car (last-pair (ref-stack env))))

(define (circular-reference? o env)
  (memq o (ref-stack env)))

(define (fixing-literals! env)
  (set-car! (cddr env) #f))

(define (fixing-literals? env)
  (not (caddr env)))

(define top-level caddr)

(define (clear-top-level! env)
  (set-car! (cddr env) '()))

(define (top-level-add! name o env)
  (set-car! (cddr env) (cons (list o name) (caddr env))))

(define (add-setexp! setexp env)
  (let ((entry (cdr (assq (root env) (top-level env)))))
    (set-cdr! entry (cons setexp (cdr entry)))))

(define (setexps o env)
  (map (lambda (info)
	 ((caddr info)
	  (object->var (car info) env)
	  (object->var (cadr info) env)))
       (reverse (cond ((assq o (top-level env)) => cddr)
		      (else '())))))

(define (writing! var env)
  (set-car! (cdddr env) var))

(define (writing? var env)
  (eq? (cadddr env) var))

(define (add-binding! x env)
  (let ((name (gensym "%o"))
	(entry (hashq-ref (hash-table env) x)))
    (set-cdr! (cdddr env) (acons name x (cddddr env)))
    (set-car! entry name)))

(define (register-binding! x env)
  (let ((entry (hashq-ref (hash-table env) x)))
    (or (and entry (car entry))
	(add-binding! x env))))

(define (bindings env)
  (cddddr env))

(define (literal? o env)
  (or (immediate? o)
      (cdr (hashq-ref (hash-table env) o))))

;;; Only used in setexps
(define (object->var o env)
  (cond ((assq o (top-level env)) => cadr)
	(else (car (hashq-ref (hash-table env) o)))))

(define (special-reference? o env)
  ;; See write-component
  (let ((var (car (hashq-ref (hash-table env) o))))
    (or (and var
	     (or (eq? o (root env))
		 (not (assq o (top-level env)))))
	(and (symbol? var)
	     (not (writing? var env))))))

;;;
;;; Enumeration
;;;

(define (enumerate-component! o env)
  (cond ((immediate? o)
	 (enumerate! o env))
	((readable? o) #f)
	((fixing-literals? env)
	 (let ((entry (hashq-ref (hash-table env) o)))
	   ;; handle vars
	   (if (and (not (null? ref-stack))
		    (symbol? (car entry)))
	       #f
	       (let ((literal? (if (circular-reference? o env)
				   (cdr entry)
				   (and (cdr entry)
					(begin
					  (push-ref! o env)
					  (let ((literal? (enumerate! o env)))
					    (pop-ref! env)
					    literal?))))))
		 (set-cdr! entry literal?)
		 literal?))))
	((hashq-ref (hash-table env) o)
	 =>
	 (lambda (x)
	   (or (car x)
	       (and (not (null? (ref-stack env)))
		    (assq o (top-level env)))
	       (add-binding! o env))
	   (and (not (null? (ref-stack env)))
		(or (eq? o (root env))
		    (not (assq o (top-level env))))
		(begin
		  (or (null? (cdr (ref-stack env))) ;container is root
		      (begin
			(register-binding! (container env) env)
			;(set-cdr! (hashq-ref (hash-table env)
			;		     (container-container env))
			;	  #f)
			))
		  (if (not (car x))
		      (set-car! x #t))
		  #t))))
	(else
	 (let ((entry (cons #f #f)))
	   (hashq-set! (hash-table env) o entry)
	   (push-ref! o env)
	   (let ((literal? (enumerate! o env)))
	     (pop-ref! env)
	     (if literal?
		 (set-cdr! entry #t))
	     literal?)))))

(define (write-component o file env)
  (cond ((immediate? o)
	 (write-readably o file env)
	 #t)
	((readable? o)
	 (write (readable-expression o) file)
	 #t)
	(else
	 (let ((var (car (hashq-ref (hash-table env) o))))
	   (cond ((and var
		       (not (null? (ref-stack env)))
		       (or (eq? o (root env))
			   (not (assq o (top-level env)))))
		  #f)
		 ((or (boolean? var)
		      (writing? var env))
		  (push-ref! o env)
		  (write-readably o file env)
		  (pop-ref! env)
		  #t)
		 (else
		  (display var file)
		  #t))))))

(define (write-circref cont comp setexp file env)
  (display #f file)
  (add-setexp! (list cont comp setexp) env))

;;;
;;; Main engine
;;;

(define-method save-objects ((alist <pair>) (file <string>) . rest)
  (let ((port (open-output-file file)))
    (apply save-objects alist port rest)
    (close-port port)))

(define-method save-objects ((alist <pair>) (file <output-port>) . rest)
  (let ((excluded (if (>= (length rest) 1) (car rest) '()))
	(uses     (if (>= (length rest) 2) (cadr rest) '()))
	(env (make-env)))
    (for-each (lambda (pair)
		(top-level-add! (car pair) (cdr pair) env)
		(enumerate-component! (cdr pair) env))
	      alist)
    (fixing-literals! env)
    (for-each (lambda (pair)
		(enumerate-component! (cdr pair) env))
	      alist)
    (if (not (null? uses))
	(begin
	  (write `(use-modules ,@uses) file)
	  (newline file)))
    (clear-top-level! env)
    (let ((bindings (bindings env)))
      (let ((write-binding
	     (lambda (binding file)
	       (top-level-add! (car binding) (cdr binding) env)
	       (display "(" file)
	       (display (car binding) file)
	       (display #\space file)
	       (if (literal? (cdr binding) env)
		   (display #\' file))
	       (write-component (cdr binding) file env)
	       (display ")" file)))
	    (write-definitions
	     (lambda ()
	       (let ((prefix (if (null? bindings) "(define " "  (set! ")))
		 (for-each (lambda (pair)
			     (top-level-add! (car pair) (cdr pair) env)
			     (display prefix file)
			     (display (car pair) file)
			     (display #\space file)
			     (if (literal? (cdr pair) env)
				 (display #\' file))
			     (write-component (cdr pair) file env)
			     (display ")\n" file)
			     (or (immediate? (cdr pair))
				 (set-car! (hashq-ref (hash-table env)
						      (cdr pair))
					   (car pair))))
			   alist))))
	    (write-circref-patches
	     (lambda ()
	       (for-each (lambda (entry)
			   (for-each (lambda (setexp)
				       (display (if (null? bindings) "" "  ")
						file)
				       (write ((caddr setexp)
					       (object->var (car setexp) env)
					       (object->var (cadr setexp) env))
					      file)
				       (newline file))
				     (reverse (cddr entry))))
			 (reverse (top-level env))))))
	(if (null? bindings)
	    (begin
	      (write-definitions)
	      (write-circref-patches))
	    (begin
	      (for-each (lambda (pair)
			  (display "(define " file)
			  (display (car pair) file)
			  (display " #f)\n" file))
			alist)
	      (display "(let* (" file)
	      (writing! (caar bindings) env)
	      (write-binding (car bindings) file)
	      (for-each (lambda (pair)
			  (display "\n       " file)
			  (writing! (car pair) env)
			  (write-binding pair file))
			(cdr bindings))
	      (writing! #f env)
	      (display ")\n" file)
	      (write-definitions)
	      (write-circref-patches)
	      (display "  )\n" file)))))))

(define-method load-objects ((file <string>))
  (let* ((port (open-input-file file))
	 (objects (load-objects port)))
    (close-port port)
    objects))

(define-method load-objects ((file <input-port>))
  (let ((m (make-module)))
    (module-use! m the-scm-module)
    (module-use! m %module-public-interface)
    (save-module-excursion
     (lambda ()
       (set-current-module m)
       (let loop ((sexp (read file)))
	 (if (not (eof-object? sexp))
	     (begin
	       (eval-in-module sexp m)
	       (loop (read file)))))))
    (module-map (lambda (name var)
		  (cons name (variable-ref var)))
		m)))
