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
	write-readably write-component write-component-procedure
	literal? readable make-readable)

;;;
;;; save-objects ALIST PORT
;;;
;;; ALIST ::= ((NAME . OBJECT) ...)
;;;
;;; Save OBJECT ... to PORT so that when the data is read and evaluated
;;; OBJECT ... are re-created under names NAME ... .
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
;;;   Should use `write-component' to print each component object.
;;;   Use `literal?' to decide if a component is a literal.
;;;
;;; Utilities:
;;;
;;; enumerate-component! OBJECT ENV
;;;
;;; write-component OBJECT PATCHER PORT ENV
;;;   PATCHER is an expression which, when evaluated, stores OBJECT
;;;   into its current location.
;;;
;;;   Example:
;;;
;;;     (write-component (car ls) `(set-car! ,ls ,(car ls)) file env)
;;;
;;;   write-component is a macro.
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

;;;
;;; Readables
;;;

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
  (or (not (vector? o))
      (let ((literal? #t))
	(array-for-each (lambda (o)
			  (if (not (enumerate-component! o env))
			      (set! literal? #f)))
			o)
	literal?)))

(define-method write-readably ((o <vector>) file env)
  (if (not (vector? o))
      (write o file)
      (let ((n (vector-length o)))
	(if (zero? n)
	    (display "#()" file)
	    (begin
	      (display (if (literal? o env)
			   "#("
			   "(vector ")
		       file)
	      (write-component (vector-ref o 0)
			       `(vector-set! ,o 0 ,(vector-ref o 0))
			       file
			       env)
	      (do ((i 1 (+ 1 i)))
		  ((= i n))
		(display #\space file)
		(write-component (vector-ref o i)
				 `(vector-set! ,o ,i ,(vector-ref o i))
				 file
				 env))
	      (display #\) file))))))


;;;
;;; Arrays
;;;

(define-method enumerate! ((o <array>) env)
  (enumerate-component! (shared-array-root o) env))

(define (make-mapper array)
  (let* ((dims (array-dimensions array))
	 (n (array-rank array))
	 (indices (reverse (if (<= n 11)
			       (list-tail '(t s r q p n m l k j i)  (- 11 n))
			       (let loop ((n n)
					  (ls '()))
				 (if (zero? n)
				     ls
				     (loop (- n 1)
					   (cons (gensym "i") ls))))))))
    `(lambda ,indices
       (+ ,(shared-array-offset array)
	  ,@(map (lambda (ind dim inc)
		   `(* ,inc ,(if (pair? dim) `(- ,ind ,(car dim)) ind)))
		 indices
		 (array-dimensions array)
		 (shared-array-increments array))))))

(define-method write-readably ((o <array>) file env)
  (let ((root (shared-array-root o)))
    (cond ((binding? root env)
	   (display "(make-shared-array " file)
	   (write-component root
			    (goops-error "write-readably(<array>): internal error")
			    file
			    env)
	   (display #\space file)
	   (display (make-mapper o))
	   (for-each (lambda (dim)
		       (display #\space file)
		       (display dim file))
		     (array-dimensions o))
	   (display #\) file))
	  ((literal? o env)
	   (write o file))
	  (else
	   (let ((dims (array-dimensions o)))
	     (display "(list->uniform-array " file)
	     (display dims file)
	     (display " '() (list" file)
	     (let loop ((dims dims)
			(indices '()))
	       (cond ((null? dims))
		     ((null? (cdr dims))
		      ;; inner loop
		      (let ((n (car dims)))
			(do ((i 0 (+ 1 i)))
			    ((= i n))
			  (display #\space file)
			  (let ((indices (reverse (cons i indices))))
			    (write-component
			     (apply array-ref o indices)
			     `(array-set! ,o
					  ,(apply array-ref o indices)
					  ,@indices)
			     file
			     env)))))
		     (else
		      (let ((n (car dims)))
			(do ((i 0 (+ 1 i)))
			    ((= i n))
			  (display " (list" file)
			  (loop (cdr dims) (cons i indices))
			  (display #\) file)))))
	       (display #\) file)))))))

;;;
;;; Pairs
;;;

;;; These methods have more complex structure than is required for
;;; most objects, since they take over some of the logic of
;;; `write-component'.
;;;

(define-method enumerate! ((o <pair>) env)
  (let ((literal? (enumerate-component! (car o) env)))
    (and (enumerate-component! (cdr o) env)
	 literal?)))

(define-method write-readably ((o <pair>) file env)
  (let ((proper? (list? o))
	(1? (or (not (pair? (cdr o)))
		(binding? (cdr o) env)))
	(literal? (literal? o env))
	(infos '()))
    (display (cond (literal? #\()
		   (proper? "(list ")
		   (1? "(cons ")
		   (else "(list* "))
	     file)
    (write-component (car o) `(set-car! ,o ,(car o)) file env)
    (do ((ls (cdr o) (cdr ls))
	 (prev o ls))
	((or (not (pair? ls))
	     (binding? ls env))
	 (if (not (null? ls))
	     (begin
	       (if literal?
		   (display " ." file))
	       (display #\space file)
	       (write-component ls `(set-cdr! ,prev ,ls) file env)))
	 (display #\) file))
      (display #\space file)
      (set! infos (cons (object-info ls env) infos))
      (set! (visiting? (car infos)) #t)
      (write-component (car ls) `(set-car! ,ls ,(car ls)) file env)
      )
    (for-each (lambda (info)
		(set! (visiting? info) #f))
	      infos)
    ))

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
			     (write-component val
					      (if aname
						  `(set! (,aname ,o) ,val)
						  `(slot-set! ,o ',name ,val))
					      file env))))
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

(define-class <environment> ()
  (object-info 	  #:accessor object-info
	       	  #:init-form (make-hash-table 61))
  (excluded	  #:accessor excluded
		  #:init-form (make-hash-table 61))
  (pass-2?	  #:accessor pass-2?
		  #:init-value #f)
  (container-info #:accessor container-info)
  (objects	  #:accessor objects
		  #:init-value '())
  (locals	  #:accessor locals
		  #:init-value '())
  (stand-ins	  #:accessor stand-ins
		  #:init-value '())
  (definitions	  #:accessor definitions
		  #:init-value '())
  (patchers	  #:accessor patchers
		  #:init-value '())
  )

(define-method (object-info o env)
  (hashq-ref (object-info env) o))

(define-method ((setter object-info) o env x)
  (hashq-set! (object-info env) o x))

(define (excluded? o env)
  (hashq-get-handle (excluded env) o))

(define (add-patcher! patcher env)
  (set! (patchers env) (cons patcher (patchers env))))

(define-class <object-info> ()
  (visiting? #:accessor visiting?
	     #:init-value #f)
  (binding   #:accessor binding
	     #:init-value #f)
  (literal?  #:accessor literal?)
  )

(define-method (binding o env)
  (binding (object-info o env)))

(define binding? binding)

(define-method (literal? (info <boolean>))
  #t)

(define-method (literal? o env)
  (or (immediate? o)
      (excluded? o env)
      (literal? (object-info o env))))

;;;
;;; Enumeration
;;;

;;; Enumeration has two passes.
;;;
;;; Pass 1: Detect common substructure, circular references and order
;;;
;;; Pass 2: Detect literals

(define (enumerate-component! o env)
  (cond ((immediate? o) #t)
	((readable? o) #f)
	((excluded? o env) #t)
	((pass-2? env)
	 (let ((info (object-info o env)))
	   (if (binding? info)
	       (not (visiting? info))
	       (and (enumerate! o env)
		    (begin
		      (set! (literal? info) #t)
		      #t)))))
	((object-info o env)
	 => (lambda (info)
	      (set! (binding info) #t)
	      (if (visiting? info)
		  ;; circular reference--mark container
		  (set! (binding (container-info env)) #t))))
	(else
	 (let ((info (make <object-info>)))
	   (set! (object-info o env) info)
	   (set! (container-info env) info)
	   (set! (visiting? info) #t)
	   (enumerate! o env)
	   (set! (visiting? info) #f)
	   (set! (objects env) (cons o (objects env)))))))

(define (write-component-procedure o file env)
  "Return #f if circular reference"
  (cond ((immediate? o) (write o file) #t)
	((readable? o) (write (readable-expression o) file) #t)
	((excluded? o env))
	(else
	 (let ((info (object-info o env)))
	   (cond ((not (binding? info)) (write-readably o file env) #t)
		 ((not (eq? (visiting? info) #:defined)) #f) ;forward reference
		 (else (display (binding info) file) #t))))))

;;; write-component OBJECT PATCHER FILE ENV
;;;
(define write-component
  (procedure->memoizing-macro
    (lambda (exp env)
      `(or (write-component-procedure ,(cadr exp) ,@(cdddr exp))
	   (begin
	     (display #f ,(cadddr exp))
	     (add-patcher! ,(caddr exp) env))))))

;;;
;;; Main engine
;;;

(define binding-name car)
(define binding-object cdr)

(define (pass-1! alist env)
  ;; Determine object order and necessary bindings
  (for-each (lambda (binding)
	      (enumerate-component! (binding-object binding) env))
	    alist))

(define (make-local i)
  (string->symbol (string-append "%o" (number->string i))))

(define (name-bindings! alist env)
  ;; Name top-level bindings
  (for-each (lambda (b)
	      (let ((o (binding-object b)))
		(if (not (or (immediate? o)
			     (readable? o)
			     (excluded? o env)))
		    (set! (binding (object-info o env))
			  (binding-name b)))))
	    alist)
  ;; Name rest of bindings and create stand-in and definition lists
  (let loop ((ls (objects env))
	     (i 0)
	     (locs '())
	     (sins '())
	     (defs '()))
    (if (null? ls)
	(begin
	  (set! (locals env) locs)
	  (set! (stand-ins env) sins)
	  (set! (definitions env) defs))
	(let ((info (object-info (car ls) env)))
	  (cond ((not (binding? info))
		 (loop (cdr ls) i locs sins defs))
		((boolean? (binding info))
		 (set! (binding info) (make-local i))
		 (loop (cdr ls)
		       (+ 1 i)
		       (cons (car ls) locs)
		       sins
		       defs))
		((null? locs)
		 (loop (cdr ls)
		       i
		       locs
		       sins
		       (cons (car ls) defs)))
		(else
		 (let ((real-name (binding info)))
		   (set! (binding info) (make-local i))
		   (loop (cdr ls)
			 (+ 1 i)
			 (cons (car ls) locs)
			 (acons real-name (binding info) sins)
			 defs))))))))

(define (pass-2! env)
  (set! (pass-2? env) #t)
  (for-each (lambda (o)
	      (let ((info (object-info o env)))
		(set! (literal? (object-info o env)) (enumerate! o env))
		(set! (visiting? info) #:pass-2)))
	    (append (locals env) (definitions env))))

(define (write-define! name val literal? file)
  (display "(define " file)
  (display name file)
  (display #\space file)
  (if literal? (display #\' file))
  (write val file)
  (display ")\n" file))

(define (write-empty-defines! file env)
  (for-each (lambda (o)
	      (write-define! (binding o env) #f #f file))
	    (definitions env)))

(define (write-definition! prefix o file env)
  (display prefix file)
  (let ((info (object-info o env)))
    (display (binding info) file)
    (display #\space file)
    (if (literal? info)
	(display #\' file))
    (write-readably o file env)
    (set! (visiting? info) #:defined)
    (display #\) file)))

(define (write-let*-head! file env)
  (display "(let* (" file)
  (write-definition! "(" (car (locals env)) file env)
  (for-each (lambda (o)
	      (write-definition! "\n(" o file env))
	    (cdr (locals env)))
  (display ")\n" file))

(define (write-stand-in-patches! file env)
  (for-each (lambda (patch)
	      (display "  (set! " file)
	      (display (car patch) file)
	      (display #\space file)
	      (display (cdr patch) file)
	      (display ")\n" file))
	    (stand-ins env)))

(define (write-definitions! prefix file env)
  (for-each (lambda (o)
	      (write-definition! prefix o file env)
	      (newline file))
	    (definitions env)))

(define (write-patches! prefix file env)
  (for-each (lambda (patch)
	      (display prefix file)
	      (display (let name-objects ((patcher patch))
			 (cond ((object-info patcher env) => binding)
			       ((pair? patcher)
				(cons (name-objects (car patcher))
				      (name-objects (cdr patcher))))
			       (else patcher)))
		       file)
	      (newline file))
	    (patchers env)))

(define (write-immediates! alist file)
  (for-each (lambda (b)
	      (if (immediate? (binding-object b))
		  (write-define! (binding-name b)
				 (binding-object b)
				 #t
				 file)))
	    alist))

(define (write-readables! alist file)
  (for-each (lambda (b)
	      (if (readable? (binding-object b))
		  (write-define! (binding-name b)
				 (readable-expression (binding-object b))
				 #f
				 file)))
	    alist))

(define-method save-objects ((alist <pair>) (file <string>) . rest)
  (let ((port (open-output-file file)))
    (apply save-objects alist port rest)
    (close-port port)))

(define-method save-objects ((alist <pair>) (file <output-port>) . rest)
  (let ((excluded (if (>= (length rest) 1) (car rest) '()))
	(uses     (if (>= (length rest) 2) (cadr rest) '())))
    (let ((env (make <environment> #:excluded excluded)))
      (pass-1! alist env)
      (name-bindings! alist env)
      (pass-2! env)
      (if (not (null? uses))
	  (begin
	    (write `(use-modules ,@uses) file)
	    (newline file)))
      (write-immediates! alist file)
      (if (null? (locals env))
	  (begin
	    (write-definitions! "(define " file env)
	    (write-patches! "" file env))
	  (begin
	    (write-empty-defines! file env)
	    (write-let*-head! file env)
	    (write-stand-in-patches! file env)
	    (write-definitions! "  (set! " file env)
	    (write-patches! "  " file env)
	    (display "  )\n" file)))
      (write-readables! alist file))))

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
