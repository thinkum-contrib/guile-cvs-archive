;;; installed-scm-file

;;;; 	Copyright (C) 1998 Free Software Foundation, Inc.
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


;;;; This software is a derivative work of other copyrighted softwares; the
;;;; copyright notices of these softwares are placed in the file COPYRIGHTS
;;;;
;;;; This file is based upon stklos.stk from the STk distribution by
;;;; Erick Gallesio <eg@unice.fr>.
;;;;

(define-module (oop goops)
  :use-module (oop goops goopscore)
  :no-backtrace
  )

(export			  ; Define the exported symbols of this file
    is-a?
    ensure-metaclass ensure-metaclass-with-supers
    define-class   class make-class
    define-generic make-generic ensure-generic
    define-accessor make-accessor ensure-accessor
    define-method make-method method add-method!
    object-eqv? object-equal?
    write-object display-object Tk-write-object
    slot-unbound slot-missing 
    slot-definition-name  slot-definition-options slot-definition-allocation
    slot-definition-getter slot-definition-setter slot-definition-accessor
    slot-definition-init-value
    slot-definition-init-thunk slot-definition-init-keyword 
    slot-init-function class-slot-definition
    method-source
    compute-cpl compute-std-cpl compute-get-n-set
    allocate-instance initialize make-instance make
    no-next-method  no-applicable-method no-method
    change-class 
    shallow-clone deep-clone
    class-redefinition
    apply-generic apply-method apply-methods compute-applicable-methods 
    method-more-specific? sort-applicable-methods
    class-subclasses class-methods
    slot-value
    goops-error
)

;;; *fixme* Should go into goops.c

(export
    stklos-version instance?  slot-ref-using-class
    slot-set-using-class! slot-bound-using-class?
    slot-exists-using-class? slot-ref slot-set! slot-bound? class-of
    class-name class-direct-supers class-direct-subclasses
    class-direct-methods class-direct-slots class-precedence-list
    class-slots class-environment generic-function-name
    generic-function-methods method-generic-function method-specializers
    method-procedure slot-exists? make find-method get-keyword)

;; We don't want the following procedure to turn up in backtraces:
(for-each (lambda (proc)
	    (set-procedure-property! proc 'system-procedure #t))
	  (list apply-next-method
		apply-generic-0
		apply-generic-1
		apply-generic-2
		apply-generic-3))

;;;
;;; {Utilities}
;;;

(define (goops-error format-string . args)
  (save-stack)
  (scm-error 'goops-error #f format-string args '()))

(define (mapappend func . args)
  (if (memv '()  args)
      '()
      (append (apply func (map car args))
	      (apply mapappend func (map cdr args)))))

(define (find-duplicate l)	; find a duplicate in a list; #f otherwise
  (cond 
    ((null? l)			#f)
    ((memv (car l) (cdr l))	(car l))
    (else 			(find-duplicate (cdr l)))))

(define (top-level-env)
  (if *top-level-lookup-closure*
      (list *top-level-lookup-closure*)
      '()))

(define (top-level-env? env)
  (or (null? env)
      (procedure? (car env))))

(define (map* fn . l) 		; A map which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (cons (apply fn      (map car l))
			  (apply map* fn (map cdr l))))
   (else            (apply fn l))))

(define (for-each* fn . l) 	; A for-each which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (apply fn (map car l)) (apply for-each* fn (map cdr l)))
   (else            (apply fn l))))


;;
;; is-a?
;;
(define (is-a? obj class)
  (and (memq class (class-precedence-list (class-of obj))) #t))


;;;
;;; {Meta classes}
;;;

(define ensure-metaclass-with-supers
  (let ((table-of-metas '()))
    (lambda (meta-supers)
      (let ((entry (assoc meta-supers table-of-metas)))
	(if entry
	    ;; Found a previously created metaclass
	    (cdr entry)
	    ;; Create a new meta-class which inherit from "meta-supers"
	    (let ((new (make <class> #:dsupers meta-supers
			             #:slots   '()
				     #:name   (gensym "metaclass"))))
	      (set! table-of-metas (cons (cons meta-supers new) table-of-metas))
	      new))))))

(define (ensure-metaclass supers env)
  (if (null? supers)
      <class>
      (let* ((all-metas (map (lambda (x) (class-of x)) supers))
	     (all-cpls  (apply append
			       (map (lambda (m)
				      (cdr (class-precedence-list m))) 
				    all-metas)))
	     (needed-metas '()))
	;; Find the most specific metaclasses.  The new metaclass will be
	;; a subclass of these.
	(for-each
	 (lambda (meta)
	   (if (and (not (member meta all-cpls))
		      (not (member meta needed-metas)))
	     (set! needed-metas (append needed-metas (list meta)))))
	 all-metas)
	;; Now return a subclass of the metaclasses we found.
	(if (null? (cdr needed-metas))
	    (car needed-metas)  ; If there's only one, just use it.
	    (ensure-metaclass-with-supers needed-metas)))))

;;;
;;; {Classes}
;;;

;;; (define-class NAME (SUPER ...) SLOT-DEFINITION ... OPTION ...)
;;;
;;;   SLOT-DEFINITION ::= SLOT-NAME | (SLOT-NAME OPTION ...)
;;;   OPTION ::= KEYWORD VALUE
;;;
(define (define-class-pre-definition keyword exp env)
  (case keyword
    ((#:getter #:setter)
     (if (defined? exp env)
	 `(define ,exp (ensure-generic ,exp ',exp))
	 `(define ,exp (make-generic ',exp))))
    ((#:accessor)
     (if (defined? exp env)
	 `(define ,exp (ensure-accessor ,exp ',exp))
	 `(define ,exp (make-accessor ',exp))))
    (else #f)))

;;; This code should be implemented in C.
;;;
(define define-class
  (letrec (;; Some slot options require extra definitions to be made.
	   ;; In particular, we want to make sure that the generic
	   ;; function objects which represent accessors exist
	   ;; before `make-class' tries to add methods to them.
	   ;;
	   ;; Postpone error handling to class macro.
	   ;;
	   (pre-definitions
	    (lambda (slots env)
	      (do ((slots slots (cdr slots))
		   (definitions '()
		     (if (pair? (car slots))
			 (do ((options (cdar slots) (cddr options))
			      (definitions definitions
				(cond ((not (symbol? (cadr options)))
				       definitions)
				      ((define-class-pre-definition
					 (car options)
					 (cadr options)
					 env)
				       => (lambda (definition)
					    (cons definition definitions)))
				      (else definitions))))
			     ((not (and (pair? options)
					(pair? (cdr options))))
			      definitions))
			 definitions)))
		  ((or (not (pair? slots))
		       (keyword? (car slots)))
		   (reverse definitions)))))
	   
	   ;; Syntax
	   (name cadr)
	   (slots cdddr))
    
    (procedure->macro
      (lambda (exp env)
	(cond ((not (top-level-env? env))
	       (goops-error "define-class: Only allowed at top level"))
	      ((not (and (list? exp) (>= (length exp) 3)))
	       (goops-error "missing or extra expression"))
	      (else
	       (let ((name (name exp)))
		 `(begin
		    ;; define accessors
		    ,@(pre-definitions (slots exp) env)
		 
		    ,(if (defined? name env)
		      
			 ;; redefine an old class
			 `(define ,name
			    (let ((old ,name)
				  (new (class ,@(cddr exp) #:name ',name)))
			      (if (is-a? old <class>)
				  (class-redefinition old new)
				  new)))
		      
			 ;; define a new class
			 `(define ,name
			    (class ,@(cddr exp) #:name ',name)))))))))))

;;; (class (SUPER ...) SLOT-DEFINITION ... OPTION ...)
;;;
;;;   SLOT-DEFINITION ::= SLOT-NAME | (SLOT-NAME OPTION ...)
;;;   OPTION ::= KEYWORD VALUE
;;;
(define class
  (letrec ((slot-option-keyword car)
	   (slot-option-value cadr)
	   (process-slot-options
	    (lambda (options)
	      (let loop ((options options)
			 (res '()))
		(if (null? options)
		    (reverse res)
		    (case (slot-option-keyword options)
		      ((#:init-form)
		       (loop (cddr options)
			     (append (list `(lambda ()
					      ,(slot-option-value options))
					   #:init-thunk
					   (list 'quote
						 (slot-option-value options))
					   #:init-form)
				     res)))
		      (else
		       (loop (cddr options)
			     (cons (cadr options)
				   (cons (car options)
					 res))))))))))
    
    (procedure->memoizing-macro
      (let ((supers cadr)
	    (slots cddr)
	    (options cdddr))
	(lambda (exp env)
	  (cond ((not (and (list? exp) (>= (length exp) 2)))
		 (goops-error "missing or extra expression"))
		((not (list? (supers exp)))
		 (goops-error "malformed superclass list: %S" (supers exp)))
		(else
		 (let ((slot-defs (cons #f '())))
		   (do ((slots (slots exp) (cdr slots))
			(defs slot-defs (cdr defs)))
		       ((or (null? slots)
			    (keyword? (car slots)))
			`(make-class
			  ;; evaluate super class variables
			  (list ,@(supers exp))
			  ;; evaluate slot definitions, except the slot name!
			  (list ,@(cdr slot-defs))
			  ;; evaluate class options
			  ,@slots
			  ;; place option last in case someone wants to
			  ;; pass a different value
			  #:environment ',env))
		     (set-cdr!
		      defs
		      (list (if (pair? (car slots))
				`(list ',(slot-definition-name (car slots))
				       ,@(process-slot-options
					  (slot-definition-options
					   (car slots))))
				`(list ',(car slots))))))))))))))

(define (make-class supers slots . options)
  (let ((env (or (get-keyword #:environment options #f)
		 (top-level-env))))
    (let* ((name (get-keyword #:name options (make-unbound)))
	   (supers (if (null? supers) 
		       (list <object>)
		       supers))
	   (metaclass (or (get-keyword #:metaclass options #f)
			  (ensure-metaclass supers env))))

      ;; Verify that all direct slots are different and that we don't inherit
      ;; several time from the same class
      (let ((tmp1 (find-duplicate supers))
	    (tmp2 (find-duplicate (map slot-definition-name slots))))
	(if tmp1
	    (goops-error "make-class: super class %S is duplicate in class %S"
			 tmp1 name))
	(if tmp2
	    (goops-error "make-class: slot %S is duplicate in class %S"
			 tmp2 name)))

      ;; Everything seems correct, build the class
      (apply make metaclass
	     #:dsupers supers
	     #:slots slots 
	     #:name name
	     #:environment env
	     options))))

;;;
;;; {Generic functions and accessors}
;;;

(define define-generic
  (procedure->macro
    (lambda (exp env)
      (let ((name (cadr exp)))
	(cond ((not (symbol? name))
	       (goops-error "bad generic function name: %S" name))
	      ((defined? name env)
	       `(define ,name
		  (if (is-a? ,name <generic>)
		      (make <generic> #:name ',name)
		      (ensure-generic ,name ',name))))
	      (else
	       `(define ,name (make <generic> #:name ',name))))))))

(define (make-generic . name)
  (let ((name (and (pair? name) (car name))))
    (make <generic> #:name name)))

(define (ensure-generic old-definition . name)
  (let ((name (and (pair? name) (car name))))
    (cond ((is-a? old-definition <generic>) old-definition)
	  ((procedure-with-setter? old-definition)
	   (make <generic-with-setter>
		 #:name name
		 #:default (procedure old-definition)
		 #:setter (setter old-definition)))
	  ((procedure? old-definition)
	   (make <generic> #:name name #:default old-definition))
	  (else (make <generic> #:name name)))))

(define define-accessor
  (procedure->macro
    (lambda (exp env)
      (let ((name (cadr exp)))
	(cond ((not (symbol? name))
	       (goops-error "bad accessor name: %S" name))
	      ((defined? name env)
	       `(define ,name
		  (if (and (is-a? ,name <generic-with-setter>)
			   (is-a? (setter ,name) <generic>))
		      (make-accessor ',name)
		      (ensure-accessor ,name ',name))))
	      (else
	       `(define ,name (make-accessor ',name))))))))

(define (make-setter-name name)
  (string->symbol (string-append "setter:" (symbol->string name))))

(define (make-accessor . name)
  (let ((name (and (pair? name) (car name))))
    (make <generic-with-setter>
	  #:name name
	  #:setter (make <generic> #:name (make-setter-name name)))))

(define (ensure-accessor proc . name)
  (let ((name (and (pair? name) (car name))))
    (cond ((is-a? proc <generic-with-setter>)
	   (if (is-a? (setter proc) <generic>)
	       proc
	       (upgrade-generic-with-setter proc (setter proc))))
	  ((is-a? proc <generic>)
	   (upgrade-generic-with-setter proc (make-generic name)))
	  ((procedure-with-setter? proc)
	   (make <generic-with-setter>
		 #:name name
		 #:default (procedure proc)
		 #:setter (ensure-generic (setter proc) name)))
	  ((procedure? proc)
	   (ensure-accessor (ensure-generic proc name) name))
	  (else
	   (make-accessor name)))))

(define (upgrade-generic-with-setter generic setter)
  (let ((methods (generic-function-methods generic))
	(gws (make <generic-with-setter>
		   #:name (generic-function-name generic)
		   #:setter setter)))
    ;; Steal old methods
    (for-each (lambda (method)
		(slot-set! method 'generic-function gws))
	      methods)
    (slot-set! gws 'methods methods)
    gws))

;;;
;;; {Methods}
;;;

(define define-method
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((name (cadr exp)))
	(cond ((not (symbol? name))
	       (goops-error "bad method name: %S" name))
	      ((defined? name env)
	       `(begin
		  (if (not (is-a? ,name <generic>))
		      (define-generic ,name))
		  (add-method! ,name (method ,@(cddr exp)))))
	      (else
	       `(begin
		  (define-generic ,name)
		  (add-method! ,name (method ,@(cddr exp))))))))))

(define (make-method specializers procedure)
  (make <method>
	#:specializers specializers
	#:procedure procedure))

(define method
  (letrec ((specializers
	    (lambda (ls)
	      (cond ((null? ls) (list ls))
		    ((pair? ls) (cons (if (pair? (car ls))
					  (cadar ls)
					  '<top>)
				      (specializers (cdr ls))))
		    (else '(<top>)))))
	   (formals
	    (lambda (ls)
	      (if (pair? ls)
		  (cons (if (pair? (car ls)) (caar ls) (car ls))
			(formals (cdr ls)))
		  ls))))
    (procedure->memoizing-macro
      (lambda (exp env)
	(let ((args (cadr exp))
	      (body (cddr exp)))
	  `(make <method>
		 #:specializers (list* ,@(specializers args))
		 #:procedure (lambda ,(cons 'next-method (formals args))
			       ,@(if (null? body)
				     (list *unspecified*)
				     body))))))))

;;; {add-method!}
;;;

(define (add-method-in-classes! m)
  ;; Add method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
	       (let ((dm (class-direct-methods x)))
		 (if (not (memv m dm))
		     (slot-set! x 'direct-methods (cons m dm)))))
	     (method-specializers m)))

(define (remove-method-in-classes! m)
  ;; Remove method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
	       (slot-set! x
			  'direct-methods
			  (delv! m (class-direct-methods x))))
	     (method-specializers m)))

(define (compute-new-list-of-methods gf new)
  (let ((new-spec (method-specializers new))
	(methods  (generic-function-methods gf)))
    (let loop ((l methods))
      (if (null? l)
	  (cons new methods)
	  (if (equal? (method-specializers (car l)) new-spec)
	      (begin 
		;; This spec. list already exists. Remove old method from dependents
		(remove-method-in-classes! (car l))
		(set-car! l new) 
		methods)
	      (loop (cdr l)))))))

(define (internal-add-method! next-method gf m)
  (slot-set! m  'generic-function gf)
  (slot-set! gf 'methods (compute-new-list-of-methods gf m))
  (add-method-in-classes! m)
  *unspecified*)

(define-generic add-method!)

(internal-add-method! #f add-method!
		      (make <method>
			#:specializers (list <generic> <method>)
			#:procedure internal-add-method!))

;;;
;;; {Access to meta objects}
;;;

;;;
;;; Methods
;;;
(define-method method-source ((m <method>))
  (let* ((spec (map* class-name (slot-ref m 'specializers)))
	 (proc (procedure-source (slot-ref m 'procedure)))
	 (args (cdadr proc))
	 (body (cddr proc)))
    (cons 'method
	  (cons (map* list args spec)
		body))))

;;;
;;; Slots
;;;
(define slot-definition-name car)

(define slot-definition-options cdr)

(define (slot-definition-allocation s)
  (get-keyword #:allocation (cdr s) #:instance))

(define (slot-definition-getter s)
  (get-keyword #:getter (cdr s) #f))

(define (slot-definition-setter s)
  (get-keyword #:setter (cdr s) #f))

(define (slot-definition-accessor s)
  (get-keyword #:accessor (cdr s) #f))

(define (slot-definition-init-value s)
  (get-keyword #:init-value (cdr s) (make-unbound)))

(define (slot-definition-init-thunk s)
  (get-keyword #:init-thunk (cdr s) #f))

(define (slot-definition-init-keyword s)
  (get-keyword #:init-keyword (cdr s) #f))

(define (class-slot-definition class slot-name)
  (assq slot-name (class-slots class)))

(define (slot-init-function class slot-name)
  (cadr (assq slot-name (slot-ref class 'getters-n-setters))))


;;;
;;; {Standard methods used by the C runtime}
;;;

;;; Methods to compare objects
;;;

(define-method object-eqv? (x y)    #f)
(define-method object-equal? (x y)  (eqv? x y))

;;;
;;; methods to display/write an object
;;;

;     Code for writing objects must test that the slots they use are
;     bound. Otherwise a slot-unbound method will be called and will 
;     conduct to an infinite loop.

;; Write
(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define-method write-object (o file)
  (display "#<instance " file)
  (display-address o file)
  (display #\> file))

(define-method write-object ((o <object>) file)
  (let ((class (class-of o)))
    (if (slot-bound? class 'name)
	(begin
	  (display "#<" file)
	  (display (class-name class) file)
	  (display #\space file)
	  (display-address o file)
	  (display #\> file))
	(next-method))))

(define-method write-object ((class <class>) file)
  (let ((meta (class-of class)))
    (if (and (slot-bound? class 'name)
	     (slot-bound? meta 'name))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (display #\space file)
	  (display (class-name class) file)
	  (display #\space file)
	  (display-address class file)
	  (display #\> file))
	(next-method))))

(define-method write-object ((gf <generic>) file)
  (let ((meta (class-of gf)))
    (if (and (slot-bound? meta 'name)
	     (slot-bound? gf 'methods))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (let ((name (generic-function-name gf)))
	    (if name
		(begin
		  (display #\space file)
		  (display name file))))
	  (display " (" file)
	  (display (length (generic-function-methods gf)) file)
	  (display ")>" file))
	(next-method))))

(define-method write-object ((o <method>) file)
  (let ((meta (class-of o)))
    (if (and (slot-bound? meta 'name)
	     (slot-bound? o 'specializers))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (display #\space file)
	  (display (map* (lambda (spec)
			   (if (slot-bound? spec 'name)
			       (slot-ref spec 'name)
			       spec))
			 (method-specializers o))
		   file)
	  (display #\space file)
	  (display-address o file)
	  (display #\> file))
	(next-method))))

;; Teach all created classes how to print themselves
(for-each (lambda (class)
	    (slot-set! class 'print write-object))
	  (list <top> <object> <class>
		<procedure-class> <entity-class>
		<operator-class> <operator-with-setter-class>
		<entity> <entity-with-setter>
		<method> <simple-method> <accessor-method>
		<generic> <generic-with-setter>
		<boolean> <char>
		<list> <pair> <null>
		<string> <symbol>
		<vector>
		<number> <complex> <real> <integer>
		<keyword>
		<unknown>
		<procedure>))

;; Display (do the same thing as write by default)
(define-method display-object (o file) 
  (write-object o file))

;; Tk-write-object is called when a STklos object is passed to a Tk-command.
;; By default, we do the same job as write; but if an object is a <Tk-widget>
;; we will pass it its Eid. The method for <Tk-widget> is defined elsewhere.
(define-method Tk-write-object (o file)
  (write-object o file))

;;;
;;; slot access
;;;

(define-method slot-unbound ((c <class>) (o <object>) s)
  (goops-error "Slot `%S' is unbound in object %S" s o))

(define-method slot-missing ((c <class>) (o <object>) s)
  (goops-error "No slot with name `%S' in object %S" s o))
  

(define-method slot-missing ((c <class>) (o <object>) s value)
  (slot-missing c o s))

;;; Methods for the possible error we can encounter when calling a gf

(define-method no-next-method ((gf <generic>) args)
  (goops-error "No next method when calling %S\nwith %S as argument" gf args))

(define-method no-applicable-method ((gf <generic>) args)
  (goops-error "No applicable method for %S\nin call %S"
	       gf (cons (generic-function-name gf) args)))

(define-method no-method ((gf <generic>) args)
  (goops-error "No method defined for %S"  gf))

;;;
;;; {Cloning functions (from rdeline@CS.CMU.EDU)}
;;;

(define-method shallow-clone ((self <object>))
  (let ((clone (%allocate-instance (class-of self)))
	(slots (map slot-definition-name
		    (class-slots (class-of self)))))
    (for-each (lambda (slot)
		(if (slot-bound? self slot)
		    (slot-set! clone slot (slot-ref self slot))))
	      slots)
    clone))

(define-method deep-clone  ((self <object>))
  (let ((clone (%allocate-instance (class-of self)))
	(slots (map slot-definition-name
		    (class-slots (class-of self)))))
    (for-each (lambda (slot)
		(if (slot-bound? self slot)
		    (slot-set! clone slot
			       (let ((value (slot-ref self slot)))
				 (if (instance? value)
				     (deep-clone value)
				     value)))))
	      slots)
    clone))

;;;
;;; {Class redefinition utilities}
;;;

;;; (class-redefinition OLD NEW)
;;;

;;; Has correct the following conditions:

;;; Methods
;;; 
;;; 1. New accessor specializers refer to new header
;;; 
;;; Classes
;;; 
;;; 1. New class cpl refers to the new class header
;;; 2. Old class header exists on old super classes direct-subclass lists
;;; 3. New class header exists on new super classes direct-subclass lists

(define-method class-redefinition ((old <class>) (new <class>))
  ;; Work on direct methods:
  ;;		1. Remove accessor methods from the old class 
  ;;		2. Patch the occurences of new in the specializers by old
  ;;		3. Displace the methods from old to new
  (remove-class-accessors! old)					;; -1-
  (let ((methods (class-direct-methods new)))
    (for-each (lambda (m)
     	         (update-direct-method! m new old))	;; -2-
              methods)
    (slot-set! new
	       'direct-methods
	       (append methods (class-direct-methods old))))

  ;; Substitute old for new in new cpl
  (set-car! (slot-ref new 'cpl) old)
  
  ;; Remove the old class from the direct-subclasses list of its super classes
  (for-each (lambda (c) (slot-set! c 'direct-subclasses
				   (delv! old (class-direct-subclasses c))))
	    (class-direct-supers old))

  ;; Replace the new class with the old in the direct-subclasses of the supers
  (for-each (lambda (c)
	      (slot-set! c 'direct-subclasses
			 (cons old (delv! new (class-direct-subclasses c)))))
	    (class-direct-supers new))

  ;; Swap object headers
  (%modify-class old new)

  ;; Now old is NEW!

  ;; Redefine all the subclasses of old to take into account modification
  (for-each 
       (lambda (c)
	 (update-direct-subclass! c new old))
       (class-direct-subclasses new))

  ;; Invalidate class so that subsequent instances slot accesses invoke
  ;; change-object-class
  (slot-set! new 'redefined old)

  old)

;;;
;;; remove-class-accessors!
;;;

(define-method remove-class-accessors! ((c <class>))
  (for-each (lambda (m)
	      (if (is-a? m <accessor-method>)
		  (remove-method-in-classes! m)))
	    (class-direct-methods c)))

;;;
;;; update-direct-method!
;;;

(define-method update-direct-method! ((m  <method>)
				      (old <class>)
				      (new <class>))
  (let loop ((l (method-specializers m)))
    ;; Note: the <top> in dotted list is never used. 
    ;; So we can work if we had only proper lists.
    (if (pair? l)       	  
	(begin
	  (if (eqv? (car l) old)  
	      (set-car! l new))
	  (loop (cdr l))))))

;;;
;;; update-direct-subclass!
;;;

(define-method update-direct-subclass! ((c <class>)
					(old <class>)
					(new <class>))
  (class-redefinition c
		      (make-class (class-direct-supers c)
				  (class-direct-slots c)
				  #:name (class-name c)
				  #:environment (slot-ref c 'environment)
				  #:metaclass (class-of c))))

;;;
;;; {Utilities for INITIALIZE methods}
;;;

;;; compute-slot-accessors
;;;
(define (compute-slot-accessors class slots env)
  (for-each
      (lambda (s)
	(let ((name            (slot-definition-name     s))
	      (getter-function (slot-definition-getter   s))
	      (setter-function (slot-definition-setter   s))
	      (accessor        (slot-definition-accessor s)))
	  (if getter-function
	      (add-method! getter-function
			   (make <accessor-method>
				 #:specializers (list class)
				 #:procedure (lambda (nm o)
					       (slot-ref o name)))))
	  (if setter-function
	      (add-method! setter-function
			   (make <accessor-method>
				 #:specializers (list class <top>)
				 #:procedure (lambda (nm o v)
					       (slot-set! o name v)))))
	  (if accessor
	      (begin
		(add-method! accessor
			     (make <accessor-method>
				   #:specializers (list class)
				   #:procedure (lambda (nm o)
						 (slot-ref o name))))
		(add-method! (setter accessor)
			     (make <accessor-method>
				   #:specializers (list class <top>)
				   #:procedure (lambda (nm o v)
						 (slot-set! o name v))))))))
      slots))

;;; compute-getters-n-setters
;;; 
(define (compute-getters-n-setters class slots env)

  (define (compute-slot-init-function s)
    (or (slot-definition-init-thunk s)
	(let ((init (slot-definition-init-value s)))
	  (and (not (unbound? init))
	       (lambda () init)))))

  (define (verify-accessors slot l)
    (if (pair? l)
	(let ((get (car l)) 
	      (set (cadr l)))
	  (if (not (and (closure? get)
			(= (car (procedure-property get 'arity)) 1)))
	      (goops-error "Bad getter closure for slot `%S' in %S: %S"
			   slot class get))
	  (if (not (and (closure? set)
			(= (car (procedure-property set 'arity)) 2)))
	    (goops-error "Bad setter closure for slot `%S' in %S: %S"
			 slot class set)))))

  (map (lambda (s)
	 (let* ((g-n-s (compute-get-n-set class s))
		(name  (slot-definition-name s)))
	   ; For each slot we have '(name init-function getter setter)
	   ; If slot, we have the simplest form '(name init-function . index)
	   (verify-accessors name g-n-s)
	   (cons name
		 (cons (compute-slot-init-function s)
		       g-n-s))))
       slots))

;;; compute-cpl
;;;
(define-method compute-cpl ((class <class>))
  (compute-std-cpl class))

(define (compute-std-cpl class)
  (let* ((h (make-vector 7 '()))
	 (tail (list <object> <top>))
	 (anchor (cons #f '()))
	 (ls anchor))
    ;; Do a depth first traversal of inheritance tree and convert it
    ;; to a list of descriptors of the form (<class> . #dependencies)
    ;; Each time a class is encountered, the dependency count is
    ;; incremented.  (0 actually means 1 dependency.)
    (let traverse ((class class))
      (if (not (memq class tail))
	  (begin
	    (let* ((handle (hashq-create-handle! h class -1))
		   (el (list handle)))
	      (set-cdr! handle (+ (cdr handle) 1))
	      (set-cdr! ls el)
	      (set! ls el))
	    (do ((classes (slot-ref class 'direct-supers)
			  (cdr classes)))
		((null? classes))
	      (traverse (car classes))))))
    ;; Convert the descriptor list to a list of classes while
    ;; decrementing the dependency counts.  Skip descriptors with
    ;; non-zero dependency count.
    (do ((dls (cdr anchor) (cdr dls))
	 (cpl (cdr anchor)))
	((null? dls)
	 ;; We are finished.  Patch the tail of the list.
	 (if (null? cpl)
	     ;; We had a 1-1 correspondence between dls and cpl
	     ;; ls countains the last pair of the original list
	     (set-cdr! ls tail)
	     ;; We skipped one or more descriptors.  Replace the
	     ;; pair after the computed classes with the tail.
	     (begin
	       (set-car! cpl (car tail))
	       (set-cdr! cpl (cdr tail))))
	 ;; The result
	 (cdr anchor))
      ;; Check dependency count
      (if (zero? (cdar dls))
	  (begin
	    ;; Replace the descriptor with the actual class
	    ;; and move to next pair
	    (set-car! cpl (caar dls))
	    (set! cpl (cdr cpl)))
	  ;; Decrement dependency count and skip class
	  (set-cdr! (car dls) (- (cdar dls) 1))))))


;;; compute-get-n-set
;;;
(define-method compute-get-n-set ((class <class>) s)
  (case (slot-definition-allocation s)
    ((#:instance) ;; Instance slot
     ;; get-n-set is just its offset
     (let ((already-allocated (slot-ref class 'nfields)))
       (slot-set! class 'nfields (+ already-allocated 1))
       already-allocated))

    ((#:class)  ;; Class slot
     ;; Class-slots accessors are implemented as 2 closures around 
     ;; a Scheme variable. As instance slots, class slots must be
     ;; unbound at init time.
     (let ((name (slot-definition-name s)))
       (if (memq name (map slot-definition-name (class-direct-slots class)))
	   ;; This slot is direct; create a new shared variable
	   (let ((shared-variable (make-unbound)))
	     (list (lambda (o)   shared-variable)
		   (lambda (o v) (set! shared-variable v))))
	   ;; Slot is inherited. Find its definition in superclass
	   (let loop ((l (cdr (class-precedence-list class))))
	     (let ((r (assoc name (slot-ref (car l) 'getters-n-setters))))
	       (if r
		   (cddr r)
		   (loop (cdr l))))))))

    ((#:each-subclass) ;; slot shared by instances of direct subclass.
     ;; (Thomas Buerger, April 1998)
     (let ((shared-variable (make-unbound)))
       (list (lambda (o)   shared-variable)
	     (lambda (o v) (set! shared-variable v)))))

    ((#:virtual) ;; No allocation
     ;; slot-ref and slot-set! function must be given by the user
     (let ((get (get-keyword #:slot-ref  (slot-definition-options s) #f))
	   (set (get-keyword #:slot-set! (slot-definition-options s) #f))
	   (env (class-environment class)))
       (if (not (and get set))
	   (goops-error "You must supply a :slot-ref and a :slot-set! in %S"
			s))
       (list get set)))
    (else    (next-method))))

(define-method compute-get-n-set ((o <object>) s)
  (goops-error "Allocation \"%S\" is unknown" (slot-definition-allocation s)))

;;;
;;; {Initialize}
;;;

(define-method initialize ((object <object>) initargs)
  (%initialize-object object initargs))

(define-method initialize ((class <class>) initargs)
  (next-method)
  (let ((dslots (get-keyword #:slots initargs '()))
	(supers (get-keyword #:dsupers	  initargs '()))
	(env    (get-keyword #:environment initargs (top-level-env))))

    (slot-set! class 'name	  	(get-keyword #:name initargs '???))
    (slot-set! class 'direct-supers 	supers)
    (slot-set! class 'direct-slots  	dslots)
    (slot-set! class 'direct-subclasses '())
    (slot-set! class 'direct-methods    '())
    (slot-set! class 'cpl		(compute-cpl class))
    (slot-set! class 'redefined		#f)
    (slot-set! class 'environment	env)
    (let ((slots (%compute-slots class)))
      (slot-set! class 'slots	  	  slots)
      (slot-set! class 'nfields	  	  0)
      (slot-set! class 'getters-n-setters (compute-getters-n-setters class 
								     slots 
								     env)))

    ;; Update the "direct-subclasses" of each inherited classes
    (for-each (lambda (x)
		(slot-set! x
			   'direct-subclasses 
			   (cons class (slot-ref x 'direct-subclasses))))
	      supers)

    ;; Build getters - setters - accessors
    (compute-slot-accessors class dslots env)

    ;; Support for the underlying structs:
    
    ;; Inherit class flags (invisible on scheme level) from supers
    (%inherit-magic! class supers)

    ;; Set the layout slot
    (%prep-layout! class)

    ;; Set the struct print closure
    (slot-set! class 'print write-object)))

(define object-procedure-tags
  '(utag_closure utag_subr_1 utag_subr_2 utag_subr3 utag_lsubr_2))

(define (initialize-object-procedure object initargs)
  (let ((proc (get-keyword #:procedure initargs #f)))
    (cond ((not proc))
	  ((pair? proc)
	   (apply set-object-procedure! object proc))
	  ((memq (tag proc) object-procedure-tags)
	   (set-object-procedure! object proc))
	  (else
	   (set-object-procedure! object
				  (lambda args (apply proc args)))))))

(define-method initialize ((class <operator-class>) initargs)
  (next-method)
  (initialize-object-procedure class initargs))

(define-method initialize ((owsc <operator-with-setter-class>) initargs)
  (next-method)
  (%set-object-setter! owsc (get-keyword #:setter initargs #f)))

(define-method initialize ((entity <entity>) initargs)
  (next-method)
  (initialize-object-procedure entity initargs))

(define-method initialize ((ews <entity-with-setter>) initargs)
  (next-method)
  (%set-object-setter! ews (get-keyword #:setter initargs #f)))

(define-method initialize ((generic <generic>) initargs)
  (let ((previous-definition (get-keyword #:default initargs #f))
	(name (get-keyword #:name initargs #f)))
    ;; Primitive apply-generic-<n> for direct instances of <generic>
    (next-method generic (append initargs
				 (list #:procedure
				       (list apply-generic-0
					     apply-generic-1
					     apply-generic-2
					     apply-generic-3))))
    (slot-set! generic 'methods (if (is-a? previous-definition <procedure>)
				    (list (make <method>
						#:specializers <top>
						#:procedure
						(lambda (nm . l)
						  (apply previous-definition 
							 l))))
				    '()))
    (if name
	(set-procedure-property! generic 'name name))
    ))

(define-method initialize ((method <method>) initargs)
  (next-method)
  (slot-set! method 'generic-function (get-keyword #:generic-function initargs #f))
  (slot-set! method 'specializers (get-keyword #:specializers initargs '()))
  (slot-set! method 'procedure (get-keyword #:procedure initargs (lambda l '()))))

;;;
;;; {Change-class}
;;;

(define (change-object-class old-instance old-class new-class)
  (let ((new-instance (allocate-instance new-class ())))
    ;; Initalize the slot of the new instance
    (for-each (lambda (slot)
		(if (and (slot-exists-using-class? old-class old-instance slot)
			 (eq? (slot-definition-allocation
			       (class-slot-definition old-class slot))
			      #:instance)
			 (slot-bound-using-class? old-class old-instance slot))
		    ;; Slot was present and allocated in old instance; copy it 
		    (slot-set-using-class!
		     new-class 
		     new-instance 
		     slot 
		     (slot-ref-using-class old-class old-instance slot))
		    ;; slot was absent; initialize it with its default value
		    (let ((init (slot-init-function new-class slot)))
		      (if init
			  (slot-set-using-class!
			       new-class 
			       new-instance 
			       slot
			       (apply init '()))))))
	      (map slot-definition-name (class-slots new-class)))
    ;; Exchange old an new instance in place to keep pointers valids
    (%modify-instance old-instance new-instance)
    old-instance))


;; *fixme* Doesn't class-of cause an implicit call to change-object-class?
(define-method change-class ((old-instance <object>) (new-class <class>))
  (change-object-class old-instance (class-of old-instance) new-class))

;;;
;;; {make}
;;;
;;; A new definition which overwrites the previous one which was built-in
;;;

(define-method allocate-instance ((class <class>) initargs)
  (%allocate-instance class))

(define-method make-instance ((class <class>) . initargs)
  (let ((instance (allocate-instance class initargs)))
    (initialize instance initargs)
    instance))

(define make make-instance)

;;;
;;; {apply-generic}
;;;
;;; Protocol for calling standard generic functions.  This protocol is
;;; not used for real <generic> functions (in this case we use a
;;; completely C hard-coded protocol).  Apply-generic is used by
;;; goops for calls to subclasses of <generic> and <generic-with-setter>.
;;; The code below is similar to the first MOP described in AMOP. In
;;; particular, it doesn't used the currified approach to gf
;;; call. There are 2 reasons for that:
;;;   - the protocol below is exposed to mimic completely the one written in C
;;;   - the currified protocol would be imho inefficient in C.
;;;

(define-method apply-generic ((gf <generic>) args)
  (if (null? (slot-ref gf 'methods))
      (no-method gf args))
  (let ((methods (compute-applicable-methods gf args)))
    (if methods
	(apply-methods gf (sort-applicable-methods gf methods args) args)
	(no-applicable-method gf args))))

(define-method compute-applicable-methods ((gf <generic>) args)
  (apply find-method gf args))

(define-method sort-applicable-methods ((gf <generic>) methods args)
  (let ((targs (map class-of args)))
    (sort methods (lambda (m1 m2) (method-more-specific? m1 m2 targs)))))

(define-method method-more-specific? ((m1 <method>) (m2 <method>) targs)
  (%method-more-specific? m1 m2 targs))

(define-method apply-method ((gf <generic>) methods build-next args)
  (apply (method-procedure (car methods))
	 (build-next (cdr methods) args)
	 args))

(define-method apply-methods ((gf <generic>) (l <list>) args)
  (letrec ((next (lambda (procs args)
		   (lambda new-args
		     (let ((a (if (null? new-args) args new-args)))
		       (if (null? procs)
			   (no-next-method gf a)
			   (apply-method gf procs next a)))))))
    (apply-method gf l next args)))

;;;
;;; {<composite-metaclass> and <active-metaclass>}
;;;

;(autoload "active-slot"    <active-metaclass>)
;(autoload "composite-slot" <composite-metaclass>)
;(export <composite-metaclass> <active-metaclass>)

;;;
;;; {Tools}
;;;

;; list2set
;;
;; duplicate the standard list->set function but using eq instead of
;; eqv which really sucks a lot, uselessly here
;;
(define (list2set l)	       
  (let loop ((l l)
	     (res '()))
    (cond		       
     ((null? l) res)
     ((memq (car l) res) (loop (cdr l) res))
     (else (loop (cdr l) (cons (car l) res))))))

(define (class-subclasses c)
  (letrec ((allsubs (lambda (c)
		      (cons c (mapappend allsubs
					 (class-direct-subclasses c))))))
    (list2set (cdr (allsubs c)))))

(define (class-methods c)
  (list2set (mapappend class-direct-methods
		       (cons c (class-subclasses c)))))
