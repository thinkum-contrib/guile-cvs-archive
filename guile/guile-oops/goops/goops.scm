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
  :use-module (oop goopscore)
  :use-module (oop compat))

(export			  ; Define the exported symbols of this file
    is-a?
    ensure-metaclass ensure-metaclass-with-supers
    define-class   class make-class ensure-class
    define-generic make-generic
    define-method method add-method!
    object-eqv? object-equal?
    write-object display-object Tk-write-object
    slot-unbound slot-missing 
    slot-definition-name  slot-definition-options slot-definition-allocation
    slot-definition-getter slot-definition-setter slot-definition-accessor
    slot-definition-init-form slot-definition-init-keyword 
    slot-init-function class-slot-definition
    method-body
    compute-get-n-set
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
    method-procedure slot-exists? make find-method)
 
;=============================================================================
;
;			      U t i l i t i e s
;
;=============================================================================

(define (goops-error format-string . args)
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

;--------------------------------------------------
;*fixme* Should we be able to define things in environment frames?
(define (set-symbol! symbol value env)
  (let ((top-env (last-pair env)))
    (local-eval `(define ,symbol ,value) top-env)))

;(define (lookup-symbol symbol env)
;  (let ((module (%get-module env)))
;    (eval symbol (module-environment module))))
(define lookup-symbol local-eval)

(define make-closure local-eval)

(define (top-level-env)
  (if *top-level-lookup-closure*
      (list *top-level-lookup-closure*)
      '()))

(define (top-level-env? env)
  (or (null? env)
      (procedure? (car env))))

;=============================================================================

;;
;; is-a?
;;
(define (is-a? obj class)
  (and (memv class (class-precedence-list (class-of obj))) #t))


;=============================================================================
;
; 			M e t a c l a s s e s   s t u f f
;
;=============================================================================

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
	   (when (and (not (member meta all-cpls))
		      (not (member meta needed-metas)))
	     (set! needed-metas (append needed-metas (list meta)))))
	 all-metas)
	;; Now return a subclass of the metaclasses we found.
	(if (null? (cdr needed-metas))
	    (car needed-metas)  ; If there's only one, just use it.
	    (ensure-metaclass-with-supers needed-metas)))))

;=============================================================================
;
; 			D e f i n e - c l a s s
;
;=============================================================================

;==== Define-class
(define define-class
  (procedure->memoizing-macro
    (lambda (exp env)
      (if (not (top-level-env? env))
	  (goops-error "define-class: Only allowed at top level")
	  (let ((name (cadr exp))
		(supers (caddr exp))
		(slots (cadddr exp))
		(options (cddddr exp)))
	    `(define ,name
	       (let ((old (and (defined? ',name) ,name))
		     (new (make-class (list ,@supers)
				      ',slots
				      #:name ',name
				      #:environment ',env
				      ,@options)))
		 (if (is-a? old <class>)
		     (class-redefinition old new)
		     new))))))))

;==== Class
(define class
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((supers (cadr exp))
	    (slots (caddr exp))
	    (options (cdddr exp)))
	    `(make-class (list ,@supers)
			 ',slots
			 #:environment ',env
			 ,@options)))))

;==== Make-class
(define (make-class supers slots . options)
  (let ((env (or (get-keyword #:environment options #f)
		 (top-level-env))))
    (let* ((name (get-keyword #:name options #f))
	   (supers (if (null? supers) 
		       (list <object>)
		       supers))
	   (metaclass (or (get-keyword #:metaclass options #f)
			  (ensure-metaclass supers env))))

      ;; Verify that all direct slots are different and that we don't inherit
      ;; several time from the same class
      (let ((tmp1 (find-duplicate supers))
	    (tmp2 (find-duplicate (map (lambda (s)
					 (if (pair? s)
					     (slot-definition-name s)
					     s))
				       slots))))
	(when tmp1
	      (goops-error "make-class: super class %S is duplicate in class %S"
			   tmp1 name))
	(when tmp2
	      (goops-error "make-class: slot %S is duplicate in class %S"
			   tmp2 name)))

      ;; Everything seems correct, build the class
      (apply make metaclass #:dsupers supers #:slots slots 
	     #:name name #:environment env options))))

;=============================================================================
;
; 			D e f i n e - g e n e r i c
;
;=============================================================================

; ==== Define-generic
(define define-generic
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((name (cadr exp)))
	(if (defined? name env)
	    `(let ((proc ,name))
	       (set! ,name (make-generic))
	       ;; set! doesn't automagically set the name property
	       (set-procedure-property! ,name 'name ',name)
	       (if (is-a? proc <procedure>)
		   (add-method! ,name
				(make <method>
				      #:specializers <top>
				      #:procedure (lambda (nm . rest)
						    (apply proc rest))))))
	    `(define ,name (make-generic)))))))

;==== Make-generic
(define (make-generic)
  (make <generic>))


;=============================================================================
;
; 			D e f i n e - m e t h o d
;
;=============================================================================

(define define-method
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((name (cadr exp)))
	(if (defined? name env)
	    `(begin
	       (if (not (is-a? ,name <generic>))
		   (define-generic ,name))
	       (add-method! ,name (method ,@(cddr exp))))
	`(begin
	   (define-generic ,name)
	   (add-method! ,name (method ,@(cddr exp)))))))))

;==== Make-method

(define (make-method specializers procedure)
  (make <method>
	#:specializers specializers
	#:procedure procedure))

;====  Method

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
			       ,@body)))))))

;==== Add-method!

(define (add-method-in-classes! m)
  ;; Add method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
	       (let ((dm (class-direct-methods x)))
		 (unless (memv m dm)
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
    (let Loop ((l methods))
      (if (null? l)
	  (cons new methods)
	  (if (equal? (method-specializers (car l)) new-spec)
	      (begin 
		;; This spec. list already exists. Remove old method from dependents
		(remove-method-in-classes! (car l))
		(set-car! l new) 
		methods)
	      (Loop (cdr l)))))))

;;
;; Add-method!
;;
(define (add-method! gf m)
  (slot-set! m  'generic-function gf)
  (slot-set! gf 'methods (compute-new-list-of-methods gf m))
  (add-method-in-classes! m)
  *unspecified*)

;=============================================================================
;
; 			      Access to Meta objects
;
; A lot of them are in C
;=============================================================================

;;;
;;; Methods
;;;
(define-method method-body ((m <method>))
  (let* ((spec (map class-name (slot-ref m 'specializers)))
	 (proc (procedure-body (slot-ref m 'procedure)))
	 (args (cdadr proc))
	 (body (cddr proc)))
    (cons 'method
	  (cons (map list args spec)
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

(define (slot-definition-init-form s)
  (let* ((none (list '**none**))
	 (v1   (get-keyword #:init-form (cdr s) none))
	 (v2   (get-keyword #:initform  (cdr s) none))) ; Backward compatibility
    (if (eq? v1 none)
	(if (eq? v2 none)
	    (make-unbound)
	    v2)
	v1)))

(define (slot-definition-init-keyword s)
  (get-keyword #:init-keyword (cdr s) #f))

(define (slot-init-function class slot-name)
  (cadr (assoc slot-name (slot-ref class 'getters-n-setters))))

(define (class-slot-definition class slot-name)
  (assoc slot-name (class-slots class)))


;=============================================================================
;
; 			    Standard methods   
; 			used by the C runtime
;
;=============================================================================

;==== Methods to compare objects

(define-method object-eqv? (x y)    #f)
(define-method object-equal? (x y)  (eqv? x y))

;==== Methods to display/write an object

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
    (if (and (slot-bound? gf 'name)
	     (slot-bound? meta 'name)
	     (slot-bound? gf 'methods))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (display #\space file)
	  (display (generic-function-name gf) file)
	  (display " (" file)
	  (display (length (generic-function-methods gf)) file)
	  (display ")>" file))
	(next-method))))

;; Teach all created classes how to print themselves
(for-each (lambda (class)
	    (slot-set! class 'print write-object))
	  (list <top> <object> <class>
		<procedure-class> <entity-class>
		<method> <simple-method> <accessor-method>
		<generic>
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

;==== Slot access

(define-method slot-unbound ((c <class>) (o <object>) s)
  (goops-error "Slot `%S' is unbound in object %S" s o))

(define-method slot-missing ((c <class>) (o <object>) s)
  (goops-error "No slot with name `%S' in object %S" s o))
  

(define-method slot-missing ((c <class>) (o <object>) s value)
  (slot-missing c o s))

; ==== Methods for the possible error we can encounter when calling a gf

(define-method no-next-method ((gf <generic>) args)
  (goops-error "No next method when calling %S\nwith %S as argument" gf args))

(define-method no-applicable-method ((gf <generic>) args)
  (goops-error "No applicable method for %S\nin call %S"
	 gf (cons (generic-function-name gf) args)))

(define-method no-method ((gf <generic>) args)
  (goops-error "No method defined for %S"  gf))

;=============================================================================
;
;		    Cloning functions (from rdeline@CS.CMU.EDU)
;
;=============================================================================

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

;=============================================================================
;
; 		     	Class redefinition utilities
;;
;=============================================================================

;==== Class-redefinition

(define-method class-redefinition ((old <class>) (new <class>))
  ;; Work on direct methods:
  ;;		1. Remove accessor methods from the old class 
  ;;		2. Patch the occurences of old in the specializers by new
  ;;		3. Displace the methods from old to new
  (remove-class-accessors! old)					;; -1-
  (let ((methods (class-direct-methods old)))
    (for-each (lambda (m)
		(update-direct-method! m old new))	;; -2-
	      methods)
    (slot-set! new 'direct-methods methods))			;; -3-

  ;; Remove the old class from the direct-subclasses list of its super classes
  (for-each (lambda (c) (slot-set! c 'direct-subclasses 
				   (delv! old (class-direct-subclasses c))))
	    (class-direct-supers old))

  ;; Redefine all the subclasses of old to take into account modification
  (for-each 
       (lambda (c)
	 (update-direct-subclass! c old new))
       (class-direct-subclasses old))

  ;; Invalidate class so that subsequent instances slot accesses invoke
  ;; change-object-class
  (slot-set! old 'redefined old)

  ;; Swap object headers
  (%modify-class old new)

  old)

;==== Remove-class-accessors!

(define-method remove-class-accessors! ((c <class>))
  (for-each (lambda (m)
	      (if (is-a? m <accessor-method>)
		  (remove-method-in-classes! m)))
	    (class-direct-methods c)))

;==== Update-direct-method!

(define-method update-direct-method! ((m  <method>)
				      (old <class>)
				      (new <class>))
  (let Loop ((l (method-specializers m)))
    (when (pair? l)       	; Note: the <top> in dotted list is never used. 
      (if (eqv? (car l) old)  ; So we can work if we had only proper lists.
	(set-car! l new))
      (Loop (cdr l)))))

;==== Update-direct-subclass!

(define-method update-direct-subclass! ((c <class>)
					(old <class>)
					(new <class>))
  (let ((new-supers (map (lambda (cls) (if (eqv? cls old) new cls))
			 (class-direct-supers c))))
    ;; Create a new class with same name as c. This will automagically call 
    ;; class-redefinition on this subclass and redefine all its descent
    (ensure-class (class-name c)
		  new-supers
		  (class-direct-slots c)
		  (class-of c)
		  (slot-ref c 'environment))))

;=============================================================================
;
; 			Utilities for INITIALIZE methods
;
;=============================================================================

;;;
;;; compute-slot-accessors
;;;
(define (compute-slot-accessors class slots env)
  (for-each
      (lambda (s)
	(let ((name          (slot-definition-name     s))
	      (getter-name   (slot-definition-getter   s))
	      (setter-name   (slot-definition-setter   s))
	      (accessor-name (slot-definition-accessor s)))
	  (when getter-name
	    (ensure-method getter-name (list 'o) (list class)
			   `((slot-ref o ',name)) 
			   env <accessor-method>))
	  (when setter-name
	    (ensure-method setter-name (list 'o 'v) (list class <top>)
			   `((slot-set! o ',name v)) 
			   env <accessor-method>))
	  (when accessor-name
	    (ensure-method accessor-name (list 'o) (list class)
			   `((slot-ref o ',name)) 
			   env <accessor-method>)
	    (let* ((af (lookup-symbol accessor-name env))
		   (sf (setter af))
		   (gf (or sf (make <generic> #:name `(setter ,name))))
		   (mt (ensure-method #f '(o v) (list class <top>)
				     `((slot-set! o ',name v))
				     env <accessor-method>)))
	      (add-method gf mt)
	      (setf! (setter af) gf)))))
      slots))

;;;
;;; compute-getters-n-setters
;;; 
(define (compute-getters-n-setters class slots env)

  (define (compute-slot-init-function s)
    (let ((init (slot-definition-init-form s)))
      (and (not (unbound? init)) (make-closure `(lambda () ,init) env))))

  (define (verify-accessors slot l)
    (if (pair? l)
	(let ((get (car l)) 
	      (set (cadr l)))
	  (unless (and (closure? get)
		       (= (car (procedure-property get 'arity)) 1))
	    (goops-error "Bad getter closure for slot `%S' in %S: %S" slot class get))
	  (unless (and (closure? set)
		       (= (car (procedure-property set 'arity)) 2))
	    (goops-error "Bad setter closure for slot `%S' in %S: %S" slot class set)))))

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

;;;
;;; compute-cpl
;;;
(define (compute-cpl class)
  
  (define (filter-cpl class)
    (let ((res  '()))
      (for-each (lambda (item)
		  (unless (or (eq? item <object>) 
			      (eq? item <top>) 
			      (member item res))
		   (set! res (cons item res))))
	      class)
      res))

  (let* ((supers   (slot-ref class 'direct-supers))
	 (big-list (apply append (cons class supers) (map compute-cpl supers))))
    (reverse (cons <top>
		   (cons <object>
			 (filter-cpl big-list))))))

;;;
;;; Compute-get-n-set
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
		   (let Loop ((l (cdr (class-precedence-list class))))
		     (let ((r (assoc name (slot-ref (car l) 'getters-n-setters))))
		       (if r
			   (cddr r)
			   (Loop (cdr l))))))))

    ((#:each-subclass) ;; slot shared by instances of direct subclass.
		    ;; (Thomas Buerger, April 1998)
	     (let ((shared-variable (make-unbound)))
	       (list (lambda (o)   shared-variable)
		     (lambda (o v) (set! shared-variable v)))))

    ((#:virtual);; No allocation
     	     ;; slot-ref and slot-set! function must be given by the user
     	     (let ((get (get-keyword #:slot-ref  (slot-definition-options s) #f))
		   (set (get-keyword #:slot-set! (slot-definition-options s) #f))
		   (env (class-environment class)))
	       (unless (and get set)
		  (goops-error "You must supply a :slot-ref and a :slot-set! in %S" s)) ;error message originally used ~A
	       (list (make-closure get env)
		     (make-closure set env))))
    (else    (next-method))))

(define-method compute-get-n-set ((o <object>) s)
  (goops-error "Allocation \"%S\" is unknown" (slot-definition-allocation s)))

;=============================================================================
;
; 			    I n i t i a l i z e
;
;=============================================================================

(define-method initialize ((object <object>) initargs)
  (%initialize-object object initargs))

(define-method initialize ((class <class>) initargs)
  (next-method)
  (let ((dslots (map (lambda (s) (if (pair? s) s (list s)))
		     (get-keyword #:slots initargs '())))
	(supers (get-keyword #:dsupers	  initargs '()))
	(env    (get-keyword #:environment initargs (global-environment))))

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
		(slot-set! x 'direct-subclasses 
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


(define-method initialize ((generic <generic>) initargs)
  (let ((previous-definition (get-keyword #:default initargs #f)))
    (next-method)
    (if (eq? (class-of generic) <generic>)
	;; Primitive apply-generic-<n> for direct instances of <generic>
	(set-object-procedure! generic
			       apply-generic-0
			       apply-generic-1
			       apply-generic-2
			       apply-generic-3)
	(set-object-procedure! apply-generic))
    (slot-set! generic 'name    (get-keyword #:name initargs '???))
    (slot-set! generic 'methods (if (is-a? previous-definition <procedure>)
				    (list (make <method>
						#:specializers <top>
						#:procedure
						    (lambda (nm . l)
						      (apply previous-definition 
							     l))))
				    '()))))

(define-method initialize ((method <method>) initargs)
  (next-method)
  (slot-set! method 'generic-function (get-keyword #:generic-function initargs #f))
  (slot-set! method 'specializers (get-keyword #:specializers initargs '()))
  (slot-set! method 'procedure (get-keyword #:procedure initargs (lambda l '()))))


;=============================================================================
;
; 			C h a n g e - c l a s s
;
;=============================================================================

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


(define-method change-class ((old-instance <object>) (new-class <class>))
  (change-object-class old-instance (class-of old-instance) new-class))

;=============================================================================
;
; 				     M a k e 
;
;     A new definition which overwrite the previous one which was built-in
;
;=============================================================================

(define-method allocate-instance ((class <class>) initargs)
  (%allocate-instance class))

(define-method make-instance ((class <class>) . initargs)
  (let ((instance (allocate-instance class initargs)))
    (initialize instance initargs)
    instance))

(define make make-instance)

;=============================================================================
;
;				a p p l y - g e n e r i c
;
; Protocol for calling standard generic functions.
; This protocol is  not used for real <generic> functions (in this case we use
; a completely C hard-coded protocol). 
; Apply-generic is used by STklos for calls to subclasses of <generic>.
;
; The code below is similar to the first MOP described in AMOP. In particular,
; it doesn't used the currified approach to gf call. There are 2 reasons for 
; that: 
;   - the protocol below is exposed to mimic completely the one written in C
;   - the currified protocol would be imho inefficient in C.
;=============================================================================

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

;=============================================================================
;
;		     <Composite-metaclass> and <Active-metaclass>
; 
;=============================================================================

;(autoload "active-slot"    <active-metaclass>)
;(autoload "composite-slot" <composite-metaclass>)
;(export <composite-metaclass> <active-metaclass>)

;=============================================================================
;
; 				T o o l s
;
;=============================================================================

(define (list2set l)			;; duplicate the standard list->set
  (let Loop ((l l) (res '()))		;; function but using eq instead of eqv
    (cond				;; which really sucks a lot, uselessly here
     ((null? l) 	 res)
     ((memq (car l) res) (Loop (cdr l) res))
     (else		 (Loop (cdr l) (cons (car l) res))))))


(define (class-subclasses c)
  (letrec ((allsubs (lambda (c)
		      (cons c (mapappend allsubs (class-direct-subclasses c))))))
    (list2set (cdr (allsubs c)))))

(define (class-methods c)
  (list2set (mapappend class-direct-methods (cons c (class-subclasses c)))))

;;
;; Clos like SLOT-VALUE 
;; Note: SLOT-VALUE is a gf whereas SLOT-REF and SLOT-SET! are functions.
;;
(define-method slot-value ((o <object>) s)
  (slot-ref o s))

;(define-method (setter slot-value) ((o <object>) s v)
;  (slot-set! o s v))

;=============================================================================
;
; 			     Backward compatibility
;
;=============================================================================
(define class-cpl 		 class-precedence-list)		; Don' use these
(define get-slot-allocation	 slot-definition-allocation)	; obolete defs
(define slot-definition-initform slot-definition-init-form)	; anymore

(export class-cpl get-slot-allocation slot-definition-initform)

(provide "stklos")
