; -*- Scheme -*-
;
(load "macro.scm")

;
; macro to speedup integer arith
;

(define-macro (+ . args)
  (let ((len (length args)))
	(cond
	 ((eq? len 0) 	0)
	 ((eq? len 1)	(car args))
	 ((eq? len 2)
	  (let ((x (car args)) (y (cadr args)))
		(cond
		 ((eq? x 1)  `(1+ ,y))
		 ((eq? x 2)  `(2+ ,y))
		 ((eq? y 1)  `(1+ ,x))
		 ((eq? y 2)  `(2+ ,x))
		 (else `(add2 ,x ,y)))))
	  (else
	   `(addv ,@args)))))

(macro-set-func! + addv);

(define-macro (* . args)
  (let ((len (length args)))
	(cond
	 ((eq? len 0) 	1)
	 ((eq? len 1)	(car args))
	 ((eq? len 2)
	  (let ((x (car args)) (y (cadr args)))
		(cond
		 ((eq? x 1)  y)
		 ((eq? y 1)  x)
		 (else  `(mul2 ,x ,y)))))
	 (else `(mulv ,@args)))))

(macro-set-func! * mulv);

(define-macro (- . args)
  (let ((len (length args)))
	(cond
	 ((eq? len 0) 	0)
	 ((eq? len 1)	`(sub2 0 ,(car args)))
	 ((eq? len 2)
	  (let ((x (car args)) (y (cadr args)))
		(cond
;		 ((eq? x 1)  `(1- ,y))
;		 ((eq? x 2)  `(2- ,y))
		 ((eq? y 1)  `(1- ,x))
		 ((eq? y 2)  `(2- ,x))
		 (else `(sub2 ,x ,y)))))
	  (else
	   `(subv ,@args)))))

(macro-set-func! - subv)

(define-macro (/ . args)
  (let ((len (length args)))
	(cond
	 ((eq? len 0) 	0)
	 ((eq? len 1)	`(div2 1 ,(car args)))
	 ((eq? len 2)	`(div2 ,(car args) ,(cadr args)))
	 (else `(divv ,@args)))))

(macro-set-func! / divv)

;
; Math extensions
;
(define-macro (< . args)
  (let ((len (length args)))
	(cond ((or (eq? len 0) (eq? len 1)) #t)
		  ((eq? len 2) `(*i-n2<* ,(car args) ,(cadr args)))
		  (else `(*i-nv<* ,@args)))))

(macro-set-func! < *i-nv<*)

(define-macro (<= . args)
  (let ((len (length args)))
	(cond ((or (eq? len 0) (eq? len 1)) #t)
		  ((eq? len 2) `(*i-n2<=* ,(car args) ,(cadr args)))
		  (else `(*i-nv<=* ,@args)))))

(macro-set-func! <= *i-nv<=*)

(define-macro (>= . args)
  (let ((len (length args)))
	(cond ((or (eq? len 0) (eq? len 1)) #t)
		  ((eq? len 2) `(*i-n2>=* ,(car args) ,(cadr args)))
		  (else `(*i-nv>=* ,@args)))))

(macro-set-func! >= *i-nv>=*)

(define-macro (> . args)
  (let ((len (length args)))
	(cond ((or (eq? len 0) (eq? len 1)) #t)
		  ((eq? len 2) `(*i-n2>* ,(car args) ,(cadr args)))
		  (else `(*i-nv>* ,@args)))))

(macro-set-func! > *i-nv>*)

(define-macro (= . args)
  (let ((len (length args)))
	(cond ((or (eq? len 0) (eq? len 1)) #t)
		  ((eq? len 2) `(*i-n2=* ,(car args) ,(cadr args)))
		  (else `(*i-nv=* ,@args)))))

(macro-set-func! = *i-nv=*)

;
; Math constants
;
(define pi    3.14159265358979323846)
(define pi/2  1.57079632679489661923)
(define pi/4  0.78539816339744830962)

;
; Hash type constants
;
(define SCM_HASH_T_GEN		0)
(define SCM_HASH_T_SYMBOL	1)
(define SCM_HASH_T_ATOM		2)

;
; Special symbols
;
(define nil '())

; Library extensions

;; Load library if not allready loaded

(define (require-lib lib)
  (if (not (member lib library-list)) (load-library lib)))

;; Conditional loading

(define (needs object file)
  (if (undefined-object? (hash-ref symbol-hash object))
	  (load file)))

;
; Example of FFI function
;
;(define system  (make-extfunc "" :int "system"	'(:string)))

;
;(define printf  (make-extfunc "" :void "printf"	'(:string . :any)))
;(define sprintf (make-extfunc "" :string "sprintf" '(:string . :any)))
;(define fopen   (make-extfunc "" :item "fopen" '(:string :string)))
;(define fclose  (make-extfunc "" :int "fclose" 	 '(:item)))
;(define fprintf (make-extfunc "" :void "fprintf" '(:item . :any)))

;
; Quick and dirty apropos
;
(define (apropos str)
  (let ((l '()))
	(if (symbol? str)						; convert symbol to string on need
		(set! str (symbol->string str)))
	(for-each
	 (lambda (x)
	   (if (string-index (symbol->string (car x)) str)
		   (set! l (cons (car x) l))))
	 (hash->list symbol-hash))
	l))

;
; case
;
(define-macro (case key . clauses)
  ;; conditionally execute the clause eqv? to key
  (define (case-make-clauses key)
    `(cond ,@(map
              (lambda (clause)
                (if (pair? clause)
                    (let ((klist (car clause))
                          (exprs (cdr clause)))
                      (cond ((eq? klist 'else)
                             `(else ,@exprs))
                            ((pair? klist)
                             (if (= (length klist) 1)
                                 `((eqv? ,key ',(car klist)) ,@exprs)
                                 `((memv ,key ',klist) ,@exprs)))
                            (else
                             `((eqv? ,key ',klist) ,@exprs))))
                    (error "case: bad syntax" clause)))
              clauses)))
  (if (pair? key)
	  (let ((newkey (gensym)))
		`(let ((,newkey ,key))
		   ,(case-make-clauses newkey)))
	  (case-make-clauses key)))
;
; port supplement
;

;; open-port;
;; type constant
(define file-port 0)
(define string-port 1)

;; mode constant
(define read-mode 1)
(define create-mode 2)
(define append-mode 3)
(define update-mode 4)
(define update-create-mode 5)
(define update-append-mode 6)

(define (call-with-input-file string proc)
  (let* ((file (open-input-file string))
		 (result (proc file)))
	(close-input-port file)
	result))

(define (call-with-output-file string proc)
  (let* ((file (open-output-file string))
		 (result (proc file)))
	(close-port file)
	result))

(define (call-with-input-string string proc)
  (let* ((port (open-input-string string))
		 (res  (proc port)))
	(close-port port)
	res))

(define (call-with-output-string proc)
  (let* ((port (open-output-string)))
	(proc port)
	(close-port port)))

;
; define a new qscheme type.
;
; This type can be use with the ffi interface
;
(define (define-type x)
  (make-type 
   (cond
	((symbol? x)  (symbol->string x))
	((keyword? x) (keyword->string x))
	((string? x)  x)
	(else (error "bad type" x)))))

;;; Short equiv
(define href  hash-ref)
(define hset! hash-set!)

; General purpose functions
(load "sgtk/defextern.scm")

;; (load "repl.scm")

