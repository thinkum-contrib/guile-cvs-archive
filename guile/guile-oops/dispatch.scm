;;;; 	Copyright (C) 1999 Free Software Foundation, Inc.
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


(define-module (oop goops dispatch)
  :use-module (oop goops)
  :use-module (oop goops util)
  :use-module (oop goops compile)
  :no-backtrace
  )

(export memoize-method!)

;;;
;;; This file implements method memoization.  It will finally be
;;; implemented on C level in order to obtain fast generic function
;;; application also during the first pass through the code.
;;;

;;;
;;; Constants
;;;

(define hashsets 8)
(define hashset-index 10)

(define hash-threshold 3)
(define initial-hash-size 4) ;must be a power of 2 and >= hash-threshold

(define initial-hash-size-1 (- initial-hash-size 1))

(define the-list-of-#f '(#f))

;;;
;;; Method cache
;;;

;; (#@dispatch args N-SPECIALIZED #((TYPE1 ... ENV FORMALS FORM1 ...) ...) GF)
;; (#@dispatch args N-SPECIALIZED HASHSET MASK
;;             #((TYPE1 ... ENV FORMALS FORM1 ...) ...)
;;             GF)

;;; Representation

;; non-hashed form

(define method-cache-n-specialized caddr)

(define (set-method-cache-n-specialized! exp n)
  (set-car! (cddr exp) n))

(define method-cache-entries cadddr)

(define (set-method-cache-entries! mcache entries)
  (set-car! (cdddr mcache) entries))

(define (method-cache-n-methods exp)
  (n-cache-methods (method-cache-entries exp)))

(define (method-cache-methods exp)
  (cache-methods (method-cache-entries exp)))

;; hashed form

(define (set-hashed-method-cache-hashset! exp hashset)
  (set-car! (cdddr exp) hashset))

(define (set-hashed-method-cache-mask! exp mask)
  (set-car! (cddddr exp) mask))

(define (hashed-method-cache-entries exp)
  (list-ref exp 5))

(define (set-hashed-method-cache-entries! exp entries)
  (set-car! (list-cdr-ref exp 5) entries))

;; either form

(define (method-cache-generic-function exp)
  (list-ref exp (if (method-cache-hashed? exp) 6 4)))

;;; Predicates

(define (method-cache-hashed? x)
  (integer? (cadddr x)))

(define max-non-hashed-index (- hash-threshold 2))

(define (passed-hash-threshold? exp)
  (and (> (vector-length (method-cache-entries exp)) max-non-hashed-index)
       (car (vector-ref (method-cache-entries exp) max-non-hashed-index))))

;;; Converting a method cache to hashed form

(define (method-cache->hashed! exp)
  (set-cdr! (cddr exp) (cons 0 (cons initial-hash-size-1 (cdddr exp))))
  exp)

;;;
;;; Cache entries
;;;

(define (n-cache-methods entries)
  (do ((i (- (vector-length entries) 1) (- i 1)))
      ((or (< i 0) (car (vector-ref entries i)))
       (+ i 1))))

(define (cache-methods entries)
  (do ((i (- (vector-length entries) 1) (- i 1))
       (methods '() (let ((entry (vector-ref entries i)))
		      (if (car entry) (cons entry methods) methods))))
      ((< i 0) methods)))

;;;
;;; Method insertion
;;;

(define (method-cache-insert! exp entry)
  (let* ((entries (method-cache-entries exp))
	 (n (n-cache-methods entries)))
    (if (>= n (vector-length entries))
	;; grow cache
	(let ((new-entries (make-vector (* 2 (vector-length entries))
					the-list-of-#f)))
	  (do ((i 0 (+ i 1)))
	      ((= i n))
	    (vector-set! new-entries i (vector-ref entries i)))
	  (vector-set! new-entries n entry)
	  (set-method-cache-entries! exp new-entries))
	(vector-set! entries n entry))))

(define (hashed-method-cache-insert! exp entry)
  (let* ((cache (hashed-method-cache-entries exp))
	 (size (vector-length cache)))
    (let* ((entries (cons entry (cache-methods cache)))
	   (size (if (<= (length entries) size)
		     size
		     ;; larger size required
		     (let ((new-size (* 2 size)))
		       (set-hashed-method-cache-mask! exp (- new-size 1))
		       new-size)))
	   (min-misses size)
	   (best #f))
      (do ((hashset 0 (+ 1 hashset)))
	  ((= hashset hashsets))
	(let* ((test-cache (make-vector size the-list-of-#f))
	       (misses (cache-try-hash! min-misses hashset test-cache entries)))
	  (cond ((zero? misses)
		 (set! min-misses 0)
		 (set! best hashset)
		 (set! cache test-cache)
		 (set! hashset (- hashsets 1)))
		((< misses min-misses)
		 (set! min-misses misses)
		 (set! best hashset)
		 (set! cache test-cache)))))
      (set-hashed-method-cache-hashset! exp best)
      (set-hashed-method-cache-entries! exp cache))))

;;;
;;; Caching
;;;

(define environment? pair?)

(define (cache-hashval hashset entry)
  (let ((hashset-index (+ hashset-index hashset)))
    (do ((sum 0)
	 (classes entry (cdr classes)))
	((environment? (car classes)) sum)
      (set! sum (+ sum (struct-ref (car classes) hashset-index))))))

(define (cache-try-hash! min-misses hashset cache entries)
  (let ((misses 0)
	(mask (- (vector-length cache) 1)))
    (catch 'misses
	   (lambda ()
	     (do ((ls entries (cdr ls)))
		 ((null? ls) misses)
	       (do ((i (logand mask (cache-hashval hashset (car ls)))
		       (logand mask (+ i 1))))
		   ((not (car (vector-ref cache i)))
		    (vector-set! cache i (car ls)))
		 (set! misses (+ 1 misses))
		 (if (>= misses min-misses)
		     (throw 'misses misses)))))
	   (lambda (key misses)
	     misses))))

;;;
;;; Memoization
;;;

(define (memoize-method! gf args exp)
  (if (not (slot-ref gf 'used-by))
      (slot-set! gf 'used-by '()))
  (let ((applicable ((if (eq? gf compute-applicable-methods)
			 %compute-applicable-methods
			 compute-applicable-methods)
		     gf args)))
    (cond ((not applicable)
	   (no-applicable-method gf args))
	  ((method-cache-hashed? exp)
	   (method-cache-install! hashed-method-cache-insert!
				  exp args applicable))
	  ((passed-hash-threshold? exp)
	   (method-cache-install! hashed-method-cache-insert!
				  (method-cache->hashed! exp)
				  args
				  applicable))
	  (else
	   (method-cache-install! method-cache-insert! exp args applicable)))))

(set-procedure-property! memoize-method! 'system-procedure #t)

(define method-cache-install!
  (letrec ((first-n
	    (lambda (ls n)
	      (if (or (zero? n) (null? ls))
		  '()
		  (cons (car ls) (first-n (cdr ls) (- n 1)))))))
    (lambda (insert! exp args applicable)
      (let* ((specializers (method-specializers (car applicable)))
	     (n-specializers
	      (if (list? specializers)
		  (length specializers)
		  (+ 1 (slot-ref (method-cache-generic-function exp)
				 'n-specialized)))))
	(if (> n-specializers (method-cache-n-specialized exp))
	    (set-method-cache-n-specialized! exp n-specializers))
	(let ((types (map class-of (first-n args n-specializers))))
	  (insert! exp (method-entry applicable types)))))))

;;;
;;; Method entries
;;;

(define (method-entry methods types)
  (cond ((assoc types (slot-ref (car methods) 'code-table))
	 => (lambda (types-code) (cdr types-code)))
	(else (let* ((method (car methods))
		     ;;*fixme* new seek primitive => no double types
		     (place-holder (list #f))
		     (entry (append types place-holder)))
		;; In order to handle recursion nicely, put the entry
		;; into the code-table before compiling the method 
		(slot-set! (car methods) 'code-table
			   (acons types entry
				  (slot-ref (car methods) 'code-table)))
		(let ((cmethod (compile-method methods types)))
		  (set-car! place-holder (car cmethod))
		  (set-cdr! place-holder (cdr cmethod)))
		entry))))
