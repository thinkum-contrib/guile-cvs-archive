;;;; setf.scm --- support for generalized references
;;;;
;;;;  	Copyright (C) 1998 Free Software Foundation, Inc.
;;;;
;;;;   This file is part of GUILE.
;;;;   
;;;;   GUILE is free software; you can redistribute it and/or modify
;;;;   it under the terms of the GNU General Public License as
;;;;   published by the Free Software Foundation; either version 2, or
;;;;   (at your option) any later version.
;;;;   
;;;;   GUILE is distributed in the hope that it will be useful, but
;;;;   WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;   GNU General Public License for more details.
;;;;   
;;;;   You should have received a copy of the GNU General Public
;;;;   License along with GUILE; see the file COPYING.  If not, write
;;;;   to the Free Software Foundation, Inc., 59 Temple Place, Suite
;;;;   330, Boston, MA 02111-1307 USA
;;;;
;;;; Author:           Christian Lynbech
;;;; Modifications by: Mikael Djurfeldt
;;;; $Id$

;; This is a first shot at setf support.

;; A fuller implementation would make define-setf behave more like
;; define-macro, allowing more powerful and general expander
;; functions, as well as catering for nested expansions.

;; From NEWS:

;; * New syntax: setf! PLACE VALUE
;; Puts VALUE in location specified by PLACE.  setf! is more general than
;; set! in the sense that PLACE can be a form (GETTER EXP1 EXP2 ...).  The
;; setf! expression will bw transformed into (SETTER EXP1 EXP2 ... VALUE)
;; where SETTER is a procedure or macro which has previously been
;; associated with GETTER.
;; 
;; Example:
;; 
;;  (setf! (car x) 4) <=> (set-car! x 4)
;; 
;; * New syntax: setter GETTER
;; The name of the SETTER of GETTER.  The way to associate a SETTER with
;; a GETTER is: (setf! (setter GETTER) SETTER)
;; 
;; Example:
;; 
;;   (setf! (setter car) set-car!)

;; EXAMPLES:

;; (setf! (setter car) (lambda (o v) (set-car! o v)))
;; (define x (list 1 2))
;; (car x) ;=> 1
;; (setf! (car x) 4)
;; (car x) ;=> 4

;; (setf! (setter vector-ref) (lambda (vec idx val) (vector-set! vec idx val)))
;; (define x (vector 1 2))
;; (vector-ref x 0) ;=> 1
;; (setf! (vector-ref x 0) 4)
;; (vector-ref x 0) ;=> 4


(define-module (ice-9 setf))

(export setf! setter)

(define setf!
  (procedure->memoizing-macro
   (lambda (exp env)
     (if (pair? (cadr exp))
	 (let* ((v (local-eval (caadr exp) env))
		(p (cond ((procedure? v) (procedure-property v 'setter))
			 ((macro? v) (object-property v 'setter))
			 (else #f))))
	   (or (procedure? p)
	       (macro? p)
	       (scm-error 'misc-error
			  'setf!
			  "No setter found for %S"
			  (list v)
			  '()))
	   `(,p
	     ,@(cdadr exp)
	     ,(caddr exp)))
	 (cons set! (cdr exp))))))


(define setter
  (procedure->memoizing-macro
   (lambda (exp env)
     (let ((p (local-eval (cadr exp) env)))
       (cond ((procedure? p) (procedure-property p 'setter))
	     ((macro? p) (object-property p 'setter))
	     (else (scm-error 'misc-error
			      'setter
			      "Setter must be a procedure or a macro: %S"
			      (list p)
			      '())))))))


(define (set-setter! getter setter)
  (cond ((procedure? getter) (set-procedure-property! getter 'setter setter))
	((macro? getter) (set-object-property! getter 'setter setter))
	(else (scm-error 'misc-error
			 'setf!
			 "Getter must be a procedure or macro: %S"
			 (list getter)
			 '()))))


(set-object-property! setter 'setter set-setter!)

;; a few examples

(setf! (setter car) (lambda (o v) (set-car! o v)))
(setf! (setter cdr) (lambda (o v) (set-cdr! o v)))

(setf! (setter vector-ref) (lambda (vec idx val) (vector-set! vec idx val)))

;;; setf.scm ends here.
