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


(define-module (oop goops compile)
  :use-module (oop goops)
  :use-module (oop goops util)
  :no-backtrace
  )

(export compile-method cmethod-code cmethod-environment)

(define source-formals cadr)
(define source-body cddr)

(define cmethod-code cdr)
(define cmethod-environment car)

;;;
;;; Next methods
;;;

(define (make-final-make-next-method method)
  (lambda default-args
    (lambda args
      (@apply method (if (null? args) default-args args)))))	  

(define (make-final-make-no-next-method gf)
  (lambda default-args
    (lambda args
      (no-next-method gf (if (null? args) default-args args)))))

(define (make-make-next-method vcell gf methods types)
  (lambda default-args
    (lambda args
      (if (null? methods)
	  (begin
	    (set-cdr! vcell (make-final-make-no-next-method gf))
	    (no-next-method gf (if (null? args) default-args args)))
	  (let* ((code (compile-method methods types))
		 (method (local-eval (cons 'lambda (cmethod-code code))
				     (cmethod-environment code))))
	    (set-cdr! vcell (make-final-make-next-method method))
	    (@apply method (if (null? args) default-args args)))))))

;;;
;;; Method compilation
;;;

;;; NOTE: This section is far from finished.  It will finally be
;;; implemented on C level.

(define (compile-method methods types)
  (let* ((proc (method-procedure (car methods)))
	 (src (procedure-source proc))
	 (formals (source-formals src))
	 (body (source-body src)))
    (if (and (pair? (car body))
	     (source-property (car body) 'standard-accessor-method))
	(cons (procedure-environment proc)
	      (cons formals
		    body))
	(let ((vcell (cons 'goops:make-next-method #f)))
	  (set-cdr! vcell
		    (make-make-next-method
		     vcell
		     (method-generic-function (car methods))
		     (cdr methods) types))
	  ;;*fixme*
	  `(,(cons vcell (procedure-environment proc))
	    ,formals
	    ;;*fixme* Only do this on source where next-method can't be inlined
	    (let ((next-method ,(if (list? formals)
				    `(goops:make-next-method ,@formals)
				    `(apply goops:make-next-method
					    ,@(improper->proper formals)))))
	      ,@body))))))
