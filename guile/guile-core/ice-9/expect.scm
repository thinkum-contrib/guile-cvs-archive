;;;; 	Copyright (C) 1996, 1998, 1999 Free Software Foundation, Inc.
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


(define-module (ice-9 expect) :use-module (ice-9 regex))

;;; Expect: a macro for selecting actions based on what it reads from a port.
;;; The idea is from Don Libes' expect based on Tcl.
;;; This version by Gary Houston incorporating ideas from Aubrey Jaffer.


(define-public expect-port #f)
(define-public expect-timeout #f)
(define-public expect-timeout-proc #f)
(define-public expect-eof-proc #f)
(define-public expect-char-proc #f)

;;; expect: each test is a procedure which is applied to the accumulating
;;; string.
(defmacro-public expect clauses
  (let ((s (gentemp))
	(c (gentemp))
	(port (gentemp))
	(timeout (gentemp)))
    `(let ((,s "")
	   (,port (or expect-port (current-input-port)))
	   ;; when timeout occurs, in floating point seconds.
	   (,timeout (if expect-timeout
			 (let* ((secs-usecs (gettimeofday)))
			   (+ (car secs-usecs)
			      expect-timeout
			      (/ (cdr secs-usecs)
				 1000000))) ; one million.
			 #f)))
       (let next-char ()
	 (if (and expect-timeout
		  (or (>= (get-internal-real-time) ,timeout)
		      (and (not (char-ready? ,port))
			   (not (expect-select ,port ,timeout)))))
	     (if expect-timeout-proc
		 (expect-timeout-proc ,s)
		 #f)
	     (let ((,c (read-char ,port)))
	       (if expect-char-proc
		   (expect-char-proc ,c))
	       (cond ((eof-object? ,c)
		      (if expect-eof-proc
			  (expect-eof-proc ,s)
			  #f))
		     (else
		      (set! ,s (string-append ,s (string ,c)))
		      (cond
		       ,@(let next-expr ((tests (map car clauses))
					 (exprs (map cdr clauses))
					 (body '()))
			   (cond
			    ((null? tests)
			     (reverse body))
			    (else
			     (next-expr
			      (cdr tests)
			      (cdr exprs)
			      (cons
			       `((,(car tests) ,s ,port)
				 ,@(cond ((null? (car exprs))
					  '())
					 ((eq? (caar exprs) '=>)
					  (if (not (= (length (car exprs))
						      2))
					      (scm-error 'misc-error
							 "expect"
							 "bad recipient: %S"
							 (list (car exprs))
							 #f)
					      `((apply ,(cadar exprs)
						       (,(car tests) ,s ,port)))))
					 (else 
					  (car exprs))))
			       body)))))
		       (else (next-char)))))))))))


(define-public expect-strings-compile-flags regexp/newline)
(define-public expect-strings-exec-flags regexp/noteol)

;;; the regexec front-end to expect:
;;; each test must evaluate to a regular expression.
(defmacro-public expect-strings clauses
  `(let ,@(let next-test ((tests (map car clauses))
			  (exprs (map cdr clauses))
			  (defs '())
			  (body '()))
	    (cond ((null? tests)
		   (list (reverse defs) `(expect ,@(reverse body))))
		  (else
		   (let ((rxname (gentemp)))
		     (next-test (cdr tests)
				(cdr exprs)
				(cons `(,rxname (make-regexp
						 ,(car tests)
						 expect-strings-compile-flags))
				      defs)
				(cons `((lambda (s port)
					  (expect-regexec
					   ,rxname s port))
					,@(car exprs))
				      body))))))))

;;; simplified select: returns #t if input is waiting or #f if timed out.
;;; timeout is an absolute time in floating point seconds.
(define-public (expect-select port timeout)
  (let* ((secs-usecs (gettimeofday))
	 (relative (- timeout 
		      (car secs-usecs)
		      (/ (cdr secs-usecs)
			 1000000))))	; one million.
    (and (> relative 0)
	 (pair? (car (select (list port) '() '()
			     relative))))))

;;; return a regexp match as a list of strings, for the => syntax.
(define-public (expect-regexec rx s port)
  ;; if expect-strings-exec-flags contains regexp/noteol,
  ;; check whether at EOF.  if so, remove regexp/noteol
  (let* ((eof-next? 
	  (and (logand expect-strings-exec-flags regexp/noteol)
	       (eof-object? (peek-char port))))
	 (flags (if eof-next?
		    (logxor expect-strings-exec-flags regexp/noteol)
		    expect-strings-exec-flags))
	 (match (regexp-exec rx s 0 flags)))
    (if match
	(do ((i (- (match:count match) 1) (- i 1))
	     (result '() (cons (match:substring match i) result)))
	    ((< i 0) result))
	#f)))

