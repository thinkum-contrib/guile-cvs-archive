;;;; 	Copyright (C) 1996, 1998, 2001, 2002 Free Software Foundation, Inc.
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
;;;; As a special exception, the Free Software Foundation gives permission
;;;; for additional uses of the text contained in its release of GUILE.
;;;;
;;;; The exception is that, if you link the GUILE library with other files
;;;; to produce an executable, this does not by itself cause the
;;;; resulting executable to be covered by the GNU General Public License.
;;;; Your use of that executable is in no way restricted on account of
;;;; linking the GUILE library code into it.
;;;;
;;;; This exception does not however invalidate any other reasons why
;;;; the executable file might be covered by the GNU General Public License.
;;;;
;;;; This exception applies only to the code released by the
;;;; Free Software Foundation under the name GUILE.  If you copy
;;;; code from other Free Software Foundation releases into a copy of
;;;; GUILE, as the General Public License permits, the exception does
;;;; not apply to the code that you add in this way.  To avoid misleading
;;;; anyone as to the status of such modified files, you must delete
;;;; this exception notice from them.
;;;;
;;;; If you write modifications of your own for GUILE, it is your choice
;;;; whether to permit this exception to apply to your modifications.
;;;; If you do not wish that, delete this exception notice.
;;;;
;;;; ----------------------------------------------------------------
;;;; threads.scm -- User-level interface to Guile's thread system
;;;; 4 March 1996, Anthony Green <green@cygnus.com>
;;;; Modified 5 October 1996, MDJ <djurfeldt@nada.kth.se>
;;;; Modified 6 April 2001, ttn
;;;; ----------------------------------------------------------------
;;;;

;;; Commentary:

;; This module is documented in the Guile Reference Manual.
;; Briefly, one procedure is exported: `%thread-handler';
;; as well as four macros: `make-thread', `begin-thread',
;; `with-mutex' and `monitor'.

;;; Code:

(define-module (ice-9 threads)
  :export (par-map
	   par-for-each
	   %thread-handler)
  :export-syntax (make-thread
		  begin-thread
		  parallel
		  letpar
		  with-mutex
		  monitor))



(define (par-map proc . arglists)
  (let* ((m (make-mutex))
	 (c (make-condition-variable))
	 (n (length (car arglists)))
	 (counter (- n 1))
	 (res (make-list n))
	 (ls res))
    (lock-mutex m)
    (apply for-each
	   (lambda args
	     (let ((res ls))
	       (set! ls (cdr ls))
	       (call-with-new-thread
		(lambda ()
		  (set-car! res (apply proc args))
		  ;; synchronize
		  (lock-mutex m)
		  (if (zero? counter)
		      (signal-condition-variable c)
		      (set! counter (- counter 1)))
		  (unlock-mutex m))
		%thread-handler)))
	   arglists)
    (wait-condition-variable c m)
    res))

(define (par-for-each proc . arglists)
  (let* ((m (make-mutex))
	 (c (make-condition-variable))
	 (counter (- (length (car arglists)) 1)))
    (lock-mutex m)
    (apply for-each
	   (lambda args
	     (call-with-new-thread
	      (lambda ()
		(apply proc args)
		;; synchronize
		(lock-mutex m)
		(if (zero? counter)
		    (signal-condition-variable c)
		    (set! counter (- counter 1)))
		(unlock-mutex m))
	      %thread-handler))
	   arglists)
    (wait-condition-variable c m)))

(define (%thread-handler tag . args)
  (fluid-set! the-last-stack #f)
  (let ((n (length args))
	(p (current-error-port)))
    (display "In thread:" p)
    (newline p)
    (if (>= n 3)
        (display-error #f
                       p
                       (car args)
                       (cadr args)
                       (caddr args)
                       (if (= n 4)
                           (cadddr args)
                           '()))
        (begin
          (display "uncaught throw to " p)
          (display tag p)
          (display ": " p)
          (display args p)
          (newline p)))
    #f))

; --- MACROS -------------------------------------------------------

(defmacro make-thread (proc . args)
  `(call-with-new-thread
    (lambda ()
      (,proc ,@args))
    %thread-handler))

(defmacro begin-thread (first . rest)
  `(call-with-new-thread
    (lambda ()
      (begin
	,first ,@rest))
    %thread-handler))

(defmacro parallel forms
  (cond ((null? forms) '(begin))
	((null? (cdr forms)) (car forms))
	(else
	 (let* ((m (make-symbol "m"))
		(c (make-symbol "c"))
		(counter (make-symbol "counter"))
		(sync (make-symbol "sync"))
		(n-forms (length forms))
		(vars (map (lambda (i)
			     (make-symbol (string-append "res"
							 (number->string i))))
			   (iota n-forms))))
	   `(let* ((,m (make-mutex))
		   (,c (make-condition-variable))
		   (,counter ,(- n-forms 1))
		   (,sync (lambda ()
			    (lock-mutex ,m)
			    (if (zero? ,counter)
				(signal-condition-variable ,c)
				(set! ,counter (- ,counter 1)))
			    (unlock-mutex ,m)))
		   ,@(map (lambda (var)
			    `(,var #f))
			  vars))
	      (lock-mutex ,m)       
	      ,@(map (lambda (var form)
		       `(call-with-new-thread (lambda ()
						(set! ,var ,form)
						(,sync))
					      %thread-handler))
		     vars
		     forms)
	      (wait-condition-variable ,c ,m)
	      (values ,@vars))))))

(defmacro letpar (bindings . body)
  `(call-with-values
       (lambda ()
	 (parallel ,@(map cadr bindings)))
     (lambda ,(map car bindings)
       ,@body)))

(defmacro with-mutex (m . body)
  `(dynamic-wind
       (lambda () (lock-mutex ,m))
       (lambda () (begin ,@body))
       (lambda () (unlock-mutex ,m))))

(defmacro monitor (first . rest)
  `(with-mutex ,(make-mutex)
     (begin
       ,first ,@rest)))

;;; threads.scm ends here
