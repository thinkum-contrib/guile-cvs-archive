;;;; goops.scm --- benchmarks for GOOPS
;;;; Copyright (C) 1999  Mikael Djurfeldt <djurfeldt@nada.kth.se>
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(define-module (benchmarks goops)
  #:use-module (benchmarks lib)
  #:use-module (oop goops)
  #:use-module (ice-9 format))

(export type-dispatch type-dispatch-run)


(define n-dispatch 500000)

(define (percent-increase x1 x2)
  (- (/ (* 100.0 x2) x1) 100.0))

(define (p+ x y)
  (+ x y))

(define (p- x y)
  (- x y))

(define (procedure-sum n res)
  (if (zero? n)
      res
      (procedure-sum (p- n 1) (p+ 1 res))))


(define-method gf+ ((x <number>) (y <number>))
  (+ x y))

(define-method gf- ((x <number>) (y <number>))
  (- x y))

(define-method generic-sum ((n <number>) (res <number>))
  (if (zero? n)
      res
      (generic-sum (gf- n 1) (gf+ 1 res))))

(define (dispatch-test title operator evaluator)
  (time-thunk-median title
		     times:user+gc
		     (lambda ()
		       (operator n-dispatch 0))
		     evaluator)
  (time-thunk-median:times (last-data)))

(define fill-cache
  (let* ((fill-args '((0 0.0) (0 0+1i) (0.0 0) (0.0 0.0) (0.0 0+1i)))
	 (n-args (length fill-args)))
    (lambda (gf m)
      ;; This causes cache invalidation
      (add-method! gf (method ()))
      ;; Fill cache by applying the generic to combinations of non-integer args
      (for-each (lambda (args)
		  (apply gf args))
		(reverse (list-tail (reverse fill-args) (- n-args m)))))))

(define (type-dispatch . m)
  (benchmark-title 'type-dispatch 1)
  (let ((m (optarg1 m 0)))
    (for-each (lambda (gf) (fill-cache gf m))
	      (list generic-sum gf- gf+))
    (let* ((d-proc (dispatch-test 'd:no-dispatch      procedure-sum 'debug))
	   (d-gf   (dispatch-test 'd:generic-dispatch generic-sum   'debug))
	   (d (percent-increase (times:user+gc d-proc) (times:user+gc d-gf)))
	   (d-user (percent-increase (times:user d-proc) (times:user d-gf)))
	   (n-proc (dispatch-test 'n:no-dispatch      procedure-sum 'normal))
	   (n-gf   (dispatch-test 'n:generic-dispatch generic-sum   'normal))
	   (n (percent-increase (times:user+gc n-proc) (times:user+gc n-gf)))
	   (n-user (percent-increase (times:user n-proc) (times:user n-gf)))
	   (data (list m d d-user n n-user)))
      (log-text (format "In the loop:

  (define-method generic-sum ((n <number>) (res <number>))
    (if (zero? n)
        res
        (generic-sum (gf- n 1) (gf+ 1 res))))

with 3 type dispatches per turn (generic-sum, gf-, gf+)
and ~D previous methods in the method cache
the execution time is increased by ~1,1F% (user ~1,1F%) due to type dispatch
in the debugging evaluator and by ~1,1F% (user ~1,1F%) in the normal.\n"
		      
			m d d-user n n-user))
      (log-data (cons 'type-dispatch data))
      data)))

(define (type-dispatch-run)
  (let ((results (map type-dispatch (iota 6))))
    (log-text (format "Summary\n\n~A ~A ~A ~A ~A\n"
		      "n-methods"
		      "debug:tot"
		      "debug:user"
		      "norm:tot"
		      "norm:user"))
    (for-each (lambda (data)
		(log-text (apply format "~D         ~8,1F% ~9,1F% ~7,1F% ~8,1F%"
				 (map (lambda (n) (list-ref data n))
				      (iota 5)))))
	      results)))
