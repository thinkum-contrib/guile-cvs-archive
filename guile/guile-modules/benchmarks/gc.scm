;;;; gc.scm --- benchmarks for the garbage collector
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

(define-module (benchmarks gc)
  #:use-module (benchmarks lib)
  #:use-module (ice-9 format))

(export gc-time-run)


(define n-dispatch 500000)

(define (percent-of x1 x2)
  (/ (* 100.0 x1) x2))

(define (p+ x y)
  (+ x y))

(define (p- x y)
  (- x y))

(define (procedure-sum n res)
  (if (zero? n)
      res
      (procedure-sum (p- n 1) (p+ 1 res))))


(define (dispatch-test title operator evaluator)
  (time-thunk-median title
		     times:user+gc
		     (lambda ()
		       (operator n-dispatch 0))
		     evaluator)
  (time-thunk-median:times (last-data)))

(define (gc-time-run)
  (benchmark-title 'gc-time 1)
  (let* ((d-proc (dispatch-test 'debug:sum procedure-sum 'debug))
	 (d (percent-of (times:gc d-proc) (times:user+gc d-proc)))
	 (n-proc (dispatch-test 'normal:sum procedure-sum 'normal))
	 (n (percent-of (times:gc n-proc) (times:user+gc n-proc)))
	 (data (list d n)))
      (log-text (format "GC time is ~1,1F% of total time in the debugging evaluator and
~1,1F% of total time in the normal."
			d n))
      (log-data (cons 'gc-time data))
      data))
