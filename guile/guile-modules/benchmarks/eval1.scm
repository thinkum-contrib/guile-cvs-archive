;;;; eval1.scm --- benchmark for the evaluator
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

(define-module (benchmarks eval1)
  #:use-module (benchmarks lib)
  #:use-module (ice-9 format))

(export eval1)


(define (p+ x y)
  (+ x y))

(define (p- x y)
  (- x y))

(define (procedure-sum n res)
  (if (zero? n)
      res
      (procedure-sum (p- n 1) (p+ 1 res))))


(define (test title operator n evaluator)
  (time-thunk-median title
		     times:user+gc
		     (lambda ()
		       (operator n 0))
		     evaluator)
  (time-thunk-median:times (last-data)))

(define (eval1 n)
  (benchmark-title 'eval1 1)
  (test 'procedure-sum procedure-sum n 'normal))
