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

(define-module (benchmarks eval2)
  #:use-module (benchmarks lib)
  #:use-module (ice-9 format))

(export eval2)


(define (compute n res)
  (if (>= n 0.0)
      (compute (- n 1.0) (* (- res 0.1) (+ res 0.1)))))


(define (test title operator n evaluator)
  (time-thunk-median title
		     times:user+gc
		     (lambda ()
		       (operator n 0.0))
		     evaluator)
  (time-thunk-median:times (last-data)))

(define (eval2 n)
  (benchmark-title 'eval2 1)
  (test 'compute compute n 'normal))
