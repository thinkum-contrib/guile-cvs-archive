
;;; {Transcendental Functions}
;;;
;;; Derived from "Transcen.scm", Complex trancendental functions for SCM.
;;; Written by Jerry D. Hedden, (C) FSF.
;;; See the file `COPYING' for terms applying to this program.
;;;

(define (exp z)
  (if (real? z) ($exp z)
      (make-polar ($exp (real-part z)) (imag-part z))))

(define (log z)
  (if (and (real? z) (>= z 0))
      ($log z)
      (make-rectangular ($log (magnitude z)) (angle z))))

(define (sqrt z)
  (if (real? z)
      (if (negative? z) (make-rectangular 0 ($sqrt (- z)))
	  ($sqrt z))
      (make-polar ($sqrt (magnitude z)) (/ (angle z) 2))))

(define expt
  (let ((integer-expt integer-expt))
    (lambda (z1 z2)
      (cond ((exact? z2)
	     (integer-expt z1 z2))
	    ((and (real? z2) (real? z1) (>= z1 0))
	     ($expt z1 z2))
	    (else
	     (exp (* z2 (log z1))))))))

(define (sinh z)
  (if (real? z) ($sinh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sinh x) ($cos y))
			  (* ($cosh x) ($sin y))))))
(define (cosh z)
  (if (real? z) ($cosh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cosh x) ($cos y))
			  (* ($sinh x) ($sin y))))))
(define (tanh z)
  (if (real? z) ($tanh z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cosh x) ($cos y))))
	(make-rectangular (/ ($sinh x) w) (/ ($sin y) w)))))

(define (asinh z)
  (if (real? z) ($asinh z)
      (log (+ z (sqrt (+ (* z z) 1))))))

(define (acosh z)
  (if (and (real? z) (>= z 1))
      ($acosh z)
      (log (+ z (sqrt (- (* z z) 1))))))

(define (atanh z)
  (if (and (real? z) (> z -1) (< z 1))
      ($atanh z)
      (/ (log (/ (+ 1 z) (- 1 z))) 2)))

(define (sin z)
  (if (real? z) ($sin z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sin x) ($cosh y))
			  (* ($cos x) ($sinh y))))))
(define (cos z)
  (if (real? z) ($cos z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cos x) ($cosh y))
			  (- (* ($sin x) ($sinh y)))))))
(define (tan z)
  (if (real? z) ($tan z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cos x) ($cosh y))))
	(make-rectangular (/ ($sin x) w) (/ ($sinh y) w)))))

(define (asin z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($asin z)
      (* -i (asinh (* +i z)))))

(define (acos z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($acos z)
      (+ (/ (angle -1) 2) (* +i (asinh (* +i z))))))

(define (atan z . y)
  (if (null? y)
      (if (real? z) ($atan z)
	  (/ (log (/ (- +i z) (+ +i z))) +2i))
      ($atan2 z (car y))))

(set! abs magnitude)

(define (log10 arg)
  (/ (log arg) (log 10)))

