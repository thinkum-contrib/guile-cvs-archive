;(define dsin (make-extfunc "" <double> "sin" (list <double>)))
(define (loop i l) (if (>= i l) i (begin (sin i) (loop (+ 1 i) l))))
(loop 0 1000000)
