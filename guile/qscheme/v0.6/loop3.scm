;(neval '(define (loop i l) (if (>= i l) i (loop (+ i 1) l))))
(neval '(define (loop i l) (if (< i l) (loop (+ i 1) l))))
(loop 0 3000000)
