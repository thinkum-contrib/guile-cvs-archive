;;; {Lists}
;;;
(module (ice-9 lists)
	(open (ice-9 guile))
	(export list-index make-list))
	      
(define (list-index l k)
  (let loop ((n 0)
	     (l l))
    (and (not (null? l))
	 (if (eq? (car l) k)
	     n
	     (loop (+ n 1) (cdr l))))))

(define (make-list n . init)
  (if (pair? init) (set! init (car init)))
  (let loop ((answer '())
	     (n n))
    (if (<= n 0)
	answer
	(loop (cons init answer) (- n 1)))))


