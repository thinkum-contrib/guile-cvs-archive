(define-module (tcltk hand)
  :use-module (tcltk tcltk)
  :use-module (ice-9 threads))

(define pi 3.1416)

(define x0 100)
(define y0 100)
(define (x1 a) (+ x0 (* 100 ($cos a))))
(define (y1 a) (- x0 (* 100 ($sin a))))

(define angle (/ pi 2))
(define delta 0.005)
(define h #f)
(define c #f)

(define (setup-window)
  (tk-main-window "Hand" "200x200")
  (set! c (canvas #f))
  (pack c)
  (set! h (c 'create 'line x0 y0 (x1 angle) (y1 angle))))

(define moving? #t)

(define (hand-loop)
  (if moving?
      (begin
	(set! angle (+ angle delta))
	(c 'coords h x0 y0 (x1 angle) (y1 angle))
	(usleep 10000)
	(hand-loop))))

(define cv (make-condition-variable))

(define-public (hand)
  (setup-window)
  (begin-thread
   (catch #t
	  (lambda ()
	    (let ((mu (make-mutex)))
	      (lock-mutex mu)
	      (let loop ()
		(set! moving? #t)
		(hand-loop)
		(wait-condition-variable cv mu)
		(loop))))
	  (lambda args #f))))

(define-public (start)
  (signal-condition-variable cv))

(define-public (stop)
  (set! moving? #f))
