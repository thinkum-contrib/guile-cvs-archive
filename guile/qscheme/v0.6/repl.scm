; -*- scheme -*-
; New eval

; (define (neval x) (nexec (make-code (nasm (ncomp x '())) '())))

(define *repl-prompt* "\nok ")

(neval
 '(define (repl)
	(let ((input '()))
	  (display *repl-prompt*)
	  (set! input (read))
	  (if (not (null? input))
		  (begin (write (neval input '())) (repl))))))


				   
