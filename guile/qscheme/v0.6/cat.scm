#! qscheme -*- scheme -*-

(define (print-file fname)
  (let ((port (open-input-file fname))
		(buf  (make-string 0)))
	(while (read-line buf port)
		   (print buf))
	(close-port port)))

(define (fprint-file fname)
  (let ((fd (fopen fname "r"))
		(buf (make-string 0)))
	(while (positive? (fgetline buf fd))
		   (fwrite buf stdout))
	(fclose fd)))

(define (doit)
  (let* ((i 0)
		 (l (vector-length argv)))
	(while (< i l)
		   (print-file (vref argv i))
		   (set! i (+ i 1)))))
(doit)
