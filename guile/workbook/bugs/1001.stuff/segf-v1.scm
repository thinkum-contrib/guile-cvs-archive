;;; see notes

(define (assignments commandls)
  (let	((env (the-environment )))
  (for-each
   (lambda (triplet)
     (let ((x (car triplet))
	   (y (cadr triplet))
	   (z (caddr triplet)))
       (local-eval `(define ,x (option-ref options ,y ,z)) env)))

   '((a b c)(d e f)))))
