;;; {Features}
;;
(module (ice-9 provide)
	(open (ice-9 guile))
	(export provide feature?))

(define (provide sym)
  (if (not (memq sym *features*))
      (set! *features* (cons sym *features*))))

(define (feature? feature)
  (and (memq feature *features*) #t))

