(define-module (lang elisp lists)
  #:use-module (lang elisp fset))

(let ((null (lambda (l)
	      (or (not l)
		  (null? l)))))

  (fset 'cons
	(lambda (x y)
	  (cons x (or y '()))))

  (fset 'null null)

  (fset 'not null)

  (fset 'car
	(lambda (l)
	  (if (null l)
	      #f
	      (car l))))

  (fset 'cdr
	(lambda (l)
	  (if (null l)
	      #f
	      (cdr l))))

  (fset 'eq
	(lambda (x y)
	  (or (eq? x y)
	      (and (null x) (null y)))))

  (fset 'setcar set-car!)

  (fset 'setcdr
	(lambda (cell newcdr)
	  (set-cdr! cell
		    (if (null newcdr)
			'()
			newcdr))))

)
