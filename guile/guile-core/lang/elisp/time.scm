(define-module (lang elisp time)
  #:use-module (lang elisp fset))

(fset 'current-time
      (lambda ()
	(let ((now (current-time)))
	  (list (ash now -16)
		(logand now (- (ash 1 16) 1))
		0))))
