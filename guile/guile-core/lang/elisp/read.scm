(define-module (lang elisp read)
  #:use-module (lang elisp fset))

;;; MEGA HACK!!!!

(fset 'read (lambda (str)
	      (cond ((string=? str "?\\M-\\^@")
		     -134217728)
		    (else
		     (with-input-from-string str read)))))
