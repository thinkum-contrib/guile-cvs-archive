(define-module (lang elisp system)
  #:use-module (lang elisp fset))

(fset 'system-name
      (lambda ()
	(vector-ref (uname) 1)))

(define-public system-type
  (let ((uname (vector-ref (uname) 0)))
    (if (string=? uname "Linux")
	"gnu/linux"
	uname)))
