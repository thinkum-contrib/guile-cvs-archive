(define-module (lang elisp features)
  #:use-module (lang elisp fset))

(define features '())

(define (provide feature)
  (or (memq feature features)
      (set! features (cons feature features))))

;;; {Elisp Exports}

(export features)

(fset 'provide provide)
