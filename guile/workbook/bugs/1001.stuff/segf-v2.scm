;;; see notes

(define (fnc args)
  (local-eval  '(define a b) (the-environment)))
