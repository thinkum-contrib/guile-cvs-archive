;;; {Hooks}
;;;
;;; Warning: Hooks are now first class objects and add-hook! and remove-hook!
;;; procedures.  This interface is only provided for backward compatibility
;;; and will be removed.
;;;
(module-open (ice-9 guile))
(if (not (defined? new-add-hook!))
    (begin
      (define new-add-hook! add-hook!)
      (define new-remove-hook! remove-hook!)))

(define (run-hooks hook)
  (if (and (pair? hook) (eq? (car hook) 'hook))
      (run-hook hook)
      (for-each (lambda (thunk) (thunk)) hook)))

(define *suppress-old-style-hook-warning* #f)

(define abort-hook (make-hook))



