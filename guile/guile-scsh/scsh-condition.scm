;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Add scsh conditions to s48.

;;; A syscall-error condition-type:

;;(define-condition-type 'syscall-error '(error))

;;(define syscall-error? (condition-predicate 'syscall-error))

(define (errno-error errno syscall . stuff)
  (let ((msg (errno-msg errno)))
    (scm-error 'system-error syscall "%s" msg (list errno))))

(define (with-errno-handler* handler thunk)
  (catch 'system-error
	 thunk
	 (lambda args
	   (let ((errno (car (list-ref args 4)))
		 (message (car (list-ref args 3)))
		 (subr (list-ref args 1)))
	   (handler errno (list message
				subr
				'()))	; data
	   (throw 'system-error subr "%s" (list-ref args 3) #f)))))
		      

;;; (with-errno-handler
;;;   ((errno data) ; These are vars bound in this scope.
;;;    ((errno/exist) . body1)
;;;    ((errno/wouldblock errno/again) . body2)
;;;    (else . body3))
;;; 
;;;   . body)

(define-syntax with-errno-handler
  (lambda (exp rename compare)
    (let* ((%lambda (rename 'lambda))
	   (%cond (rename 'cond))
	   (%else (rename 'else))
	   (%weh (rename 'with-errno-handler*))
	   (%= (rename '=))		 
	   (%begin (rename `begin))
	   (%or (rename `or))
	   (%call/cc (rename 'call-with-current-continuation))
	   (%cwv (rename 'call-with-values))

	   (%ret (rename 'ret)) ; I think this is the way to gensym.

	   (err-var (caaadr exp))
	   (data-var (car (cdaadr exp)))
	   (clauses (cdadr exp))
	   (body (cddr exp))

	   (arms (map (lambda (clause)
			(let ((test (if (compare (car clause) %else)
					%else
					(let ((errs (car clause)))
					  `(,%or . ,(map (lambda (err)
							   `(,%= ,err ,err-var))
							 errs))))))
			  `(,test
			    (,%cwv (,%lambda () . ,(cdr clause)) ,%ret))))
		      clauses)))

      `(,%call/cc (,%lambda (,%ret)
         (,%weh
	    (,%lambda (,err-var ,data-var)
	      (,%cond . ,arms))
	    (,%lambda () . ,body)))))))

;;;; S48 already has this machinery, i.e., (SET-INTERACTIVE?! flag)
;;;; Interactive => breakpoint on errors.
;;;; Noninteractive => exit on errors.
;
;(define $interactive-errors? (make-fluid #f))
;
;(define (with-interactive-errors val thunk)
;  (let-fluid $interactive-errors? val thunk))
;
;(define (set-interactive-errors! val)
;  (set-fluid! $interactive-errors? val))
;
;;;; Just quit if non-interactive. Otherwise, punt to next handler.
;;;; A hack, because we use the default handler for the interactive
;;;; case.
;
;(define (scsh-error-handler condition more)
;  (if (and (error? condition)
;	   (not (fluid $interactive-errors?)))
;      (begin (display condition (error-output-port))
;	     (exit -1))
;      (more)))
