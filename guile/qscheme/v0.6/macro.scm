; -*- scheme -*-
;

; (define macro
;   (macro-set-code!
;    (make-macro)
;    (lambda (*macro-form*)
;	 (display "; macro: *macro-form*=") (print *macro-form*)
;	 (display "; macro: code=") (print `(lambda ( ,(cadr *macro-form*) ) ,@(cddr *macro-form*)))
; 	 `(macro-set-code!
; 	   (make-macro)
; 	   (lambda ( ,(cadr *macro-form*) ) ,@(cddr *macro-form*))))))

(define macro
  (make-macro
   (lambda (**macroform**)
	 `(make-macro (lambda ( ,(cadr **macroform**) )
					,@(cddr **macroform**))))))


(define define-macro
  (macro
   f
   (let ((formal (cadr f)) (body (cddr f)))
	 (let ((name (car formal))
		   (args (cdr formal)))
;	   (display "name=") (print name)
;	   (display "args=") (print args)
;	   (display "formal=") (print formal)
;	   (display "body=") (print body)

;	   (display "macro=")
;	   (print
;		`(define ,name
;		   (macro params (apply (lambda ,args ,@body) (cdr params)))))

	   (eval
		`(define ,name
		   (macro params (apply (lambda ,args ,@body) (cdr params))))
		(the-env))


;	   (display "function=")
;	   (print
;		(if (pair? args)
;		  `(macro-set-func! ,name (eval '(lambda ,args (,name ,@args))
;										(the-env)))
;		  `(macro-set-func! ,name (eval '(lambda ,args (apply ,name ,args))
;										(the-env)))))

;	   (eval
;		(if (pair? args)
;		  `(macro-set-func! ,name (eval '(lambda ,args (,name ,@args))
;										(the-env)))
;		  `(macro-set-func! ,name (eval '(lambda ,args (apply ,name ,args))
;										(the-env))))
;		(the-env))

	   #t
	   ))))



