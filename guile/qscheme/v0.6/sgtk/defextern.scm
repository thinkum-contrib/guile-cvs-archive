;; -*- scheme -*-

(define *lib* "")

(define-macro (extern type name . args)
  `(make-extfunc *lib* ,type ,(symbol->string name) (quote ,@args)))

; simplified exteral declarator:
;
; (define-extern :RETURN-TYPE CFUNC-NAME :ARG1 :ARG2... [. :ANY])
;
(define (cname->scheme x)
  (string->symbol (string-translate (symbol->string x) "_" "-")))

(define-macro (define-extern t n . a)
; (write
  `(define ,(cname->scheme n)
	 (make-extfunc *lib* ,t ,(symbol->string n) (quote ,a))))
; )


