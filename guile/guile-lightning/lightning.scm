(define-module (lightning))

(export assemble disassemble make-closure
	register-asm-macro define-asm-macro)

(dynamic-call "scm_init_lightning" (dynamic-link "libguile-lightning"))

(define asm-macros (make-hash-table 31))

(define (register-asm-macro name transformer)
  (hashq-set! asm-macros name transformer))

(define (get-asm-macro name)
  (hashq-ref asm-macros name #f))

(define (asm-macroexpand insns)
  (let loop ((res '())
	     (insns insns))
    (cond 
     ((null? insns)
      (reverse! res))
     ((and (list? (car insns))
	   (get-asm-macro (caar insns)))
      => (lambda (transformer)
	   (loop res
		 (append! (apply transformer (cdar insns))
			  (cdr insns)))))
     (else
      (loop (cons (car insns) res)
	    (cdr insns))))))

(define prim-assemble assemble)

(define (assemble insns)
  (prim-assemble (asm-macroexpand insns)))

(define-macro (define-asm-macro head . body)
  `(register-asm-macro ',(car head) (lambda ,(cdr head) ,@body)))
