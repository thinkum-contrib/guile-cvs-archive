(use-modules (ice-9 time))
(use-modules (lightning assembler)
	     (lightning compiler))

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define numargs
  (make-closure
   (assemble '(  (pop r2)
		 (add sp sp r1)
		 (add ret r1 (scm 0))
		 (mov r1 4)
		 (jmp r2)))
   #f))

(define my-values
  (make-closure
   (assemble '(  (pop r2)
		 (beq l0 r1 0)
		 (ld r0 sp)
		 (b l1)
	       l0
		 (mov r0 (scm ,(if #f #f)))
	       l1
		 (add sp sp r1)
		 (jmp r2)))
   #f))

(define-asm-macro (scm-blt-constfix label a fix tmp1)
  (let ((l0 (gensym "ll"))
	(l1 (gensym "ll")))
    `(  (bmc ,l0 ,a (scm 0))
        (blt ,label ,a ,fix)
        (b ,l1)
      ,l0
        (prepare 2)
	(mov ,tmp1 ,fix)
	(pusharg ,tmp1)
	(pusharg ,a)
	(finish (subr "scm_less_p"))
	(retval ,tmp1)
	(bne ,label ,tmp1 (scm #f))
      ,l1)))

(define-asm-macro (scm-add-constfix res a fix)
  (let ((l0 (gensym "ll"))
	(l1 (gensym "ll"))
	(fix-sans-tag (logand #xffffffff (* 4 (cadr fix))))) ;; XXX
  `(  (bmc ,l0 ,a (scm 0))
      (mov ,res ,a)
      (boadd ,l0 ,res ,fix-sans-tag)
      (b ,l1)
    ,l0
      (prepare 2)
      (mov ,res ,fix)
      (pusharg ,res)
      (pusharg ,a)
      (finish (subr "scm_sum"))
      (retval ,res)
    ,l1)))

(define-asm-macro (scm-add res a b)
  (let ((l0 (gensym "ll"))
	(l1 (gensym "ll")))
    `(  (bmc ,l0 ,a (scm 0))
	(bmc ,l0 ,b (scm 0))
	(sub ,res ,a (scm 0))
	(boadd ,l0 ,res ,b)
	(b ,l1)
      ,l0
        (prepare 2)
	(pusharg ,b)
	(pusharg ,a)
	(finish (subr "scm_sum"))
	(retval ,res)
      ,l1)))

(define-asm-macro (check-args n name)
  `(  (beq argsok r1 ,(* 4 n))
      (prepare 1)
      (mov r0 ,name)
      (pusharg r0)
      (finish (subr "scm_error_num_args_subr"))
    argsok))

(define invoke-code
  (assemble `(  (bms l0 r0 6)
		(ld r2 r0)
		(bne l0 r2 (codetag))
		(ldx r2 r0 4)
		(ldx r0 r0 8)
		(jmp r2)
	      l0
	        (push r1)
		(prepare 3)
		(add r2 sp 8)
	        (push r2)
		(push r1)
		(push r0)
		(finish (subr "scm_invoke"))
		(pop r1)
		(pop r2)
		(add sp sp r1)
		(mov r1 4)
		(jmp r2))))

(define (var sym)
  `(var ,(module-variable (current-module) sym)))

;; Most general

(define-asm-macro (invoke sym)
  `((ld r0 ,(var sym))
    (call (code ,invoke-code))))

;; When proc is known to be one of our code smobs.

(define-asm-macro (fast-invoke sym)
  `((ld r0 ,(var sym))
    (ldx r2 r0 4)
    (ldx r0 r0 8)
    (call r2)))

(define asm-fib #f)

(define asm-fibvector
  (assemble `(  (push v0)
 		(push v1)
 		(push v2)
		(check-args 1 "asm-fib")
	        (ldx v0 sp ,(+ 4 (* 3 4)))
		(scm-blt-constfix l0 v0 (scm 2) r0)
		(scm-add-constfix r0 v0 (scm -2))
		(push r0)
		(mov r1 4)
		(fast-invoke asm-fib)
		(retval v2)
		(scm-add-constfix r0 v0 (scm -1))
		(push r0)
		(mov r1 4)
		(fast-invoke asm-fib)
		(mov r1 4)
		(retval v1)
		(scm-add ret v1 v2)
		(b l1)
	      l0
	        (mov ret (scm 1))
	      l1
 	        (pop v2)
 		(pop v1)
 		(pop v0)
 		(pop r2)
		(add sp sp 4)
 		(mov r1 4)
 		(jmp r2))))

(set! asm-fib (make-closure asm-fibvector #f))

(define asm-fib2 #f)

(define asm-fibvector2
  (assemble `(  (push v0)
 		(push v1)
 		(push v2)
		(check-args 1 "asm-fib")
	        (ldx v0 sp ,(+ 4 (* 3 4)))
		(mov r0 (scm 2))
		(push r0)
		(push v0)
		(mov r1 8)
		(invoke <)
		(bne l0 r0 (scm #f))
		(mov r0 (scm -2))
		(push r0)
		(push v0)
		(mov r1 8)
		(invoke +)
		(push r0)
		(mov r1 4)
		(invoke asm-fib2)
		(mov v2 r0)
		(mov r0 (scm -1))
		(push r0)
		(push v0)
		(mov r1 8)
		(invoke +)
		(push r0)
		(mov r1 4)
		(invoke asm-fib2)
		(push r0)
		(push v2)
		(mov r1 8)
		(invoke +)
		(b l1)
	      l0
	        (mov ret (scm 1))
	      l1
 	        (pop v2)
 		(pop v1)
 		(pop v0)
 		(pop r2)
		(add sp sp 4)
 		(mov r1 4)
 		(jmp r2))))

(set! asm-fib2 (make-closure asm-fibvector2 #f))

; (compile-show '(lambda-template (n)
; 	         (invoke (global +) (quote 1) (local n))))

; (define y #f)
; (compile-show '(lambda-template (n)
; 	         (if (invoke (global <) (local n) (quote 2))
; 		     (quote 1)
; 		     (invoke (global +)
; 			     (invoke (global y) 
; 				     (invoke (global +)
; 					     (local n) (quote -2)))
; 			     (invoke (global y)
; 				     (invoke (global +) 
; 					     (local n) (quote -1)))))))

(define code '(lambda-template (n)
		(labels ((loop ((i :reg 1)
				(sum :reg 1))
			       (invoke (global simple-format)
				       (quote #t)
				       (quote "~A\n")
				       (local sum))
			       (if (invoke (global <=)
					   (local i) (local n))
				   (goto loop
					 (invoke (global +)
						 (local i) (quote 1))
					 (invoke (global +)
						 (local i) (local sum)))
				   (local sum))))
			(goto loop (quote 1) (quote 0)))))

;(compile-show code)

;(define x (compile code))

(define (y n) 
  (let loop ((i 1)
	     (sum 0))
    (if (<= i n)
	(loop (+ i 1) (+ i sum))
	sum)))

; (compile-show '(lambda-template ()
; 	         (labels ((l1 (a)
; 			      (goto l2 (quote 1)))
; 			  (l2 (b)
; 			      (labels ((l3 (c)
; 					   (goto l4 (local b))))
; 				      (goto l3 (quote 1))))
; 			  (l4 (d)
; 			      (local d)))
; 			 (goto l1 (quote 0)))))

; (define sum-ints (compile
; 		  '(lambda-template (n)
; 		     (labels ((loop (i sum)
; 				    (if (invoke (global <=)
; 						(local i) (local n))
; 					(goto loop
; 					      (invoke (global +)
; 						      (local i) (quote 1))
; 					      (invoke (global +)
; 						      (local i) (local sum)))
; 					(local sum))))
; 			     (goto loop (quote 1) (quote 0))))))

(define code2
  '(lambda-template (n)
    (labels ((outer ((i :reg 1))
		    (if (invoke (global <=) (local i) (quote 0))
			(quote #t)
			(begin 
			  (labels ((loop ((i :reg 1) (sum :reg 1))
					 (if (invoke (global <=)
						     (local i) (quote 10000))
					     (goto loop
						   (invoke (global +)
							   (local i)
							   (quote 1))
						   (invoke (global +)
							   (local i)
							   (local sum)))
					     (local sum))))
				  (goto loop (quote 1) (quote 0)))
			  (goto outer (invoke (global -)
					      (local i)
					      (quote 1)))))))
	     (goto outer (local n)))))

(define reverse-map-code
  '(lambda-template (p l)
     (labels ((loop ((l :reg 1) (x :reg 1))
		    (if (invoke (global pair?)
				(local l))
			(goto loop
			      (invoke (global cdr)
				      (local l))
			      (invoke (global cons)
				      (invoke (local p)
					      (invoke (global car)
						      (local l)))
				      (local x)))
			(local x))))
	     (goto loop (local l) (quote ())))))

;(define reverse-map (compile reverse-map-code))