;;; !!! obsolete, does no longer work. !!!

(use-modules (ice-9 time))
(use-modules (lightning))

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define-asm-macro (arg-prolog . args)
  (let ((n (length args)))
    (cons `(prolog ,n)
	  (map (lambda (name) `(arg ,name))
	       args))))

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

(define asm-inc (assemble `(  (arg-prolog n)
			      (getarg r1 n)
			      (scm-add-constfix ret r1 (scm 1))
			      (ret))))

(define asm-dec (assemble `(  (arg-prolog n)
			      (getarg r1 n)
			      (scm-add-constfix ret r1 (scm -1))
			      (ret))))

(define asm-fib (assemble `(fib
			      (arg-prolog n)
			      (getarg v0 n)
			      (scm-blt-constfix l0 v0 (scm 2) r0)
			      (scm-add-constfix r0 v0 (scm -2))
			      (prepare 1)
			      (pusharg r0)
			      (finish (label fib))
			      (retval v2)
 			      (prepare 1)
 			      (pusharg v0)
 			      (finish (proc ,asm-dec))
 			      (retval r0)
			      (prepare 1)
			      (pusharg r0)
			      (finish (label fib))
			      (retval v1)
			      (scm-add ret v1 v2)
			      (b l1)
			    l0
			      (mov ret (scm 400))
			    l1
			      (ret))))

(define asm-fixfib (assemble `(fib
			         (arg-prolog n)
				 (getarg v0 n)
				 (bge l0 v0 (scm 2))
				 (mov ret (scm 400))
				 (ret)
			       l0
				 (sub r0 v0 (scm 2))
				 (add r0 r0 (scm 0))
				 (prepare 1)
				 (pusharg r0)
				 (finish (label fib))
				 (retval v2)
				 (sub r0 v0 (scm 1))
				 (add r0 r0 (scm 0))
				 (prepare 1)
				 (pusharg r0)
				 (finish (label fib))
				 (retval v1)
				 (sub ret v2 (scm 0))
				 (add ret ret v1)
				 (ret))))

;(disassemble asm-fib)
