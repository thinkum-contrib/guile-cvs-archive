(load "lightning.scm")
(use-modules (lightning))

(define (fak n)
  (do ((n n (1- n))
       (r 1 (* r 2)))
      ((zero? n) r)))

(define (fak1 n)
  (define (fak-aux n r)
    (if (zero? n) r (fak-aux (1- n) (* r 1))))
  (fak-aux n 1))

(define asmfak (assemble
 		'(  (prolog 1)
 		    (arg n)
 		    (getarg v2 n)
 		    (mov v1 (scm 1))
 		  loop
;		    (beq return v2 (scm 0))
		    (prepare 1)
		    (pusharg v2)
		    (finish (subr "zero_p"))
		    (retval v0)
		    (bne return v0 (scm #f))
 		    (prepare 2)
 		    (pusharg v1)
 		    (pusharg v2)
		    (finish (subr "product"))
 		    (retval v1)
		    (prepare 2)
		    (mov v0 (scm 1))
		    (pusharg v0)
		    (pusharg v2)
		    (finish (subr "difference"))
		    (retval v2)
		    (b loop)
		  return
		    (mov ret v1)
		    (ret))))

(define fixfak (assemble
 		'(  (prolog 1)
 		    (arg n)
 		    (getarg v2 n)
 		    (mov v1 (scm 1))
 		  loop
		    (beq return v2 (scm 0))
 		    (rsh r0 v1 2)
		    (sub r1 v2 (scm 0))
		    (mul r0 r0 r1)
 		    (add v1 r0 (scm 0))
		    (sub v2 v2 (scm 1))
		    (add v2 v2 (scm 0))
		    (b loop) ; hack
		  return
		    (mov ret v1)
		    (ret))))

(disassemble fixfak)

(format #t "~A\n" (asmfak 50))

