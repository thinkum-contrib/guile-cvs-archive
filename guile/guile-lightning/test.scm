(use-modules (ice-9 time))
(use-modules (lightning))

(define numargs (make-closure
		 (assemble '(  (pop r2)
			       (add sp sp r1)
			       (add ret r1 (scm 0))
			       (mov r1 4)
			       (jmp r2)))
		 #f))

(define lvalues (make-closure
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
