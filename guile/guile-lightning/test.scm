(load "lightning.scm")
(use-modules (lightning))

(define c (assemble '((leaf 1)
		      (arg n)
		      (getarg r0 n)
		      (add ret r0 4)
		      (ret))))

(disassemble c)

(format #t "~A\n" (c 2))
