;; Peephole optimizer for compiler.scm

(define-module (lightning peephole)
  :use-module (ice-9 common-list))

(export peephole-optimize)

;; Very simple implementation.  I have this only because I don't want
;; to look at the crappy register spilling code that the compiler
;; emits.

;; XXX - should really use a more general pattern matcher for this.

;; This tries to match PATTERN against CODE.  PATTERN is a list of
;; instruction patterns, and each of these insn pats must match the
;; corresponding instruction in code.  Each instruction pattern is a
;; cons-tree with symbols as leaves.  The instruction in code must
;; have the same shape and matching leaves, except for variables in
;; the pattern.  A variable is a symbol starting with "?".
;;
;; Special variables are "?" (anonymous), "?R..." (must match register),
;; "?N.." (must match number), "?L" (must match label).
;;
;; MATCH-INSNS returns a alist of pattern variables and their values
;; when there is a match, or #f otherwise.

(define (match-insns pattern code)

  (define (enter-variable vals var val)
    (cond ((eq? var '?)
	   vals)
	  ((and (char=? (string-ref (symbol->string var) 1) #\R)
		(not (memq val '(r0 r1 r2 v0 v1 v2))))
	   #f)
	  ((and (char=? (string-ref (symbol->string var) 1) #\N)
		(not (number? val)))
	   #f)
	  ((and (char=? (string-ref (symbol->string var) 1) #\L)
		(not (symbol? val)))
	   #f)
	  (else
	   (let ((c (assq var vals)))
	     (if (not c)
		 (acons var val vals)
		 (if (equal? val (cdr c))
		     vals
		     #f))))))

  (define (match1 vals p i)
    (cond ((not vals)
	   #f)
	  ((symbol? p)
	   (if (char=? (string-ref (symbol->string p) 0) #\?)
	       (enter-variable vals p i)
	       (if (eq? p i)
		   vals
		   #f)))
	  ((pair? p)
	   (and (pair? i)
		(match1 (match1 vals (car p) (car i))
			(cdr p) (cdr i))))
	  (else
	   (if (eqv? p i)
	       vals
	       #f))))

  (let loop ((vals '())
	     (pat pattern)
	     (co code))
    (cond ((or (not vals) (null? pat))
	   (if vals
	       (pk 'match pattern 'on (list-head code (length pattern))
		   'with vals))
	   vals)
	  ((null? co)
	   #f)
	  (else
	   (loop (match1 vals (car pat) (car co))
		 (cdr pat)
		 (cdr co))))))

;; Fill a template by replacing `?' variables with values from VALS.

(define (fill-templ vals templ)
  (define (get-value var)
    (let ((c (assq var vals)))
      (if c
	  (cdr c)
	  (error "undefined pattern variable:" var))))
  (cond ((symbol? templ)
	 (if (char=? (string-ref (symbol->string templ) 0) #\?)
	     (get-value templ)
	     templ))
	((pair? templ)
	 (cons (fill-templ vals (car templ))
	       (fill-templ vals (cdr templ))))
	(else
	 templ)))

;; We transform the following patterns
;;
;; (mov A A)
;; =>
;; nothing
;;
;; (mov A REG)
;; (push A)
;; (die A)
;; =>
;; (push REG)
;;
;; (mov A REG)
;; (stx B C A)
;; (die A)
;; =>
;; (stx B C REG)
;;
;; (ldx A B C)
;; (stx C B A)
;; (die A)
;; =>
;; nothing

(define optimizations '())

(define (register-procedure-optimization pat proc)
  ;; XXX - hash on first instruction in PAT.
  (set! optimizations (cons (cons pat proc) optimizations)))

(define (register-template-optimization pat tmp)
  (register-procedure-optimization pat
				   (lambda (vals code all-code)
				     (append! (pk 'yielding
						  (fill-templ vals tmp))
					      (list-tail code (length pat))))))

(define-macro (defopt pat tmp)
  `(register-template-optimization ',pat ',tmp))

(define-macro (defopt-proc pat . body)
  `(register-procedure-optimization ',pat 
				    (lambda (vals code all-code)
				      ,@body)))
(defopt
  ((mov ?R ?R))
  ;;
  ())

(defopt
  ((mov ?R1 ?R2)
   (push ?R1)
   (die ?R1))
  ;;
  ((push ?R2)))

(defopt
  ((mov ?R1 ?R2)
   (stx ?A ?B ?R1)
   (die ?R1))
  ;;
  ((stx ?A ?B ?R2)))

(defopt
  ((ldx ?A ?B ?C)
   (stx ?C ?B ?A)
   (die ?A))
  ;;
  ())

(define (find-label code)
  (let loop ((c code))
    (cond ((or (null? c) (symbol? (car c)))
	   c)
	  (else
	   (loop (cdr c))))))

(defopt-proc ((b ?)
	      (? . ?))
  ;; remove dead code after an unconditional branch
  (cons (car code)
	(find-label (cdr code))))

(defopt-proc ((jmp ?)
	      (? . ?))
  ;; remove dead code after an unconditional jump
  (cons (car code)
	(find-label (cdr code))))

(defopt
  ((b ?L)
   ?L)
  ;;
  (?L))

(defopt-proc (?L)
  ;; Remove unused labels
  (let* ((label (fill-templ vals '?L))
	 (use (find-if (lambda (insn)
			 (and (pair? insn) (eq? (cadr insn) label)))
		       all-code)))
    (if (not use)
	(cdr code)
	code)))

(define (peephole-optimize all-code)

  (define (optimize1 code)
    (let loop ((opts optimizations))
      (cond ((null? opts)
	     #f)
	    ((match-insns (car (car opts)) code)
	     => (lambda (vals)
		  ((cdr (car opts)) vals code all-code)))
	    (else
	     (loop (cdr opts))))))

  (let loop ((code all-code)
	     (res '())
	     (changed #f))
    (cond
     ((null? code)
      (let ((opt (reverse! res)))
	(if changed
	    (peephole-optimize opt)
	    opt)))
     ((optimize1 code)
      => (lambda (opt-code)
	   (if (not (eq? opt-code code))
	       (loop opt-code res #t)
	       (loop (cdr code) (cons (car code) res) changed))))
     (else
      (loop (cdr code) (cons (car code) res) changed)))))
	   
;   (define (is-seq ops code)
;     (let loop ((ops ops)
; 	       (code code))
;       (cond ((null? ops)
; 	     #t)
; 	    ((or (null? code) (not (pair? (car code)))
; 		 (not (eq? (caar code) (car ops))))
; 	     #f)
; 	    (else
; 	     (loop (cdr ops) (cdr code))))))

;   (define (reg? x)
;     (memq x '(r0 r1 r2 v0 v1 v2)))

;   (define (splice c1 c2)
;     (append! (reverse! c2) c1))

;      ((match-insns '((mov ?1 ?1)) code)
;       => (lambda (vals)
; 	   (loop (cdr code) opt #t)))
;      ((match-insns '((mov ?R1 ?R2)
; 		     (push ?R1)
; 		     (die ?R1))
; 		   code)
;       => (lambda (vals)
; 	   (loop (cdddr code)
; 		 (splice (fill-templ vals '((push ?R2)
; 					    (die ?R1)))
; 			 opt)
; 		 #t)))
; ;      ((and (is-seq '(mov push die) code)
; ; 	   (eq? (cadr (car code))
; ; 		(cadr (cadr code))
; ; 		(cadr (caddr code)))
; ; 	   (reg? (caddr (car code))))
; ;       (loop (cdddr code) (cons (caddr code)
; ; 			       (cons `(push ,(caddr (car code))) opt)) #t))
;      ((and (is-seq '(mov stx die) code)
; 	   (eq? (cadr (car code))
; 		(cadddr (cadr code))
; 		(cadr (caddr code)))
; 	   (reg? (caddr (car code))))
;       (let* ((old-stx (cadr code))
; 	     (new-stx `(stx ,(cadr old-stx) ,(caddr old-stx)
; 			    ,(caddr (car code)))))
; 	(loop (cdddr code) (cons (caddr code) (cons new-stx opt)) #t)))
;      ((and (is-seq '(ldx stx die) code)
; 	   (eq? (cadr (car code))
; 		(cadddr (cadr code))
; 		(cadr (caddr code)))
; 	   (eq? (caddr (car code))
; 		(caddr (cadr code)))
; 	   (eqv? (cadddr (car code))
; 		 (cadr (cadr code))))
;       (loop (cdddr code) (cons (caddr code) opt) #t))
;      (else
;       (loop (cdr code) (cons (car code) opt) changed)))))
