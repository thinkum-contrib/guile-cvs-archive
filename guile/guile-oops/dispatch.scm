(define hashsets 8)
(define hashset-index 10)

(define hash-threshold 4)
(define initial-hash-size 4) ;must be >= hash-threshold

;;; Method cache

;; (#@dispatch N-SPECIALIZED #((TYPE1 ... FORMALS ENV FORM1 ...) ...))
;; (#@hash-dispatch N-SPECIALIZED HASHSET MASK
;;                  #((TYPE1 ... FORMALS ENV FORM1 ...) ...))

;;; Representation

(define (method-cache-hashed? x)
  (integer? (caddr x)))

(define method-cache-n-specialized cadr)
(define (set-method-cache-n-specialized! exp n)
  (set-car! (cdr exp) n))
(define method-cache-entries caddr)

(define (n-cache-methods entries)
  (let ((len (vector-length entries)))
    (do ((i 0 (+ 1 i)))
	((or (= i len) (not (car (vector-ref entries i))))
	 i))))

(define (method-cache-n-methods exp)
  (n-cache-methods (method-cache-entries exp)))

(define (cache-methods entries)
  (do ((i (- (vector-length entries) 1) (- i 1))
       (methods '() (let ((entry (vector-ref entries i)))
		      (if (car entry) (cons entry methods) methods))))
      ((< i 0) methods)))

(define (method-cache-methods exp)
  (cache-methods (method-cache-entries exp)))

(define d #f)

(define (method-cache-insert! exp entry)
  (set! d exp)
  (vector-set! (method-cache-entries exp)
	       (method-cache-n-methods exp)
	       entry)
  ;;(write-line (list 'method-cache-insert! '--> d))
  )

(define (set-hashed-method-cache-hashset! exp hashset)
  (set-car! (cddr exp) hashset))

(define (set-hashed-method-cache-mask! exp mask)
  (set-car! (cdddr exp) mask))

(define (hashed-method-cache-entries exp)
  (list-ref exp 4))

(define (set-hashed-method-cache-entries! exp entries)
  (set-car! (cddddr exp) entries))

(define (hashed-method-cache-insert! exp entry)
  (let* ((cache (hashed-method-cache-entries exp))
	 (size (vector-length cache)))
    (let* ((entries (cons entry (cache-methods cache)))
	   (size (if (<= (length entries) size)
		     size
		     (let ((new-size (* 2 size)))
		       (set-hashed-method-cache-mask! exp (- new-size 1))
		       new-size)))
	   (min-misses size)
	   (best #f))
      (do ((hashset 0 (+ 1 hashset)))
	  ((= hashset hashsets))
	;;(write-line (list "*** hashset " hashset))
	(let* ((test-cache (make-vector size '(#f)))
	       (misses (cache-try-hash! hashset test-cache entries)))
	  (if (< misses min-misses)
	      (begin
		(set! min-misses misses)
		(set! best hashset)
		(set! cache test-cache)))))
      ;;(write-line (list "=== hashset " best " was selected"))
      (set-hashed-method-cache-hashset! exp best)
      (set-hashed-method-cache-entries! exp cache))))

;;; Caching

(define environment? pair?)

(define (cache-hashval hashset entry)
  (let ((hashset-index (+ hashset-index hashset)))
    (do ((sum 0)
	 (classes entry (cdr classes)))
	((environment? (car classes)) sum)
      (set! sum (+ sum (struct-ref (car classes) hashset-index))))))

(define (cache-try-hash! hashset cache entries)
  (let ((misses 0)
	(mask (- (vector-length cache) 1)))
    (do ((ls entries (cdr ls)))
	((null? ls) misses)
      (do ((i (logand mask (cache-hashval hashset (car ls)))
	      (logand mask (+ i 1))))
	  ((not (car (vector-ref cache i)))
	   ;;(write-line (list (car ls) 'at i))
	   (vector-set! cache i (car ls)))
	;;(write-line (list i 'occupied))
	(set! misses (+ 1 misses))))))

(define e (list (list <generic> <number> '(()))
		(list <number> <real> <pair> '(()))
		(list <integer> '(()))
		(list <integer> <integer> '(()))))

(define (make-method-cache)
  `(@dispatch 0 ,(make-vector initial-hash-size '(#f))))

(define (method-cache->hashed! exp)
  (set-cdr! (cdr exp) (cons 0 (cons (- initial-hash-size 1) (cddr exp))))
  (set-car! exp '@hash-dispatch)
  exp)

(define (method-cache-install! insert! exp args applicable)
  (let* ((specializers (method-specializers (car applicable)))
	 (n-specializers (if (list? specializers)
			     (length specializers)
			     (min (+ 1 (length* specializers))
				  (length args)))))
    (if (> n-specializers (method-cache-n-specialized exp))
	(set-method-cache-n-specialized! exp n-specializers))
    (let ((types (map class-of (first-n args n-specializers))))
      (insert! exp (method-entry applicable types)))))

;;; Memoization

(define (memoize-method! gf args exp)
  ;;*fixme* Want to use generic cam.  How to avoid looping?
  (let ((applicable (%compute-applicable-methods gf args)))
    (cond ((not applicable)
	   (no-applicable-method gf args))
	  ((method-cache-hashed? exp)
	   (method-cache-install! hashed-method-cache-insert!
				  exp args applicable))
	  ((>= (+ 1 (method-cache-n-methods exp)) hash-threshold)
	   (method-cache-install! hashed-method-cache-insert!
				  (method-cache->hashed! exp)
				  args
				  applicable))
	  (else
	   (method-cache-install! method-cache-insert! exp args applicable)))))

;;; Compilation

(define (method-entry methods types)
  (cond ((assoc types (slot-ref (car methods) 'code-table))
	 => (lambda (types-code) (cdr types-code)))
	(else (let ((method (car methods))
		    ;;*fixme* new seek primitive => no double types
		    (entry (append types (compile-method methods types))))
		(slot-set! (car methods) 'code-table
			   (acons types entry
				  (slot-ref (car methods) 'code-table)))
		entry))))

(define source-formals cadr)
(define source-body cddr)

(define code-code cdr)
(define code-environment car)

(define (make-final-make-next-method method)
  (lambda default-args
    (lambda args
      (@apply method (if (null? args) default-args args)))))	  

(define (make-make-next-method vcell methods types)
  (lambda default-args
    (lambda args
      (let* ((code (compile-method methods types))
	     (method (local-eval (cons 'lambda (code-code code))
				 (code-environment code))))
	(set-cdr! vcell (make-final-make-next-method method))
	(@apply method (if (null? args) default-args args))))))

(define (compile-method methods types)
  (let* ((proc (method-procedure (car methods)))
	 (src (procedure-source proc))
	 (formals (cdr (source-formals src)))
	 (vcell (cons 'goops:make-next-method #f)))
    (set-cdr! vcell (make-make-next-method vcell (cdr methods) types))
    ;;*fixme*
    `(,(cons vcell (procedure-environment proc))
      ,formals
      ;;*fixme* Only do this on source where next-method can't be inlined
      (let ((next-method ,(if (list? formals)
			      `(goops:make-next-method ,@formals)
			      `(apply goops:make-next-method
				      ,@(improper->proper formals)))))
	,@(source-body src)))))

;;; Utilities

(define (length* ls)
  (do ((n 0 (+ 1 n))
       (ls ls (cdr ls)))
      ((not (pair? ls)) n)))

(define (improper->proper ls)
  (if (pair? ls)
      (cons (car ls) (improper->proper (cdr ls)))
      (list ls)))

(define (first-n ls n)
  (if (zero? n)
      '()
      (cons (car ls) (first-n (cdr ls) (- n 1)))))

(define-generic foo)

(set-object-procedure! foo
		       (lambda (g) args)
		       (lambda (g a1) (@dispatch 0 #((#f) (#f) (#f) (#f))))
		       (lambda (g a1 a2) (@dispatch 0 #((#f) (#f) (#f) (#f))))
		       (lambda (g a1 a2 a3 . rest) (@dispatch 0 #((#f) (#f) (#f) (#f)))))

(define (apply-0-arity-method gf)
  ;;*fixme* Want to use generic cam.  How to avoid looping?
  (let ((applicable (%compute-applicable-methods gf '())))
    (if (not applicable)
	(no-applicable-method gf '())
	(let ((code (compile-method applicable '())))
	  (set-object-procedure! gf (local-eval (code-code code)
						(code-environment code)))
	  (gf)))))

(define (make-apply-generic)
  (let ((e (the-environment)))
    (list apply-0-arity-method
	  (local-eval `(lambda (gf a1) ,(make-method-cache)) e)
	  (local-eval `(lambda (gf a1 a2) ,(make-method-cache)) e)
	  (local-eval `(lambda (gf a1 a2 a3 . rest) ,(make-method-cache)) e))))
