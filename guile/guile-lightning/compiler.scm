;;; Very simple minded compiler for a language quite different from
;;; Scheme.

;; The language:
;;
;; Fully parenthesized prefix syntax with nothing but special forms.
;;
;; Special forms are
;;
;; - (lambda-template (ARGS... :rest REST :env ENV) BODY...)
;;
;;   ARGS, REST and ENV are symbols and BODY is a list of expressions.
;;   Both `:rest' and `:env' are optional.
;;
;;   Makes a closure template, argument ENV refers to the
;;   environment given to MAKE_CLOSURE, below.  Closure templates
;;   can not be invoked.  Rest argument denoted by `:rest'.
;;
;;   [ This is not the most expressive way to handle arguments.  Maybe
;;     have explicit (ref-arg N) and (args-list N) etc forms? ]
;;
;; - (make-closure TEMPLATE ENV)
;;
;;   Make a closure form a closure template and an environment.  A
;;   closure can be invoked.  ENV can be anything.
;;
;; - (if TEST THEN ELSE)
;;
;;   The usual conditional.  Else clause is mandatory.
;;
;; - (quote OBJ)
;;
;;   Refers to the Scheme object OBJ, which can be anything.
;;
;; - (local SYMBOL)
;;
;;   Retrieve the value of the local variable SYMBOL, as established
;;   by LOCALS or LAMBDA-TEMPLATE.
;;
;; - (set-local SYMBOL VAL)
;;
;;   Set the value of the local variable SYMBOL, as established
;;   by LOCALS or LAMBDA-TEMPLATE.
;;
;;   Note that SET-LOCAL does not interact correctly with call/cc.
;;   The values of locals are copied by call/cc and any changes to
;;   them between the call to call/cc and when the continuation is
;;   invoked are lost.
;;  
;; - (global SYMBOL)
;;
;;   Retrieve the value of the global variable that is named by symbol
;;   in the current module.  (Current at the time of linking.)
;;
;; - (invoke PROC ARGS...)
;;
;;   Invokes PROC with ARGS.  PROC can evaluate to any Scheme object.
;;   However, when PROC is of the form `(global VAR)', we cooperate
;;   with the module system to generate a more efficient calling
;;   sequence.
;;
;; - (inline OP ARGS...)
;;
;;   Generate inline code for OP and ARGS.  OP must have been
;;   registered with the compiler previously as an inline-operator.
;;   Typical OPs are `+', `car', etc.
;;
;; - (labels ((LABEL (ARGS...) BODY) ...) BODY)
;;
;;   Establish labels that can be jumped to with GOTO, which see.  The
;;   labels are visible within all of the BODYs in the labels form.
;;   The continuation of each BODY is the continuation of the LABELS
;;   form.
;;
;;   ARGS can be of the form (SYMBOL :reg N) to indicate a register
;;   preference level.  Just SYMBOL is equivalent to (SYMBOL :reg 0).
;;
;; - (goto LABEL ARGS...)
;;
;;   Transfer control to LABEL, passing it ARGS.
;;
;; - (functions ((FUNC (ARGS...) BODY) ...) BODY)
;;
;;   Like `labels', but you need to use `call' instead of `goto'.
;;
;; - (call FUNC args...)
;;
;;   Invoke FUNC, passing it ARGS.
;;
;;
;; Example:
;;
;;   (lambda-template (n)
;;     (if (inline < (local n) (quote 2))
;;         (quote 1)
;;         (inline + (invoke (global fib) (inline - (local n) (quote 2)))
;;                   (invoke (global fib) (inline - (local n) (quote 1))))))
;;
;;
;; Note that there is no `let' equivalent.  Use `labels' instead.
;;
;; A `functions' form where all `calls' appear in tail positions is
;; semantically equivalent to a `labels' form, but the compiler
;; doesn't detect this.  It still emits code to handle the general
;; code.  You have to help it by explicitely using a `labels' form
;; when possible.  This might change in the future, but right now,
;; `functions' isn't even implemented at all.
;;
;; In general, this compiler does no optimizations that could have
;; been performed on its input source.  The expertise of this compiler
;; should be argument shuffling to implement tail calls and
;; parameter-passing gotos, branch optimizations, (simple) register
;; allocation, and inlining of certain operations like fixnum
;; arithmetic and cons cell walking.
;;
;; Things like closure-conversion, removal of explicit lambdas etc are
;; left to the upper layers.

;; Jobs for the peephole optimizer:
;;
;; - dead code removal
;; - branch contractions (with folding of conditions)
;; - various ad-hoc optimizations based on experience
;; - ...

;; TODO:
;;
;; - register allocation
;; - find common tails in `labels'
;; - tail calling
;; - rest args
;; - inline ops
;; - peephole optimizer

(read-set! keywords 'prefix)

(define-module (lightning compiler)
  :use-module (lightning assembler)
  :use-module (oop goops)
  :use-module (ice-9 receive)
  :use-module (ice-9 common-list))

(export compile-to-asm compile-show compile)

(defmacro define-struct (name base-and-options . fields)
  (define (field-name f)
    (if (list? f) (car f) f))
  (define field-defaulted? list?)
  (define field-default cadr)
  (define (accessors)
    (map (lambda (f)
	   (symbol-append name '- (field-name f)))
	 fields))
  (define (field->slot f)
    (let ((fn (field-name f)))
      ;; XXX - keep synchronized with copy-struct below
      `(,fn :accessor ,(symbol-append name '- fn)
	    :init-keyword ,(symbol->keyword fn)
	    ,@(if (field-defaulted? f) 
		  `(:init-value ,(field-default f)) 
		  '()))))
  (let loop ((base '())
	     (bo base-and-options))
    (if (or (null? bo) (keyword? (car bo)))
	`(begin
	   (export ,name ,@(accessors) ,(symbol-append name '?))
	   (define-class ,name ,(reverse! base) 
	     ,@(map field->slot fields)
	     ,@bo)
	   (define (,(symbol-append name '?) x) (is-a? x ,name)))
	(loop (cons (car bo) base) (cdr bo)))))

(define (list-front! start end)
  (let loop ((l start))
    (cond
     ((eq? (cdr l) end)
      (set-cdr! l '()))
     (else
      (loop (cdr l)))))
  start)

(define (peek . stuff)
  (display ";;; ")
  (write stuff)
  (newline)
  (car (last-pair stuff)))

(set! pk peek)

(define (form? sym exp)
  (and (pair? exp) (eq? sym (car exp))))

;;; The compilation environment

;; The environment describes the stack at a certain point in the code.
;; It includes the value of the stack pointer and the locations of the
;; local variables (stack offset or register).  It also includes
;; information about labels so that the stack can be correctly unwound
;; when jumping to a label.

(define (make-empty-env)
  '())

(define (extend-env frame env)
  (cons frame env))

;; We have pushed a new local variable VAR
;;
(define (env-push-local var env)
  (extend-env `(push ,var) env))

;; We have just pushed some unnamed value
;;
(define (env-push env)
  (env-push-local #f env))

(define (env-stack-depth target? env)
  (let loop ((e env)
	     (offset 0))
    (cond ((target? e)
	   offset)
	  ((form? 'push (car e))
	   (loop (cdr e) (+ offset 4)))
	  ((form? 'labels (car e))
	   (loop (cdr e) offset))
	  (else
	   (error "unsupported environment frame:" (car e))))))

(define (lookup-local var env)
  (env-stack-depth (lambda (e)
		     (and (form? 'push (car e))
			  (eq? (cadr (car e)) var)))
		   env))

(define (unwind-env target-env env)
  (let ((offset (env-stack-depth (lambda (e)
				   (eq? e target-env))
				 env)))
    `((add sp sp ,offset))))

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

(define genlabel
  (let ((seqno 0))
    (lambda ()
      (set! seqno (1+ seqno))
      (string->symbol (string-append "l" (number->string seqno))))))

(define (compile-with-return exp env)
  `(,@(compile-expression exp env 'r0)
    ,@(compile-expression '(local :ret) env 'r2)
    ,@(unwind-env '() env)
    (mov r1 4)
    (jmp r2)))

(define (find-common-tail! l1 l2)
  (let loop ((r1 (reverse! l1))
	     (r2 (reverse! l2))
	     (t '()))
    (cond ((or (null? r1) (null? r2) (not (equal? (car r1) (car r2))))
	   (values (reverse! r1) (reverse! r2) t))
	  (else
	   (loop (cdr r1) (cdr r2) (cons (car r1) t))))))

;; Make an evironment frame for representing a `labels' statement.
;; It contains a list of subframes according to
;;
;;   (name label args)
;;
;; where NAME is the label from the statement, LABEL is a generated
;; unique assembler label, ARGS is the list of arguments of this
;; label.

(define (make-labels-frame labels)
  `(labels ,@(map (lambda (l)
		    (list (car l) (genlabel) (cadr l)))
		  labels)))

(define (find-labels-frame target env)
  (let loop ((e env))
    (cond ((null? e)
	   (error "undefined label:" target))
	  ((and (form? 'labels (car e))
		(assq target (cdr (car e))))
	   e)
	  (else
	   (loop (cdr e))))))

;; Find the free locals in EXP

(define (unions . lists) (reduce union lists))

(define (union-map p l) (apply unions (map p l)))

(define (free-locals exp)
  (cond
   ((form? 'global exp)
    '())
   ((form? 'quote exp)
    '())
   ((form? 'local exp)
    (list (cadr exp)))
   ((form? 'set-local exp)
    (list (cadr exp)))
   ((form? 'invoke exp)
    (union-map free-locals (cdr exp)))
   ((form? 'if exp)
    (union-map free-locals (cdr exp)))
   ((form? 'begin exp)
    (union-map free-locals (cdr exp)))
   ((form? 'goto exp)
    (union-map free-locals (cdr exp)))
   ((form? 'labels exp)
    (let ((labels (cadr exp))
	  (body (cddr exp)))
      (apply unions
	     (union-map free-locals body)
	     (map (lambda (l)
		    (let ((args (cadr l))
			  (body (cddr l)))
		      (set-difference (union-map free-locals body)
				      args)))
		  labels))))
   (else
    (error "unsupported form:" exp))))

(define-struct arg-node ()
  exp slot (conflicts '()) (id #f) (comp-id #f))

(define volatile-regs '(r0 r1 r2))

;; Generate code for pushing ARGS and simultanously unwinding the
;; stack to TARGET-ENV.

;; REG-ARGS is a list of (reg . exp) pairs, where REG is a symbol and
;; EXP is the expression that computes the value for that argument.
;; STACK-ARGS is just a list of expressions that will be pushed in
;; reverse order.

(define (compile-tail-args reg-args stack-args target-env env)

  ;; We partition the arguments into `easy' and `tough'.  An easy
  ;; argument is one that is in a stack slot above the current stack
  ;; pointer.  We can just push such an arg.

  ;; A tough argument will overwrite a stack slot that might be needed
  ;; by other arguments.  We find a suitable sequence of computing the
  ;; tough args by topological sorting them.  An argument is
  ;; represented by a node that has directed edges to all argument
  ;; nodes that need the slot stored into by the given argument.

  ;; Cycles in the graph are broken by selecting one node from the
  ;; strongly connected component, pushing it, and replacing it with
  ;; an expression that retrieves the pushed value.

  (define (push-conflict node conf)
    (set! (arg-node-conflicts node)
	  (cons conf (arg-node-conflicts node))))

  (define (make-nodes reg-args stack-args stack-offset env)
    (pk 'stack-offset stack-offset)
    (let ((nodes (let loop ((a stack-args)
			    (n '())
			    (slot stack-offset))
		   (cond ((null? a)
			  (append! 
			   (map (lambda (r)
				  (make arg-node :exp (cdr r) :slot (car r)))
				reg-args)
			   n))
			 (else
			  (loop (cdr a)
				(cons (make arg-node :exp (car a) :slot slot)
				      n)
				(+ slot 1)))))))
      (define (update-conflicts n target)
	(for-each (lambda (nn)
		    (if (and (not (eq? nn n))
			     (eqv? (arg-node-slot nn) target))
			(push-conflict nn n)))
		  nodes))
      (for-each (lambda (n)
		  (for-each (lambda (l)
			      (update-conflicts  n 
						 (/ (lookup-local l env) 4)))
			    (free-locals (arg-node-exp n)))
		  (for-each (lambda (r)
			      (update-conflicts n r))
			    volatile-regs))
		nodes)
      nodes))

  (define (show-nodes nodes)
    (for-each (lambda (n)
		(format #t "~A: ~S\n" (arg-node-slot n) (arg-node-exp n))
		(format #t " ~S\n" (map arg-node-slot (arg-node-conflicts n))))
	      nodes))

  (define comp-id #f)
  (define tough-code #f)
  (define tough-offset #f)
  (define tough-env #f)

  ;; Schedule the NODES without paying attention to connections to
  ;; already scheduled nodes.
  ;;
  (define (schedule-component nodes comp-id)
    (let ((stack '())
	  (id 0))

      (define (min-id m lst)
	(apply min m (pick number? lst)))

      (define (visit-node n)
	(if (= (arg-node-comp-id n) comp-id)
	    (visit n)
	    #t))

      (define (visit n)
	(or (arg-node-id n)
	    (begin
	      (set! (arg-node-id n) id)
	      (set! id (1+ id))
	      (let ((old-stack stack))
		(set! stack (cons n stack))
		(let ((m (min-id (arg-node-id n)
				 (map visit-node (arg-node-conflicts n)))))
		  (cond
		   ((= m (arg-node-id n))
		    (let ((comp (list-front! stack old-stack)))
		      (schedule-strongly-connected comp)
		      (set! stack old-stack)
		      ;; XXX - not needed because boxes now belong
		      ;; to another component anyway?
		      (for-each (lambda (n)
				  (set! (arg-node-id n) #t))
				comp))))
		  m)))))

      (for-each (lambda (n)
		  (set! (arg-node-comp-id n) comp-id)
		  (set! (arg-node-id n) #f))
		nodes)
      (for-each visit nodes)))

  ;; Schedule a strongly connected component.  We also come here for
  ;; components of a single element.
  ;;
  (define (schedule-strongly-connected comp)
    (let* ((arg (car comp))
	   (slot (arg-node-slot arg))
	   (target (if (number? slot) 'r0 slot))
	   (store-code (if (number? slot)
			   `((stx ,(+ tough-offset (* 4 slot)) sp r0))
			   `())))
      (set! tough-code
	    `(,@tough-code
	      ,@(compile-expression (arg-node-exp arg) tough-env target)))
      (cond ((null? (cdr comp))
	     ;; XXX - special casing this ought to be unnecessary when
	     ;; the peephole optimizer removes empty push/pop
	     ;; sequences.
	     (pk 'storing (arg-node-slot (car comp)))
	     (set! tough-code
		   `(,@tough-code
		     ,@store-code)))
	    (else
	     (pk 'pushing (arg-node-slot (car comp)))
	     (let ((env tough-env)
		   (offset tough-offset))
	       (set! tough-code
		     `(,@tough-code
		       (push r0)))
	       (set! tough-offset (+ 4 tough-offset))
	       (set! tough-env (env-push tough-env))
	       (set! comp-id (1+ comp-id))
	       (schedule-component (cdr comp) comp-id)
	       (set! tough-offset offset)
	       (set! tough-env env)        ;; XXX - dynamic scope
	       (pk 'popping (arg-node-slot (car comp)))
	       (set! tough-code
		     `(,@tough-code
		       (pop ,target)
		       ,@store-code)))))))

  (define (handle-tough-args reg-args stack-args stack-offset env)
    (let ((nodes (make-nodes reg-args stack-args stack-offset env)))
      (show-nodes nodes)
      (set! comp-id 0)
      (set! tough-code '())
      (set! tough-offset 0)
      (set! tough-env env)
      (schedule-component nodes comp-id)
      tough-code))

  (let* ((n-stack-args (length stack-args))
	 (n-reg-args (length reg-args))
	 (n-stack-slots (/ (env-stack-depth (lambda (e)
					      (eq? e target-env))
					    env)
			   4))
	 (n-tough (min n-stack-args n-stack-slots))
	 (n-easy (- n-stack-args n-tough)))
    (pk 'tail-args
	'n-stack-args n-stack-args
	'n-reg-args n-reg-args 
	'n-stack-slots n-stack-slots)
    (let loop ((rev-args (reverse (list-head stack-args n-easy)))
	       (env env)
	       (code '()))
      (cond ((null? rev-args)
	     (append! code
		      (handle-tough-args reg-args
					 (list-tail stack-args n-easy)
					 (+ (- n-stack-slots n-tough)
					    n-easy)
					 env)
		      (if (> n-stack-slots n-stack-args)
			  `((add sp sp ,(* 4 (- n-stack-slots n-stack-args))))
			  '())))
	    (else
	     (loop (cdr rev-args)
		   (env-push env)
		   `(,@code
		     ,@(compile-expression (car rev-args) env 'r0)
		     (push r0))))))))

(define (compile-expression exp env target)
  (cond

   ((form? 'local exp)
    (case target
      ((:tail)
       (compile-with-return exp env))
      ((:none)
       '())
      (else
       (let ((acc (lookup-local (cadr exp) env)))
	  (cond ((number? acc)
		 `((ldx ,target sp ,acc)))
		(else
		 (error "unsupported access method:" acc)))))))

   ((form? 'set-local exp)
    (let ((acc (lookup-local (cadr exp) env))
	  (val (caddr exp)))
      `(,@(compile-expression val env 'r0)
	,@(cond ((number? acc)
		 `((stx sp ,acc r0)))
		(else
		 (error "unsupported access method:" acc)))
	,@(compile-expression (cadr exp) env target))))

   ((form? 'quote exp)
    (case target
      ((:tail)
       (compile-with-return exp env))
      ((:none)
       '())
      (else
       `((mov ,target (scm ,(cadr exp)))))))

   ((form? 'global exp)
    (case target
      ((:tail)
       (compile-with-return exp env))
      ((:none)
       '())
      (else
       (let ((var (module-variable (current-module) (cadr exp))))
	 (if var
	     `((ld ,target (var ,var)))
	     (error "undefined global variable:" (cadr exp)))))))

;    ((form? 'invoke exp)
;     (if (eq? target :tail)
; 	(begin
; 	  (display ";;; no tail-calls yet.\n")
; 	  (compile-with-return exp env))
; 	;; push args in reverse order
; 	(let loop ((args (reverse (cddr exp)))
; 		   (env env)
; 		   (code '()))
; 	  (cond ((not (null? args))
; 		 (loop (cdr args)
; 		       (env-push env)
; 		       (append! code
; 				`(,@(compile-expression (car args)
; 							env 'r0)
; 				  (push r0)))))
; 		(else
; 		 ;; load argument count into r1,
; 		 ;; load proc into r0 and jump to "invoke"
; 		 (append! code
; 			  `(
; 			    ,@(compile-expression (cadr exp) env 'r0)
; 			    (mov r1 ,(* 4 (length (cddr exp))))
; 			    (call (code ,invoke-code))
; 			    (mov ,target r0))))))))

   ((form? 'invoke exp)
    (let ((proc (cadr exp))
	  (args (cddr exp)))
      (cond ((eq? target :tail)
	     `(,@(compile-tail-args (list (cons 'r0 proc))
				    (cons '(local :ret) args)
				    '() env)
	       (mov r1 ,(* 4 (length args)))
	       (jmp (code ,invoke-code))))
	    (else
	     `(,@(compile-tail-args (list (cons 'r0 proc))
				    args env env)
	       (mov r1 ,(* 4 (length args)))
	       (call (code ,invoke-code))
	       ,@(if (not (eq? target :none))
		     `((mov ,target r0))
		     '()))))))

   ((form? 'if exp)
    (let ((else-label (genlabel))
	  (end-label (genlabel))
	  (then-code (compile-expression (caddr exp) env target))
	  (else-code (compile-expression (cadddr exp) env target)))
      (receive (then-code else-code tail-code)
          (find-common-tail! then-code else-code)
	`(
	  ,@(compile-expression (cadr exp) env 'r0)
	  (beq ,else-label r0 (scm #f))
	  ,@then-code
	  (b ,end-label)
	  ,else-label
	  ,@else-code
	  ,end-label
	  ,@tail-code))))

   ((form? 'begin exp)
    (let loop ((body (cdr exp)))
      (cond ((null? body)
	     (compile-expression `(quote ,(if #f #f)) env target))
	    ((null? (cdr body))
	     (compile-expression (car body) env target))
	    (else
	     (append! (compile-expression (car body) env 'r0)
		      (loop (cdr body)))))))

   ((form? 'labels exp)
    (let* ((labels (cadr exp))
	   (bodies (map cddr labels))
	   (body (cons 'begin (cddr exp)))
	   (frame (make-labels-frame labels))
	   (env (extend-env frame env))
	   (end-label (genlabel)))
      `(,@(compile-expression body env target)
	(b ,end-label)
	,@(apply append!
		 (map (lambda (l b)
			(let ((label (cadr l))
			      (body (cons 'begin b)))
			(let loop ((rev-args (reverse (caddr l)))
				   (inner-env env))
			  (cond ((null? rev-args)
				 `(,label
				   ,@(compile-expression body inner-env target)
				   ,@(unwind-env env inner-env)
				   (b ,end-label)))
				(else
				 (loop (cdr rev-args)
				       (env-push-local (car rev-args)
						       inner-env)))))))
		      (cdr frame) bodies))
	,end-label)))

   ((form? 'goto exp)
    (let* ((label (cadr exp))
	   (args (cddr exp))
	   (target-env (find-labels-frame label env))
	   (target (assq label (cdr (car target-env))))
	   (target-label (cadr target))
	   (target-args (caddr target)))
      (if (not (= (length args) (length target-args)))
	  (error "wrong number of arguments in goto:" exp))
      (pk 'goto label target-label)
      `(,@(compile-tail-args '() args target-env env)
	(b ,target-label))))

   (else
    (error "unsupported form:" exp))))

(define (compile-to-asm form)
  (if (or (not (list? form)) (not (eq? (car form) 'lambda-template)))
      (error "only lambda-templates can be compiled"))
  (let* ((rev-args (reverse (cadr form)))
	 (nargs (length rev-args)))
    (let loop ((a rev-args)
	       (env '()))
      (cond ((not (null? a))
	     (loop (cdr a)
		   (env-push-local (car a) env)))
	    (else
	     (let ((env (env-push-local :ret env))
		   (argsok (genlabel)))
	       `(  (beq ,argsok r1 ,(* 4 nargs))
		   (prepare 1)
		   (mov r0 "some procedure")
		   (pusharg r0)
		   (finish (subr "scm_error_num_args_subr"))
		 ,argsok
		   ,@(compile-expression `(begin ,@(cddr form))
					 env :tail))))))))

(define (compile form)
  (make-closure (assemble (compile-to-asm form)) #f))

(define (compile-show form)
  (display-asm (compile-to-asm form)))
