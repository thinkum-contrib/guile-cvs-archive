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
;;   Make a closure from a closure template and an environment.  A
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
;;   by LABELS, FUNCTIONS or LAMBDA-TEMPLATE.
;;
;; - (set-local SYMBOL VAL)
;;
;;   Set the value of the local variable SYMBOL, as established
;;   by LABELS, FUNCTIONS or LAMBDA-TEMPLATE.
;;
;;   Note that SET-LOCAL does not interact correctly with call/cc.
;;   The values of locals are copied by call/cc and any changes to
;;   them between the call to call/cc and when the continuation is
;;   invoked are lost.
;;  
;; - (global SYMBOL)
;;
;;   Retrieve the value of the global variable that is named by SYMBOL
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
;; case.  You have to help it by explicitely using a `labels' form
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
;; - take preference strength into account for register allocation
;; - find common tails in `labels'
;; - rest args
;; - inline ops
;; - continuously extend peephole optimizer

(read-set! keywords 'prefix)

(define-module (lightning compiler)
  :use-module (lightning assembler)
  :use-module (lightning peephole)
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
;; It includes the locations of the local variables (stack offset or
;; register) and where the various registers have been spilled.  It
;; also includes information about labels so that the stack can be
;; correctly unwound when jumping to a label.

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

;; We have allocated a variable to a register
;;
(define (env-alloc-reg var reg env)
  (extend-env `(reg ,var ,reg) env))

;; We have spilled a register to the stack
;;
(define (env-spill-reg reg env)
  ;; There must be at most one spill frame per register allocation and
  ;; each spill frame must have a register allocation.
  (define (find-reg env)
    (cond ((or (null? env)
	       (and (form? 'spill (car env))
		    (eq? (caddr (cadr (car env))) reg)))
	   (pk 'nope)
	   #f)
	  ((and (form? 'reg (car env))
		(eq? (caddr (car env)) reg))
	   (car env))
	  (else
	   (find-reg (cdr env)))))
  (pk 'spilling reg)
  (let ((alloc-frame (find-reg env)))
    (if alloc-frame
	(extend-env `(spill ,alloc-frame) env)
	env)))

(define (env-stack-depth frame env)
  (let loop ((e env)
	     (offset 0))
    (cond ((eq? frame e)
	   offset)
	  ((form? 'push (car e))
	   (loop (cdr e) (+ offset 1)))
	  ((form? 'reg (car e))
	   (loop (cdr e) offset))
	  ((form? 'spill (car e))
	   (loop (cdr e) (+ offset 1)))
	  ((form? 'labels (car e))
	   (loop (cdr e) offset))
	  (else
	   (error "unsupported environment frame:" (car e))))))

;; Lookup the local variable VAR.  VAR can be a symbol, in which case
;; we look for the most recent `push' or `reg' frame.  When VAR is
;; defined in a `reg' frame we will defer to its spill frame, if any.
;; VAR can also be a frame, in which case we find the variable defined
;; by that frame.  This function returns the part of the environment
;; beginning with the frame defining VAR.

(define (lookup-local-frame var env)
  (let loop ((e env)
	     (spill-e #f))
    (cond ((null? e)
	   (error "undefined local variable:" var))
	  ((form? 'push (car e))
	   (if (or (eq? var (car e))
		   (eq? (cadr (car e)) var))
	       e
	       (loop (cdr e) spill-e)))
	  ((form? 'reg (car e))
	   (if (or (eq? var (car e))
		   (eq? (cadr (car e)) var))
	       (if (and spill-e (eq? (cadr (car spill-e)) (car e)))
		   spill-e
		   e)
	       (loop (cdr e) spill-e)))
	  ((form? 'spill (car e))
	   (if (or (eq? var (cadr (car e)))
		   (eq? (cadr (cadr (car e))) var))
	       (loop (cdr e) e)
	       (loop (cdr e) spill-e)))
	  ((form? 'labels (car e))
	   (loop (cdr e) spill-e))
	  (else
	   (error "unsupported environment frame:" (car e))))))

;; Lookup the local variable defined by VAR, as explained above.
;; Return its palcement, which is either a stack offset or a register
;; name.

(define (lookup-local var env)
  (let ((frame (lookup-local-frame var env)))
    (if (form? 'reg (car frame))
	(caddr (car frame))
	(env-stack-depth frame env))))

;; Lookup the variable that is in register REG and is not spilled.
;; Return the part of the environment that starts with the defining
;; frame.

(define (lookup-register-frame reg env)
  (let loop ((e env))
    (cond ((null? e)
	   #f)
	  ((form? 'push (car e))
	   (loop (cdr e)))
	  ((form? 'reg (car e))
	   (if (eq? (caddr (car e)) reg)
	       e
	       (loop (cdr e))))
	  ((form? 'spill (car e))
	   (if (eq? (caddr (cadr (car e))) reg)
	       #f
	       (loop (cdr e))))
	  ((form? 'labels (car e))
	   (loop (cdr e)))
	  (else
	   (error "unsupported environment frame:" (car e))))))

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
  (pk 'return)
  `(,@(compile-tail-args `((r0 . ,exp) (r2 . (local :ret))) base-env env)
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
;; unique assembler label, ARGS is the list of arguments and their
;; placements in the form
;;
;;   ((PLACE . NAME) ...)
;;
;; A place of `stack' denotes stack passing, else it names a register.

(define (make-labels-frame labels env)
  `(labels ,@(map (lambda (l)
		    (list (car l) (genlabel)
			  (allocate-places (cadr l) env)))
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

;; Register allocation

(define (allocate-places args env)

  (define (get-reg-pref a)
    (let ((pref-opt (and (pair? a) (memq :reg a))))
      (if pref-opt
	  (cadr pref-opt)
	  0)))

  (define (get-arg-name a)
    (if (pair? a) (car a) a))

  (let loop ((available-regs non-volatile-regs)
	     (a args)
	     (res '()))
    (cond ((null? a)
	   (reverse! res))
	  ((or (null? available-regs)
	       (zero? (get-reg-pref (car a))))
	   (loop available-regs
		 (cdr a)
		 (cons (cons 'stack (get-arg-name (car a)))
		       res)))
	  (else
	   (loop (cdr available-regs)
		 (cdr a)
		 (cons (cons (car available-regs) (get-arg-name (car a)))
		       res))))))

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
    (union (list (cadr exp)) (free-locals (caddr exp))))
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

;; Find the registers clobbered by exp

(define (clobbered-regs exp)
  (cond
   ((form? 'global exp)
    '())
   ((form? 'quote exp)
    '())
   ((form? 'local exp)
    '())
   ((form? 'set-local exp)
    (clobbered-regs (caddr exp)))
   ((form? 'invoke exp)
    volatile-regs)
   ((form? 'if exp)
    (union-map clobbered-regs (cdr exp)))
   ((form? 'begin exp)
    (union-map clobbered-regs (cdr exp)))
   ((form? 'goto exp)
    '())
   ((form? 'labels exp)
    volatile-regs)
   (else
    (error "unsupported form:" exp))))


(define-struct arg-node ()
  exp slot (conflicts '()) (id #f) (comp-id #f))

(define volatile-regs '(r0 r1 r2))
(define non-volatile-regs '(v0 v1 v2))

;; Generate code for pushing ARGS and simultanously unwinding the
;; stack to TARGET-ENV.

;; ARGS is a list of (reg . exp) pairs, where REG is a symbol and EXP
;; is the expression that computes the value for that argument.  When
;; REG is the symbol `stack', the argument will be passed on the
;; stack, else it will be passed in the register denoted by REG.

(define (compile-tail-args args target-env env)

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

  ;; We also deal with register spilling here.  For each argument that
  ;; is to be passed in a non-volatile register, we spill that
  ;; register into a stack slot.  The value spilled will be the one
  ;; that is live in the register at the target env, but will be found
  ;; using the current env.  The non-volatile registers that are not
  ;; used to pass arguments are directly loaded with the value live in
  ;; the target env, using the current env to find that value.

  ;; This spilling is implemented by creating addition arguments that
  ;; describe the spill slots, and register values.  This might seem
  ;; to lead to a lot of overhead, but one should realize that most of
  ;; these additional arguments lead to noops in typical loops.

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
			      (update-conflicts n (lookup-local l env)))
			    (free-locals (arg-node-exp n)))
		  (for-each (lambda (r)
			      (update-conflicts n r))
			    (adjoin 'r1 (clobbered-regs (arg-node-exp n)))))
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
	   (target (if (number? slot) 'r1 slot))
	   (store-code (if (number? slot)
			   `((stx ,(* 4 (+ tough-offset slot)) sp r1)
			     (die r1))
			   `())))
      (cond ((null? (cdr comp))
	     (pk 'storing (arg-node-slot (car comp)))
	     (set! tough-code
		   `(,@tough-code
		     ,@(compile-expression (arg-node-exp arg)
					   tough-env target)
		     ,@store-code)))
	    (else
	     (pk 'pushing (arg-node-slot (car comp)))
	     (let ((env tough-env)
		   (offset tough-offset))
	       (set! tough-code
		     `(,@tough-code
		       ,@(compile-expression (arg-node-exp arg)
					     tough-env 'r1)
		       (push r1)
		       (die r1)))
	       (set! tough-offset (+ 1 tough-offset))
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

  (define (pick-stack-args args)
    (let loop ((a args)
	       (s '()))
      (cond ((null? a)
	     (reverse! s))
	    ((eq? (car (car a)) 'stack)
	     (loop (cdr a) (cons (cdr (car a)) s)))
	    (else
	     (loop (cdr a) s)))))

  (define (pick-reg-args args)
    (let loop ((a args)
	       (r '()))
      (cond ((null? a)
	     (reverse! r))
	    ((eq? (car (car a)) 'stack)
	     (loop (cdr a) r))
	    (else
	     (loop (cdr a) (cons (car a) r))))))

  ;; Construct a `(local ...)' expression that refers to the variable
  ;; that is stored in REG in TARGET-ENV.  Return #f when the register
  ;; is not allocated in TARGET-ENV.
  ;;
  (define (make-register-value-expression reg)
    (if (memq reg non-volatile-regs)
	(let ((frame (lookup-register-frame reg target-env)))
	  (if frame
	      `(local ,(car frame))
	      #f))
	#f))

  ;; Make the stack-args that will spill the used registers.
  ;;
  (define (make-spill-args reg-args)
    (pk 'spill-args reg-args
    (let loop ((sa '())
	       (ra reg-args))
      (cond ((null? ra)
	     sa)
	    (else
	     (let* ((reg (car (car ra)))
		    (exp (make-register-value-expression reg)))
	       (if exp
		   (loop (cons exp sa) (cdr ra))
		   (loop sa (cdr ra)))))))))

  ;; Make the reg-args that will load the unused registers.
  ;;
  (define (make-unspill-args reg-args)
    (pk 'unspill-args reg-args
    (let loop ((ua '())
	       (regs (set-difference non-volatile-regs
				     (map car reg-args))))
      (cond ((null? regs)
	     ua)
	    (else
	     (let* ((reg (car regs))
		    (exp (make-register-value-expression reg)))
	       (if exp
		   (loop (cons (cons reg exp) ua) (cdr regs))
		   (loop ua (cdr regs)))))))))

  (let* ((reg-args-1 (pick-reg-args args))
	 (stack-args (append! (pick-stack-args args)
			      (make-spill-args reg-args-1)))
	 (reg-args (append! reg-args-1
			    (make-unspill-args reg-args-1)))
	 (n-stack-args (length stack-args))
	 (n-reg-args (length reg-args))
	 (n-stack-slots (env-stack-depth target-env env))
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
		     (push r0)
		     (die r0))))))))

(define (stackify args)
  (map (lambda (a) (cons 'stack a)) args))

(define (splice-places places args)
  (map (lambda (p a) (cons (car p) a)) places args))

(define (env-for-args args env)
  (pk 'for-args args
      (let loop ((a args)
		 (env env))
    (cond ((null? a)
	   (let loop ((rev-args (reverse args))
		      (env env))
	     (cond ((null? rev-args)
		    env)
		   (else
		    (loop (cdr rev-args)
			  (if (eq? (car (car rev-args)) 'stack)
			      (env-push-local (cdr (car rev-args)) env)
			      (env-alloc-reg (cdr (car rev-args))
					     (car (car rev-args)) env)))))))
	  ((memq (car (car a)) non-volatile-regs)
	   (loop (cdr a)
		 (env-spill-reg (car (car a)) env)))
	  (else
	   (loop (cdr a)
		 env))))))

(define (env-alloc-non-volatile-regs env)
  (let loop ((regs non-volatile-regs)
	     (env env))
    (cond ((null? regs)
	   env)
	  (else
	   (loop (cdr regs)
		 (env-alloc-reg #f (car regs) env))))))

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
		 `((ldx ,target sp ,(* 4 acc))))
		(else
		 `((mov ,target ,acc))))))))

   ((form? 'set-local exp)
    (let ((acc (lookup-local (cadr exp) env))
	  (val (caddr exp)))
      (case target
	((:tail)
	 (compile-with-return exp env))
	(else
	 `(,@(compile-expression val env 'r0)
	   ,@(cond ((number? acc)
		    `((stx ,(* 4 acc) sp r0)
		      (die r0)))
		   (else
		    `((mov ,acc r0)
		      (die r0))))
	   ,@(if (not (eq? target :none))
		 `((mov ,target (scm ,(if #f #f))))
		 '()))))))

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

   ((form? 'invoke exp)
    (let ((proc (cadr exp))
	  (args (cddr exp)))
      (cond ((eq? target :tail)
	     `(,@(compile-tail-args (list* (cons 'r0 proc)
					   (stackify
					    (cons '(local :ret)
						  args)))
				    base-env env)
	       (mov r1 ,(* 4 (length args)))
	       (jmp (code ,invoke-code))))
	    (else
	     `(,@(compile-tail-args (list* (cons 'r0 proc)
					   (stackify args))
				    env env)
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
	   (frame (make-labels-frame labels env))
	   (env (extend-env frame env))
	   (end-label (genlabel)))
      `(,@(compile-expression body env target)
	(b ,end-label)
	,@(apply append!
		 (map (lambda (l b)
			(let ((label (cadr l))
			      (body (cons 'begin b))
			      (inner-env (env-for-args (caddr l) env)))
			  `(,label
			    ,@(compile-expression body inner-env target)
			    ,@(compile-tail-args '() env inner-env)
			    (b ,end-label))))
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
      `(,@(compile-tail-args (splice-places target-args args) target-env env)
	(b ,target-label))))

   (else
    (error "unsupported form:" exp))))

(define base-env (env-alloc-non-volatile-regs (make-empty-env)))

(define (compile-to-asm form . opt-nopeep)
  (if (or (not (list? form)) (not (eq? (car form) 'lambda-template)))
      (error "only lambda-templates can be compiled"))
  (let* ((args (cadr form))
	 (nargs (length args))
	 (env (env-push-local :ret (env-for-args (stackify args) base-env)))
	 (argsok (genlabel))
	 (code `(  (beq ,argsok r1 ,(* 4 nargs))
		   (prepare 1)
		   (mov r0 "some procedure")
		   (pusharg r0)
		   (finish (subr "scm_error_num_args_subr"))
		 ,argsok
		   ,@(compile-expression `(begin ,@(cddr form))
					 env :tail))))
    (if (or (null? opt-nopeep) (car opt-nopeep))
	(peephole-optimize code)
	code)))

(define (compile form)
  (make-closure (assemble (compile-to-asm form)) #f))

(define (compile-show form . opt-no-peep)
  (display-asm (apply compile-to-asm form opt-no-peep)))
