;; -*- tab-width: 4; -*-
;; QScheme 0.5 compiler
;;
;; Generates code for the virtual machine
;;
;; $Id$

;; need the simple formater for error messages
(load "format2str.scm")	

;; Note:
;; TODO = code to write
;; KKK  = incomplete code

;;----------------------------------------------------------------
;; Global variables
;;----------------------------------------------------------------

(define verbose? #t)

(define-macro (when cond . x)  `(if ,cond (begin ,@x)))
(define-macro (unless cond . x) `(if (not ,cond) (begin ,@x)))

(define (info msg . more)
  (when verbose?
	(display "INFO:") (display msg) (display " ")
	(until (null? more)
	  (write (car more)) (display " ")
	  (set! more (cdr more)))
	(newline)))


;;----------------------------------------------------------------
;; Environment 
;;----------------------------------------------------------------
(define (env-search env atom)
  (info "env-search:" env atom)
  (let ((depth 0) (a #f))
	(until (or a (null? env))
	  (set! a (assoc atom (car env)))
	  (unless a
		(set! depth (+ 1 depth))
		(set! env (cdr env))))
	(info "env-search: result=" a)
	(if a (cons depth a) #f)))


;;----------------------------------------------------------------
;; Assembler generator
;;----------------------------------------------------------------
(define va! vector-append)

(define (compile-instr code instr . args)
  (vector-append code `(,instr ,@args)))

;; Push values on stack
(define (compile-pushlit c var)		(va! c `(%pushlit  ,var)))
(define (compile-pushsym c var)		(va! c `(%pushsym  ,var)))
(define (compile-pushloc c var lev)	(va! c `(%pushloc  ,(+ (cdr var) (* lev 65536)))))
(define (compile-pushext c var)		(va! c `(%pushext  ,var)))  

;; Various set
(define (compile-setloc c var lev)	(va! c `(%setloc  ,var ,lev)))
(define (compile-setsym c var)		(va! c `(%setsym  ,var)))
(define (compile-setext c var)		(va! c `(%setext  ,var)))

;; environment 
(define (compile-pushenv c)			(va! c '(%pushenv)))
(define (compile-prepcall c)		(va! c '(%prepcall)))

;;call vm prim
(define (compile-call-prim c prim)	(va! c `(%callprim ,prim)))

;;call C prim
(define (compile-call-cprim   c prim)	(va! c `(%callcprim ,prim)))
(define (compile-call-cprim0  c prim)	(va! c `(%callcprim0  ,prim)))
(define (compile-call-cprim1  c prim)	(va! c `(%callcprim1  ,prim)))
(define (compile-call-cprim2  c prim)	(va! c `(%callcprim2  ,prim)))
(define (compile-call-cprim3  c prim)	(va! c `(%callcprim3  ,prim)))
(define (compile-call-cprim4  c prim)	(va! c `(%callcprim4  ,prim)))
(define (compile-call-cprim5  c prim)	(va! c `(%callcprim5  ,prim)))
(define (compile-call-cprim6  c prim)	(va! c `(%callcprim6  ,prim)))
(define (compile-call-cprim7  c prim)	(va! c `(%callcprim7  ,prim)))
(define (compile-call-cprim8  c prim)	(va! c `(%callcprim8  ,prim)))
(define (compile-call-cprim9  c prim)	(va! c `(%callcprim9  ,prim)))
(define (compile-call-cprim10 c prim)	(va! c `(%callcprim10 ,prim)))

;; call whatever is on the stack
(define (compile-call c)			(va! c `(%call)))
(define (compile-call-closure c)	(va! c `(%callclos)))

;;KKK: Change here: do not compile directly but optimize the code and
;;generate a (%asm #(code)) for later compilation

(define (compile-make-proc c env optarg code)
  (let* ((compiled-body (vm3-asm code))
		 (proc (make-procedure compiled-body env (length (car env)) optarg)))
	(info "compile-make-proc: body code:")
	(vm3-dis compiled-body)
	(compile-pushlit c proc)))

(define (compile-make-closure c)	(va! c `(%makeclos)))

;; compile-return
(define (compile-return c)			(va! c '(%return)))
(define (compile-end c)				(va! c '(%end)))

;; label generation
(define *compile-current-label* 0)

(define (compile-next-label)
  (set! *compile-current-label* (+ 1 *compile-current-label*))
  *compile-current-label*)

;; Branches
(define (compile-label c lab)		(va! c `(%lab ,lab)))
(define (compile-brf c lab)			(va! c `(%brf ,lab)))
(define (compile-brt c lab)			(va! c `(%brt ,lab)))
(define (compile-bra c lab)			(va! c `(%bra ,lab)))

;; Stack maint
(define (compile-drop c)			(va! c '(%drop)))

;; lets
(define (compile-letstart c)		(va! c '(%letstart)))
(define (compile-letbody  c n)		(va! c `(%letbody ,n)))
(define (compile-letend   c)		(va! c '(%letend)))

(define (compile-instr-letrec  c n)		(va! c `(%letrec ,n)))


;;----------------------------------------------------------------
;; Compiler
;;----------------------------------------------------------------

;; Compiler for object and expression. Invokes compile-pair when pair
;; is given.
(define (compile-object icode obj env)
  ;; Compile code for a global object reference.
  ;; If symbol does not exist, create a new one with unbound value. If
  ;; symbol is bound to a macro or to an extern variable, generate code
  ;; accordingly

  (define (compile-global-object icode obj)
	(let ((sym (hash-search symbol-hash obj)))
	  (info "found in global env:" sym)
	  (unless sym
			  (info "unbound symbol:" obj)
			  (set! sym (make-symbol obj))
			  (hash-set! symbol-hash obj sym))
	  (let ((svalue (symbol-value sym)))
		(cond 
		 ((extern-variable? svalue)
		  (compile-pushext icode svalue))
		 ((and (macro? svalue) (not (null? (macro-func svalue))))
		  (compile-pushlit  icode (macro-func svalue)))
		 (else
		  (compile-pushsym  icode sym))))))

  (cond
   ((pair? obj)
	(compile-pair icode obj env))

   ((pure-symbol? obj)
	(info "got pure-symbol" obj)
	;; is this symbol already bound ?
	(let ((value (hash-search symbol-hash obj)))
	  (info "pure-symbol value" obj value)))

   ((atom? obj)
	(info "got atom" obj)
	(info "environment is" env)
	(if (null? env)
		(compile-global-object icode obj env)

		;; have an env chain
		(let ((r (env-search env obj)))
		  (if r
			  (begin (info "found in env" r)
					 (compile-pushloc icode (cdr r) (car r)))
			  (compile-global-object icode obj env)))))

   ;; assume default is a ref to a literal
   (else (info "literal object" obj)
		 (compile-pushlit icode obj))
   ))


;; compile arguments for funcall
(define (compile-args icode args env)
  (if (null? args)
	  icode
	  (begin (compile-object icode (car args) env)
			 (compile-args icode (cdr args) env))))

;; Compiler for quote

(define (compile-quote icode args env)
  (compile-pushlit icode (car args)))

;; The quasiquote compiler

(define (backquotify ic l env level)
  (cond
   ((not (pair? l))
	(compile-pushlit ic l))
   ((eqv? (car l) 'quasiquote)
	(backquotify ic (cadr l) env (+ 1 level))
	(compile-pushlit 	ic 'quasiquote)
	(compile-call-prim  ic list2))
   ((eqv? (car l) 'unquote)
	(if (eq? level 1)
		(compile-object ic (cadr l) env)
		(begin
		  (backquotify ic (cadr l) env (- 1 level))
		  (compile-pushlit ic 'unquote)
		  (compile-call-prim ic list2))))
   ((and (pair? (car l)) (eqv? (caar l) 'unquote-splicing))
	(compile-object ic (car (cdar l)) env)
	(unless (null? (cdr l))
			(backquotify ic (cdr l) env level)
			(compile-call-prim ic qq-append2)))
   (else
	(backquotify ic (car l) env level)
	(backquotify ic (cdr l) env level)
	(compile-call-prim ic cons)))
  ic)

(define (compile-qquote icode arg env)
  (info "compile-qquote: " arg)
  (backquotify icode (car arg) env 1))

;; Compiler for set

(define (compile-set! icode args env)
  (info "compile-syntax: set!" args)
  (compile-object icode (cadr args) env)
  (let ((sym (env-search env (car args))))
	(if sym								;found in local env
		(begin							;local symbol
		  (compile-setloc   icode (cdr sym) (car sym)))
		(begin
		  (set! sym (hash-search symbol-hash (car args)))
		  (if (not sym) 
			  (throw 'comp-error	"set! needs an existing symbol"))
		  (cond
		   ((extern-variable? (symbol-value sym))
			(compile-setvar icode sym))
		   (else (compile-setsym icode sym)))))))


;; Compiler for if

(define (compile-if icode args env)
  (info "compile-syntax: if" args)
  (let ((len (length args)))
	(info "compile-syntax: if nargs" len)
	(cond
	 ((eq? len 2)
	  (let ((labx (compile-next-label)))
		;; (if E T)   => E (brf x) T (lab x)
		(set! labx (compile-next-label))
		(compile-object icode (car args) env)
		(compile-brf    icode labx)
		(compile-object icode (cadr args) env)
		(compile-label  icode labx)
		))
	 ((eq? len 3)
	  (let ((labx (compile-next-label)) (laby (compile-next-label)))
		;; (if E T F) => E (brf x) T (bra y) (lab x) F (lab y)
		(compile-object icode (car args) env)
		(compile-brf    icode labx)
		(compile-object icode (cadr args) env)
		(compile-bra    icode laby)
		(compile-label  icode labx)
		(compile-object icode (caddr args) env)
		(compile-label  icode laby)
		))
	 (else
	  (throw 'comp-err "if syntax: (if <expr> <true> [<false>])")))))

;; Compiler for (begin ...)

(define (compile-begin icode args env)
  (info "compile-syntax: begin" args env)
  (until (null? args)
		 (compile-object icode (car args) env)
		 (set! args (cdr args))
		 (unless (null? args) (compile-drop icode))))

;; Compiler for (lambda (..) ..)
(define (compile-lambda icode args env)
  (let ((optarg #f))

	(define (check-arg-unicity obj list)
	  (if (assoc obj list)
		  (throw 'comp-err "argument defined twice")))


	(define (build-arg-list l arglist argnum)
	  (let ((arglist '()) (argnum 0) (last #f) (new #f))
		
		(while (pair? l)
		  (check-arg-unicity (car l) arglist)
		  (set! new  (cons (cons (car l) argnum) '()))
		  (if last   (set-cdr! last new)   (set! arglist new))
		  (set! last new);
		  (set! l (cdr l))
		  (set! argnum (+ 1 argnum)))
		(if (not (null? l))
			(begin
			  (check-arg-unicity l arglist)
			  (set! new (cons (cons l argnum) '()))
			  (if last   (set-cdr! last new)   (set! arglist new))
			  (set! optarg #t)))
		arglist))

	(display "lambda: args=") (print args)
	(if (not (>= (length args) 2))
		(throw 'comp-err "lambda requires at least 2 elements"))
	(let ((formal (car args))
		  (body (cdr args))
		  (newenv '())
		  (body-code (make-vector 0)))

	  (set! newenv (cons (build-arg-list formal) env))
	  (display "formal=")(print formal)
	  (display "body=") (print body)
	  (display "newenv=") (print newenv)
	  (display "nargs=")  (print (length (car newenv)))
	  (display "optargs=") (print optarg)

	  (compile-begin body-code body newenv)
	  (compile-return body-code)
	  (compile-make-proc icode newenv optarg (compile-optimize body-code))
	  (compile-make-closure icode)
	  )))

;; compiler for define
;; accepts: (define ATOM [EXPR]) | (define PAIR ...)

(define (compile-define icode args env)
  (let ((first (car args)) (rest (cdr args)))
	(cond
	 ((atom? first)						; define for a variable
	  (if (>= (length rest) 2) (throw 'comp-error "too many expr"))
	  (if (< (length rest) 1) (set! rest (list (unbound))))
	  (let ((sym (env-search env first)))
		(if sym
			(begin
			  (info "compile-define: local symbol found" sym)
			  (compile-object icode (car rest) env)
			  (compile-setloc icode (cdr sym) (car sym)))
			(begin
			  (info "compile-define: not a local symbol" first)
			  (set! sym (hash-search symbol-hash first))
			  (unless sym
					  (info "compile-define: global symbol does not exists" first)
					  (set! sym (make-symbol first))
					  (hash-set! symbol-hash first sym))
			  (compile-object icode (car rest) env)
			  (compile-setsym icode sym)))))
	 ((pair? first)						; define for a function
	  (info "compile-define: function" first)
	  )
	 (else
	  (throw 'comp-error "bad define syntax"))
	 );cond
	);let
  )

;; compiler for let
;;
;; Let values are taken outside of the (let ...) scope
;; Runtime translation:
;;
;;TODO

;; Check unicity of symbol in let binding
;;
(define (compile-let:check-binding-unicity args)
  (if (or (null? args) (null? (cdr args)))
	  #f
	  (let ((search (assoc (caar args) (cdr args))))
		(info "compile-let:check-binding-unicity: testing"
			  (caar args) (cdr args) search)
		(if search
			search
			(compile-let:check-binding-unicity (cdr args))))))
		
;; Check for correct binding format in let declaration 
;;
(define (compile-let:check-bindings-format l)
  ;; first check for valid list
  (if (not (list? l))
	  (throw 'comp-err
		(format->string 
		 "let binding: bad let binding syntax. ~w is not a list" bind)))

  ;; check for unique symbols
  (let ((sym (compile-let:check-binding-unicity l)))
	(if sym
		(throw 'comp-err
		 (format->string "let binding: symbol '~a is defined multiple times"
						 (car sym))))))

;; Compile code for binding creation
;;
(define (compile-let-binding code bind env)
  (unless (null? bind)
	(let ((expr (cdar bind)))
	  (if (null? expr)
		  (compile-unbound code expr env)
		  (compile-object code (car expr) env)))
	(compile-let-binding code (cdr bind) env)))

;; Build a new env for let
;;
(define (compile-let:build-new-env l)
	(let ((blist '()) (num 0) (last #f) (new #f))
	  (while (pair? l)
		(set! new (cons (cons (caar l) num) '()))
		(if last (set-cdr! last new) (set! blist new))
		(set! last new)
		(set! l (cdr l))
		(set! num (+ num 1)))
	  blist))
  
;; Compiler for let
;;
(define (compile-let code args env)

  (let ((bind (car args)) (body (cdr args)) (newenv '()))

	(info "compile-let: bind=" bind)
	(info "compile-let: body=" body)
	(compile-let:check-bindings-format bind)

	(set! newenv  (cons (compile-let:build-new-env bind) env))
	(info "compile-let: newenv=" newenv)

	(compile-letstart 		code)
	(compile-let-binding  	code bind env)
	(compile-letbody 		code (length (car newenv)))
	(compile-begin    		code body newenv)
	(compile-letend			code)
	)
  )

;; compiler for let*
;;
(define (compile-let* code args env)
  (let ((bind (car args)) (body (cdr args))	(newenv '()))

	(info "compile-let: bind=" bind)
	(info "compile-let: body=" body)
	(compile-let:check-bindings-format bind)

	(set! newenv (cons (compile-let:build-new-env bind) env))
	(info "compile-let: newenv=" newenv)

	(compile-instr-letrec	code (length (car newenv)))
	(compile-let-binding	code bind newenv)
	(compile-begin    		code body newenv)
	(compile-letend			code)
	)
  )

;; compiler for letrec
;;
;;TODO
(define compile-letrec compile-let*)

;; the-env
;;
;;TODO
(define (compile-the-env code args env)
  )

;; compiler for and
;;
;;TODO
(define (compile-and code args env)
  )

;; compiler for or
;;
;;TODO
(define (compile-or code args env)
  )

;; compiler for cond
;;
;;TODO
(define (compile-cond code args env)
  )

;; compiler for catch
;;
;;TODO
(define (compile-catch code args env)
  )

;; compiler for do
;;
;;TODO
(define (compile-do code args env)
  )


;; compiler for loops
;;
(define (compile-loop code args env br-op)
  (let* ((testlab (compile-next-label))
		 (looplab (compile-next-label)))
	(compile-bra		code testlab)
	(compile-label	code looplab)
	(compile-begin	code (cdr args) env)
	(compile-drop		code)
	(compile-label	code testlab)
	(compile-object	code (car args))
	(compile-instr	code br-op looplab)
	(compile-pushlit  code #t)

	;;(compile-object   code (car args) env)
	;;(compile-instr    code br-op testlab)
	;;(compile-label	code looplab)
	;;(compile-begin    code (cdr args) env)
	;;(compile-drop     code)
	;;(compile-bra		code looplab)
	;;(compile-label    code testlab)

	code))

;; compiler for (while TEST EXPR...)
;;
(define (compile-while code args env)  
  (compile-loop code args env '%brt))

;; compiler for (until TEST EXPR...)
;;
(define (compile-until code args env)
  (compile-loop code args env '%brf))


;; Possible implementation of the syntax compiler is to use a hash
;; whose key is the type and whose value is a compiler function

(define compile-syntax-hash
  #{
	(quote 			. #.compile-quote)
	(quasiquote		. #.compile-qquote)
	(set!			. #.compile-set!)
	(if				. #.compile-if)
	(begin			. #.compile-begin)
	(lambda			. #.compile-lambda)
	(define			. #.compile-define)
	(let			. #.compile-let)
	(let*			. #.compile-let*)
	(letrec			. #.compile-letrec)
	(while			. #.compile-while)
	(until			. #.compile-until)
	})

(define (syntax? obj)
  (let ((val (hash-search compile-syntax-hash obj)))
	(if (not val) #f val)))

(define (compile-funcall icode func args env)

  ;; syntax compiler
  (define (compile-syntax icode syntax args env)
	(let ((func (href compile-syntax-hash syntax)))
	  (if (not func) (throw 'comp-err "bad syntax"))
	  (func icode args env)))

  (info "compiling funcall" func args)
  (when (atom? func) (info "compile-funcall: func is atom" func))
  (when (pure-symbol? func) (info "compile-funcall: func is pure-symbol" func))

  (let ((sym (env-search env func)))
	(if sym								; local symbol

		(begin							; symbol found in env chain
		  (compile-prepcall	icode)
		  (compile-args 	icode args env)
		  (compile-pushloc 	icode (cdr sym) (car sym))
		  (compile-call		icode))

		(begin							; should be a global symbol
		  (set! sym (hash-ref symbol-hash func))
		  (info "compile-funcall: global symbol" sym)
		  (cond
		   ((primitive? sym)			; is a VM primitive
			(let ((arity (primitive-arity sym)))
			  (if (negative? arity)
				  (compile-pushenv	icode)
				  (if (not (eq? (length args) arity))
					  (throw 'comp-error "bad arity"))))
			(compile-args 		icode args env)
										;			(compile-call-prim	icode sym)
			(compile-call-prim	icode func)
			)

		   ((cprimitive? sym)			; is a cprimitive
			(let ((arity (cprimitive-arity sym)))
			  (if (negative? arity)
				  (begin				; variable number of args
					(compile-pushenv icode)
					(compile-args icode args env)
					(compile-call-cprim icode sym)) 
				  (begin 
					(if (not (eq? (length args) arity))
						(throw 'comp-error "bad arity"))
					(compile-args icode args env)
					(case arity
					  ((0)  (compile-call-cprim0  icode sym))
					  ((1)  (compile-call-cprim1  icode sym))
					  ((2)  (compile-call-cprim2  icode sym))
					  ((3)  (compile-call-cprim3  icode sym))
					  ((4)  (compile-call-cprim4  icode sym))
					  ((5)  (compile-call-cprim5  icode sym))
					  ((6)  (compile-call-cprim6  icode sym))
					  ((7)  (compile-call-cprim7  icode sym))
					  ((8)  (compile-call-cprim8  icode sym))
					  ((9)  (compile-call-cprim9  icode sym))
					  ((10) (compile-call-cprim10 icode sym))
					  (else (throw 'comp-error "too large arity for cprim"))
					  )))))

		   ((macro? sym)
			(compile-object icode (execute-macro sym (cons sym args) env) env))

		   ((syntax? func)
			(compile-syntax icode func args env))

		   ((closure? sym)
			(compile-prepcall icode)
			(compile-args 			icode args env)
			(compile-pushsym 		icode (hash-search symbol-hash func))
			(compile-call-closure 	icode))

		   (else
			(info "compile-funcall: symbol should be unbound" sym)
			(compile-pushsym 	icode (hash-search symbol-hash func))
			(compile-call 		icode)))
		  )))
  icode
  )

(define (compile-pair icode form env)
  (let ((f (car form)))
	(info "compiling pair" form)
	(cond
	 ((pair? f)
	  (info "compile-pair: first is pair" f)
	  (compile-prepcall 	icode)
	  (compile-args 		icode (cdr form) env)
	  (compile-pair 		icode f env)
	  (compile-call			icode)
										;	  (throw 'comp-error "not implemented")
	  )
	 ((number? f)
	  (info "compile-pair: first is number" f)
	  (throw 'comp-error "illegal (number ...) form"))
	 ((symbol? f)
	  (compile-funcall icode f (cdr form) env))
	 ((macro? f)
	  (info "compile-pair: macro" f))
	 (else
	  (throw 'comp-error "unsupported func"))))
  )


;; resolve label

(define (compile-resolve-labels code)
  (let ((i 0) (len (vector-length code))
		(instr '())
		(h '())
		)
	(info "compile-resolve-label")
	(set! h (make-hash))

	;; first pass: build hash of labels
	(while (< i len)
	  (set! instr (vref code i))
	  (if (eq? (car instr) '%lab)
		  (begin
			(info "compile-resolve-label: found label" (cadr instr))
			(hset! h (cadr instr) i)
			)
		  )
	  (set! i (+ i 1))
	  )
	(info "compile-resolve-label: label hash is" h)

	;; 2nd pass: resolve branches
	(set! i 0)
	(while (< i len)
	  (set! instr (vref code i))
	  (case (car instr)
		((%bra %brf %brt)
		 (info "compile-resolve-label: step" i " to " (href h (cadr instr)))
		 (set-cdr! instr (cons (href h (cadr instr)) '())))
		((%lab)
		 (info "compile-resolve-label: %lab " i " to %nop")
		 (vset! code i '%nop)))
	  (set! i (+ i 1)))

	;; removing nop
	(set! i 0)
	(while (< i (vector-length code))
	  (info "compile-resolve-label: code" code)
	  (if (eq? (vref code i) '%nop)
		  (let ((j 0) (l i) (ins '()))
			(info "compile-resolve-label: %nop at" i)
			(while (< j l)
			  (set! ins (vref code j))
			  (if (and (member (car ins) '(%bra %brf %brt))
					   (> (cadr ins) l))
				  (begin
					(set-car! (cdr ins) (- (cadr ins) 1))
					(info "compile-resolve-label: new instr" ins)))
			  
			  (set! j (+ 1 j)))
			(vector-remove! code i))
		  ;; not a nop
		  (set! i (+ i 1)))
	  )
	))


(define (compile-optimize code)
  (define (extract-labels code)
	(let ((i 0) (len 0) (instr) (v))
	  (set! i 0)
	  (set! v (make-vector 0 0))
	  (while (< i (vector-length code))
		(set! instr (vref code i))
		(if (eq? (car instr) '%lab)
			(begin
			  (info "found label" (cadr instr))
			  (vector-append v (cons i (cadr instr)))
			  (vector-remove! code i))

			(set! i (+ i 1))))
	  
	  (info "label crossref" v)
	  v))

  (define (make-labref code)
	(let ((i 0)
		  (len (vector-length code))
		  (instr)
		  (labref))
	  (set! labref '())
	  (while (< i len)
		(set! instr (vref code i))
		(if (eq? (car instr) '%lab)
			(begin
			  (set! labref (cons (cons (cadr instr) i) labref))))
		(set! i (+ i 1)))
	  
	  (info "labref is" labref)
	  labref))
  
  (let ((i 0) (instr) (limit) (labref))
	(info "compile-optimize" code)
	;; (compile-resolve-labels code)
	;; (set! labref (extract-labels code))

	(set! limit (vector-length code))
	(set! labref (make-labref code))

	(while (< i limit)
	  (set! instr (vref code i))
	  ;; transform (%callprim prim) => (prim)
	  (if (eq? (car instr) '%callprim)
		  (begin
			(info "optimize: removing %callprim" instr)
			(vset! code i (cdr instr))))
	  (set! i (+ i 1)))
	
	(set! i 0)
	(while (< i limit)
	  (set! instr (vref code i))

	  ;; try to propagate return and end
	  (if (eq? (car instr) '%bra)
		  (let ((where (assoc (cadr instr) labref)))
			(info "branch at" i)
			(info "target of branch is" (cdr where))
			(let ((j (cdr where)) (propagate #f) (flag #t))
			  (while (and flag (not propagate) (< j limit))
				(if (not (eq? (car (vref code j)) '%lab))
					(if (member (car (vref code j)) '(%return %end))
						(set! propagate j)
						(set! flag #f)))
				(set! j (+ j 1)))
			  (info "propagate from" propagate)
			  (when propagate
					(vset! code i (vref code propagate))
					(info "propagate:" code)
					(set! i -1)))		;start again
			))
	  
	  (set! i (+ i 1))
	  )
	)
  code)

(define (compile-expr form env)
  (let ((icode (make-vector 0)))
	(compile-object icode form env)
	(compile-end icode)
	(compile-optimize icode)
	icode))
