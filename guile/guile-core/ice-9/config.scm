;;;; This is the (ice-9 config) module.  It exports the module
;;;; configuration language and creates the (user guile) module.

;; from r4rs
(define (call-with-current-continuation proc)
  (@call-with-current-continuation proc))


;; the export list
(define the-module-exports 
  '(the-module 
    module 
    module-go 
    go 
    module-ref
    module-open     
    module-protect  
    module-export   
    module-export   
    module-access   
    module-close    
    module-create   
    module-use      
    (module-use (alias use-modules))
    (module-export (alias export))
    (module-export (alias export-syntax))
    define-module 
    define-public
    module-config-export
    module-config-protect
    module-config-open
    (define-public (alias define*))
    (define (alias define-private))
    symbol-property
    set-symbol-property!
    remove-symbol-property!
    (remove-symbol-property! (alias symbol-property-remove!))
    ))

(define the-module-protects 
  '(module-eval-environment
    module-interface-environment
    module-package-name
    module-name))




;;; Symbol properties
(define symbol-property
  (procedure->memoizing-macro
   (lambda (x env)
     `(,environment-symbol-property (the-environment) ,(cadr x) ,(caddr x) ))))

(define set-symbol-property!
  (procedure->memoizing-macro
   (lambda (x env)
     `(,environment-set-symbol-property! (the-environment)  ,(cadr x) ,(caddr x) ,(cadddr x) ))))

(define remove-symbol-property!
  (procedure->memoizing-macro
   (lambda (x env)
     `(,environment-remove-symbol-property! (the-environment) ,(cadr x) ,(caddr x) ))))


;;; {Error Handling}
;;;

;; from ice-9 error
(define (error . args)
  (if (null? args)
      (scm-error 'misc-error #f "?" #f #f)
      (let loop ((msg "%s")
		 (rest (cdr args)))
	(if (not (null? rest))
	    (loop (string-append msg " %S")
		  (cdr rest))
	    (scm-error 'misc-error #f msg args #f)))))



;;; {Low Level Modules}


;; the module-signature (also called `module') maps modules to:
;; (vector eval-env export-env protect-env package-sym module-sym)
(define (module-eval-environment module) (vector-ref module 0))


;;; the accessor procedures

;; export environment		 
(define (module-export-environment module) 
  (vector-ref module 1))

;;; protect environment
(define (module-protect-environment module) 
  (vector-ref module 2))

;;; package and module name 
(define (module-package-name module) (vector-ref module 3))
(define (module-name module) (vector-ref module 4))
(define (make-module eval export protect package name) (vector eval export protect package name (list )))

;; Return the interface environment for module.  The interface is
;; either the protect environment if the module is accessed from
;; another package or the module's export environment
(define module-interface-environment
  (lambda (current-module module)
    (if (eq? (module-package-name current-module)
	     (module-package-name module))
	(module-protect-environment module)
	(module-export-environment module))))

;; export-list used by define-public
(define (module-export-list module) (vector-ref module 5))
(define (module-set-export-list! module val) (vector-set! module 5 val))

;; return the current module-signature 
(define the-module
  (procedure->syntax
   (lambda (x env)
     (let ((m (hashq-ref (environment-module-hash) (car (reverse env)))))
       (if (not m) 
	   (error "PANIC: Could not find module for " (car (reverse env)))
	   m)))))


;;; Convert a list of symbols into a package/module name

;; (ice-9 my boot) -> (ice-9/my/ . boot)
;; (boot) -> (user/ . boot)
(define symlist->package+module
  (lambda (symlist)
    (if (not (pair? symlist)) (error "not a symbol list:" symlist))
    (let ((symlist (if (null? (cdr symlist)) (cons 'user symlist) symlist)))
      (let loop ((dirs "")
		 (syms symlist))
	(let ((sym (car syms)))
	  (if (not (symbol? sym)) (error "not a valid symbol:" sym))
	  (if (null? (cdr syms))
	      (cons (string->symbol dirs) sym)
	      (loop (string-append dirs sym "/") (cdr syms))))))))

;; (ice-9/my/ . boot) -> ice-9/my/boot
(define package+module->module-sym
  (lambda (name-pair)
    (string->symbol
     (string-append (symbol->string (car name-pair))
		    (symbol->string (cdr name-pair))))))

;; (ice-9/my/ . boot) -> ice-9/my/
(define package+module->package-sym
  (lambda (name-pair)
    (car name-pair)))

;; (ice-9 my boot) -> ice-9/my/boot
(define symlist->module-sym
  (lambda (mod)
    (package+module->module-sym (symlist->package+module mod))))


;;; The implementation (low level proc. interface)

;; load and evaluate package-sym module-sym in its own environment
(define module-load-internal
  (lambda (package-sym module-sym)
    (let* (
	   (skel (make-module-skeleton package-sym module-sym))
	   (skel-env (module-eval-environment skel))
	   (c_name (string-append (symbol->string module-sym) ".scm"))
	   (full_name (%search-load-path c_name)))
      
      (if (not full_name)
	  ;; try to invoke c-function directly
	  (let (
		(c_module_init (and
				(environment-bound? (c-module-registry) module-sym)
				(environment-ref (c-module-registry) module-sym))))
	    (if (not c_module_init)
		(begin
		  ;; remove all references to the created module
		  (environment-undefine (module-registry) module-sym)
		  #f)
		(begin
		  (c_module_init skel-env)

		  ;; now modify the export and protect environment so
		  ;; that all cells are visible
		  (let ((export (module-export-environment skel)))
		    (interface-environment-set-interface! 
		     export 
		     (list (cons skel-env (environment-fold skel-env (lambda (sym val last) (cons sym last)) (list )))))
		    (interface-environment-set-interface! (module-protect-environment skel) (list (cons export #f))))

		  skel)))
	  
	  (begin 
	    (primitive-load full_name skel-env)
	    skel)))))

;; load `(package module)' 
(define module-load-proc
  (lambda (mod)
    (let* (
	   (name-pair (symlist->package+module mod))
	   (package-sym (package+module->package-sym name-pair))
	   (module-sym (package+module->module-sym name-pair)))
      (module-load-internal package-sym module-sym))))

(define last-indent 0)

;; load package-sym/module-sym only if module has not been loaded
(define module-auto-load
  (lambda (package-sym module-sym)
    (let (
	  (module (and 
		   (environment-bound? (module-registry) module-sym) 
		   (environment-ref (module-registry) module-sym)))
	  (indent (lambda (count)
	    (do ((i 0 (+ i 1))) ((= i count)) (display " ")))))
		    
      (if (not module)

	  (begin
	    (display "loading ") 
	    (indent last-indent)
	    (display module-sym) (force-output)
	    (newline)

	    (set! last-indent (+ last-indent 2))
	    (let ((module (module-load-internal package-sym module-sym)))

	      (set! last-indent (- last-indent 2))
	      module))

	  module))))

;; Create a continuation to start a new repl in module.
;; #f means: start a repl in (user config)
(define module-go
  (lambda (module)

    (define (y-or-n? question eof-value count)
      (let ((i-port (current-input-port))
	    (o-port (current-output-port)))
	(let loop ((count count))
	  (display question o-port)
	  (display " (y/n)? " o-port)
	  (force-output)
	  (let ((line (read i-port)))
	    (cond ((eof-object? line)
		   (newline o-port)
		   (if (= count 0)
		       eof-value
		       (begin (display "I'll only ask another " o-port)
			      (write count o-port)
			      (display " times." o-port)
			      (newline o-port)
			      (loop (- count 1)))))
		  ((< (string-length line) 1) (loop count))
		  ((char=? (string-ref line 0) #\y) #t)
		  ((char=? (string-ref line 0) #\n) #f)
		  (else (loop count)))))))

    (let ((guile (module-access-proc '(ice-9 repl)))
	  (user/config (module-create (user config)))
	  (quit  (lambda args
		   (apply throw 'quit args))))
      
      ;; (user config) opens the current module (ice-9 config) and
      ;; imports some symbols
      (let ((interface (eval-environment-imported (module-eval-environment user/config)))
	    (export (module-export-environment (the-module)))
	    (import-list '(module-ref module-access module-close module-create the-module
				      module-use define-module module module-go go
				      (module-config-export (alias config-export))
				      (module-config-protect (alias config-protect))
				      (module-config-open (alias config-open)))))

	(interface-environment-set-interface! interface (list (cons export import-list))))


      (let (
	    (top-repl (module-ref-proc (the-module) guile 'top-repl)))

	(let (
	      (in-module (call-with-current-continuation (lambda (k) (set! module-go k) module))))

	  (letrec (
	      (confirm
	       (lambda ()
		 (newline) 
		 (y-or-n? "Do you really want to quit guile" #t 1)))
		 
	      (user-repl
	       (lambda ()
		 (set-interaction-environment! (module-eval-environment in-module))
		 (let ((ret-val (top-repl in-module)))
		   (newline)
		   ret-val)))

	      (config-repl 
	       (lambda () 
		 (set-interaction-environment! (module-eval-environment user/config))
		 (top-repl user/config))))
	  
	    (let (
		  (ret-val
		   (or (and in-module (user-repl)) (config-repl))))

	      (if (not (confirm)) (module-go #f))

	      ret-val)))))))

;; fix the interface of the guile module.
(let* (
       (guile-module (environment-ref (module-registry) 'ice-9/guile))
       (guile-eval-env (module-eval-environment guile-module))
       (guile-export-list (environment-fold guile-eval-env (lambda (sym val rest) (cons sym rest)) '()))
       (guile-interface (list (cons guile-eval-env guile-export-list)))
       (export (module-export-environment guile-module))
       (protect (module-protect-environment guile-module)))
  
  ;; get out your barf bag ...  It is probably better to export
  ;; %load-path from (ice-9 config) and *features* from whereever but
  ;; not from the guile module
  ;;
					; export mutable
  (for-each (lambda (s) 
	      (set-car! (memq s guile-export-list) (list s 'mutable-location)))
	    (list 
	     '%load-path 
	     '*features*
					; complete weired ... (from r4rs.scm)
	     '%load-hook
	     'apply
					; argh... (popen.scm)
	     'gc-thunk
	     ))


  
					; now set the interface
  (interface-environment-set-interface! export guile-interface)
  (interface-environment-set-interface! protect guile-interface))

;; update the config module `(ice-9 config)'
(let* (
      (config-module (environment-ref (module-registry) 'ice-9/config))
      (eval (module-eval-environment config-module))
      (export (module-export-environment config-module))
      (protect (module-protect-environment config-module)))

  (interface-environment-set-interface! export (list (cons eval the-module-exports)))
  (interface-environment-set-interface! protect (list (cons eval the-module-protects) (cons export #f))))

;; create a simple but full-featured module.
(define make-module-skeleton
  (lambda (package-sym module-sym) 
    (let* (
	   (config-export-env (module-export-environment 
			       (environment-ref (module-registry) 'ice-9/config)))
	   (local (make-leaf-environment))
	   (import (make-interface-environment (list (cons config-export-env #f))))
	   (eval (make-eval-environment local import))
	   (export (make-interface-environment (list (list eval)))) ;; export nothing
	   (protect (make-interface-environment (list (cons export #f) (list eval))))
	   (module  (make-module eval export protect package-sym module-sym)))

      (hashq-set! (environment-module-hash) eval module)
      (environment-define (module-registry) module-sym module)

      module)))

(define module-create-proc
  (lambda (mod)
    (let* (
	   (name-pair (symlist->package+module mod))
	   (package-sym (package+module->package-sym name-pair))
	   (module-sym (package+module->module-sym name-pair))
	   (module (and
		    (environment-bound? (module-registry) module-sym) 
		    (environment-ref (module-registry) module-sym))))

      (if (not module)
	  (let* ( 
		 (c_name (string-append (symbol->string module-sym) ".scm"))
		 (full_name (%search-load-path c_name)))
	    (if (not full_name)
		(let* (
		       (skel (make-module-skeleton package-sym module-sym))
		       (eval-env (module-eval-environment skel)))
		  skel)
		
		(error "module exists but has not been loaded" module-sym)))
	  (error "module exists and has been loaded" module-sym)))))


(define module-load-error
  (lambda (module-sym)
    (error "module could not be loaded" module-sym)))

;; add an export environment to our module skeleton or set a new
(define (module-export-proc module export-list)
  (let (
	(export (module-export-environment module))
	(eval (module-eval-environment module)))

    (if (pair? (module-export-list module)) 
	(error "You can either use `define-public' or `export', but not both."))
    
    (module-set-export-list! module #f) ;; can't use `define-public' anymore

    (interface-environment-set-interface! export (list (cons eval export-list)))
    (if #f #f)))

;; package boundary
(define (module-protect-proc module protect-list)
  (let ((protect (module-protect-environment module))
	(export (module-export-environment module))
	(eval (module-eval-environment module)))
    
    (interface-environment-set-interface! protect (list (cons eval protect-list) (cons export #f)))
    (if #f #f)))


;; check for circular references
;; every interface-environment has a list of environment specs
;; an environment spec is a list of the environment and its exported symbols
;; every eval-environment has an interface-environment
(define referenced 
  (lambda (environment l)
    (letrec (
	     (search 
	      (lambda (l)
		(cond ((null? l) #f) 
		      ((eq? (caar l) environment) #t)
	              (else 
			(or (search (get-list-from-env (caar l)))
			    (search (cdr l)))))))
	     
	     (get-list-from-env 
	      (lambda (env)
		(cond ((interface-environment? env) 
			(interface-environment-interface env))
	
		       ((eval-environment? env)
			(get-list-from-env (eval-environment-imported env)))

		       (else (error "FIXME: unsupported environment" env))))))
      
      (search l))))
		  
		  
(define module-access-proc
  (lambda (mod)
    (let* (
	   (name-pair (symlist->package+module mod))
	   (package-sym (package+module->package-sym name-pair))
	   (module-sym (package+module->module-sym name-pair)))

      (or (module-auto-load package-sym module-sym) 
	  (module-load-error module-sym)))))


;; remove module from module-registry
(define module-close-proc
  (lambda (mod)
    (let* (
	   (name-pair (symlist->package+module mod))
	   (package-sym (package+module->package-sym name-pair))
	   (module-sym (package+module->module-sym name-pair)))

      (environment-undefine (module-registry) module-sym))))


(define module-open-internal
  (lambda (skel imports)

    (let* (
	  (eval (module-eval-environment skel))
	  (import (eval-environment-imported eval)))
      
      (if (referenced eval imports)
 	  (error "Can't open modules: circular dependency for " eval))

      (interface-environment-set-interface! import imports))))

(define import-symlist->import-list
  (lambda (skel import-symlist)
    (letrec 
	((fix-imports 
	  (lambda (l)
	    (cond	
	     ((null? l) '())
	     ((not (list? (caar l))) (cons (cons (car l) #f) (fix-imports (cdr l))))
	     (else (cons (car l) (fix-imports (cdr l))))))))

      (let* (
	     (import-symlist (fix-imports import-symlist))
	     (import-modlist (map (lambda (x) (cons (module-access-proc (car x)) (cdr x))) import-symlist))
	     (import-list (map (lambda (x) (cons (module-interface-environment skel (car x)) (cdr x))) import-modlist)))

 	import-list))))

(define module-open-proc
  (lambda (skel import-symlist)
    (module-open-internal skel (import-symlist->import-list skel import-symlist))))
    
(define module-use-proc
  (lambda (skel import-symlist)
    (let* (
	   (eval (module-eval-environment skel))
	   (interface-env (eval-environment-imported eval))
	   (interface (interface-environment-interface interface-env))
	   (import-list (import-symlist->import-list skel import-symlist)))

      (module-open-internal skel (append import-list interface))
      (if #f #f)
      )))


(define module-ref-proc
  (lambda (curr module sym)
    (let (
	  (name (module-name module))
	  (interface-env (module-interface-environment curr module))
	  )

      (cond
       ((environment-bound? interface-env sym)
	(environment-ref interface-env sym))
       
       (else (scm-error 'misc-error #f "Symbol `%s' is not exported from `%s'." (list sym name) #f))))))


(define (module-proc current-module args)
  (let*  (
	  (mod (car args))
	  (arglist (cdr args))
	  (current-module-sym (module-name current-module))
	  (sym (symlist->module-sym mod))
	  (same-module (eq? current-module-sym sym))
	  (has-exports (assq 'export arglist))
	  (skel (if same-module current-module (module-create-proc mod))))

    (let loop ((arglist arglist)
	       (modules '())
	       (exports '())
	       (protects '()))

      (if (null? arglist)
	  (begin

	    (if (not same-module) 
		(error "Can't manipulate a foreign module's interface."))
	    
	    (module-open-proc skel modules)
	    
	    (if has-exports (module-export-proc skel exports))
	    
	    (module-protect-proc skel protects)
	    )


	  (begin
	    (if (not (pair? (car arglist)))
		(error  "unrecognized module argument in" current-module-sym))
	    
	    (let ((fkt (caar arglist)))
	      (case fkt
		((open)
		 (loop (cdr arglist)
		       (append modules (cdar arglist))
		       exports
		       protects))

		((export)
		 (loop (cdr arglist)
		       modules
		       (append exports (cdar arglist))
		       protects))

		((protect)
		 (loop (cdr arglist)
		       modules
		       exports
		       (append protects (cdar arglist))))
		(else	
		 (error "unrecognized module argument in"  current-module-sym)))))))))

(define (go-proc module)
  (module-go (module-access-proc module)))


;;; High level interface

;; declare a module
(define module
  (procedure->memoizing-macro
   (lambda (x env)
     `(,module-proc (,the-module) (,quote ,(cdr x) )))))

;; open module and start a repl in it
(define go
  (procedure->memoizing-macro
   (lambda (x env)
     (if (not (list? (cadr x))) (error "not a list:" (cadr x)))
     `(,go-proc (,quote ,(cadr x))))))


;;; low level interface

;; access symbol exported from module
(define module-ref
  (procedure->memoizing-macro
   (lambda (x env)
     `(,module-ref-proc (,the-module) ,(cadr x) ,(caddr x)))))

;; add module to import-list
(define module-use
  (procedure->memoizing-macro
   (lambda (x env)
     `(,module-use-proc (,the-module) (,quote ,(cdr x))))))

;; start a new import-list
(define module-open
  (procedure->memoizing-macro
   (lambda (x env)
     `(,module-open-proc (,the-module) (,quote ,(cdr x))))))

;; load a module
(define module-load
  (procedure->memoizing-macro
   (lambda (x env)
     (if (not (
	       list? (cadr x))) (error "not a list:" (cadr x)))
     `(,module-load-proc (,quote ,(cadr x))))))

;; publish symbols (visible to all other modules)
(define module-protect
  (procedure->memoizing-macro
   (lambda (x env)
     `(,module-protect-proc (,the-module) (,quote ,(cdr x) )))))

;; make symbols visible to modules that belong the current package
(define module-export
  (procedure->memoizing-macro
   (lambda (x env)
     `(,module-export-proc (,the-module) (,quote ,(cdr x))))))

;; open a module and return a handle
(define module-access
  (procedure->memoizing-macro
   (lambda (x env)
     (if (not (list? (cadr x))) (error "not a list:" (cadr x)))
     `(,module-access-proc (,quote ,(cadr x))))))

;; close a module (remove it from the module-registry)
(define module-close
  (procedure->memoizing-macro
   (lambda (x env)
     (if (not (list? (cadr x))) (error "not a list:" (cadr x)))
     `(,module-close-proc (,quote ,(cadr x))))))

;; create a in-memory module and return a handle
(define module-create
  (procedure->memoizing-macro
   (lambda (x env)
     (if (not (list? (cadr x))) (error "not a list:" (cadr x)))
     `(,module-create-proc (,quote ,(cadr x) )))))

;; import symbols from other modules
(define module-config-open
  (procedure->memoizing-macro
   (lambda (x env)
     `(,module-open-proc (,module-access-proc (,quote ,(cadr x))) (,quote ,(cddr x))))))

;; make symbols visible within the current package
(define module-config-protect
  (procedure->memoizing-macro
   (lambda (x env)
     `(,module-protect-proc (,module-access-proc (,quote ,(cadr x))) (,quote ,(cddr x))))))

;; make symbols visible to the world
(define module-config-export
  (procedure->memoizing-macro
   (lambda (x env)
     `(,module-export-proc (,module-access-proc (,quote ,(cadr x))) (,quote ,(cddr x))))))


;;; For backward compatibility 
;;; 

(define (define-module-proc current-module args)
  (let*  (
	  (kws (cdr args))
	  (current-module-sym (module-name current-module))
	  (mod (car args))
	  (sym (symlist->module-sym mod))

	  ;; do not create a new module if the module names are the
	  ;; same and the current module has not been closed
	  (same-module (and (eq? current-module-sym sym) 
			    (environment-bound? (module-registry) current-module-sym)))

	  (skel (if same-module 
		    current-module 
		    (module-create-proc mod))))


    (let loop ((kws kws)
	       (modules '()))
      (if (null? kws)
	  (begin
	    ;; append (ice-9 guile) and (ice-9 config) for convenience
	    (module-open-proc skel (cons '(ice-9 config) (cons '(ice-9 guile) modules)))
	    (if (not same-module) (module-go (module-access-proc mod)))
	    )
	  
	  (let ((keyword (car kws)))
	    (let ((keyword (if (keyword? keyword) (keyword->symbol keyword) keyword)))
	      (case keyword
		((use-module use-syntax autoload :use-module :use-syntax :autoload)
		 (let (
		       (mod (cadr kws)))
		   
		   (if (not (pair? mod))
		       (error "unrecognized defmodule argument" kws))
		   
		   (loop (cddr kws)
			 (cons mod modules))))
		((no-backtrace :no-backtrace)
		 (loop (cdr kws) modules))
		(else	
		 (error "unrecognized defmodule argument" kws)))))))))

(define define-module
  (procedure->memoizing-macro
   (lambda (x env)
     `(,define-module-proc (,the-module) (,quote ,(cdr x) )))))

;; rewrite me 
(define define-public 
  (procedure->memoizing-macro
   (lambda (x env)
     (let ((args (cdr x)))
       (letrec (
		(syntax (lambda () 
			  (error "bad syntax" (list 'define-public args))))
		(defined-name 
		  (lambda (n)
		    (cond 
		     ((symbol? n) n)
		     ((pair? n) (defined-name (car n)))
		     (else (syntax))))) 
		)

	 (if (null? args) (syntax))

	 (let (
	       (name (defined-name (car args))))

	   `(begin
	      (letrec (
		       ;; according to environment proposal either
		       ;; symbol or (sym tags)
		       (get-symbol
			(lambda (s)
			  (cond 
			   ((pair? s) (car s))
			   (else s))))
		       
		       ;; test if symbol is already exported
		       (exported?
			(lambda (sym l)
			  (cond
			   ((null? l) #f)
			   ((eq? (get-symbol (car l)) sym) #t)
			   (else (exported? sym (cdr l)))))))
		
		(let* (
		       (module-eval-environment ,module-eval-environment)
		       (module-export-environment ,module-export-environment)
		       (interface-environment-interface ,interface-environment-interface)
		       (environment-bound? ,environment-bound?)
		       (environment-ref ,environment-ref)
		       (environment-define ,environment-define)
		       (the-module ,the-module)
		       (the-environment ,the-environment)
		       (interface-environment-set-interface! ,interface-environment-set-interface!)
		       (module-export-list ,module-export-list)
		       
		       (module (the-module))
		       (eval-env (module-eval-environment module))
		       (export (module-export-environment module))
		       (export-list (module-export-list module))

		       (val (,and (,environment-bound? eval-env ',name)
				 (,environment-ref eval-env ',name))))
		  
		  (if (not (list? export-list))
		      (,error "You can either use `define-public' or `export', but not both"))

		  (if (not (memq ',name export-list))
		      (begin
			(environment-define eval-env ',name val)
			(set! export-list (cons ',name export-list))
			(,module-set-export-list! module export-list)
			(interface-environment-set-interface! 
			 export
			 (list (cons eval-env export-list)))))


		  
		  ;; Now (re)define the var normally.  Bernard URBAN
		  ;; suggests we use eval here to accomodate Hobbit; it lets
		  ;; the interpreter handle the define-private form, which
		  ;; Hobbit can't digest.
		  (eval '(define ,@ args) (the-environment))

		  )))))))))


;; the entry point (called by (ice-9 repl))
(define start
  (lambda ()
    (module-go #f)))

