;;; this is an add-on module for those who want to "load" the module
;;; system.


;; load the standard module system and add the module which exports
;; the module configuration language to the current module's import
;; list
(eval
 `(letrec (
	   

					; return #f if module system
					; has already been loaded
	   (load-module-system

	    (let (
		  (load-sys-modules
		   (lambda (repl-env)
		     (if (not (environment-bound? (module-registry) 'ice-9/config))
			 (begin
			   (eval '(make-guile-module) repl-env)
			   (eval '(make-repl-module) repl-env)
			   (eval '(make-user-module) repl-env)
			   (let (
				 (ice-9/config (eval '(make-config-module) repl-env)))
			     (primitive-load-path "ice-9/config.scm" (vector-ref ice-9/config 0))
			     #t))
			 #f))))
	      
	      (lambda ()
					; the code that creates system
					; modules comes from the repl
					; module.
		
		(if (not (environment-bound? (module-registry) 'ice-9/repl))
		    
		    (let* (
			   (import (make-interface-environment (list (cons (scheme-guile-environment 2) #t))))
			   (eval-env (make-eval-environment (make-leaf-environment) import)))
		      (primitive-load-path "ice-9/repl.scm" eval-env) 
		      (load-sys-modules eval-env))
		    (let* (
			   (repl-module (environment-ref (module-registry) 'ice-9/repl))
			   (eval-env (vector-ref repl-module 0)))
		      (load-sys-modules eval-env))))))
	    
					; fix current interface
	   (set-new-interface 
	    (lambda (current-interface-env)
	      (let (
		    (guile-export (vector-ref (environment-ref (module-registry) 'ice-9/guile) 1))
		    (config-export (vector-ref (environment-ref (module-registry) 'ice-9/config) 1)))
		
		(interface-environment-set-interface! 
		 current-interface-env
		 (list (cons config-export #f) (cons guile-export #f))))))
	   
	   
					; append config-export to
					; current interface
	   (append-interface 
	    (lambda (current-interface-env)
	      (let (
		    (import-interface (interface-environment-interface current-interface-env))
		    (config-export (vector-ref (environment-ref (module-registry) 'ice-9/config) 1)))
		
		(if (not (assq config-export import-interface))
		    (interface-environment-set-interface!
		     current-interface-env
		     (cons (cons config-export #t) import-interface)))))))
    
    (let* (
	   (current-eval-env ,(the-environment))
	   (current-interface-env (eval-environment-imported current-eval-env)))
      
      (if (load-module-system)
	  (set-new-interface  current-interface-env)
	  (append-interface current-interface-env))))

 (scheme-guile-environment 2))


