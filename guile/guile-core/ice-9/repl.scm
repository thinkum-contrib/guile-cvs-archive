;;; This is the module (ice-9 repl).
;;;

;; the current module
(define make-repl-module
  (lambda ()
    (let ((ice-9/repl (and (environment-bound? (module-registry) 'ice-9/repl)
			   (environment-ref (module-registry) 'ice-9/repl))))
      (if (not ice-9/repl)
	  (let* (
		 (eval (the-environment))
		 (export (make-interface-environment (list (list eval 'top-repl))))
		 (protect (make-interface-environment (list (list eval 'top-repl 'save-stack 'abort-hook))))
		 (ice-9/repl (vector eval export protect 'ice-9/ 'ice-9/repl)))
	    (environment-define (module-registry) 'ice-9/repl ice-9/repl)
	    (hashq-set! (environment-module-hash) eval ice-9/repl))
	  ice-9/repl))))

;; the guile module
(define make-guile-module
  (lambda ()
    (let ((ice-9/guile (and (environment-bound? (module-registry) 'ice-9/guile)
			   (environment-ref (module-registry) 'ice-9/guile))))

      (if (not ice-9/guile)
	  (let* (
		 (eval (scheme-guile-environment 2))
		 (export (make-interface-environment (list (cons eval #t))))
		 (protect (make-interface-environment (list (cons eval #t))))
		 (ice-9/guile (vector eval export protect 'ice-9/ 'ice-9/guile)))
	    (environment-define (module-registry) 'ice-9/guile ice-9/guile)
	    (hashq-set! (environment-module-hash) eval ice-9/guile))
	  ice-9/guile))))
	 
(define make-user-module
  (lambda ()
    (let ((user/guile (and (environment-bound? (module-registry) 'user/guile)
			   (environment-ref (module-registry) 'user/guile))))

      (if (not user/guile)
	  (let* (
		 (eval-env (guile-user-environment 2))
		 (export (make-interface-environment (list (cons eval-env #t))))
		 (protect (make-interface-environment (list (cons eval-env #t))))
		 (user/guile (vector eval-env export protect 'user/ 'user/guile)))
	    
	    (environment-define (module-registry) 'user/guile user/guile)
	    (hashq-set! (environment-module-hash) eval-env user/guile))
	  user/guile))))

(define make-config-module
  (lambda ()
    (let ((ice-9/config (and (environment-bound? (module-registry) 'ice-9/config)
			   (environment-ref (module-registry) 'ice-9/config))))

      (if (not ice-9/config)
	  (let* ((import (make-interface-environment (list (cons (scheme-guile-environment 2) #t))))
		 (eval-env (make-eval-environment (make-leaf-environment) import))
		 (export (make-interface-environment (list (cons eval-env #t))))
		 (protect (make-interface-environment (list (cons eval-env #t))))
		 (ice-9/config (vector eval-env export protect 'ice-9/ 'ice-9/config)))
	    
	    (environment-define (module-registry) 'ice-9/config ice-9/config)
	    (hashq-set! (environment-module-hash) eval-env ice-9/config))
	  ice-9/config))))
	    



;;; some more redundancy

(define (module-name module) (vector-ref module 4))
(define (module-eval-environment module) (vector-ref module 0))

(define (run-hooks hook)
  (if (and (pair? hook) (eq? (car hook) 'hook))
      (run-hook hook)
      (for-each (lambda (thunk) (thunk)) hook)))



;;; {Running Repls}
;;;

(define (repl read evaler print)
  (let loop ((source (read (current-input-port))))
    (print (evaler source))
    (loop (read (current-input-port)))))

;; A provisional repl that acts like the SCM repl:
;;
(define scm-repl-silent #f)
(define (assert-repl-silence v) (set! scm-repl-silent v))

(define *unspecified* (if #f #f))
(define (unspecified? v) (eq? v *unspecified*))

(define scm-repl-print-unspecified #f)
(define (assert-repl-print-unspecified v) (set! scm-repl-print-unspecified v))

(define scm-repl-verbose #f)
(define (assert-repl-verbosity v) (set! scm-repl-verbose v))

(define scm-repl-prompt "guile> ")

(define (set-repl-prompt! v) (set! scm-repl-prompt v))

(define (default-lazy-handler key . args)
  (save-stack lazy-handler-dispatch)
  (apply throw key args))

(define enter-frame-handler default-lazy-handler)
(define apply-frame-handler default-lazy-handler)
(define exit-frame-handler default-lazy-handler)

(define (lazy-handler-dispatch key . args)
  (case key
    ((apply-frame)
     (apply apply-frame-handler key args))
    ((exit-frame)
     (apply exit-frame-handler key args))
    ((enter-frame)
     (apply enter-frame-handler key args))
    (else
     (apply default-lazy-handler key args))))

(define abort-hook (make-hook))

;; these definitions are used if running a script.
;; otherwise redefined in error-catching-loop.
(define (set-batch-mode?! arg) #t)
(define (batch-mode?) #t)

(define (error-catching-loop thunk)
  (let ((status #f)
	(interactive #t))
    (define (loop first)
      (let ((next 
	     (catch #t

		    (lambda ()
		      (lazy-catch #t
				  (lambda ()
				    (dynamic-wind
					(lambda () (unmask-signals))
					(lambda ()
					  (with-traps
					   (lambda ()
					     (first)
					     
					     ;; This line is needed because mark
					     ;; doesn't do closures quite right.
					     ;; Unreferenced locals should be
					     ;; collected.
					     ;;
					     (set! first #f)
					     (let loop ((v (thunk)))
					       (loop (thunk)))
					     #f)))
					(lambda () (mask-signals))))

				  lazy-handler-dispatch))
		    
		    (lambda (key . args)
		      (case key
			((quit)
			 (force-output)
			 (set! status (car args))
			 #f)

			((switch-repl)
			 (apply throw 'switch-repl args))

			((abort)
			 ;; This is one of the closures that require
			 ;; (set! first #f) above
			 ;;
			 (lambda ()
			   (run-hook abort-hook)
			   (force-output)
			   (display "ABORT: "  (current-error-port))
			   (write args (current-error-port))
			   (newline (current-error-port))
			   (if interactive
			       (if (and (not has-shown-debugger-hint?)
					(not (memq 'backtrace
						   (debug-options-interface)))
					(stack? (fluid-ref the-last-stack)))
				   (begin
				     (newline (current-error-port))
				     (display
				      "Type \"(backtrace)\" to get more information.\n"
				      (current-error-port))
				     (set! has-shown-debugger-hint? #t)))
			       (primitive-exit 1))
			   (set! stack-saved? #f)))

			(else
			 ;; This is the other cons-leak closure...
			 (lambda ()
			   (cond ((= (length args) 4)
				  (apply handle-system-error key args))
				 (else
				  (apply bad-throw key args))))))))))
	(if next (loop next) status)))

    (set! set-batch-mode?! (lambda (arg)
			     (cond (arg 
				    (set! interactive #f)
				    (restore-signals))
				   (#t
				    (error "sorry, not implemented")))))
    (set! batch-mode? (lambda () (not interactive)))

    (loop (lambda () #t))))

;;(define the-last-stack (make-fluid)) Defined by scm_init_backtrace ()
(define stack-saved? #f)

(define (save-stack . narrowing)
  (or stack-saved?
      (cond ((not (memq 'debug (debug-options-interface)))
	     (fluid-set! the-last-stack #f)
	     (set! stack-saved? #t))
	    (else
	     (fluid-set!
	      the-last-stack
	      (case (stack-id #t)
		((repl-stack)
		 (apply make-stack #t save-stack eval #t 0 narrowing))
		((load-stack)
		 (apply make-stack #t save-stack 0 #t 0 narrowing))
		((tk-stack)
		 (apply make-stack #t save-stack tk-stack-mark #t 0 narrowing))
		((#t)
		 (apply make-stack #t save-stack 0 1 narrowing))
		(else
		 (let ((id (stack-id #t)))
		   (and (procedure? id)
			(apply make-stack #t save-stack id #t 0 narrowing))))))
	     (set! stack-saved? #t)))))

(define before-error-hook (make-hook))
(define after-error-hook (make-hook))
(define before-backtrace-hook (make-hook))
(define after-backtrace-hook (make-hook))

(define has-shown-debugger-hint? #f)

(define (handle-system-error key . args)
  (let ((cep (current-error-port)))
    (cond ((not (stack? (fluid-ref the-last-stack))))
	  ((memq 'backtrace (debug-options-interface))
	   (run-hook before-backtrace-hook)
	   (newline cep)
	   (display-backtrace (fluid-ref the-last-stack) cep)
	   (newline cep)
	   (run-hook after-backtrace-hook)))
    (run-hook before-error-hook)
    (apply display-error (fluid-ref the-last-stack) cep args)
    (run-hook after-error-hook)
    (force-output cep)
    (throw 'abort key)))


;;(define has-shown-backtrace-hint? #f) Defined by scm_init_backtrace ()

;; Replaced by C code:
;;(define (backtrace)
;;  (if (fluid-ref the-last-stack)
;;      (begin
;;	(newline)
;;	(display-backtrace (fluid-ref the-last-stack) (current-output-port))
;;	(newline)
;;	(if (and (not has-shown-backtrace-hint?)
;;		 (not (memq 'backtrace (debug-options-interface))))
;;	    (begin
;;	      (display
;;"Type \"(debug-enable 'backtrace)\" if you would like a backtrace
;;automatically if an error occurs in the future.\n")
;;	      (set! has-shown-backtrace-hint? #t))))
;;      (display "No backtrace available.\n")))

(define (error-catching-repl r e p)
  (error-catching-loop (lambda () (p (e (r))))))5

(define (gc-run-time)
  (cdr (assq 'gc-time-taken (gc-stats))))

(define before-read-hook (make-hook))
(define after-read-hook (make-hook))

;;; The default repl-reader function.  We may override this if we've
;;; the readline library.
(define repl-reader
  (lambda (prompt)
    (display prompt)
    (force-output)
    (run-hook before-read-hook)
    (read (current-input-port))))

(define (scm-style-repl module)
  (letrec (
	   (start-gc-rt #f)
	   (start-rt #f)
	   (repl-report-start-timing (lambda ()
				       (set! start-gc-rt (gc-run-time))
				       (set! start-rt (get-internal-run-time))))
	   (repl-report (lambda ()
			  (display ";;; ")
			  (display (inexact->exact
				    (* 1000 (/ (- (get-internal-run-time) start-rt)
					       internal-time-units-per-second))))
			  (display "  msec  (")
			  (display  (inexact->exact
				     (* 1000 (/ (- (gc-run-time) start-gc-rt)
						internal-time-units-per-second))))
			  (display " msec in gc)\n")))

	   (consume-trailing-whitespace
	    (lambda ()
	      (let ((ch (peek-char)))
		(cond
		 ((eof-object? ch))
		 ((or (char=? ch #\space) (char=? ch #\tab))
		  (read-char)
		  (consume-trailing-whitespace))
		 ((char=? ch #\newline)
		  (read-char))))))
		 
	   (-read (lambda ()
		    (let ((val
			   (let ((prompt (string-append (module-name module) "> "  )))
			     (repl-reader prompt))))

		      (run-hooks after-read-hook)
		      (if (eof-object? val)
			  (begin
			    (repl-report-start-timing)
			    (if scm-repl-verbose
				(begin
				  (newline)
				  (display ";;; EOF -- quitting")
				  (newline)))
			    (throw 'quit #f)))

		      ;; As described in R4RS, the READ procedure updates the
		      ;; port to point to the first characetr past the end of
		      ;; the external representation of the object.  This
		      ;; means that it doesn't consume the newline typically
		      ;; found after an expression.  This means that, when
		      ;; debugging Guile with GDB, GDB gets the newline, which
		      ;; it often interprets as a "continue" command, making
		      ;; breakpoints kind of useless.  So, consume any
		      ;; trailing newline here, as well as any whitespace
		      ;; before it.
		      (consume-trailing-whitespace)

		      val)))

	   (-eval (lambda (sourc)
		    (repl-report-start-timing)
		    (start-stack 'repl-stack (eval sourc (module-eval-environment module)))))

	   (-print (lambda (result)
		     (if (not scm-repl-silent)
			 (begin
			   (if (or scm-repl-print-unspecified
				   (not (unspecified? result)))
			       (begin
				 (write result)
				 (newline)))
			   (if scm-repl-verbose
			       (repl-report))
			   (force-output)))))

	   (-quit (lambda (args)
		    (if scm-repl-verbose
			(begin
			  (display ";;; QUIT executed, repl exitting")
			  (newline)
			  (repl-report)))
		    args))

	   (-abort (lambda ()
		     (if scm-repl-verbose
			 (begin
			   (display ";;; ABORT executed.")
			   (newline)
			   (repl-report)))
		     (repl -read -eval -print))))

    (let ((status (error-catching-repl -read
				       -eval
				       -print)))
      (-quit status))))


;; this is just (scm-style-repl) with a wrapper to install and remove 
;; signal handlers.  Every module may have exactly one repl.


(define (top-repl module) 
  (let ((old-handlers #f)
	(signals `((,SIGINT . "User interrupt")
		   (,SIGFPE . "Arithmetic error")
		   (,SIGBUS . "Bad memory access (bus error)")
		   (,SIGSEGV . "Bad memory access (Segmentation violation)"))))

    (dynamic-wind

	;; call at entry
	(lambda ()
	  (let ((make-handler (lambda (msg)
				(lambda (sig)
				  (save-stack %deliver-signals)
				  (scm-error 'signal
					     #f
					     msg
					     #f
					     (list sig))))))
	    (set! old-handlers
		  (map (lambda (sig-msg)
			 (sigaction (car sig-msg)
				    (make-handler (cdr sig-msg))))
		       signals))))

	;; the protected thunk.
	(lambda ()
	  (scm-style-repl module))


	;; call at exit.
	(lambda ()
	  (map (lambda (sig-msg old-handler)
		 (if (not (car old-handler))
		     ;; restore original C handler.
		     (sigaction (car sig-msg) #f)
		     ;; restore Scheme handler, SIG_IGN or SIG_DFL.
		     (sigaction (car sig-msg)
				(car old-handler)
				(cdr old-handler))))
	       signals old-handlers)))))

;;; This hook is run at the very end of an interactive session.
;;;
(define exit-hook (make-hook))



;; now provide a function which starts the repl
(define start
  (lambda ()
    (make-guile-module)
    (make-repl-module)
    (let ((user/guile (make-user-module)))

      (set-interaction-environment! (vector-ref user/guile 0))

      (let (
	    (ret-val (top-repl user/guile))

	    (start-module-system 
	     (lambda ()
	       ;; create (ice-9 config) and re-start the repl in there
	       (let* ((ice-9/config (make-config-module))
		      (eval-env (vector-ref ice-9/config 0)))
		 (primitive-load-path "ice-9/config.scm" eval-env)
		 (eval '(start) eval-env)))))

	(if (and (eqv? #f ret-val) (isatty? (current-input-port)))
	    (begin 
	      (newline) 
	      (set! ret-val (start-module-system))))
	
	(or ret-val 0)))))


