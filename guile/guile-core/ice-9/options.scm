(define-module (ice-9 options) :use-module (ice-9 defmacro))
;;; {Command Line Options}
;;;

(define (get-option argv kw-opts kw-args return)
  (cond
   ((null? argv)
    (return #f #f argv))

   ((or (not (eq? #\- (string-ref (car argv) 0)))
	(eq? (string-length (car argv)) 1))
    (return 'normal-arg (car argv) (cdr argv)))

   ((eq? #\- (string-ref (car argv) 1))
    (let* ((kw-arg-pos (or (string-index (car argv) #\=)
			   (string-length (car argv))))
	   (kw (symbol->keyword (substring (car argv) 2 kw-arg-pos)))
	   (kw-opt? (member kw kw-opts))
	   (kw-arg? (member kw kw-args))
	   (arg (or (and (not (eq? kw-arg-pos (string-length (car argv))))
			 (substring (car argv)
				    (+ kw-arg-pos 1)
				    (string-length (car argv))))
		    (and kw-arg?
			 (begin (set! argv (cdr argv)) (car argv))))))
      (if (or kw-opt? kw-arg?)
	  (return kw arg (cdr argv))
	  (return 'usage-error kw (cdr argv)))))

   (else
    (let* ((char (substring (car argv) 1 2))
	   (kw (symbol->keyword char)))
      (cond

       ((member kw kw-opts)
	(let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
	       (new-argv (if (= 0 (string-length rest-car))
			     (cdr argv)
			     (cons (string-append "-" rest-car) (cdr argv)))))
	  (return kw #f new-argv)))

       ((member kw kw-args)
	(let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
	       (arg (if (= 0 (string-length rest-car))
			(cadr argv)
			rest-car))
	       (new-argv (if (= 0 (string-length rest-car))
			     (cddr argv)
			     (cdr argv))))
	  (return kw arg new-argv)))

       (else (return 'usage-error kw argv)))))))

(define (for-next-option proc argv kw-opts kw-args)
  (let loop ((argv argv))
    (get-option argv kw-opts kw-args
		(lambda (opt opt-arg argv)
		  (and opt (proc opt opt-arg argv loop))))))

(define (display-usage-report kw-desc)
  (for-each
   (lambda (kw)
     (or (eq? (car kw) #t)
	 (eq? (car kw) 'else)
	 (let* ((opt-desc kw)
		(help (cadr opt-desc))
		(opts (car opt-desc))
		(opts-proper (if (string? (car opts)) (cdr opts) opts))
		(arg-name (if (string? (car opts))
			      (string-append "<" (car opts) ">")
			      ""))
		(left-part (string-append
			    (with-output-to-string
			      (lambda ()
				(map (lambda (x) (display (keyword-symbol x)) (display " "))
				     opts-proper)))
			    arg-name))
		(middle-part (if (and (< (string-length left-part) 30)
				      (< (string-length help) 40))
				 (make-string (- 30 (string-length left-part)) #\ )
				 "\n\t")))
	   (display left-part)
	   (display middle-part)
	   (display help)
	   (newline))))
   kw-desc))
		  

	   
(define (transform-usage-lambda cases)
  (let* ((raw-usage (delq! 'else (map car cases)))
	 (usage-sans-specials (map (lambda (x)
				    (or (and (not (list? x)) x)
					(and (symbol? (car x)) #t)
					(and (boolean? (car x)) #t)
					x))
				  raw-usage))
	 (usage-desc (delq! #t usage-sans-specials))
	 (kw-desc (map car usage-desc))
	 (kw-opts (apply append (map (lambda (x) (and (not (string? (car x))) x)) kw-desc)))
	 (kw-args (apply append (map (lambda (x) (and (string? (car x)) (cdr x))) kw-desc)))
	 (transmogrified-cases (map (lambda (case)
				      (cons (let ((opts (car case)))
					      (if (or (boolean? opts) (eq? 'else opts))
						  opts
						  (cond
						   ((symbol? (car opts))  opts)
						   ((boolean? (car opts)) opts)
						   ((string? (caar opts)) (cdar opts))
						   (else (car opts)))))
					    (cdr case)))
				    cases)))
    `(let ((%display-usage (lambda () (display-usage-report ',usage-desc))))
       (lambda (%argv)
	 (let %next-arg ((%argv %argv))
	   (get-option %argv
		       ',kw-opts
		       ',kw-args
		       (lambda (%opt %arg %new-argv)
			 (case %opt
			   ,@ transmogrified-cases))))))))


;;; {Run-time options}

((let* ((names '((eval-options-interface
		  (eval-options eval-enable eval-disable)
		  (eval-set!))
		 
		 (debug-options-interface
		  (debug-options debug-enable debug-disable)
		  (debug-set!))
		 
		 (evaluator-traps-interface
		  (traps trap-enable trap-disable)
		  (trap-set!))
		 
		 (read-options-interface
		  (read-options read-enable read-disable)
		  (read-set!))
		 
		 (print-options-interface
		  (print-options print-enable print-disable)
		  (print-set!))

		 (readline-options-interface
		  (readline-options readline-enable readline-disable)
		  (readline-set!))
		 ))
	(option-name car)
	(option-value cadr)
	(option-documentation caddr)

	(print-option (lambda (option)
			(display (option-name option))
			(if (< (string-length
				(symbol->string (option-name option)))
			       8)
			    (display #\tab))
			(display #\tab)
			(display (option-value option))
			(display #\tab)
			(display (option-documentation option))
			(newline)))

	;; Below follows the macros defining the run-time option interfaces.

	(make-options (lambda (interface)
			`(lambda args
			   (cond ((null? args) (,interface))
				 ((list? (car args))
				  (,interface (car args)) (,interface))
				 (else (for-each ,print-option
						 (,interface #t)))))))

	(make-enable (lambda (interface)
		       `(lambda flags
			  (,interface (append flags (,interface)))
			  (,interface))))

	(make-disable (lambda (interface)
			`(lambda flags
			   (let ((options (,interface)))
			     (for-each (lambda (flag)
					 (set! options (delq! flag options)))
				       flags)
			     (,interface options)
			     (,interface)))))

	(make-set! (lambda (interface)
		     `((name exp)
		       (,'quasiquote
			(begin (,interface (append (,interface)
						   (list '(,'unquote name)
							 (,'unquote exp))))
			       (,interface))))))
	)
   (procedure->macro
    (lambda (exp env)
      (cons 'begin
	    (apply append
		   (map (lambda (group)
			  (let ((interface (car group)))
			    (append (map (lambda (name constructor)
					   `(define ,name
					      ,(constructor interface)))
					 (cadr group)
					 (list make-options
					       make-enable
					       make-disable))
				    (map (lambda (name constructor)
					   `(defmacro ,name
					      ,@(constructor interface)))
					 (caddr group)
					 (list make-set!)))))
			names)))))))

(export eval-options eval-enable eval-disable eval-set! debug-options
debug-enable debug-disable traps trap-enable trap-disable trap-set!
read-enable read-disable read-set! print-enable print-disable
print-set! readline-options readline-enable readline-disable
readline-set!)

