(define-module (ice-9 compile)
  :use-module (ice-9 session) ;source, name
  :use-module (ice-9 slib)    ;pretty-print
  :use-module (ice-9 string-fun))

(require 'pretty-print)

(define (compile-command file)
  (string-append "gcc -c -O2 " file))

(load-from-path "ice-9/guile-hobbit.scm")

(define-public (compile . args)
  "(compile [\"<filename sans extension>\"] <module name>\
(compile [\"<filename sans extension>\"] <proc1> <proc2> ..."
  (let ((fname (if (string? (car args)) (car args) (tmpnam)))
	(args (if (string? (car args)) (cdr args) args)))
    (let ((module (if (pair? (car args))
                      (resolve-module (car args) #t)
 		      (build-module args))))
      (write-module module fname)
      (compile-file fname)
      (dynamic-load fname))))

(define (build-module procs)
  procs)

(define (e-no-anonproc proc)
  (scm-error 'misc-error
	     "write-module"
	     "No support for anonymous objects: %S"
	     (list proc)
	     '()))

(define (e-non-null-env proc env)
  (scm-error 'misc-error
	     "write-module"
	     "No support for non-null environments: %S %S"
	     (list proc env)
	     '()))

(define (write-module procs fname)
  (let ((out (open-output-file (string-append fname ".scm"))))
    (for-each (lambda (proc)
		(let ((name (name proc))
		      (source (source proc))
		      (env (procedure-environment
			    (if (macro? proc)
				(macro-transformer proc)
				proc))))
		  (if (not name)
		      (e-no-anonproc proc))
		  (if (not (or (null? env)
			       (procedure? (car env))))
		      (e-non-null-env proc env))
		  (pretty-print
		   (list 'define
			 name
			 (if (macro? proc)
			     (list (case (macro-type proc)
				     ((syntax) 'procedure->syntax)
				     ((macro) 'procedure->macro)
				     ((macro!) 'procedure->memoizing-macro))
				   source)
			     source))
		   out)
		  (newline out)))
	      procs)
    (close-port out)))

(define (e-no-libtool)
  (scm-error 'misc-error
	     "compile-file"
	     "Couldn't find ice-9/libtool in %load-path!"
	     '()
	     '()))

(define (e-compilation-error logfile)
  (scm-error 'misc-error
	     "compile-file"
	     "Compilation error! See %S."
	     (list logfile)
	     '()))

(define-public (compile-file fname)
  (let ((libtool (%search-load-path "ice-9/libtool"))
	(sname (string-append fname ".scm"))
	(cname (string-append fname ".c"))
	(lname (string-append fname ".log"))
	(dname (dirname fname)))
    (if (not libtool)
	(e-no-libtool))
    (let ((log (open-output-file lname))
	  (output #f)
	  (error #f))
      (dynamic-wind
       (lambda ()
	 (set! output (set-current-output-port log))
	 (set! error (set-current-error-port log)))
       (lambda ()
	 (hobbit sname)
	 (if (not (zero? (system (string-append "cd "
						(if (zero? (string-length dname))
						    "."
						    dname)
						"; "
						libtool
						" --silent --mode=compile "
						(compile-command cname)))))
	     (e-compilation-error lname)))
       (lambda ()
	 (set-current-output-port output)
	 (set-current-error-port error)
	 (close-port log))))))

(define (e-no-object obname)
  (scm-error 'misc-error
	     "dynamic-load"
	     "Couldn't find any object %S in load path"
	     (list obname)
	     '()))

(define (basename fname)
  (cadr (split-after-char-last #\/ fname list)))

(define (dirname fname)
  (car (split-after-char-last #\/ fname list)))

(define-public (dynamic-load fname)
  (let* ((obname (string-append fname ".lo"))
	 (fullname (%search-load-path obname))
	 (init (string->symbol
		(call-with-output-string
		 (lambda (p)
		   (display-var (string->symbol
				 (string-append "scm_init_" fname))
				p))))))
    (if (not fullname)
	(e-no-object obname))
    (dynamic-call init (dynamic-link fullname))))

(define-public (procedure-dependencies . procedures)
  (let ((source (procedure-source proc)))
    (if (not source)
	'()
	(dependencies source (procedure-environment proc)))))

(define (dependencies s e)
  'to-be-continued...)
