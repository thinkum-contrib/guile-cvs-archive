(define-module (lang elisp load)
  #:use-module (ice-9 optargs)
  #:use-module (lang elisp signal)
  #:use-module (lang elisp format)
  #:use-module (lang elisp fset))

(define the-elisp-module (resolve-module '(lang elisp)))

(define load-path '("/usr/share/emacs/20.7/lisp/"))

(define* (load file #:optional noerror nomessage nosuffix must-suffix)
  (define (load1 filename)
    (let ((pathname (let loop ((dirs load-path))
		      (cond ((null? dirs) #f)
			    ((file-exists? (string-append (car dirs)
							  filename))
			     (string-append (car dirs) filename))
			    (else (loop (cdr dirs)))))))
      (if pathname
	  (begin
	    (or nomessage
		(message "Loading %s..." pathname))
	    (with-input-from-file pathname
	      (lambda ()
		(let loop ((form (read)))
		  (or (eof-object? form)
		      (begin
			;; Note that `eval' already incorporates use
			;; of the specified module's transformer, so
			;; we only need to call transformer explicitly
			;; here if it is possible for `load' to be
			;; used before the `use-syntax' below has
			;; taken effect.
			(eval ;(transformer form)
			      form
			      the-elisp-module)
			(loop (read)))))))
	    (or nomessage
		(message "Loading %s...done" pathname))
	    t)
	  #f)))
  (or (and (not nosuffix)
	   (load1 (string-append file ".el")))
      (and (not must-suffix)
	   (load1 file))
      noerror
      (signal 'file-error
	      (list (format "Can't find file `%s' in load-path!" file)))))

;;; {Elisp Exports}

(export load-path)

(fset 'load load)
