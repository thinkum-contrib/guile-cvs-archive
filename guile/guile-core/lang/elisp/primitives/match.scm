(define-module (lang elisp primitives match)
  #:use-module (lang elisp internals fset)
  #:use-module (ice-9 regex))

(define last-match #f)

(fset 'string-match-workaround
      (lambda (regexp string . start)

	;; Work around Guile/libc regex parenthesis bug by
	;; substituting the regexps that actually occur in the elisp
	;; code that we want to read.
	(let loop ((buggy-patterns '(("^[0-9]+\\.\\([0-9]+\\)" .
				      "^[0-9]+\\.([0-9]+)"))))
	  (or (null? buggy-patterns)
	      (if (string=? regexp (caar buggy-patterns))
		  (set! regexp (cdar buggy-patterns))
		  (loop (cdr buggy-patterns)))))

	(set! last-match (apply string-match regexp string start))
	(if last-match
	    (match:start last-match)
	    #f)))

(fset 'string-match
      (lambda (regexp string . start)

	(define (emacs-string-match pattern str . args)
	  (let ((rx (make-emacs-regexp pattern))
		(start (if (pair? args) (car args) 0)))
	    (regexp-exec rx str start)))

	(set! last-match (apply emacs-string-match regexp string start))
	(if last-match
	    (match:start last-match)
	    #f)))

(fset 'match-beginning
      (lambda (subexp)
	(match:start last-match subexp)))

(fset 'match-end
      (lambda (subexp)
	(match:end last-match subexp)))

(fset 'substring substring)
