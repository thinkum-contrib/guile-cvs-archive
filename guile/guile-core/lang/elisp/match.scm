(define-module (lang elisp match)
  #:use-module (lang elisp fset)
  #:use-module (ice-9 regex))

(define last-match #f)

(fset 'string-match
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

(fset 'match-beginning
      (lambda (subexp)
	(match:start last-match subexp)))

(fset 'match-end
      (lambda (subexp)
	(match:end last-match subexp)))

(fset 'substring substring)
