(define-module (lang elisp match)
  #:use-module (lang elisp fset)
  #:use-module (ice-9 regex))

(define last-match #f)

(fset 'string-match
      (lambda (regexp string . start)
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
