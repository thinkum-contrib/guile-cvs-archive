;;; {Error Handling}
;;;

(module (ice-9 error)
	(open (ice-9 guile)
	      ((ice-9 repl) save-stack))
	(export error warn))
	
(define (error . args)
  (save-stack)
  (if (null? args)
      (scm-error 'misc-error #f "?" #f #f)
      (let loop ((msg "%s")
		 (rest (cdr args)))
	(if (not (null? rest))
	    (loop (string-append msg " %S")
		  (cdr rest))
	    (scm-error 'misc-error #f msg args #f)))))

;; bad-throw is the hook that is called upon a throw to a an unhandled
;; key (unless the throw has four arguments, in which case
;; it's usually interpreted as an error throw.)
;; If the key has a default handler (a throw-handler-default property),
;; it is applied to the throw.
;;
(define (bad-throw key . args)
  (apply error "unhandled-exception:" key args))


(define (warn . stuff)
  (with-output-to-port (current-error-port)
    (lambda ()
      (newline)
      (display ";;; WARNING ")
      (display stuff)
      (newline)
      (car (last-pair stuff)))))