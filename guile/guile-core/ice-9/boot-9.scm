;;; attempt to create a pure r5rs environment =:))

(let ((mod-env (c-module-registry)))
  ((environment-ref mod-env 'ice-9/alist) (scheme-guile-environment 3))
					;  ((environment-ref mod-env 'ice-9/boolean) (interaction-environment))
  ((environment-ref mod-env 'ice-9/chars) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/continuations) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/error) (interaction-environment))
  ((environment-ref mod-env 'ice-9/fports) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/gc) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/hash) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/hashtab) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/ioext) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/procs) (interaction-environment))
  ((environment-ref mod-env 'ice-9/procprop) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/stackchk) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/symbols) (scheme-guile-environment 3))
					;  ((environment-ref mod-env 'ice-9/objects) (interaction-environment))
  ((environment-ref mod-env 'ice-9/stime) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/strings) (scheme-guile-environment 3))
					;  ((environment-ref mod-env 'ice-9/strorder) (interaction-environment))
  ((environment-ref mod-env 'ice-9/strop) (scheme-guile-environment 3))
  ((environment-ref mod-env 'ice-9/vectors) (scheme-guile-environment 3))
					;  ((environment-ref mod-env 'ice-9/guile-version) (interaction-environment))
  ((environment-ref mod-env 'ice-9/weaks) (scheme-guile-environment 3))
					;  ((environment-ref mod-env 'ice-9/vports) (interaction-environment))
  ((environment-ref mod-env 'ice-9/evalext) (scheme-guile-environment 3))
					;  ((environment-ref mod-env 'ice-9/simpos) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/lang) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/regex-posix) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/objprop) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/mallocs) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/net-db) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/tag) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/socket) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/sort) (interaction-environment))
					;  ((environment-ref mod-env 'ice-9/random) (interaction-environment))
  ((environment-ref mod-env 'ice-9/posix) (scheme-guile-environment 3))
)

(environment-define 
 (scheme-guile-environment 3) 
 'load
 (lambda (name)
   (start-stack 'load-stack
		(primitive-load-path name (interaction-environment)))))

(environment-define 
 (scheme-guile-environment 3) 
 'quit
 (lambda args
   (apply throw 'quit args)))

; (environment-define 
;  (scheme-guile-environment 3)
;  ' error
;    (lambda args
;      (save-stack)
;      (if (null? args)
; 	 (scm-error 'misc-error #f "?" #f #f)
; 	 (let loop ((msg "~A")
; 		    (rest (cdr args)))
; 	   (if (not (null? rest))
; 	       (loop (string-append msg " ~S")
; 		     (cdr rest))
; 	       (scm-error 'misc-error #f msg args #f))))))

;;; Environments

;; the top level environment
(define the-environment 
  (procedure->syntax 
   (lambda (x env) 
     (car (reverse env)))))



(define (command-line) (program-arguments))
