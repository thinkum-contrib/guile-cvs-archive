
;;; {Check that the interpreter and scheme code match up.}

(let ((show-line
       (lambda args
	 (with-output-to-port (current-error-port)
	   (lambda ()
	     (display (car (command-line)))
	     (display ": ")
	     (for-each (lambda (string) (display string))
		       args) 
	     (newline))))))

  (load-from-path "ice-9/version.scm")

  (if (not (string=?
	    (libguile-config-stamp)	; from the interprpreter
	    (ice-9-config-stamp)))	; from the Scheme code
      (begin
	(show-line "warning: different versions of libguile and ice-9:")
	(show-line "libguile: configured on " (libguile-config-stamp))
	(show-line "ice-9:    configured on " (ice-9-config-stamp)))))
