(define-module (lang elisp emacs))

;;;; {Elisp Evaluation}

;;;; All elisp evaluation happens within the same module - this one.
;;;; This is necessary both because elisp itself has no concept of
;;;; different modules - reflected for example in its single argument
;;;; `eval' function - and because Guile's current implementation of
;;;; elisp stores elisp function definitions in slots in global symbol
;;;; objects.

(define the-elisp-module (current-module))

;;;; {Elisp Primitives}
;;;;
;;;; In other words, Scheme definitions of elisp primitives.  This
;;;; should include everything that Emacs defines in C.

(use-modules (lang elisp buffers)
	     (lang elisp calling)
	     (lang elisp features)
	     (lang elisp format)
	     (lang elisp fset)
	     (lang elisp guile)
	     (lang elisp keymaps)
	     (lang elisp lists)
	     (lang elisp load)
	     (lang elisp match)
	     (lang elisp numbers)
	     (lang elisp pure)
	     (lang elisp read)
	     (lang elisp signal)
	     (lang elisp strings)
	     (lang elisp symprop)
	     (lang elisp system)
	     (lang elisp time))

;;; Now switch into Emacs Lisp syntax and continue by loading
;;; `loadup.el', which is what the `bare' undumped Emacs does to load
;;; in all its standard elisp code before dumping.

(use-modules (lang elisp transform))
(read-set! keywords 'prefix)
(read-set! language 'elisp)
(set-module-transformer! (current-module) transformer)

;; Everything below here is written in Elisp.
(message "Calling loadup.el to clothe the bare Emacs...")
(load "loadup.el")
(message "Guile Emacs now fully clothed")
