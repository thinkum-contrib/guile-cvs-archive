(define-module (lang elisp pure)
  #:use-module (lang elisp fset))

;; Purification, unexec etc. are not yet implemented...

(fset 'purecopy identity)
