(define-module (lang elisp buffers)
  #:use-module (ice-9 optargs)
  #:use-module (lang elisp fset))

(define* (buffer-disable-undo #:optional buffer)
  'unimplemented)

(define* (re-search-forward regexp #:optional bound noerror count)
  'unimplemented)

(define* (re-search-backward regexp #:optional bound noerror count)
  'unimplemented)

;;; {Elisp Exports}

(fset 'buffer-disable-undo buffer-disable-undo)
(fset 're-search-forward re-search-forward)
(fset 're-search-backward re-search-backward)
