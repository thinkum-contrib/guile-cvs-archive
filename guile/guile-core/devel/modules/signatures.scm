;;; devel/modules/signatures.scm
;;; This is proof of concept code -- see signatures.texi.

(define (signature-importer-proc module-name signature-name)
  (lambda (sig-alist)
    (let ((syms (assq-ref (module-ref (resolve-module module-name) sig-alist)
                          signature-name)))
      (process-use-modules module-name syms))
    sig-alist))


;;; providing module
(define-module (fun funcs)
  :export (#{(fun funcs) signatures}#))

(define (plus-0 n) (+ n 0))
(define (plus-1 n) (+ n 1))
(define (plus-2 n) (+ n 2))
(define (mult-1 n) (* n 1))
(define (mult-2 n) (* n 2))
(define (mult-3 n) (* n 3))

(define #{(fun funcs) signatures}#
  '((pretty-much-useless        plus-0 mult-1)
    (using-numero-uno           plus-1 mult-1)
    (all                        plus-0 plus-1 plus-2
                                mult-1 mult-2 mult-3)))

;;; client A module
(define-module (client A)
  :use-module ((fun funcs)
               :renamer (signature-importer-proc '(fun funcs)
                                                 'pretty-much-useless)))

;;; client B module
(define-module (client B)
  :use-module ((fun funcs)
               :renamer (signature-importer-proc '(fun funcs)
                                                 'using-numero-uno)))

;;; client C module
(define-module (client C)
  :use-module ((fun funcs)
               :renamer (signature-importer-proc '(fun funcs) 'all)))

;;; signatures.scm ends here
