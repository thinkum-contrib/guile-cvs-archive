;;; {Symbols}
;;;
(module (ice-9 symbols)
	(open (ice-9 guile))
	(export symbol-append list->symbol symbol obarray-symbol-append obarray-gensym))

(define (symbol-append . args)
  (string->symbol (apply string-append args)))

(define (list->symbol . args)
  (string->symbol (apply list->string args)))

(define (symbol . args)
  (string->symbol (apply string args)))

(define (obarray-symbol-append ob . args)
  (string->obarray-symbol (apply string-append ob args)))

(define (obarray-gensym obarray . opt)
  (if (null? opt)
      (gensym "%%gensym" obarray)
      (gensym (car opt) obarray)))

