
;;; {Keywords}
;;;

(define (symbol->keyword symbol)
  (make-keyword-from-dash-symbol (symbol-append '- symbol)))

(define (keyword->symbol kw)
  (let ((sym (keyword-dash-symbol kw)))
    (string->symbol (substring sym 1 (string-length sym)))))

(define (kw-arg-ref args kw)
  (let ((rem (member kw args)))
    (and rem (pair? (cdr rem)) (cadr rem))))

