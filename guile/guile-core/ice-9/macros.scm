;;; {Macros}
;;;

;; actually....hobbit might be able to hack these with a little
;; coaxing
;;
(module (ice-9 macros)
	(export define-macro define-syntax-macro defmacro:transformer defmacro:syntax-transformer)
	(open (ice-9 guile) (ice-9 defmacro)))


(defmacro define-macro (first . rest)
  (let ((name (if (symbol? first) first (car first)))
	(transformer
	 (if (symbol? first)
	     (car rest)
	     `(lambda ,(cdr first) ,@rest))))
    `(define ,name (defmacro:transformer ,transformer))))


(defmacro define-syntax-macro (first . rest)
  (let ((name (if (symbol? first) first (car first)))
	(transformer
	 (if (symbol? first)
	     (car rest)
	     `(lambda ,(cdr first) ,@rest))))
    `(define ,name (defmacro:syntax-transformer ,transformer))))

