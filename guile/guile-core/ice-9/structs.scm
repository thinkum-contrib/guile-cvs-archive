(module (ice-9 structs)

	(export record-type? make-record-type record-type-name record-type-fields record-constructor record-predicate
		record-accessor record-modifier record? record-type-descriptor)

	(open (ice-9 provide) (ice-9 symbols) (ice-9 lists) (ice-9 guile)))

;;; {Structs}

(define (struct-layout s)
  (struct-ref (struct-vtable s) vtable-index-layout))


;;; {Records}
;;;

;; Printing records: by default, records are printed as
;;
;;   #<type-name field1: val1 field2: val2 ...>
;;
;; You can change that by giving a custom printing function to
;; MAKE-RECORD-TYPE (after the list of field symbols).  This function
;; will be called like
;;
;;   (<printer> object port)
;;
;; It should print OBJECT to PORT.

(define (inherit-print-state old-port new-port)
  (if (pair? old-port)
      (cons (if (pair? new-port) (car new-port) new-port)
	    (cdr old-port))
      new-port))

;; 0: type-name, 1: fields
(define record-type-vtable 
  (make-vtable-vtable "prpr" 0
		      (lambda (s p)
			(cond ((eq? s record-type-vtable)
			       (display "#<record-type-vtable>" p))
			      (else
			       (display "#<record-type " p)
			       (display (record-type-name s) p)
			       (display ">" p))))))

(define (record-type? obj)
  (and (struct? obj) (eq? record-type-vtable (struct-vtable obj))))

(define (make-record-type type-name fields . opt)
  (let ((printer-fn (and (pair? opt) (car opt))))
    (let ((struct (make-struct record-type-vtable 0
			       (make-struct-layout
				(apply symbol-append
				       (map (lambda (f) "pw") fields)))
			       (or printer-fn
				   (lambda (s p)
				     (display "#<" p)
				     (display type-name p)
				     (let loop ((fields fields)
						(off 0))
				       (cond
					((not (null? fields))
					 (display " " p)
					 (display (car fields) p)
					 (display ": " p)
					 (display (struct-ref s off) p)
					 (loop (cdr fields) (+ 1 off)))))
				     (display ">" p)))
			       type-name
			       (copy-tree fields))))
      ;; Temporary solution: Associate a name to the record type descriptor
      ;; so that the object system can create a wrapper class for it.
      (set-struct-vtable-name! struct (if (symbol? type-name)
					  type-name
					  (string->symbol type-name)))
      struct)))

(define (record-type-name obj)
  (if (record-type? obj)
      (struct-ref obj vtable-offset-user)
      (error 'not-a-record-type obj)))

(define (record-type-fields obj)
  (if (record-type? obj)
      (struct-ref obj (+ 1 vtable-offset-user))
      (error 'not-a-record-type obj)))

(define (record-constructor rtd . opt)
  (let ((field-names (if (pair? opt) (car opt) (record-type-fields rtd))))
    (eval `(lambda ,field-names
	     (make-struct ',rtd 0 ,@(map (lambda (f)
					  (if (memq f field-names)
					      f
					      #f))
					(record-type-fields rtd)))) 
	  (the-environment))))

(define (record-predicate rtd)
  (lambda (obj) (and (struct? obj) (eq? rtd (struct-vtable obj)))))

(define (record-accessor rtd field-name)
  (let* ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (eval `(lambda (obj)
	     (and (eq? ',rtd (record-type-descriptor obj))
		  (struct-ref obj ,pos))) (the-environment))))

(define (record-modifier rtd field-name)
  (let* ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (eval `(lambda (obj val)
	     (and (eq? ',rtd (record-type-descriptor obj))
		  (struct-set! obj ,pos val))) (the-environment) )))


(define (record? obj)
  (and (struct? obj) (record-type? (struct-vtable obj))))

(define (record-type-descriptor obj)
  (if (struct? obj)
      (struct-vtable obj)
      (error 'not-a-record obj)))

(provide 'record)

