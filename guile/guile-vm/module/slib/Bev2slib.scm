;;;; "Bev2slib.scm" Build SLIB catalogs for Stephen Bevan's libraries.
;Copyright (C) 1998 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;; Put this file into the implementation-vicinity directory for your
;;; scheme implementation.

;;; Add the line
;;;	(load (in-vicinity (implementation-vicinity) "Bev2slib.scm"))
;;; to "mkimpcat.scm"

;;; Delete "slibcat" in your implementation-vicinity.

;;; Bind `Bevan-dir' to the directory containing directories "bawk",
;;; "mawk", "pathname", etc.  Bev2slib.scm will put entries into the
;;; catalog only for those directories and files which exist.

(let ((Bevan-dir (in-vicinity (library-vicinity) "../"));"/usr/local/lib/Bevan/"
      (catname "sitecat"))
  (call-with-output-file (in-vicinity (implementation-vicinity) catname)
    (lambda (op)
      (define (display* . args)
	(for-each (lambda (arg) (display arg op)) args)
	(newline op))
      (define (add-alias from to)
	(display " " op)
	(write (cons from to) op)
	(newline op))

      (begin
	(display* ";\"" catname "\" Site-specific SLIB catalog for "
		  (scheme-implementation-type) (scheme-implementation-version)
		  ".  -*-scheme-*-")
	(display* ";")
	(display* ";			DO NOT EDIT THIS FILE")
	(display* "; it is automagically generated by \"Bev2slib.scm\"")
	(newline op)
	)

      ;; Output association lists to file "sitecat"

      (for-each
       (lambda (dir)
	 (let* ((vic (in-vicinity Bevan-dir (string-append dir "/")))
		(map-file (in-vicinity vic (string-append dir ".map"))))

	   (display* ";;; from " map-file)
	   (display* "(")

	   (and
	    (file-exists? map-file)
	    (call-with-input-file map-file
	      (lambda (ip)
		(define files '())
		(do ((feature (read ip) (read ip)))
		    ((eof-object? feature))
		  (let* ((type (read ip))
			 (file (read ip))
			 (fsym (string->symbol (string-append "Req::" file))))
		    (and (not (assq fsym files))
			 (set! files (cons (cons fsym file) files)))
		    (add-alias feature fsym)))
		(for-each
		 (lambda (pr) (add-alias (car pr) (in-vicinity vic (cdr pr))))
		 files)
		)))

	   (display* ")")))

       '("char-set" "conc-string" "string" "string-03"
		    "avl-tree" "avl-trie"
		    "bawk" "mawk" "pathname"))

      (begin
	(display* "(")
	(add-alias 'btree (in-vicinity Bevan-dir "bawk/btree"))
	(add-alias 'read-line 'line-i/o)
	(display* ")")
	))))
