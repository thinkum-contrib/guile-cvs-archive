;;;; ports.scm --- some naive benchmarks for Guile's I/O ports
;;;; Jim Blandy <jimb@red-bean.com> --- April 1999

(define-module (benchmarks ports)
  #:use-module (benchmarks lib))
(export ports-run)

(define data-dir "/home/jimb/guile/scheme/benchmarks")

;;; This file contains one really big s-expression.
(define data-file-1 (in-vicinity data-dir "ports-data-1"))

;;; This file contains a lot of lines, of varying length.
(define data-file-2 (in-vicinity data-dir "ports-data-2"))

(define (read-sexp) 
  (time-thunk-repeated 
   'read-sexp 10
   (lambda ()
     (let ((data-1 (open-input-file data-file-1)))
       (read data-1)
       (close-port data-1)))))

(define (write-sexp)
  (let ((sexp (let* ((data-1 (open-input-file data-file-1))
		     (sexp (read data-1)))
		(close data-1)
		sexp))
	(sink (open-output-file "/dev/null")))
    (time-thunk-repeated
     'write-sexp 100 (lambda () (write sexp sink)))
    (close-port sink)))

(define (read-lines)
  (let ((data-2 (open-input-file data-file-2)))
    (time-thunk-repeated 
     'read-lines 100
     (lambda ()
       (fseek data-2 SEEK_SET 0)
       (let loop ()
	 (let ((line (read-line data-2)))
	   (or (eof-object? line)
	       (loop))))))
    (close-port data-2)))

(define (write-lines)
  (let ((lines (let ((data-2 (open-input-file data-file-2)))
		 (let loop ((lines '()))
		   (let ((line (read-line data-2)))
		     (if (eof-object? line) lines
			 (loop (cons line lines)))))))
	(sink (open-output-file "/home/jimb/foo")))
    (time-thunk-repeated 
     'write-lines 500
     (lambda ()
       (for-each (lambda (line) 
		   (display line sink)
		   (newline sink))
		 lines)))
    (close-port sink)))

(define (ports-run)
  (identify "ports" 1)

  (read-sexp)
  (write-sexp)
  (read-lines)
  (write-lines))
