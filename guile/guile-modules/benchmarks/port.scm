;;;; ports.scm --- some naive benchmarks for Guile's I/O ports
;;;; Jim Blandy <jimb@red-bean.com> --- April 1999

(define-module (benchmark port)
  #:use-module (benchmark lib))
(export port-run)

(define data-dir "/home/jimb/guile/src/modules/modules/benchmark")

;;; This file contains one really big s-expression.
(define data-file-1 (in-vicinity data-dir "port-data-1"))

;;; This file contains a lot of lines, of lengths typical of English text.
(define data-file-2 (in-vicinity data-dir "port-data-2"))

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
    (time-accumulate
     'read-lines
     (lambda ()
       (do ((i 0 (+ i 1)))
	   ((>= i 100))
	 (fseek data-2 SEEK_SET 0)
	 (gc)
	 (time-pass (lambda ()
		      (let loop ()
			(let ((line (read-line data-2)))
			  (or (eof-object? line)
			      (loop)))))))))
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


;;; When writing benchmarks, beware of GC effects!  
;;;
;;; Here's the output from a typical call to port-run on my machine:
;;; 
;;; 	$ ./port
;;; 	Opened log file, Sat 22 May 16:47:13
;;; 	System: Linux savonarola.red-bean.com 2.0.36 #1 ... i586 
;;; 	Benchmark lib version: 2
;;; 	Benchmark: ports
;;; 	Benchmark revision: 1
;;; 	read-sexp
;;; 	10 passes  3.270s user   0.830s gc   0.090s sys
;;; 	write-sexp
;;; 	100 passes  9.250s user   0.000s gc   0.060s sys
;;; 	read-lines
;;; 	100 passes  6.000s user   0.000s gc   0.150s sys
;;; 	write-lines
;;; 	500 passes  9.070s user   2.450s gc   1.550s sys
;;; 
;;; If I comment out the call to write-sexp, then I get this:
;;;
;;; 	$ ./port
;;; 	Opened log file, Sat 22 May 16:52:57
;;; 	System: Linux savonarola.red-bean.com 2.0.36 #1 ... i586 
;;; 	Benchmark lib version: 2
;;; 	Benchmark: ports
;;; 	Benchmark revision: 1
;;; 	read-lines
;;; 	100 passes  6.320s user   20.040s gc   0.120s sys
;;; 	write-lines
;;; 	500 passes  8.940s user   5.490s gc   1.640s sys
;;;
;;; The total GC time increases by a factor of eight.  I don't really
;;; know what's going on here.

(define (port-run)
  (benchmark-title "ports" 3)

  (read-sexp)
  (write-sexp)
  (read-lines)
  (write-lines))
