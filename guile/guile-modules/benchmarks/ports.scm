;;;; ports.scm --- some naive benchmarks for Guile's I/O ports
;;;; Copyright (C) 1999  Jim Blandy <jimb@red-bean.com>
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(define-module (benchmarks ports)
  #:use-module (benchmarks lib))
(export ports-run)

;;; GC behaves very oddly in these benchmarks.  If you can figure out
;;; why Guile behaves as shown below, let me know.
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
;;;
;;; Maybe what this means is that this whole benchmarking approach is
;;; wrong --- I should start a separate Guile process for each test,
;;; and try to calibrate for startup time.


;;;; Helping the benchmark find its data files.

(define (read-sexp) 
  (time-accumulate
   'read-sexp
   (lambda ()
     (do ((i 0 (+ i 1)))
	 ((>= i 10))
       (let ((data-1 (open-input-file (data-file "ports-data-1"))))
	 (time-pass (lambda () (read data-1)))
	 (close-port data-1))))))

(define (write-sexp)
  (let ((sexp (let* ((data-1 (open-input-file (data-file "ports-data-1")))
		     (sexp (read data-1)))
		(close data-1)
		sexp))
	(sink (open-output-file "/dev/null")))
    (time-thunk-repeated
     'write-sexp 100 (lambda () (write sexp sink)))
    (close-port sink)))

(define (read-lines)
  (let ((data-2 (open-input-file (data-file "ports-data-2"))))
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
  (let ((lines (let ((data-2 (open-input-file (data-file "ports-data-2"))))
		 (let loop ((lines '()))
		   (let ((line (read-line data-2)))
		     (if (eof-object? line) lines
			 (loop (cons line lines)))))))
	(sink (open-output-file "/dev/null")))
    (time-thunk-repeated 
     'write-lines 500
     (lambda ()
       (for-each (lambda (line) 
		   (display line sink)
		   (newline sink))
		 lines)))
    (close-port sink)))


(define (ports-run)
  (benchmark-title "ports" 4)
  (read-sexp)
  (write-sexp)
  (read-lines)
  (write-lines))
