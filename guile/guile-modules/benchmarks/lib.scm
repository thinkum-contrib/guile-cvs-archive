;;;; lib.scm --- utility functions for running benchmarks
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

(define-module (benchmarks lib)
  #:use-module (ice-9 format)
  #:use-module (benchmarks paths))
(export start-log end-log
	log-text log-data
	data-file
	benchmark-title
	time-thunk-once time-thunk-repeated
	time-accumulate time-pass)

(define version 4)

(or (file-is-directory? datadir)
    (error "Benchmark data directory does not exist: " datadir))


;;;; Logging functions.

;;; The I/O port to which we write results.
(define log-port #f)

(define (start-log filename)
  (end-log)
  (set! log-port (open-file filename "a"))

  ;; Record the date and time.
  (let* ((time (current-time))
	 (u (uname)))

    ;; Clean up the uname a little bit.
    (vector-set! u 3 "")

    (log-text "Opened log file, "
	      (strftime "%a %e %b %H:%M:%S" (localtime time)))
    (log-data (list 'start-log time))

    (log-text (format "System: ~{~A ~}" (vector->list u)))
    (log-data (list 'system-uname u))

    (log-text "Benchmark lib version: " version)
    (log-data (list 'benchmark-library-version version))))

(define (end-log)
  (if (output-port? log-port)
      (begin
	(newline log-port)
	(newline log-port)
	(close-port log-port)))
  (set! log-port #f))

;;; Write some text to a log file, and to the standard output.  Apply
;;; DISPLAY to each element of OBJECTS in turn, and then print a
;;; newline.  To the log file, precede the line with a Scheme comment
;;; marker.
(define (log-text . objects)
  (define (out port objects)
    (for-each (lambda (obj) (display obj port))
	      objects)
    (newline port))
  (out (current-output-port) objects)
  (if (output-port? log-port)
      (out log-port (cons "; " objects))))

;;; Write a Scheme-readable object to the log file.
(define (log-data object)
  (if (output-port? log-port)
      (begin
	(write object log-port)
	(newline log-port))))


;;;; Helping benchmarks find their files

;;; Returns FILENAME, relative to the directory the benchmark data
;;; files were installed in, and makes sure the file exists.
(define (data-file filename)
  (let ((f (in-vicinity datadir filename)))
    (or (file-exists? f)
	(error "Benchmark data file does not exists: " f))
    f))


;;;; Functions for handling time reasonably.

;;; These functions manipulate an abstract data type called "times",
;;; which represents the CPU time used by some process, broken down
;;; in some useful way.
;;;
;;; As implemented here, a times is a list:
;;;    (GC USER SYSTEM CHILD-USER CHILD-SYSTEM)
;;; where
;;;   GC is the number of seconds Guile has spent in GC
;;;   USER is the number of seconds this process has occupied the CPU
;;;   SYSTEM is the number of seconds the CPU has spent in the kernel on
;;;       behalf of this program
;;;   CHILD-USER and CHILD-SYSTEM are the same as USER and SYSTEM, but
;;;       include the time accumulated by child processes.
;;;
;;; But you're supposed to be able to use it without knowing that.
;;; This allows us to change the way we break down time, or even
;;; change the data we record, without having to tweak every
;;; benchmarking function.
;;;
;;; All the times are in the system's internal time units --- use
;;; internal-time-units-per-second to convert.  

;;; Return the times consumed so far by this Guile process.
(define (times:now)
  (cons (cdr (assq 'gc-time-taken (gc-stats)))
	(cdr (vector->list (times)))))

;;; Format a time list into a string, nicely formatted for human
;;; readers.
(define (times:format times)
  (let ((times (map (lambda (time) (/ time internal-time-units-per-second))
		    times)))
    (let ((gc (car times)))
      (format "~7,2Fs user ~7,2Fs gc ~7,2Fs sys"
	      (- (cadr times) gc) gc (caddr times)))))

;;; Return the time elapsed between two time lists.
(define (times:elapsed start end)
  (map - end start))

;;; Add two lists of elapsed times.
(define (times:add start end)
  (map + start end))

;;; Return the additive identity for times.
(define (times:zero)
  (list 0 0 0 0 0))
  

;;;; Running benchmarks.

;;; Mark subsequent tests as coming from the benchmark named NAME,
;;; revision REVISION.
(define (benchmark-title name revision)
  (log-text (format "Benchmark: ~A revision ~A" name revision))
  (log-data (list name revision)))

;;; Call THUNK once, and report the CPU time consumed.
;;; TITLE is the list of objects to use to label the result.
(define (time-thunk-once title thunk)
  (let ((start (times:now)))
    (thunk)
    (let* ((end (times:now))
	   (e (times:elapsed start end)))
      (log-text (format "~20A ~A" title (times:format e)))
      (log-data (list title e)))))


;;; Call THUNK N times, and report the total CPU time consumed.
;;; TITLE is the list of objects to use to label the result.
;;;
;;; This should really provide some measure of variance, too.
;;; Standard deviation?
(define (time-thunk-repeated title n thunk)
  (let ((start (times:now)))
    (do ((i 0 (+ i 1)))
	((>= i n))
      (thunk))
    (let* ((end (times:now))
	   (e (times:elapsed start end)))
      (log-text (format "~20A ~4D passes ~A" title n (times:format e)))
      (log-data (list title n e)))))

;;;; The following two functions, TIME-ACCUMULATE and TIME-PASS, help you
;;;; write a benchmark whose total time is the sum of times of several
;;;; separate passes.  Use them like this:
;;;; 
;;;; (time-accumulate TITLE
;;;;   (lambda ()
;;;;     (do ((i 0 (+ i 1)))
;;;;         ((>= i 10))
;;;;       (prepare-for-one-pass)
;;;;       (time-pass (lambda ()
;;;;                    (do-something))))))
;;;; 
;;;; That code will report the total CPU time consumed by the calls to
;;;; DO-SOMETHING, but will not include the CPU time consumed by calls
;;;; to PREPARE-FOR-ONE-PASS.
;;;;
;;;; You should only call TIME-PASS in the dynamic scope of a call to
;;;; TIME-ACCUMULATE.  (TIME-PASS THUNK) will call THUNK, and add the
;;;; CPU time it consumes into the total for the dynamically enclosing
;;;; TIME-ACCUMULATE call.  TIME-ACCUMULATE will report the total
;;;; number of calls to TIME-PASS that occurred in its dynamic scope,
;;;; and the total CPU time they consumed.
;;;;
;;;; This should probably report variance, too.

;;; Within the dynamic scope of a TIME-ACCUMULATE call, the value of
;;; this fluid is a vector of the form #(PASS-COUNT TIMES), where
;;; PASS-COUNT is the number of calls to TIME-PASS that we've seen so far,
;;; and TIMES is the accumulated times each TIME-PASS reported.
;;;
;;; We don't need to export this.  :)
(define accumulator (make-fluid))

;;; Vectors are so clumsy.  Does this help?
(define (vector-update vector index func . args)
  (vector-set! vector index
	       (apply func (vector-ref vector index) args)))

(define (time-accumulate title thunk)
  (with-fluids ((accumulator (vector 0 (times:zero))))
    (thunk)
    (let* ((totals (fluid-ref accumulator))
	   (passes (vector-ref totals 0))
	   (e (vector-ref totals 1)))
      (log-text (format "~20A ~4D passes ~A" title passes (times:format e)))
      (log-data (list title passes e)))))

(define (time-pass thunk)
  (let ((acc (fluid-ref accumulator)))
    (or (vector? acc)
	(error "time-pass: no enclosing TIME-ACCUMULATE call"))
    (let ((start (times:now)))
      (thunk)
      (let* ((end (times:now))
	     (elapsed (times:elapsed start end)))
	(vector-update acc 0 + 1)
	(vector-update acc 1 times:add elapsed)))))
