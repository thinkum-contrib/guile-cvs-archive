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
	log-text log-data last-data
	data-file
	benchmark-title
	time-thunk-once time-thunk-repeated
	time-thunk-median time-thunk-median:times
	time-accumulate time-pass
	times:user times:gc times:user+gc times:sys times:tot
        times:gc-mark times:gc-sweep times:cells-marked times:cells-swept
        *benchmark-obscure-gc-stuff*
	optarg1)

(define version 4)

(or (file-is-directory? datadir)
    (error "Benchmark data directory does not exist: " datadir))

(define *benchmark-obscure-gc-stuff* #f)


;;;; Logging functions.

;;; The I/O port to which we write results.
(define log-port #f)

(define last-logged #f)

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
	(newline log-port)))
  (set! last-logged object))

(define (last-data)
  last-logged)


;;;; Helping benchmarks find their files

;;; Returns FILENAME, relative to the directory the benchmark data
;;; files were installed in, and makes sure the file exists.
(define (data-file filename)
  (let ((f (in-vicinity datadir filename)))
    (or (file-exists? f)
	(error "Benchmark data file does not exist: " f))
    f))


;;;; Functions for handling time reasonably.

;;; These functions manipulate an abstract data type called "times",
;;; which represents the CPU time used by some process, broken down
;;; in some useful way.
;;;
;;; As implemented here, a times is a list:
;;;    (GC USER SYSTEM CHILD-USER CHILD-SYSTEM)
;;; where
;;;   GC is the time Guile has spent in GC
;;;   USER is the time this process has occupied the CPU
;;;   SYSTEM is the time the CPU has spent in the kernel on
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

;;; Selectors

(define (times:user times)
  (- (times:user+gc times) (times:gc times)))
(define (times:gc times)
  (assq-ref times 'gc))
(define (times:user+gc times)
  (assq-ref times 'user+gc))
(define (times:system times)
  (assq-ref times 'system))
(define (times:total times)
  (+ (times:user+gc times) (times:system times)))
(define (times:gc-mark times)
  (assq-ref times 'gc-mark))
(define (times:gc-sweep times)
  (assq-ref times 'gc-sweep))
(define (times:cells-marked times)
  (assq-ref times 'cells-marked))
(define (times:cells-swept times)
  (assq-ref times 'cells-swept))

;;; Return the times consumed so far by this Guile process.
(define (times:now)
  (let ((gcs (gc-stats)))
    `((gc . ,(assq-ref gcs 'gc-time-taken))
      ,@(apply (lambda (user system child-user child-system)
                 `((user+gc . ,user)
                   (system . ,system)
                   (child-user . ,child-user)
                   (child-system . ,child-system)))
               (cdr (vector->list (times))))
      (gc-mark . ,(assq-ref gcs 'gc-mark-time-taken))
      (gc-sweep . ,(assq-ref gcs 'gc-sweep-time-taken))
      (cells-marked . ,(assq-ref gcs 'cells-marked))
      (cells-swept . ,(assq-ref gcs 'cells-swept))
      )))

;;; Format a time list into a string, nicely formatted for human
;;; readers.
(define (times:format times)

  (define (convert time)
    (/ time internal-time-units-per-second))

  (define (scale-gc-time t)
    (* 10000 t))

  (if *benchmark-obscure-gc-stuff*
      (format "~6,2Fs user ~6,2Fs gc (~7,3F wm) ~6,2Fs sys ~6,2Fs tot"
              (convert (times:user times))
              (convert (times:gc times))
              (if (= 0 (times:cells-marked times)) 0
                  (/ (scale-gc-time (times:gc-mark times))
                     (times:cells-marked times)))
              (convert (times:system times))
              (convert (times:total times))
              )
      (format "~6,2Fs user ~6,2Fs gc ~6,2Fs sys ~6,2Fs tot"
              (convert (times:user times))
              (convert (times:gc times))
              (convert (times:system times))
              (convert (times:total times))
              )))

;;; Return the time elapsed between two time lists.
(define (map-cdrs f x y)
  (map (lambda (p0 p1)
         (cons (car p0)
               (f (cdr p0) (cdr p1))))
       x y))

(define (times:elapsed start end)
  (map-cdrs - end start))

;;; Add two lists of elapsed times.
(define (times:add start end)
  (map-cdrs + start end))

;;; Return the additive identity for times.
(define (times:zero)
  (list 0 0 0 0 0 0))


;;;; Running benchmarks.

;;; (save-options OPTIONS . BODY)
;;; OPTIONS ::= eval | debug | read | print
(define save-options
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((interface (symbol-append (cadr exp) '-options-interface))
	    (body (cddr exp)))
	`(let ((%options #f))
	   (dynamic-wind
	       (lambda () (set! %options (,interface)))
	       (lambda () ,@body)
	       (lambda () (,interface %options))))))))

;;; (time-in-evaluator EVALUATOR . BODY)
;;; EVALUATOR ::= normal | debug
(define time-in-evaluator
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((evaluator (cadr exp))
	    (body (cddr exp)))
	`(save-options debug
	  (if (eq? ,evaluator 'debug)
	      (debug-enable 'debug)
	      (debug-disable 'debug))
	  ;; Need to start running the selected evaluator
	  (local-eval '(let ((%start (times:now)))
			 ,@body
			 (times:elapsed %start (times:now)))
		      (the-environment)))))))

(define optarg1
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((rest (cadr exp))
	    (default (caddr exp)))
	`(if (null? ,rest)
	     ,default
	     (car ,rest))))))

;;; Mark subsequent tests as coming from the benchmark named NAME,
;;; revision REVISION.
(define (benchmark-title name revision)
  (log-text (format "Benchmark: ~A revision ~A" name revision))
  (log-data (list name revision)))

;;; Call THUNK once, and report the CPU time consumed.
;;; TITLE is the list of objects to use to label the result.
(define (time-thunk-once title thunk . evaluator)
  (let ((e (time-in-evaluator (optarg1 evaluator 'normal) (thunk))))
    (log-text (format "~20A ~A" title (times:format e)))
    (log-data (list title e))))

;;; Call THUNK five times, logging the result of each run.
;;; Finally, select the median using the times selector TIMES:GET.
;;; TITLE is the list of objects to use to label the result.
(define (time-thunk-median title times:get thunk . evaluator)
  (let* ((trials (map (lambda (trial)
			(let ((t (time-in-evaluator (optarg1 evaluator 'normal)
						    (thunk))))
			  (log-text (format "~20A ~D ~A"
					    title
					    (1+ trial)
					    (times:format t)))
			  (cons (1+ trial) t)))
		      (iota 5)))
	 (median (list-ref (stable-sort trials
					(lambda (x y)
					  (< (times:get (cdr x))
					     (times:get (cdr y)))))
			   2)))
    (log-text (format "\n===> ~D                 ~A\n"
		      (car median)
		      (times:format (cdr median))))
    (log-data (list title (cdr median)))))

(define time-thunk-median:times cadr)

;;; Call THUNK N times, and report the total CPU time consumed.
;;; TITLE is the list of objects to use to label the result.
;;;
;;; This should really provide some measure of variance, too.
;;; Standard deviation?
(define (time-thunk-repeated title n thunk . evaluator)
  (let ((e (time-in-evaluator (optarg1 evaluator 'normal)
	     (do ((i 0 (+ i 1)))
		 ((>= i n))
	       (thunk)))))
    (log-text (format "~18A ~4D passes ~A" title n (times:format e)))
    (log-data (list title n e))))

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
(define evaluator (make-fluid))

;;; Vectors are so clumsy.  Does this help?
(define (vector-update vector index func . args)
  (vector-set! vector index
	       (apply func (vector-ref vector index) args)))

(define (time-accumulate title thunk . eval)
  (fluid-set! evaluator (optarg1 eval 'normal))
  (with-fluids ((accumulator (vector 0 (times:zero))))
    (thunk)
    (let* ((totals (fluid-ref accumulator))
	   (passes (vector-ref totals 0))
	   (e (vector-ref totals 1)))
      (log-text (format "~18A ~4D passes ~A" title passes (times:format e)))
      (log-data (list title passes e)))))

(define (time-pass thunk)
  (let ((acc (fluid-ref accumulator)))
    (or (vector? acc)
	(error "time-pass: no enclosing TIME-ACCUMULATE call"))
    (let ((elapsed (time-in-evaluator (fluid-ref evaluator) (thunk))))
      (vector-update acc 0 + 1)
      (vector-update acc 1 times:add elapsed))))
