;;;; lib.scm --- utility functions for running benchmarks
;;;; Jim Blandy <jimb@red-bean.com> --- April 1999

(define-module (benchmarks lib)
  #:use-module (ice-9 format))
(export start-log
	end-log
	identify
	time-thunk-once
	time-thunk-repeated)


;;;; Primitive logging functions.

;;; The I/O port to which we write results.
(define log-port #f)

(define-public (start-log filename)
  (end-log)
  (set! log-port (open-file filename "a"))

  ;; Record the date and time.
  (let* ((time (current-time)))
    (log-text "Opened log file, "
	      (strftime "%a %e %b %H:%M:%S" (localtime time)))
    (log-data (list 'start-log time))))

(define-public (end-log)
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
(define-public (log-text . objects)
  (define (out port objects)
    (for-each (lambda (obj) (display obj port))
	      objects)
    (newline port))
  (out (current-output-port) objects)
  (if (output-port? log-port)
      (out log-port (cons "; " objects))))

;;; Write a Scheme-readable object to the log file.
(define-public (log-data object)
  (if (output-port? log-port)
      (begin
	(write object log-port)
	(newline log-port))))


;;;; Functions for handling time reasonably.

;;; Return a list of the form (GC USER SYSTEM CHILD-USER CHILD-SYSTEM),
;;; where:
;;;   GC is the number of seconds Guile has spent in GC
;;;   USER is the number of seconds this process has occupied the CPU
;;;   SYSTEM is the number of seconds the CPU has spent in the kernel on
;;;       behalf of this program
;;;   CHILD-USER and CHILD-SYSTEM are the same as USER and SYSTEM, but
;;;       include the time accumulated by child processes.
;;;
;;; The difference between this and the system's TIMES procedure is
;;; that this function returns times in seconds, so it's independent
;;; of internal-time-units-per-second.
(define (cpu-times-used)
  (map (lambda (time) (/ time internal-time-units-per-second))
       (cons (cdr (assq 'gc-time-taken (gc-stats)))
	     (cdr (vector->list (times))))))


;;;; Running benchmarks.

;;; Mark subsequent tests as coming from the benchmark named NAME,
;;; revision REVISION.
(define (identify name revision)
  (log-text "Benchmark: " name)
  (log-text "Revision: " revision)
  (log-data (list name revision)))

;;; Call THUNK once, and report the CPU time consumed.
;;; TITLE is the list of objects to use to label the result.
(define (time-thunk-once title thunk)
  (let ((start (cpu-times-used)))
    (thunk)
    (let ((end (cpu-times-used)))
      (let* ((elapsed (map - end start))
	     (user-no-gc (- (cadr elapsed) (car elapsed)))
	     (report (cons user-no-gc (cons (car elapsed) (cddr elapsed)))))
	(log-text title)
	(log-text (format "~,3Fs user   ~,3Fs gc   ~,3Fs sys"
			  (cadr report) (car report) (caddr report)))
	(log-data (list title start end))))))


;;; Call THUNK N times, and report the total CPU time consumed.
;;; TITLE is the list of objects to use to label the result.
;;;
;;; This should really provide some measure of variance, too.
;;; Standard deviation?
(define (time-thunk-repeated title n thunk)
  (let ((start (cpu-times-used)))
    (do ((i 0 (+ i 1)))
	((>= i n))
      (thunk))
    (let ((end (cpu-times-used)))
      (let* ((elapsed (map - end start))
	     (user-no-gc (- (cadr elapsed) (car elapsed))))
	(log-text title)
	(log-text (format "~D passes  ~,3Fs user   ~,3Fs gc   ~,3Fs sys"
			  n user-no-gc (car elapsed) (caddr elapsed)))
	(log-data (list title start end))))))

