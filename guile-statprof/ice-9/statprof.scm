
;; When you add new features, please also add tests to ./tests/ if you
;; have time, and then add the new files to ./run-tests.  Also, if
;; anyone's bored, there are a lot of existing API bits that don't
;; have tests yet.

;; TODO
;;
;; Check why tail calls aren't apply-frame counted (or are they).
;; Calculate leaf + parents.
;; make sure frames we're skipping in PROF handler are OK.

(define-module (ice-9 statprof)
  :export (statprof-active?
           statprof-start
           statprof-stop
           statprof-reset

           statprof-accumulated-time
           statprof-sample-count
           statprof-fold-call-data
           statprof-proc-call-data
           statprof-call-data-name
           statprof-call-data-calls
           statprof-call-data-samples
           statprof-call-data->stats
           
           statprof-stats-proc-name
           statprof-stats-%-time-in-proc
           statprof-stats-secs-in-proc
           statprof-stats-calls
           statprof-stats-secs-per-call
           statprof-stats-samples

           statprof-display))

;;(use-modules (ice-9 format))
(use-modules (ice-9 slib))
(require 'stdio)

;; This profiler tracks two numbers for every function called while
;; it's active.  It tracks the total number of calls, and the number
;; of times the function was active when the sampler fired.
;;
;; Globally the profiler tracks the total time elapsed and the number
;; of times the sampler was fired.
;;
;; Right now, this profiler is not per-thread and is not thread safe.

(define accumulated-time #f)            ; total so far.
(define last-start-time #f)             ; start-time when timer is active.
(define sample-count #f)                ; total count of sampler calls.
(define sampling-frequency #f)          ; in (seconds . microseconds)
(define remaining-prof-time #f)         ; time remaining when prof suspended.
(define profile-level 0)                ; for user start/stop nesting.

;; procedure-data will be a weak-key-hash where the key is the function
;; object itself and the value is the data.  The data will be a vector
;; like this: #(name call-count sample-count)
(define procedure-data #f)

;; If you change the call-data data structure, you need to also change
;; sample-uncount-frame.
(define (make-call-data name call-count sample-count)
  (vector name call-count sample-count))
(define (call-data-name cd) (vector-ref cd 0))
(define (call-data-call-count cd) (vector-ref cd 1))
(define (call-data-sample-count cd) (vector-ref cd 2))
(define (set-call-data-name! cd name) (vector-set! cd 0 name))
(define (set-call-data-call-count! cd val) (vector-set! cd 1 val))
(define (set-call-data-sample-count! cd val) (vector-set! cd 2 val))

(define-macro (accumulate-time stop-time)
  `(set! accumulated-time (+ accumulated-time (- ,stop-time last-start-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIGPROF handler

(define (sample-count-frame frame procs-seen)
  (if (frame-procedure? frame)
      (let* ((frame-proc (frame-procedure frame))
             (proc-data (hashq-ref procedure-data frame-proc)))

        (if (not (hashq-ref procs-seen frame-proc))
            (begin
              (hashq-set! procs-seen frame-proc #t)
              (if proc-data
                  (set-call-data-sample-count!
                   proc-data
                   (+ (call-data-sample-count proc-data) 1))
                  ;; I think perhaps this is impossible...
                  (hashq-set!
                   procedure-data
                   frame-proc
                   (make-call-data (procedure-name frame-proc) 0 1))))))))

(define (sample-uncount-frame frame)
  (if (frame-procedure? frame)
      (let* ((frame-proc (frame-procedure frame))
             (proc-data (hashq-ref procedure-data frame-proc)))

        (if (not proc-data)
            (let ((str (simple-format
                        #f
                        "Can't possibly happen -- ~A must have been counted!\n"
                        (procedure-name frame-proc))))
              (error str)))
        
        (let ((sample-count (call-data-sample-count proc-data)))
          (if (and (= sample-count 1)
                   (zero? (call-data-call-count proc-data)))
              (hashq-remove! procedure-data frame-proc)
              (set-call-data-sample-count! proc-data (- sample-count 1)))))))

(define (sample-stack-procs stack)
  (let ((stacklen (stack-length stack))
        ;; where to start profiling. (we need to skip the
        ;; profile funcs themselves).
        (caller-stack-num 2)
        (hit-count-call? #f)
        (procs-seen (make-hash-table 13)))
    (if (< caller-stack-num stacklen)
        (begin
          ;; We've got at least one non-profiling frame
          (set! sample-count (+ sample-count 1))
          
          ;; Now accumulate stats for the whole stack.
          (let loop ((n caller-stack-num))
            (if (< n stacklen)
                (let ((frame (stack-ref stack n)))

                  (if (and (frame-procedure? frame)
                           (eq? (frame-procedure frame) count-call))
                      ;; Trouble -- we have to uncount all the frames
                      ;; more inner than this one and ignore this one
                      ;; because we're not supposed to be sampling
                      ;; count-call and its sub-functions.
                      (begin
                        (set! hit-count-call? #t)
                        ;; Blow away the procs-seen table.
                        (set! procs-seen (make-hash-table 13))
                        (do ((i (- n 1) (- i 1)))
                            ((<= i caller-stack-num))
                          (sample-uncount-frame (stack-ref stack i))))
                      (sample-count-frame frame procs-seen))
                  (loop (+ n 1)))))))
    hit-count-call?))

(define inside-profiler? #f)

(define (profile-signal-handler sig)
  (set! inside-profiler? #t)
  ;;(trap-disable 'apply-frame)           ; unconditionally.

  (if (positive? profile-level)
      (let* ((stop-time (get-internal-run-time))
             (inside-apply-trap? (sample-stack-procs (make-stack #t))))

        (if (not inside-apply-trap?)
            (begin
              ;; disabling here is just a little more efficient, but
              ;; not necessary given inside-profiler?.  We can't just
              ;; disable unconditionally at the top of this function
              ;; and eliminate inside-profiler? because it seems to
              ;; confuse guile wrt re-enabling the trap when
              ;; count-call finishes.
              (trap-disable 'apply-frame)
              (accumulate-time stop-time)))
        
        (setitimer ITIMER_PROF
                   0 0
                   (car sampling-frequency)
                   (cdr sampling-frequency))
        
        (if (not inside-apply-trap?)
            (begin
              (set! last-start-time (get-internal-run-time))
              (trap-enable 'apply-frame)
              ))))
  (set! inside-profiler? #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count total calls.

(define (count-call trap-name continuation tail)
  (if (not inside-profiler?)
      (begin
        (accumulate-time (get-internal-run-time))
        
        (let ((frame (last-stack-frame continuation)))
          (if (frame-procedure? frame)
              (let* ((frame-proc (frame-procedure frame))
                     (proc-data (hashq-ref procedure-data frame-proc)))
                (if proc-data
                    (set-call-data-call-count! proc-data
                                               (+ (call-data-call-count proc-data)
                                                  1))
                    (hashq-set! procedure-data frame-proc
                                (make-call-data (procedure-name frame-proc)
                                                1
                                                0))))))
        
        (set! last-start-time (get-internal-run-time)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (statprof-active?) (positive? profile-level))

;; Do not call this from statprof internal functions -- user only.
(define (statprof-start)
  ;; After some head-scratching, I don't *think* I need to mask/unmask
  ;; signals here, but if I'm wrong, please let me know.
  (set! profile-level (+ profile-level 1))
  (if (= profile-level 1)
      (let* ((rpt remaining-prof-time)
             (use-rpt? (and rpt
                            (or (positive? (car rpt))
                                (positive? (cdr rpt))))))
        (set! remaining-prof-time #f)
        (set! last-start-time (get-internal-run-time))
        (if use-rpt?
            (setitimer ITIMER_PROF 0 0 (car rpt) (cdr rpt))
            (setitimer ITIMER_PROF
                       0 0
                       (car sampling-frequency)
                       (cdr sampling-frequency)))
        (trap-enable 'apply-frame)
        #t)))
  
;; Do not call this from statprof internal functions -- user only.
(define (statprof-stop)
  ;; After some head-scratching, I don't *think* I need to mask/unmask
  ;; signals here, but if I'm wrong, please let me know.
  (set! profile-level (- profile-level 1))
  (if (zero? profile-level)
      (begin
        (trap-disable 'apply-frame)
        ;; I believe that we need to do this before getting the time
        ;; (unless we want to make things even more complicated).
        (set! remaining-prof-time (setitimer ITIMER_PROF 0 0 0 0))
        (accumulate-time (get-internal-run-time))
        (set! last-start-time #f))))

(define (statprof-reset sample-seconds sample-microseconds)
  (if (positive? profile-level)
      (error "Can't reset profiler while profiler is running."))
  (set! accumulated-time 0)
  (set! last-start-time #f)
  (set! sample-count 0)
  (set! sampling-frequency (cons sample-seconds sample-microseconds))
  (set! remaining-prof-time #f)
  (set! procedure-data (make-weak-key-hash-table 131))
  (trap-set! apply-frame-handler count-call)
  ;;(debug-enable 'backtrace)
  (debug-enable 'debug)
  ;;(debug-enable 'trace)
  (trap-enable 'traps)
  (sigaction SIGPROF profile-signal-handler)
  #t)

(define (statprof-fold-called proc init)
  ;; proc should take two args (call-data prior-result).  Note that a
  ;; given proc-name may appear multiple times, but if it does, it
  ;; represents different functions with the same name.
  (if (positive? profile-level)
      (error "Can't call statprof-fold-called while profiler is running."))

  (hash-fold
   (lambda (key value prior-result)
     (proc value prior-result))
   init
   procedure-data))

(define (statprof-proc-call-data proc)
  (if (positive? profile-level)
      (error "Can't call statprof-fold-called while profiler is running."))

  ;; func should take one arg, call-data.  Note that a
  ;; given proc-name may appear multiple times, but if it does, it
  ;; represents different functions with the same name.
  (hashq-ref procedure-data proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stats

(define (statprof-call-data->stats call-data)
  ;; returns (vector proc-name
  ;;                 %-time-in-proc
  ;;                 seconds-in-proc
  ;;                 num-calls
  ;;                 secs-per-call
  ;;                 sample-count)

  (let* ((proc-name (statprof-call-data-name call-data))
         (fractional-time-in-proc (/ (statprof-call-data-samples call-data)
                                     (statprof-sample-count)))
         (seconds-in-proc (* fractional-time-in-proc
                             (statprof-accumulated-time)))
         (num-calls (statprof-call-data-calls call-data))
         (secs-per-call (/ seconds-in-proc num-calls)))

    (vector proc-name
            (* fractional-time-in-proc 100)
            seconds-in-proc
            num-calls
            secs-per-call
            (statprof-call-data-samples call-data))))

(define (statprof-stats-proc-name stats) (vector-ref stats 0))
(define (statprof-stats-%-time-in-proc stats) (vector-ref stats 1))
(define (statprof-stats-secs-in-proc stats) (vector-ref stats 2))
(define (statprof-stats-calls stats) (vector-ref stats 3))
(define (statprof-stats-secs-per-call stats) (vector-ref stats 4))
(define (statprof-stats-samples stats) (vector-ref stats 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (statprof-display . port)
  
  (if (null? port) (set! port (current-output-port)))
  
  (let* ((stats-list (statprof-fold-called
                      (lambda (data prior-value)
                        (cons (statprof-call-data->stats data)
                              prior-value))
                      '()))
         (sorted-stats (sort stats-list
                             (lambda (x y)
                               (> (statprof-stats-%-time-in-proc x)
                                  (statprof-stats-%-time-in-proc y))))))

    (define (display-stats-line stats)
      (fprintf port "%6.2f %9.2f"
               (statprof-stats-%-time-in-proc stats)
               (statprof-stats-secs-in-proc stats))
      (fprintf port " %8lu %8lu %8.2f  "
               (statprof-stats-calls stats)
               (statprof-stats-samples stats)
               (statprof-stats-secs-per-call stats))
      (display (statprof-stats-proc-name stats) port)
      (newline port))
    
    (fprintf port "%5.5s %10.10s %8.8s %8.8s %8.8s  %-8.8s\n"
             "%  " "cumulative" "" "" "total " "")
    (fprintf port "%5.5s %9.9s  %8.8s %8.8s %8.8s  %-8.8s\n"
             "time" "seconds" "calls" "samples" "s/call" "name")

    (for-each display-stats-line sorted-stats)

    (display "---\n" port)
    (simple-format #t "Sample count: ~A\n" (statprof-sample-count))
    (simple-format #t "Total time: ~A seconds\n" (statprof-accumulated-time))))

(define (statprof-display-anomolies)
  (statprof-fold-called
   (lambda (data prior-value)
     (if (and (zero? (call-data-call-count data))
              (positive? (call-data-sample-count data)))
         (simple-format #t
                        "==[~A ~A ~A]\n"
                        (call-data-name data)
                        (call-data-call-count data)
                        (call-data-sample-count data))))
   #f)
  (simple-format #t "Total time: ~A\n" (statprof-accumulated-time))
  (simple-format #t "Sample count: ~A\n" (statprof-sample-count)))

(export statprof-display-anomolies)

(define (statprof-accumulated-time)
  (if (positive? profile-level)
      (error "Can't get accumulated time while profiler is running."))
  (/ accumulated-time internal-time-units-per-second))

(define (statprof-sample-count)
  (if (positive? profile-level)
      (error "Can't get accumulated time while profiler is running."))
  sample-count)

(define statprof-call-data-name call-data-name)
(define statprof-call-data-calls call-data-call-count)
(define statprof-call-data-samples call-data-sample-count)
