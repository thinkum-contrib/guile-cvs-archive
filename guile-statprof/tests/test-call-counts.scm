
(use-modules (ice-9 statprof))

(debug-set! stack 0)

(define (simple-test-call-count)
  ;; Test to see that if we call a function N times while the profiler
  ;; is active, it shows up N times.
  (let ((num-calls 2000))

    (define (do-nothing n)
      (simple-format #f "FOO ~A\n" (+ n n)))
    
    ;; Run test.
    (statprof-reset 0 50000)
    (statprof-start)
    (let loop ((x num-calls))
      (cond
       ((positive? x)
        (do-nothing x)
        (loop (- x 1))
        #t)))
    (statprof-stop)
    
    ;;(statprof-display)

    ;; Check result.
    (let ((proc-data (statprof-proc-call-data do-nothing)))
      (if (and proc-data
               (= (statprof-call-data-calls proc-data)
                  num-calls))
          (begin
            (display "  call-counts: OK\n")
            #t)
          (error
           (simple-format #f "Expected ~A calls, got ~A.\n" 
                          num-calls
                          (and proc-data
                               (statprof-call-data-calls proc-data))))))))

(debug-enable 'debug)
(trap-enable 'traps)

(if (simple-test-call-count)
    (exit 0)
    (exit 1))
