
(use-modules (ice-9 statprof))

(define (simple-test-call-count-distribution)
  ;; test to see that if we call 3 identical functions equally, they
  ;; show up equally in the call count +- 10%
  
  (define (func-a n) (do ((i 0 (+ i 1))) ((= 200 i)) (+ i i)))
  (define (func-b n) (do ((i 0 (+ i 1))) ((= 200 i)) (+ i i)))
  (define (func-c n) (do ((i 0 (+ i 1))) ((= 200 i)) (+ i i)))
  
  (let ((num-calls 333)
        (max-allowed-drift 0.1)
        (func func-a))

    ;; Run test.
    (statprof-reset 0 30000)
    (statprof-start)
    (let loop ((x num-calls))
      (cond
       ((positive? x)
        (func x)
        (cond
         ((eq? func func-a) (set! func func-b))
         ((eq? func func-b) (set! func func-c))
         ((eq? func func-c) (set! func func-a)))
        (loop (- x 1)))))
    (statprof-stop)

    (let* ((a-data (statprof-proc-call-data func-a))
           (b-data (statprof-proc-call-data func-b))
           (c-data (statprof-proc-call-data func-c))
           ;;
           (samples (map statprof-call-data-samples
                         (list a-data b-data c-data)))
           (average (/ (apply + samples) 3))
           ;;
           (diffs (map (lambda (x) (abs (- x average)))
                       samples))
           (max-diff (apply max diffs)))

      (let ((drift-fraction (/ max-diff average)))
        (if (> drift-fraction max-allowed-drift)
            (begin
              (simple-format
               #t
               "  call-frequencies: too far apart ~A%\n"
               (* 100 drift-fraction))
              #f)
            (begin
              (simple-format 
               #t
               "  call-frequencies: within tolerance ~A%\n"
               (* 100 drift-fraction))
              #t))))))

(debug-enable 'debug)
(trap-enable 'traps)
(with-traps
 (if (simple-test-call-count-distribution)
     (exit 0)
     (exit 1))))
