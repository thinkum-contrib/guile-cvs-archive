(debug-enable 'backtrace 'debug)

(define (check x y)
  (or (= x y)
      (error "badness!")))

(define d1 3+4i)     ;; r <= i
(define d2 4+3i)     ;; r > i
(define cn 25+125i)

(check (/ d1) 0.12-0.16i)               ; single arg, case 1
(check (/ d2) 0.16-0.12i)               ; single arg, case 2
(check (/ cn d1) 23.0+11.0i)            ; <complex> / <complex>, case 1
(check (/ cn d2) 19.0+17.0i)            ; <complex> / <complex>, case 2
(check (/ 25 d1) 3.0-4.0i)              ; <real> / <complex>, case 1
(check (/ 25 d2) 4.0-3.0i)              ; <real> / <complex>, case 2
