(use-modules (ice-9 threads))

(begin-thread
 (let loop ()
   (display "a")
   (newline)
   (loop)))

(begin-thread
 (let loop ()
   (display "b")
   (newline)
   (loop)))
