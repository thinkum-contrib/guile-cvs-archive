(define %library-dir
  (lambda () (string-append (%package-data-dir) "/" (version))))

(chdir (string-append (%library-dir) "/ice-9"))
(define-module (ice-9 run-test) :use-module (ice-9 test))