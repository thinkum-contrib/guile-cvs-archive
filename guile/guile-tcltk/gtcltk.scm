(define-module (tcltk gtcltk)
  :use-module (tcltk dynlink))

(merge-compiled-code "scm_init_gtcltk" "libgtcltk")
