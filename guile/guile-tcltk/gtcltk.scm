(define-module (tcltk gtcltk)
  :use-module (tcltk dynlink))

(if (defined? 'load-extension)
    (load-extension "libguile-tcltk-gtcltk" "scm_init_tcltk_gtcltk")
    (merge-compiled-code "scm_init_gtcltk" "libguile-tcltk-gtcltk"))

