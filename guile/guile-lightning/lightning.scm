(define-module (lightning))

(dynamic-call "scm_init_lightning" (dynamic-link "libguile-lightning"))

(export assemble disassemble)
