(or (= 1 (false-if-exception (call/cc (lambda (c) (0 (c 1))))))
    (error "eager funpos checking"))
