(or (= -1 (let ((f -)) (let f ((n (f 1))) n)))
    (error "bad named-let xform"))
