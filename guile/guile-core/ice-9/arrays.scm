

;;; {Arrays}
;;;

(begin
  (define uniform-vector? array?)
  (define make-uniform-vector dimensions->uniform-array)
  ;      (define uniform-vector-ref array-ref)
  (define (uniform-vector-set! u i o)
    (uniform-array-set1! u o i))
  (define uniform-vector-fill! array-fill!)
  (define uniform-vector-read! uniform-array-read!)
  (define uniform-vector-write uniform-array-write)

  (define (make-array fill . args)
    (dimensions->uniform-array args () fill))
  (define (make-uniform-array prot . args)
    (dimensions->uniform-array args prot))
  (define (list->array ndim lst)
    (list->uniform-array ndim '() lst))
  (define (list->uniform-vector prot lst)
    (list->uniform-array 1 prot lst))
  (define (array-shape a)
    (map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
	 (array-dimensions a))))

