;;;; gc-stress.scm: for guile's test suite -*- scheme -*-

;;; [cmm] this is modified from some clever code by Greg Harvey,
;;; mostly by making it less flexible.

(define-module (benchmarks gc-stress)
  :use-module (benchmarks lib)
  :use-module (ice-9 optargs))

(export gc-stress-run)

;;; {Random list stuff} This basically creates a ton of junk,
;;; occasionally saving a reference to the junk, but mostly letting it
;;; die. This is the sort of thing a generational gc should excel at,
;;; and is a reasonably hand drawn facsimile of a reasonably hand
;;; drawn program

(define *max-elem* 8)
(define *iterations* (* 4 1024))
(define *elem-size* 64)

(define (make-number-element-generator n)
  (lambda ()
    (random n)))

(define (stupid-generator)
  #f)

(define (make-nested-list-element-generator n)
  (if (> n 4)
      (lambda ()
        (random-list (make-number-element-generator n)
                     (make-nested-list-element-generator (quotient (* n 3) 5))))
      (lambda ()
        (make-vector *elem-size*))))

(define default-element-generator
  (make-nested-list-element-generator (quotient (* *max-elem* 3)  5)))

(define default-size-generator
  (make-number-element-generator *max-elem*))

(define random-list
  (lambda* (#:optional (generate-size default-size-generator)
                       (generate-element default-element-generator)
                       (save? default-save?-function))
           (let* ((size (generate-size))
                  (lst (make-list size)))
             (do ((i 0 (+ i 1))
                  (l lst (cdr l)))
                 ((= i size) lst)
               (let ((ll (generate-element)))
                 (if (save? i)
                     (set-car! l ll)))))))

;;; do-randomlist-stuff
;;;    the first arg is the number of iterations
;;;
;;;    the second arg should be a function returning #t if we're supposed to
;;;    save the list we're about the generate; the arg is the current
;;;    iteration
;;;
;;;    the third and fourth args are passed to random list to generate a
;;;    list on each iteration. The third should be a thunk returning the
;;;    size of the list to be generated; the fourth should be a thunk
;;;    generating an element.
;;;
;;;    the fifth arg is called on each generated list. The default is
;;;    not to do anything

(define default-save?-function
  (lambda (x)
    (= 0 (remainder (random (+ x 1)) 2))))

(define do-randomlist-stuff
  (lambda* (#:key (count *iterations*) (save? default-save?-function)
                  (element-size default-size-generator)
                  (element-generator default-element-generator)
                  (munge-list (lambda (x) x)))
           (let ((remembered-list '()))
             (do ((i 0 (+ i 1)))
                 ((= i count) #t)
               (let ((new-l (munge-list (random-list element-size element-generator))))
                 (and (save? i)
                      (set! remembered-list
                            (cons remembered-list
                                  new-l)))))
             )))

(define (dispatch-test)
  (gc)
  (time-thunk-median '*
		     times:gc-mark
		     (lambda ()
		       (do-randomlist-stuff))
		     'normal)
  (time-thunk-median:times (last-data)))

(define (gc-stress-run)
  (debug-set! stack 0)
  (set! *benchmark-obscure-gc-stuff* #t)
  (benchmark-title 'gc-stress 1)
  (dispatch-test))
