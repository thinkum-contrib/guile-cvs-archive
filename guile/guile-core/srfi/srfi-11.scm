;;;; srfi-11.scm --- SRFI-11 procedures for Guile

;;; Copyright (C) 2000 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA

(define-module (srfi srfi-11)
  :use-module (ice-9 syncase))

;;;;;;;;;;;;;;
;; let-values
;;
;; Current approach is to translate
;;
;;   (let-values (((x y z) (foo a b))
;;                ((p q) (bar c)))
;;     (baz x y z p q))
;;
;; into
;;
;;   (call-with-values (lambda () (foo a b))
;;     (lambda (<tmp-x> <tmp-y> <tmp-z>)
;;       (call-with-values (lambda () (bar c))
;;         (lambda (<tmp-p> <tmp-q>)
;;           (let ((x <tmp-x>)
;;                 (y <tmp-y>)
;;                 (z <tmp-z>)
;;                 (p <tmp-p>)
;;                 (q <tmp-q>))
;;             (baz x y z p q))))))

;; I originally wrote this as a define-macro, but then I found out
;; that guile's gensym/gentemp was broken, so I tried rewriting it as
;; a syntax-rules statement.
;;
;; Since syntax-rules didn't seem powerful enough to implement
;; let-values in one definition without exposing illegal syntax (or
;; perhaps my brain's just not powerful enough :>).  I tried writing
;; it using a private helper, but that didn't work because the
;; let-values expands outside the scope of this module.  I wonder why
;; syntax-rules wasn't designed to allow "private" patterns or
;; similar...
;;
;; So in the end, I dumped the syntax-rules implementation, reproduced
;; here for posterity, and went with the define-macro one below --
;; gensym/gentemp's got to be fixed anyhow...
;
; (define-syntax let-values-helper
;   (syntax-rules ()
;     ;; Take the vars from one let binding (i.e. the (x y z) from ((x y
;     ;; z) (values 1 2 3)) and turn it in to the corresponding (lambda
;     ;; (<tmp-x> <tmp-y> <tmp-z>) ...) from above, keeping track of the
;     ;; temps you create so you can use them later...
;     ;;
;     ;; I really don't fully understand why the (var-1 var-1) trick
;     ;; works below, but basically, when all those (x x) bindings show
;     ;; up in the final "let", syntax-rules forces a renaming.

;     ((_ "consumer" () lambda-tmps final-let-bindings lv-bindings
;         body ...)
;      (lambda lambda-tmps
;        (let-values-helper "cwv" lv-bindings final-let-bindings body ...)))
    
;     ((_ "consumer" (var-1 var-2 ...) (lambda-tmp ...) final-let-bindings lv-bindings
;         body ...)
;      (let-values-helper "consumer"
;                         (var-2 ...)
;                         (lambda-tmp ... var-1)
;                         ((var-1 var-1) . final-let-bindings)
;                         lv-bindings
;                         body ...))

;     ((_ "cwv" () final-let-bindings body ...)
;      (let final-let-bindings
;          body ...))

;     ((_ "cwv" ((vars-1 binding-1) other-bindings ...) final-let-bindings
;         body ...)
;      (call-with-values (lambda () binding-1)
;        (let-values-helper "consumer"
;                           vars-1
;                           ()
;                           final-let-bindings
;                           (other-bindings ...)
;                           body ...)))))
;
; (define-syntax let-values
;   (syntax-rules ()
;     ((let-values () body ...)
;      (begin body ...))
;     ((let-values (binding ...) body ...)
;      (let-values-helper "cwv" (binding ...) () body ...))))
;
;
; (define-syntax let-values
;   (letrec-syntax ((build-consumer
;                    ;; Take the vars from one let binding (i.e. the (x
;                    ;; y z) from ((x y z) (values 1 2 3)) and turn it
;                    ;; in to the corresponding (lambda (<tmp-x> <tmp-y>
;                    ;; <tmp-z>) ...) from above.
;                    (syntax-rules ()
;                      ((_ () new-tmps tmp-vars () body ...)
;                       (lambda new-tmps
;                         body ...))
;                      ((_ () new-tmps tmp-vars vars body ...)
;                       (lambda new-tmps
;                         (lv-builder vars tmp-vars body ...)))
;                      ((_ (var-1 var-2 ...) new-tmps tmp-vars vars body ...)
;                       (build-consumer (var-2 ...)
;                                       (tmp-1 . new-tmps)
;                                       ((var-1 tmp-1) . tmp-vars)
;                                       bindings
;                                       body ...))))
;                   (lv-builder
;                    (syntax-rules ()
;                      ((_ () tmp-vars body ...)
;                       (let tmp-vars
;                           body ...))
;                      ((_ ((vars-1 binding-1) (vars-2 binding-2) ...)
;                          tmp-vars
;                          body ...)
;                       (call-with-values (lambda () binding-1)
;                         (build-consumer vars-1
;                                         ()
;                                         tmp-vars
;                                         ((vars-2 binding-2) ...)
;                                         body ...))))))
;    
;     (syntax-rules ()
;       ((_ () body ...)
;        (begin body ...))
;       ((_ ((vars binding) ...) body ...)
;        (lv-builder ((vars binding) ...) () body ...)))))

;; FIXME: This is currently somewhat unsafe (b/c gentemp/gensym is
;; broken -- right now (as of 1.4.1, it doesn't generate unique
;; symbols)
(define-macro (let-values vars . body)
  (define (let-values-helper vars body prev-tmps)
    (let* ((var-binding (car vars))
           (new-tmps (map (lambda (sym) (list sym (gentemp)))
                          (car var-binding)))
           (tmps (append new-tmps prev-tmps)))
      (if (null? (cdr vars))
          `(call-with-values (lambda () ,(cadr var-binding))
             (lambda ,(map cadr new-tmps)
               (let ,tmps
                 ,@body)))
          `(call-with-values (lambda () ,(cadr var-binding))
             (lambda ,(map cadr new-tmps)
               ,(let-values-helper (cdr vars) body tmps))))))
  
  (if (null? vars)
      `(begin ,@body)
      (let-values-helper vars body '())))

;;;;;;;;;;;;;;
;; let*-values
;;
;; Current approach is to translate
;;
;;   (let*-values (((x y z) (foo a b))
;;                ((p q) (bar c)))
;;     (baz x y z p q))
;;
;; into
;;
;;   (call-with-values (lambda () (foo a b))
;;     (lambda (x y z)
;;       (call-with-values (lambda (bar c))
;;         (lambda (p q)
;;           (baz x y z p q)))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body ...)
     (begin body ...))
    ((let*-values ((vars-1 binding-1) (vars-2 binding-2) ...) body ...)
     (call-with-values (lambda () binding-1)
       (lambda vars-1
         (let*-values ((vars-2 binding-2) ...)
           body ...))))))

; Alternate define-macro implementation...
;       
; (define-macro (let*-values vars . body)
;   (define (let-values-helper vars body)
;     (let ((var-binding (car vars)))
;       (if (null? (cdr vars))
;           `(call-with-values (lambda () ,(cadr var-binding))
;              (lambda ,(car var-binding)
;                ,@body))
;           `(call-with-values (lambda () ,(cadr var-binding))
;              (lambda ,(car var-binding)
;                ,(let-values-helper (cdr vars) body))))))
  
;   (if (null? vars)
;       `(begin ,@body)
;       (let-values-helper vars body)))

(export-syntax let-values
               let*-values)
