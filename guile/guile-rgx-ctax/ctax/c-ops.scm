;;; installed-scm-file

;;;; 	Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; 


(define-module #/ctax/c-ops)


;;; {C ops}
;;;
;;; These make Ctax a simple C-like language with no particular
;;; interpretation for dollar expressions like: $x.
;;;

;; This is used by "if", "while", "for" and "do" to interpret
;; condition expression results.
;;
(define-public (ctax-test x)
  (and x
       (not (or (and (number? x) (= 0 x))
		(and (char? x) (char=? #\000 x))))))

(define ((wrap-math f) . args)
  (apply f (map (lambda (x) (if (char? x) (char->integer x) x)) args)))

(define-public ctax-| (wrap-math logior))
(define-public ctax-^ (wrap-math logxor))
(define-public ctax-& (wrap-math logand))
(define-public ctax-== (wrap-math =))
(define-public ctax-< (wrap-math <))
(define-public ctax-> (wrap-math >))
(define-public ctax-<= (wrap-math <=))
(define-public ctax->= (wrap-math >=))
(define-public ctax-<< (wrap-math ash))
(define-public ctax-+ (wrap-math +))
(define-public ctax-- (wrap-math -))
(define-public ctax-* (wrap-math *))
(define-public ctax-/ (wrap-math /))
(define-public ctax-% (wrap-math modulo))

(define-public ctax-!= (wrap-math (lambda (a b) (not (= a b)))))
(define-public ctax->> (wrap-math (lambda (a b) (ash a (- b)))))
(define-public ctax-lognot (wrap-math (lambda (a) (lognot a))))
(define-public ctax-! (wrap-math (lambda (a) (if (number? a) (if (zero? a) 1 0) (not a)))))

(defmacro-public ctax-&& args
  `(and ,@ (map (lambda (exp) `(ctax-test ,exp)) ,args)))

(defmacro-public ctax-|| args
  `(or ,@ (map (lambda (exp) `(ctax-test ,exp)) ,args)))

(defmacro-public ctax-?: (cnd . rest)
  `(if (ctax-test ,cnd) ,@ rest))

(defmacro-public ctax-$ a (error 'undefined-$ a))

;; These are used for expressions like x[y] and x[y] = z.
;;
(define-public (ctax-array-ref dest key)
  (if (string? dest)
      (string-ref dest key)
      (vector-ref dest key)))

(define-public (ctax-array-set! dest key val)
  (if (string? dest)
      (string-set! dest key val)
      (vector-set! dest key val)))


(define-public fi id)
(define-public break id)
(define-public continue id)
