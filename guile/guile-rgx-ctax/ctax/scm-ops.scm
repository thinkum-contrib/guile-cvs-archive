;;; installed-scm-file

;;;; 	Copyright (C) 1996 Free Software Foundation, Inc.
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


(define-module #/ctax/scm-ops)


;;; {Scheme ops}
;;;
;;; These make Ctax an alternative syntax for Scheme, with
;;; no concessions to C.   For example, "while (x)" will only
;;; terminate when x is #f, not when it is 0.
;;;
;;; scmops gives no particular interpretation for dollar 
;;; expressions like: $x.
;;;

(define-public fi id)
(define-public break id)
(define-public continue id)

(define-public ctax-| logior)
(define-public ctax-^ logxor)
(define-public ctax-& logand)
(define-public ctax-== =)
(define-public ctax-< <)
(define-public ctax-> >)
(define-public ctax-<= <=)
(define-public ctax->= >=)
(define-public ctax-<< ash)
(define-public ctax-+ +)
(define-public ctax-- -)
(define-public ctax-* *)
(define-public ctax-/ /)
(define-public ctax-% modulo)
(define-public (ctax-!= a b) (not (= a b)))
(define-public (ctax->> a b) (ash a (- b)))
(define-public ctax-&& and)
(define-public ctax-|| or)
(define-public ctax-?: if)
(define-public (ctax-lognot a) (lognot a))
(define-public (ctax-! a) (not a))
(defmacro-public ctax-$ a (error 'undefined-$ a))

;; This is used by "if", "while", "for" and "do" to interpret
;; condition expression results.
;;
(define-public (ctax-test x) x)

;; These are used for expressions like x[y] and x[y] = z.
;;
(define-public ctax-array-ref vector-ref)
(define-public ctax-array-set! vector-set!)
