;;;; List functions not provided in R5RS or srfi-1

;;; Copyright (C) 2003 Free Software Foundation, Inc.
;;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(define-module (ice-9 list)
  :export (rassoc rassv rassq))

(define (generic-rassoc key alist =)
  (let loop ((ls alist))
      (and (not (null? ls))
	   (if (= key (cdar ls))
	       (car ls)
	       (loop (cdr ls))))))

(define (rassoc key alist . =)
  (generic-rassoc key alist (if (null? =) equal? (car =))))

(define (rassv key alist)
  (generic-rassoc key alist eqv?))

(define (rassq key alist)
  (generic-rassoc key alist eq?))