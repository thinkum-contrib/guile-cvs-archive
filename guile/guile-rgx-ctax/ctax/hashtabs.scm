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


(define-module #/ctax/hashtabs
  :use-module #/ice-9/mapping)


;;; {Hash Table Ops}
;;;
;;; These Ctax array operators support tagged hash tables
;;; as well as ordinary vectors.
;;;

(define-public hash_table hash-table-mapping)
(define-public (ctax-array-ref obj key) (if (vector? obj) (vector-ref obj key) (mapping-ref obj key)))
(define-public (ctax-array-set! obj key val) (if (vector? obj) (vector-set! obj key val) (mapping-set! obj key val)))
