;;; installed-scm-file

;;;; Copyright (C) 2003 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;;; 


(define-module (ice-9 weak-vector)
  :export (make-weak-vector list->weak-vector weak-vector weak-vector?
	   make-weak-key-alist-vector
	   make-weak-value-alist-vector
	   make-doubly-weak-alist-vector
	   weak-key-alist-vector?
	   weak-value-alist-vector?
	   doubly-weak-alist-vector?)  ; C
  )

(%init-weaks-builtins) ; defined in libguile/weaks.c