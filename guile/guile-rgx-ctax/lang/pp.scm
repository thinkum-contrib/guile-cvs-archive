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



(define-module #/lang/pp
  :use-module #/lang/lex
  :use-module #/lang/grammar)



(define-public (pp-lexer spec)
  (for-each (lambda (item) 
	      (display (car item))
	      (display #\tab) (display #\tab)
	      (cond
	       ((symbol? (cadr item))	(display (cadr item)))
	       ((not (cadr item))	(display "ignored"))
	       (else			(display "!!! special")))
	      (display #\space)
	      (if (memq :shortest item)
		  (display :shortest))
	      (newline))
	    spec))


(define-public (pp-grammar spec)
  (let loop ((current-sym #f)
	     (pos spec))
    (and pos
	 (let ((prod (car pos)))
	   (if (eq? current-sym (production-symbol prod))
	       (begin
		 (display (make-string (string-length current-sym) #\space))
		 (display "  | "))
	       (begin
		 (newline)
		 (display (production-symbol prod))
		 (display " -> ")))
	   (for-each (lambda (elt) (display elt) (display " "))
		     (production-expansion prod))
	   (newline)
	   (loop (production-symbol prod) (cdr pos))))))
