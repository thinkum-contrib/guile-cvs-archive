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



(define-module #/ctax/reader
  :use-module #/ctax/lexer
  :use-module #/ctax/grammar
  :use-module #/lang/lr1
  :use-module #/ice-9/lineio)



(define (ctax-read port)
  (let* ((line-port (if (lineio-port? port)
			port
			(make-line-buffering-input-port port)))
	 (lex (lambda () (ctax-lexer line-port))))

    (catch 'parser-return

	   (lambda ()
	     (parser ctax-grammar lex
		     (letrec ((err (lambda (cont states values token lexer)
				     (cond

				      ;; Quietly discard soft EOI tokens.
				      ;;
				      ((and (eq? '<eoi> (car token)) (not (cadr token)))
				       (cont states values (lex) lexer err))

				      (else
				       (error 'unexpected token))))))
		       err)
		     (current-module)))

	   (lambda (tag ret) ret))))
