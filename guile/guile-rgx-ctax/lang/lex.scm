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




(define-module #/lang/lex
  :use-module #/ice-9/lineio)


;;; {read-lexeme}
;;;
;;; read-lexeme dfa ?port?
;;;
;;; Read a lexeme, as defined by DFA, from PORT or the current input.
;;;
;;; DFA should match an entire lexeme
;;; and nothing more.   read-lexeme will return:
;;;
;;;		(<n> <token>)
;;;
;;; where <token> is a string containing the characters matched, and <n>
;;; indicates the token type.
;;;
;;; A novel regexp operator is particularly important when using this
;;; procedure: the cut operator.
;;;
;;; The cut operator, written [[:cut <n>:]] causes immediate termination
;;; of a match.   <n> must be an integer.   If <n> is 0, the match fails.
;;; Otherwise, the match succeeds, returning <n> in the match data.
;;; Using the Scheme procedure regexec, the match-data selector #\c retrieves
;;; the cut value.   For example:
;;;
;;;	(define r (regcomp "if[[:cut 1:]]\\|else[[:cut 2:]]"))
;;;	(regexec r "if" '(#\c #\0)) => (1 "if")
;;;	(regexec r "else" '(#\c #\0)) => (2 "else")
;;;
;;; The above examples illustrate how the return value of read-lexeme is 
;;; formed.  Sufficient characters are read to form a match while leaving
;;; one character left over.   The extra character is put back on the 
;;; input stream and match-data for '(#\c #\0) is returned.
;;;
;;; If the end of file is reached before a match is found, then:
;;;
;;;	(<eof> "...")
;;;
;;; is returned where <eof> is an eof-object and the string contains all
;;; characters read.
;;; 
;;; There are two exceptions to the rule that the lexer will read, then
;;; unread an "extra" character for each lexeme.   The first exception
;;; is if EOF is reached after finding a match, but before reading an
;;; an extra character.   In that case, the match is simply returned.
;;; The second exception is if the cut value of the match is less than 
;;; 0, the lexeme is immediately returned without reading additional
;;; characters.
;;;
;;; Using this interface is quite similar to using the lex language,
;;; except for the handling of illegal (non-lexable) input.  The unix
;;; lexical analyzers are able to report an error as soon as
;;; characters are read which could not possibly begin a lexeme.  This
;;; lexer, on the other hand, naively continues to read characters
;;; until eof is reached.  This reflects a deficiency of the regexec
;;; interface (it has no way to report that not only was no match
;;; found, but no match could be formed by adding more characters).
;;; The work-around is to carefully write "regexp" so that it matches
;;; illegal input and returns such as a distinct token type.  A more
;;; robust (lex-like) solution will eventually be implemented.  
;;; 


(define-public ((lexeme-reader dfa) . opt-port)
  (let  ((port (if (null? opt-port) (current-input-port) (car opt-port)))
	 (buffer "")
	 (best-tag -1)
	 (best-pos #f)
	 (dfa-pos 0))

    (reset-dfa! dfa)
    (letrec (
	     ;; expand-buffer
	     ;;
	     ;; Try to add characters to the buffer, returning
	     ;; #f if it could not be done due to EOF.
	     ;;
	     (expand-buffer (lambda ()
			      (let ((s (read-string port)))
				(and (not (eof-object? s))
				     (set! buffer (string-append buffer s))))))

	     ;; read-token
	     ;;
	     ;; The main loop of read-lexeme.
	     ;;
	     (read-token (lambda ()
			   (let* ((matched (advance-dfa! dfa (make-shared-substring buffer dfa-pos)))
				  (final (dfa-final-tag dfa))
				  (continuable? (and (> matched 0) (>= final 0) (dfa-continuable? dfa))))

			     (if (not (= 0 final))
				 (begin
				   (set! best-tag final)
				   (set! best-pos (+ dfa-pos matched))))

			     (cond
			      ;; If a longer lexeme is possible, go for it.
			      ;;
			      (continuable?	(set! dfa-pos (+ dfa-pos matched))
						(if (and (= dfa-pos (string-length buffer))
							 (not (expand-buffer)))
						    (if best-pos
							(list best-tag buffer)
							(begin
							  (unread-string (make-shared-substring buffer 1) port)
							  (list -1 (make-shared-substring buffer 0 1))))
						    (read-token)))

			      ;; If a lexeme was ever found, return it,
			      ;;
			      (best-pos		(unread-string (make-shared-substring buffer best-pos) port)
						(list best-tag (make-shared-substring buffer 0 best-pos)))

			      ;; Otherwise, generate an error for the first character:
			      ;;
			      (else		(unread-string (make-shared-substring buffer 1) port)
						(list 0 (make-shared-substring buffer 0 1))))))))

      (if (not (expand-buffer))
	  (list the-eof-object "")
	  (read-token)))))



;;; {Lex}
;;;
;;; make-lexer specification
;;;
;;; Return a lexical analyzer built from SPECIFICATION.
;;;
;;; A lexer is built from a specification that consists of regexps
;;; and actions.   The regexps are listed in order of precedence,
;;; each matching a particular token type.
;;;
;;; The actions are either a token-id (a symbol or #f), or a procedure.
;;;
;;; If an action is a token-id, <i>, then when a matching token is read,
;;; the list (<i> <lexeme>) is returned (where <lexeme> is a string
;;; consisting of the matched characters).
;;;
;;; If an action is a procedure, the procedure is called with one argument --
;;; the lexeme string.   The return value of the procedure is returned from
;;; the lexer.
;;;
;;; Here is a sample specification:
;;;
;;; (define ctax-lexer-specification
;;;   `(("[0-9]\\+\\.\\?[0-9]*" 	,(lambda (token)
;;;					   (list 'number (number->string token))))
;;;     ("if" 				if)
;;;     ("else"				else)
;;;     ("while" 			while)
;;;     ("for" 				for)
;;;     ("return" 			return)
;;;     ("do" 				do)
;;;     ("scm" 				scm)
;;;     ("break"			break)
;;;     ("continue"			continue)
;;;     ("[a-zA-Z][a-z?!<>=0-9_]*"	,(lambda (token)
;;;					   (list 'identifier (string->symbol token))))
;;;     ("//[^\n]*"			#f)
;;;     ("[ \t\n]\\+" 			#f)
;;;     ("[^ \t\n]\\+" 			error)))
;;;
;;; WARNING: all lexers should be total -- that is, they must succeed at dividing
;;; all possible input streams into tokens.   No provision is made to cleanly handle
;;; non-matching input.  That's why the example lexer includes a token type "error" --
;;; to consume non-lexable input until syncronizing characters (whitespace in this case)
;;; are found.
;;;
;;; Generally speaking, given a specification, the lexer will return the longest 
;;; matching lexeme. If two cases both match, the lexer will use the one that 
;;; occurs first in the specification.
;;;
;;; The rule that longest matches may be overrided for a particular type of lexeme by
;;; putting the keyword :shortest after the action in the lexer specification.  If such a
;;; lexeme type is ever matched, it is returned immediately without consuming addtional
;;; characters to look for a longer match.
;;;
;;;



;; lexer-regexp-and-handler spec return
;;
;; Return a regexp and a procedure suitable for lexing according
;; to the analyzer specification SPEC.
;;
;; RETURN is called:
;;
;;	(RETURN regexp handler)
;;
;; See make-lexer for an example of how this is used.
;;
;;

(define-public (lexer-regexp-and-handler spec return)
  (let* ((default (or (assq 'else spec) '(else <default>)))
	 (sans-default (delq default spec))
	 (default-at-end (append sans-default (list default)))
	 (actions (apply vector (map cadr (reverse default-at-end))))
	 (handler (lambda (retry port token-number lexeme)
		    (if (eof-object? token-number)
			(list '<eoi> #t lexeme)
			(let ((action (vector-ref actions (abs token-number))))
			  (cond
			   ((procedure? action) 	(action lexeme port))
			   (action 			(list action lexeme))
			   (else 			(retry port)))))))
	 (regexp (let recurse ((rs (reverse sans-default))
			       (n 1))
		   (if (null? rs)
		       "[[:cut 0:]]"
		       (string-append "\\("
				      (caar rs)
				      "\\)"
				      "[[:cut " (number->string
						 (if (memq :shortest (car rs))
						     (- n)
						     n))
				      ":]]\\|"
				      (recurse (cdr rs) (+ n 1)))))))
    (return regexp handler)))



;; make-lexer specification
;;
;; See the comment at the top of the page.
;;
(define-public (make-lexer specification)
  (lexer-regexp-and-handler

   specification

   (lambda (regexp handler)
     (let* ((dfa (regexp->dfa regexp))
	    (read-lexeme (lexeme-reader dfa)))

       (define (scan-token port)
	 (apply handler scan-token port (read-lexeme port)))

       (define (scan-token-interface . opt-port)
	 (let ((port (if (null? opt-port) (current-input-port)
			 (car opt-port))))
	   (if (not (lineio-port? port))
	       (throw 'error
		      "Currently, only lineio ports are supported by lex."
		      port)
	       (scan-token port))))

       scan-token-interface))))

(define-public (eof-token? obj)
  (and (pair? obj) (eq? '<eoi> (car obj))))


