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


(define-module #/ctax/lexer
  :use-module #/lang/lex)



;;; {Ctax Lexer}
;;;
;;; See the file grammar.scm for a description of the Ctax lexical langauge.
;;;


(begin
  (define scheme-id "[a-z?!<>=0-9_-][a-z:?!<>=0-9_-]*")

  (define-public ctax-lexer-spec
    `(
      ;; String Constants
      ;;
      ("\\(\"\\([^\"\\\\]\\|\\\\.\\)*\"\\)"

       ,(lambda (token port)
	  (catch #t
		 (lambda ()
		   (with-input-from-string token
		     (lambda ()
		       (list '<constant>
			     (read (current-input-port) #f #f)))))
		 (lambda ign
		   (error '<bad-string-constant> token)))))


      ;; Character constants and quoted constants
      ;;

      ;; ... octal character constants
      ;;
      ("'\\\\0[0-7]?[0-7]?[0-7]?'" ,(lambda (token port)
				      (list '<constant>
					    (string->number (make-shared-substring token 0 (+ -1 (string-length token)))
							    8)))
				   :shortest)

      ;; ... standard escape codes
      ;;
      ("'\\\\n'"			,(lambda ign '(<constant> #\nl))
					:shortest)
      ("'\\\\t'"			,(lambda ign '(<constant> #\tab))
					:shortest)
      ("'\\\\r'"			,(lambda ign '(<constant> #\cr))
					:shortest)
      ("'\\\\.'"			,(lambda (token port) `(<constant> (string-ref token 1)))
					:shortest)

      ;; ... normal character constants
      ;;
      ("'.'"				,(lambda (token port) (list '<constant> (string-ref token 0)))
					:shortest)

      ;; ... quoted symbols
      ;;
      (,(string-append "'" scheme-id)	,(lambda (token port)
					   `(<constant> ,(string->symbol (make-shared-substring token 1)))))

      ;; ... and quoted pairs/lists
      ;;
      ("'("				,(lambda (token port)
					   (catch #t
						  (lambda ()
						    (if (eq? #\' (peek-char))
							(<constant> #\')
							`(<constant>
							  ,(let ((handle (cons '() '())))
							     (let loop ((pos handle))
							       (skip-whitespace)
							       (cond

								((eq? #\) (peek-char))
								 (begin
								   (read-char port)
								   (cdr handle)))

								((eq? #\. (peek-char))
								 (begin
								   (read-char port)
								   (set-cdr! pos (read port))
								   (skip-whitespace)
								   (let ((c (read-char port)))
								     (if (eq? #\) c)
									 (cdr handle)
									 (error 'missing-paren-after-dot c)))))
								(else
								 (begin
								   (set-cdr! pos (cons (read port) '()))
								   (loop (cdr pos))))))))))
						  (lambda ign
						    (error "bad list constant" ign)))))

      ;; Quasi-Quoted Constants
      ;;
      ("`"			,(lambda ign
				   (catch #t
					  (lambda () (list '<quasi-constant> (read (current-input-port) #f read-sharp)))
					  (lambda args (apply error 'bad-scheme-quasi-constant args)))))

      ;; Unquoted Scheme expressions.  
      ;; The ctax equivalent of asm()?
      ;;
      ("#"			,(lambda ign
				   (catch #t
					  (lambda () (list '<literal> (read (current-input-port) #f read-sharp)))
					  (lambda args (apply error 'bad-scheme-literal args)))))


      ;; Block/Statement Structuring
      ("{"			<lbrace>)
      ("}"			<rbrace>)
      (";"			<semi>)

      ;; Defining Functions
      ("\\.\\.\\."		<...>)
      ("lambda"			<lambda>)
      ("scm"			<scm>)
      ("public"			<public>)
      ("static"			<static>)
      ("auto"			<auto>)

      ;; Flow Control Keywords
      ("if"			<if>)
      ("else"			<else>)
      ("for"			<for>)
      ("while"			<while>)
      ("return"			<return>)
      ("do"			<do>)
      ("break"			<break>)
      ("continue"		<continue>)


      ;; Operators
      (","			<comma>)

      ("="			<=>)
      ("\\*="			<assignment>)
      ("/="			<assignment>)
      ("%="			<assignment>)
      ("+="			<assignment>)
      ("-="			<assignment>)
      (">>="			<assignment>)
      ("<<="			<assignment>)
      ("&="			<assignment>)
      ("\\^="			<assignment>)
      ("|="			<assignment>)


      ("\\?"			<?>)
      (":"			<colon>)

      ("||"			<flowor>)
      ("&&"			<flowand>)

      ("|"			<logor>)
      ("&"			<logand>)
      ("\\^"			<logxor>) 

      ("=="			<==>)
      ("!="			<!=>)
      (">="			<>=>)
      ("<="			<<=>)
      ("<"			<<>)
      (">"			<>>)

      ("<<"			<<<>)
      (">>"			<>>>)

      ("+"			<+>)
      ("-"			<->)
      ("\\*"			<*>)
      ("/"			</>)
      ("%"			<%>)

      ("--"			<-->)
      ("++"			<++>)

      ("~"			<lognot>)
      ("!"			<!>)
      ("\\."			<dot>)
      ("->"			<arrow*>)
      ("\\.\\*"			<dot*>)
      ("->\\*"			<arrow>)
      ("\\["			<lbracket>)
      ("\\]"			<rbracket>)
      ("("			<lparen>)
      (")"			<rparen>)

      ;; Non-significant Tokens
      ("//[^\n]*"		#f)
      ("[ \t]\\+" 		#f)
      ("\n"		        ,(lambda ign '(<eoi> #f "\n")) :shortest) 

      ;; Numbers
      ("[0-9]\\+\\.\\?[0-9]*" 	,(lambda (token port) (list '<number> (string->number token))))

      ;; Keywords
      (":[a-z?:!<>=0-9_-]\\+"			,(lambda (token port)
						   (list '<keyword>
							 (symbol->keyword
							  (string->symbol (make-shared-substring token 1))))))

      ;; Identifiers
      ("[a-zA-Z_][a-zA-Z0-9_]*" 		,(lambda (token port)
						   (list '<identifier> (string->symbol token))))

      ;; Extended Identifier Syntax
      (,(string-append "\\\\" scheme-id)	,(lambda (token port)
						   (list '<identifier> (string->symbol (make-shared-substring token 1)))))

      ("[^- :<>0-9;\t\n,+*/(){}=#\\\\\"`'a-zA-Z]\\+"	,error)))


  (define-public ctax-lexer (make-lexer ctax-lexer-spec)))


(define (skip-whitespace)
  (let loop ((c (peek-char)))
    (if (char-whitespace? c)
	(begin
	  (read-char)
	  (loop (peek-char))))))
	


