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




(define-module #/lang/lr1
  :use-module #/lang/lr0
  :use-module #/lang/grammar
  :use-module #/ice-9/hcons
  :use-module #/ice-9/slib)

(require 'pretty-print)


;;; {LR(1) Parsing}
;;;
;;;
;;; An LR(1) parser is an abstract machine with these parts:
;;;
;;;	lr0-dfa
;;;	state-stack
;;;	value-stack
;;;	lexer
;;;	error
;;;
;;;	The function "error" is called if the stream returned by
;;;	the lexer is unparsable.
;;;
;;; 	The function "lexer" is an input stream that returns a new
;;;	input token each time it is called.   Every input token belongs to a category
;;;	of input tokens and the category is named by a "terminal symbol".
;;;	For example, "define-module" might be the token, and "identifier" the
;;;	corresponding terminal symbol.
;;;
;;;	The value stack is a stack of arbitrary values, constructed as the
;;;	parse proceeds.  For example, the value stack can be used to build 
;;; 	up parse trees of input.
;;;
;;;	The state-stack is a stack of lr0-dfa states.
;;;
;;;	The lr0-dfa is a deterministic finite automata with these parts:
;;;
;;;		states 			-- a set of states
;;;		next(state, symbol)	-- a transition function
;;;		reductions(state)	-- see below
;;;
;;;	If you aren't familiar with lr(1) parsing and are reading these
;;;	comments to become familiar with it, don't worry yet about the details 
;;;	of the lr0-dfa.  Just get an idea of what the basic moves are for
;;;	the parser machine.   Then later, when the lr0-dfa is explained in
;;;	detail, you can understand it by thinking of how the parser machine
;;;	would behave for such a dfa.
;;;
;;;	Parsing begins with a singleton state stack containing an arbitrarily
;;;	chosen "start state" from lr0-dfa.   The value stack also starts off
;;;	with an arbitrarily chosen value.
;;;
;;;	The parser reads one token, called the look-ahead, and evaluates:
;;;
;;;		shift-state = next (top (state-stack), 
;;;				    terminal-symbol (look-ahead))
;;;
;;;	Evaluation may lead to finding "shift-state", or it may lead to
;;;	an error if no appropriate transition is defined.
;;;
;;;	If a transition is defined, then the parser pushes shift-state
;;;	onto the state-stack, look-ahead onto the value-stack and repeats
;;;	the cycle, reading a new look-ahead token.
;;;
;;;	After building up the stacks in this way, the parser may reach a point
;;;	at which shift-state is not defined for the current look-ahead.   In this
;;; 	case, the parser consults:
;;;
;;;		reductions (top (state-stack))
;;;
;;;	"reductions" returns a set of reduction procedures.   A reduction procedure
;;;	is a mapping from parser states to parser states.  That is, a reduction
;;;	procedure takes a state-stack and a value-stack as input,
;;;	and returns a new state-stack, new value-stack as output.
;;;
;;;	Reduction procedures always work by popping 0 or more values from both
;;;	the state-stack and value-stack (popping an equal number from each) and then
;;;	pushing exactly one new value and one new state.   If, after popping values,
;;;	the top of the state stack is S, then a reduction procedure will push
;;;	S' which is some next(S,X) for some symbol X.   For a given reduction procedure,
;;;	X is always the same, but S' may vary depending on S.  Conventionally,
;;;	X is some symbol which is not a member of the set of terminal symbols.
;;;
;;;	After S' has been pushed, it may or may not be possible to shift the look-ahead.
;;;	If it is not possible to shift the look-ahead, it may or may not be possible
;;;	to perform further reductions after which it would be possible to shift the look-ahead.
;;;
;;;	The parser defined here never performs _any_ reduction without first computing
;;;	a complete sequence of reductions that will lead to the look-ahead being shiftable.
;;;	If no such sequence can be found, an error is reported.   If multiple reductions are
;;;	possible at some point, but only one of them leads to the look-ahead being shifted,
;;;	that one will be chosen.
;;;
;;;	After performing all the necessary reductions to permit shifting of the look-ahead,
;;;	the parser shifts the look-ahead, reads a new look-ahead token, and restarts its cycle.
;;;
;;;
;;;	


;; The value stack holds tokens as they are shifted,
;; and the return values of reduction functions as they
;; are used.
;;
(define-public parser-value-cons cons)
(define-public parser-value-car car)
(define-public parser-value-cdr cdr)
(define-public parser-value-cdr-ref list-tail)

;; The state stack holds lr(0) states as they are shifted.
;;
(define-public parser-context-cons (lambda (g is c) (cons is c)))
(define-public parser-context-car car)
(define-public parser-context-cdr cdr)
(define-public parser-context-cdr-ref list-tail)



(define-public (make-parser-state g is)
  (let* ((cache (grammar-cache g))
	 (key (hashq-cons cache is 'parser-state)))
    (or (hashq-ref cache key)
	(hashq-set! cache key (cons is '())))))

(define-public parser-state-item-set car)

(define parser-state-action-cache cdr)

(define (parser-state-cache-action! state token-type val)
  (set-cdr! state (acons token-type val (cdr state))) val)


(define-public parser-start-state
  (lambda (g)
    (let ((cache (grammar-cache g)))
      (or (hashq-ref cache 'parser-start-state)
	  (hashq-set! cache 'parser-start-state (make-parser-state g (item-set-closure g (item-set-start-kernel g))))))))

(define-public parser-action
  (lambda (g state token-type module)
    (cond
     ((assq token-type (parser-state-action-cache state)) => cdr)
     (else (parser-state-cache-action!
	    state token-type
	    (let* ((shift (parser-state-after-shift g state token-type))
		   (reduce (and (not shift) (item-set-reductions g (parser-state-item-set state)))))
	      (cond
	       (shift		`(shift ,shift))
	       (reduce		`(reduce ,(map (lambda (r) (production-function g r module)) reduce)))
	       (else		`(error)))))))))

(define parser-state-after-shift
  (lambda (g state token-type)
    (let ((next-item-set-kernel (item-set-successor-kernel g (parser-state-item-set state) token-type)))
      (and next-item-set-kernel (make-parser-state g (item-set-closure g next-item-set-kernel))))))



(define-public (parser g lexer error module)
  
  (let (;; The backtrack always contains at least one entry: the
	;; caller's error procedure.
	;;
	(base-backtrack-stack
	 (list (letrec ((bottom-backtrack
			 (lambda (loop states values backs token lexer err)
			   (err (lambda (states values token lexer error)
				  (loop states values (list bottom-backtrack)
					token lexer error))
				states
				values
				token
				lexer))))
		 bottom-backtrack))))
    
    (let parser-loop ((state-stack (parser-context-cons g (parser-start-state g) '()))
		      (value-stack (parser-value-cons (cons (grammar-start-symbol g) '()) '()))
		      (backtrack-stack base-backtrack-stack)
		      (token (lexer))
		      (lexer lexer)
		      (error error))

      (let ((action (parser-action g (parser-context-car state-stack) (car token) module)))
	(case (car action)
	  ((shift)	(begin
			  ; (pk 'shift token)
			  (if (promise? value-stack)
			      (set! value-stack (force value-stack)))
			  (parser-loop (parser-context-cons g (cadr action) state-stack)
				       (parser-value-cons (cdr token) value-stack)
				       base-backtrack-stack
				       (lexer)
				       lexer
				       error)))

	  ((reduce)	((caadr action) (cdadr action)
					parser-loop
					state-stack
					(if (promise? value-stack)
					    value-stack
					    (delay value-stack))
					backtrack-stack
					token
					lexer
					error))
	  
	  ((error)	((car backtrack-stack) parser-loop state-stack value-stack backtrack-stack token lexer error)))))))



(define-public production-function
  (lambda (g production module)
    (let* ((cache (grammar-cache g))
	   (key (hashq-cons cache production 'production-function)))
      (or (hashq-ref cache key)
	  (hashq-set! cache key
		      (let ((symbol (production-symbol production))
			    (body (production-body production))
			    (arity (length (production-expansion production))))
			(let* ((dollar-vars (let recursive ((n arity))
					      (if (= n 0)
						  '()
						  (cons (symbol-append '$ (number->string n))
							(recursive (+ -1 n))))))
			       (value-stack-mutator
				(eval2 `(lambda (value-stack)
					  (delay
					    (let* ((value-stack (force value-stack))
						   (stack-tail (list-tail value-stack ,arity))
						   (bindings (list-head value-stack ,arity)))
					      (parser-value-cons
					       (apply (lambda (value-stack ,@dollar-vars)
							,@body)
						      stack-tail
						      bindings)
					       stack-tail))))
				       (module-eval-thunk module))))
			  (lambda (remaining-reductions
				   parser-loop
				   state-stack
				   value-stack
				   backtrack-stack
				   token
				   lexer
				   error)

			    ; (pk 'reduce production)

			    ;; A reduction function does three things before
			    ;; continuing the parser loop...
			    ;;
			    (parser-loop

			     
			     ;; ... it shifts the non-terminal
			     ;; generated by the reduction. 
			     ;;
			     (let ((popped (parser-context-cdr-ref state-stack arity)
					   ;;
					   ;; This shift take place on the stack underneath 
					   ;; the states being removed by the reduction.
					   ))

			       (parser-context-cons g
						    
						    ;; The action associated with a just-reduced
						    ;; non-terminal symbol is always a `(shift ,new-state)
						    ;;
						    (cadr (parser-action g (parser-context-car popped) symbol module))
						    
						    popped))
			     

			     ;; ... it (lazilly) performs the body of
			     ;; the associated grammar production to
			     ;; compute a new value stack.
			     ;;
			     (value-stack-mutator value-stack)


			     ;; ... it remembers how to back-off this
			     ;; reduction in case it is discovered
			     ;; that the next token can not be
			     ;; shifted otherwise.
			     ;;
			     (cons (lambda (parser-loop
					    bogus-state-stack
					    bogus-value-stack
					    our-backtrack-stack
					    token
					    lexer
					    error)
				     ; (pk 'backtrack production)
				     (if remaining-reductions
					 ;; If there are any alternative reductions to consider, try
					 ;; those first.
					 ;;
					 ((car remaining-reductions) (cdr remaining-reductions)
								     parser-loop
								     state-stack
								     value-stack
								     backtrack-stack
								     token
								     lexer
								     error)

					 ;; When there are no more reductions to consider, try
					 ;; further backtracking.
					 ;;
					 ((car backtrack-stack) parser-loop
								state-stack
								value-stack
								backtrack-stack
								token
								lexer
								error)))
				   backtrack-stack)
			     token
			     lexer
			     error)))))))))

