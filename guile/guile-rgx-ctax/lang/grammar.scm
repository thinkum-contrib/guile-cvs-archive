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




(define-module #/lang/grammar
  :use-module #/ice-9/common-list
  :use-module #/ice-9/poe)

;;; {The Structure of Grammars}
;;;
;;; A grammar is a list of productions.   Each production
;;; is a non-terminal symbol, an expansion, and the body of a reduction
;;; procedure.
;;;
;;; ((<non-terminal> (<symbol> ...) . redux)
;;;  (<non-terminal> (<symbol> ...) . redux)
;;;  ...)
;;;
;;; All symbols, terminal and non-terminal, are represented as 
;;; Scheme symbols.
;;;
;;; In a book you might see:
;;;
;;;	A --> B x Y
;;;
;;; or in Yacc syntax:
;;;
;;;	A: B x Y	{ redux }
;;;
;;; the functions here use:
;;;
;;;	(A (B x Y) . redux)
;;;
;;; Informally, the meaning of a production:
;;;
;;;	(A (B C D E) ...)
;;;
;;; is: "If you want to parse an <A>, then parse, in order, 
;;; a <B>, <C>, <D>, and <E>".   The variable <A> is a symbol
;;; called a non-terminal.   It denotes an abstract syntactic
;;; category like "expression" or "statement".   The variables
;;; <B> and so on are also symbols.   Some of them may also be
;;; non-terminals, others may be terminals.   Terminals denote
;;; abstract syntactic categories returned by the lexical analyzer
;;; such as "identifier" or "numeric constant".  (See ../ctax/grammar.scm
;;; for an example grammar.)
;;;
;;; Productions all have an associated reduction procedure.
;;;
;;; A redux is a list of Scheme expressions, free in the variables
;;; $1, $2, ... for up to the number of elements in the expansion.
;;;
;;; During a parse of a A according to the production
;;;
;;;	(A (B C D) . redux)
;;;
;;; The parser will first parse a B, then a C, then a D.   Each 
;;; subexpression parsed returnes a value, as will the parse of A.
;;; To compute the value of parsing A, the values of B, C, and D
;;; are bound to $1, $2, and $3, and the redux for the A production
;;; is evaluated.
;;;
;;; Because of the way grammars are used by the parser, productions
;;; must not share structure (i.e., they must not contain eq? cons pairs). 
;;; If you construct a grammar, but can't be certain that none of the
;;; cons pairs in it are shared, build a usable grammar by calling copy-tree.
;;;


;;; {Productions}
;;;
;;; A production is a list of the form:
;;;
;;;	(<non-terminal-symbol>  (<expansion-symbol> <expansion-symbol>  ...) . <redux-body>)
;;;

;; production-symbol p
;; Return the symbol expanded by a production:
;;
(define-public production-symbol car)

;; production-expansion p
;; Return the list of symbols in the expansion of a 
;; production:
;;
(define-public production-expansion cadr)

;; production-body p
;; Return the body of a productions reduction procedure.
;;
(define-public production-body cddr)



;;; {Special Symbols}
;;;

;; grammar-start-symbol g
;; Return the starting symbol of g.
;;
;; By convention, the first production determines the start symbol
;; but in fact, any symbol could be used.   A grammar can have
;; mulitple entry points, for example.
;;
(define-public (grammar-start-symbol g) (production-symbol (car g)))


;; grammar-non-terminals g
;; Return the non-terminal symbols of g.
;;
;; The list is computed by looking at the lhs of each production.
;;
(define-public (grammar-non-terminals g)
  (or (hashq-ref (grammar-cache g) 'grammar-non-terminals)
      (hashq-set! (grammar-cache g) 'grammar-non-terminals
		  (uniq (map production-symbol g)))))

;; grammar-terminals g
;; Return the list of terminals of g.
;;
;; The list is computed by looking at all the symbols
;; used in expansions, and subtracting out the non-terminals.
;;
(define-public grammar-terminals
  (lambda (g)
    (or (hashq-ref (grammar-cache g) 'grammar-terminals)
	(hashq-set! (grammar-cache g) 'grammar-terminals
		    (set-difference
		     (reduce union
			     (map (lambda (prod) (uniq (production-expansion prod))) g))
		     (grammar-non-terminals g))))))

;; grammar-nullables g
;; Return those non-terminals of g that can derive the empty string.
;;
(define-public grammar-nullables
  (lambda (g)
    (or (hashq-ref (grammar-cache g) 'grammar-nullables)
	(hashq-set! (grammar-cache g) 'grammar-nullables
		    (let loop ((guess '())
			       (candidates g))
		      (let* ((this-round (pick (lambda (prod)
						 (every (lambda (s) (memq s guess)) (production-expansion prod)))
					       candidates))
			     (this-round-symbols (map production-symbol this-round))
			     (remaining-productions (set-difference candidates this-round)))
			(if (null? this-round-symbols)
			    guess
			    (loop (union guess this-round-symbols) remaining-productions))))))))


;;; {Mapping From Non-terminal Symbols To Productions}
;;;

;; symbol-productions g nt
;; Return the productions in G with lhs eq? to NT.
;;
(define-public symbol-productions
  (lambda (g nt)
     (remove-if (lambda (p) (not (eq? (production-symbol p) nt))) g)))





;;; {Memory Management}
;;;
;;; Throughout this and the other files in #/lang, it is convenient 
;;; to cache structures derived from a grammar while tying the lifetime
;;; of the cached structures to that of the grammar.
;;;
;;; The strategy for this is to weakly associate a hash table with 
;;; every grammar, and to use that hash table for caching.
;;;

(define-public (grammar-cache g)
  (or (object-property g 'grammar-cache)
      (set-object-property! g 'grammar-cache (make-weak-hash-table 509))))

