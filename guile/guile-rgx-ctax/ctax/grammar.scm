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



(define-module #/ctax/grammar)


;;; {Ctax Grammar}
;;; 
;;; Informally, ctax is a block-structured, algebraic syntax for Scheme.
;;; 

;;; {Identifiers}
;;;
;;; The set of ctax identifiers is the same as the set of C identifiers.
;;; 
;;;         x
;;;         y
;;;	    foo_bar
;;;
;;; In addition, the full range of Scheme identifiers is available in Ctax
;;; though identifiers which are not also C identifiers must be quoted with
;;; an initial backslash:
;;;
;;;         \list-ref(x);
;;;	    x = \list-ref;
;;;
;;; (Be careful to use enough whitespace when using Scheme-style identifiers 
;;;  in expressions.   For example:
;;;
;;;		\some-number+1   // Names the variable "some-number+1"
;;;
;;; while
;;;
;;;		\some-number + 1  // An addition expression.
;;;
;;; Using C-style identifiers, whitespace is not so significant:
;;;
;;;		x+1		// An addition expression
;;;
;;; 
;;; Just as in Scheme, identifiers are always variable names from the lexical
;;; environment.
;;; 

;;; {Constants}
;;;
;;; Ctax supports numeric and string constants, written with Guile 
;;; Scheme-style syntax:
;;;
;;;	10
;;;	1000000000000000000
;;;	"hello world\n"
;;;
;;; character constants, using C-style syntax:
;;;
;;;	'a'
;;;	'\n'
;;;	'\001'
;;;
;;; quoted symbols:
;;;
;;;	'a
;;;	'list-ref
;;;
;;; and quoted Scheme lists:
;;;
;;;	'(a b (c d) (long-identifier foo) ...)
;;;
;;; There is a slight lexical ambiguity because a character constant like:
;;;
;;;	'a'
;;;
;;; could be read as a quoted symbol, "a", followed by a single quote.
;;; The disambiguating rule is that if it looks like a character constant, it is.


;;; {Expressions}
;;; 
;;; Ctax has all of the operators of C, except "->" and "." (which may be
;;; added later if we can figure out what they should mean).
;;; 
;;; Ctax expressions resemble C expressions.  There are function calls:
;;; 
;;;         cos (x)
;;; 
;;; algebraic expresions:
;;; 
;;;         cos (x) * 2
;;;         a ? b : c
;;; 
;;; assignment expressions:
;;; 
;;;         a = 3
;;;         a++
;;;         ++a
;;;         a += 10
;;; 
;;; and comma expressions:
;;; 
;;;         a += 10, b += 4
;;; 
;;; Ctax supports lambda expressions which return anonymous procedures,
;;; closed over the lexical context:
;;; 
;;;         lambda (a, b) { return a * b; }
;;; 
;;; In the middle of Ctax, you can embed an expression written in s-exp Scheme
;;; by using the # operator:
;;; 
;;;         x * #(some-scheme-expression ...)
;;; 
;;; It will also be possible to embed an expression written in Ctax in the
;;; middle of a Scheme s-exp, though I haven't finished that part yet.
;;; 
;;; In Scheme, keywords are passed just like normal arguments, but by
;;; convention, we think of them as sometimes being associated with
;;; the parameter that follows.   For example, (in Scheme):
;;; 
;;;         (make-frob 'acme-style :color 'red :noise 'feep)
;;; 
;;; The keyword :color is modified by the parameter 'red and the keyword
;;; :noise is modified by 'feep.  In Ctax, keyword arguments like those
;;; need not be followed by a comma in an argument list:
;;; 
;;;         make-frob ('acme-style, :color 'red, :noise 'feep)
;;; 

;;; {Statements}
;;;
;;; Statements resemble C, too.  There are "for", "do" and "while" loops
;;; supporting "break" and "continue".  There are conditionals using "if"
;;; and "if ... else".  There are compound statements in which local
;;; variables can be defined.  There is a "return" statement.  I have not
;;; yet, but plan to support labeled loops and "break" and "continue" that
;;; take label arguments.  I also plan to add "letrec", "let*", "let" and
;;; "labeled let" blocks.
;;; 
;;;         public
;;;         foo(r)
;;;         {
;;;           auto x;
;;;           x = #(a b r c d);
;;;           return some-fn (:bar, 1, :baz, :foo r, '(dflj sdljf (sldkjf sdflkj)));
;;;         }
;;; 
;;; Var-args functions can be defined using this syntax:
;;; 
;;;         public foo(first, ... rest) { ... }
;;; 
;;; which is the same as (define foo (lambda (first . rest) ...)), or
;;; 
;;;         public foo (... vargs) {...}
;;; 
;;; which is the same as (define foo (lambda rest ...))
;;; 
;;; 

;;; {Declarations}
;;;
;;; When declaring functions or variables, types need not be specified, though
;;; eventually support will be added so that they may optionally be specified.
;;; 
;;; Top level definitions must be declared either "static" or "public".  "static"
;;; definitions use the Scheme primitive "define", "public" use "define-public".
;;;
;;; Local variables are declared using the keyword "auto" as in:
;;;
;;;	lambda (y) { auto x; x = y * y;  return cons (x, y); }
;;; 
;;; A top-level assignment statement (outside of any function definiton) acts like
;;; a static declaration.  Thus:
;;;
;;;	static x = 69;
;;;
;;; is equivalent to:
;;;
;;;	x = 69;
;;;

;;; {Commands}
;;;
;;; The ctax reader reads a complete "command" each time it is called.
;;; A command is either a function definition or variable declaration,
;;; or a statement which is evaluated at global scope.
;;;



(define-public ctax-grammar
  '(
    ;; $start$
    ;; The start symbol.
    ;;
    ;; $start$ is defined to parse a complete command followed by an arbitrarily long list
    ;; of eoi (end of input) tokens.   In practice, input is never reduced to 
    ;; the start symbol.   Instead, as soon as a $command$ is reduced, the parser
    ;; is exited (see the reduction rule for $command$).
    ;;
    ;; So why have $eoi-shifter$ at all?   At each newline, the lexer returns 
    ;; a "soft eoi" token.   If the parser can shift that eoi token, it will try
    ;; to do so.  If the soft eoi can't be shifted, it is discarded.   The only place
    ;; in the grammar where it is legal to shift an eoi token is after a complete 
    ;; command has been parsed.   Thus, the soft-eoi token returned by the lexer
    ;; is a way to probe the parser, asking "has a complete command been read".   If
    ;; a complete command _has_ been read, the reduction rule for $command$ is executed,
    ;; which causes the parser to exit immediately, returning a value.  When this happens,
    ;; and this is the important part: no look-ahead tokens beyond the soft-eoi are read.
    ;;
    ;; The net effect is that if you type something like:
    ;;
    ;; public add(a,b)
    ;; {
    ;;   return a + b;
    ;; }
    ;;
    ;; to a ctax interpreter, the parse completes after the last line without waiting
    ;; for more tokens.   Without the soft-eoi hack, the parser would need to read
    ;; an additional token of input before it could complete the parse of "add".
    ;;
    ($start$ ($command$ $eoi-shifter$) #f)
    ($eoi-shifter$ ($eoi-shifter$ <eoi>) #f)
    ($eoi-shifter$ () #f)

    ($command$ ($cmd$) (throw 'parser-return $1))

    ;; $cmd$
    ;; Top-level forms.
    ;;
    ;; These are the kinds of syntax permitted at the top level...
    ;;
    ;; ... function definitions
    ;;
    ($cmd$ (<static> <identifier> <lparen> $arg-list$ <rparen> $block$)
	   `(define ,(car $2) (ctax-statement (ctax-lambda ,$4 ,(cadr $6) ,(caddr $6)))))
    ($cmd$ (<public> <identifier> <lparen> $arg-list$ <rparen> $block$)
	   `(define-public ,(car $2) (ctax-statement (ctax-lambda ,$4 ,(cadr $6) ,(caddr $6)))))

    ;; ... variable declarations
    ;;
    ($cmd$ (<public> <identifier> <=> $exp$ <semi>)
	   `(define-public ,(car $2) (ctax-statement ,$4)))
    ($cmd$ (<static> <identifier> <=> $exp$ <semi>)
	   `(define ,(car $2) (ctax-statement ,$4)))
    ($cmd$ (<public> <identifier> <semi>)
	   `(define-public ,(car $2) #f))
    ($cmd$ (<static> <identifier> <semi>)
	   `(define ,(car $2) #f))

    ;; ... statements to be evaluated in the global scope
    ;;
    ($cmd$ ($statement$)
	   (cond
	    ;; As a convenience, we allow "static x = ...." to be 
	    ;; abbreviated "x = ...".   The parser correctly sees the former
	    ;; as a declaration, but incorrectly sees the latter as just
	    ;; an expression statement.   In this reduction rule, that
	    ;; "mistake" is patched up by recognizing certain assignment
	    ;; syntax trees and converting them into declaration trees.
	    ;;
	    ((and (pair? $1) (eq? 'ctax-assign (car $1)) (symbol? (cadr $1)))
	     `(define ,(cadr $1) (ctax-statement ,(caddr $1))))

	    (else
	     `(ctax-statement ,$1))))

    ;; $arg-list$
    ;; The syntax of formal parameter specifications.
    ;;
    ;; This covers both fixed and variable arity functions:
    ;;	lambda (a, b, c) 
    ;;  lambda (a, b, ... c)
    ;;  lambda (... c)
    ;;
    ($arg-list$ (<...> <identifier>)
		(car $2))
    ($arg-list$ ($id-list$ <comma> <...> <identifier>)
		(apply:nconc2last (append! $1 (list (car $4)))))
    ($arg-list$ ($id-list$)
		$1)
    ($arg-list$ ()
		'())
    ($id-list$ ($id-list$ <comma> <identifier>)
	       (append! $1 (list (car $3))))
    ($id-list$ (<identifier>)
	       (list (car $1)))


    ;; $block$
    ;; Curly-braces around local variable declarations and a list of statements.
    ;;
    ($block$ (<lbrace> $declarations$ $statement-list$ <rbrace>)
	     `(ctax-begin ,$2  ,$3))


    ($declarations$ ($declarations$ $declaration$)
		    (if (null? $1)
			`(,$2)
			(append! $1 (list $2))))
    ($declarations$ ()
		    '())

    ($declaration$ (<auto> <identifier> <semi>)
		   (list (car $2) #f))
    ($declaration$ (<auto> <identifier> <=> $exp$ <semi>)
		   `(,(car $2) ,$4))



    ($statement-list$ ($statement-list$ $statement$)
		      (if (null? $1)
			  `(,$2)
			  (append! $1 (list $2))))
    ($statement-list$ ()
		      '())


    ($statement$ (<if> <lparen> $exp$ <rparen> $statement$)
		 `(ctax-if ,$3 ,$5 #f))
    ($statement$ (<if> <lparen> $exp$ <rparen> $statement$ <else> $statement$)
		 `(ctax-if ,$3 ,$5 ,$7))
    ($statement$ (<while> <lparen> $exp$ <rparen> $statement$)
		 `(ctax-while ,$3 ,$5))
    ($statement$ (<do> $statement$ <lparen> $exp$ <rparen> <semi>)
		 `(ctax-do ,$2 ,$4))
    ($statement$ (<for> <lparen> $exp$ <semi> $exp$ <semi> $exp$ <rparen> $statement$)
		 `(ctax-for ,$3 ,$5 ,$7 ,$9))
    ($statement$ (<return> $exp$ <semi>)
		 `(ctax-return ,$2))
    ($statement$ (<break> <semi>)
		 '(ctax-break))
    ($statement$ (<continue> <semi>)
		 '(ctax-continue))
    ($statement$ (<semi>)
		 '())
    ($statement$ ($exp$ <semi>)
		 $1)
    ($statement$ (<lbrace> $declarations$ $statement-list$ <rbrace>)
		 `(ctax-begin ,$2  ,$3))
    

    ;; Expression syntax with the help of
    ;; "The Annotated C++ Reference Manual"
    ;; by Margaret Ellis and Bjarne Stroustrup.
    ;;
    ($exp$ ($assignment-exp$)
	   $1)

    ($exp$ ($exp$ <comma> $assignment-exp$)
	   (if (and (pair? $1) (eq? 'ctax-comma (car $1)))
	       (append! $1 (list $3))
	       `(ctax-comma ,$1 ,$3)))

    ;; short-cuts
    ;;
    ;; These short-cuts were indirectly suggested by a netnews post from 
    ;; Henry Spencer.  It said:
    ;;

    ;; Michael Meissner <meissner@cygnus.com> writes:
    ;; >...if you don't have features like %left, %right and/or
    ;; >%nonassoc, you need to specify separate rules for each level of
    ;; >precedence.  Thus a simple statement:
    ;; >	a = 1;
    ;; >Can generate 13 or more reductions in C...

    ;; Only if the grammar is naively written.  Although it is significantly
    ;; more work, it *is* possible to write a grammar that is optimized for
    ;; the common cases, and doesn't do the massive recursive plunge for
    ;; simple statements.  (My terminology here reflects my life-long
    ;; preference for recursive-descent parsers and their table-driven
    ;; equivalents, but I believe the same thing can be done for bottom-up
    ;; parsing.)

    ;; The way to do it is to short-circuit the common cases, and invoke the
    ;; full machinery only if (for example) that `1' is followed by an
    ;; operator.  This does make things messy -- you've got to do some
    ;; backing and filling to cope with that unwelcome operator -- but if
    ;; you're patient and careful, it can be done.  The resulting grammar is
    ;; not as clean and elegant as the one in the language spec, especially
    ;; for a language with many precedence levels, but it parses real
    ;; programs much more rapidly.

    ;; This trick has actually been around as folklore for some time.  I
    ;; don't recall ever seeing it published, although admittedly I'm more
    ;; than slightly behind on my reading.  I first ran into it in internal
    ;; documentation of the SP/k project at University of Toronto in 1977.
    ;; --
    ;; Henry Spencer, henry@zoo.toronto.edu
    ;; --

    ;; 
    ;; The parser in lr1.scm has backtracking built-in so we can add short-cuts
    ;; directly to the grammar, with no additional fuss.  The only limitation
    ;; is that short-cuts should not introduce new shift/reduce conflicts.
    ;;
    ;; In this case, the short-cuts allow, for example, an $assignment-exp$
    ;; to be reduced from an $unary-exp$ without intermediate reductions
    ;; $multiplicative-exp$ and $additive-exp$.   Of course, if such a short-cut
    ;; reduction would cause the next input token to be rejected, the 
    ;; backtracking machinary will avoid the reduction.
    ;;
    ($assignment-exp$ ($unary-exp$) $1)

    ($assignment-exp$ ($additive-exp$) $1)

    ($assignment-exp$ ($multiplicative-exp$) $1)

    ;; Back to essentials (not short-cuts) again:
    ;;
    ($assignment-exp$ ($conditional-exp$)
    		      $1)
    ($assignment-exp$ ($unary-exp$ <assignment> $assignment-exp$)
		      `(ctax-assign-op ,(string->symbol (car $2)) ,$1 ,$3))
    ($assignment-exp$ ($unary-exp$ <=> $assignment-exp$)
		      `(ctax-assign ,$1 ,$3))


    ;; What in C is (apparently) called a "logical or" or "logical and"
    ;; is what I call a "flow or" or "flow and" to emphasize that these
    ;; are the operators that affect flow of control.   Following SCM,
    ;; I use "logical" to name the bit operations.   This is just a matter
    ;; of preference: I'll take "flow or" over "bit or" any day.
    ;;
    ($conditional-exp$ ($flow-or-exp$)
		       $1)
    ($conditional-exp$ ($flow-or-exp$ <?> $exp$ <colon> $conditional-exp$)
		       `(ctax-?: ,$1 ,$3 ,$5))

    ($flow-or-exp$ ($flow-and-exp$)
		   $1)
    ($flow-or-exp$ ($flow-or-exp$ <flowor> $flow-and-exp$)
		   `(ctax-|| ,$1 ,$3))
    ($flow-and-exp$ ($logor-exp$)
		    $1)
    ($flow-and-exp$ ($flow-and-exp$ <flowand> $logor-exp$)
		    `(ctax-&& ,$1 ,$3))

    ($logor-exp$ ($logxor-exp$)
		 $1)
    ($logor-exp$ ($logor-exp$ <logor> $logxor-exp$)
		 `(ctax-| ,$1 ,$3))

    ($logxor-exp$ ($logand-exp$)
		  $1)
    ($logxor-exp$ ($logxor-exp$ <logxor> $logand-exp$)
		  `(ctax-^ ,$1 ,$3))

    ($logand-exp$ ($equality-exp$)
		  $1)
    ($logand-exp$ ($logand-exp$ <logand> $equality-exp$)
		  `(ctax-& ,$1 ,$3))

    ($equality-exp$ ($relational-exp$)
		    $1)
    ($equality-exp$ ($equality-exp$ <==> $relational-exp$)
		    `(ctax-== ,$1 ,$3))
    ($equality-exp$ ($equality-exp$ <!=> $relational-exp$)
		    `(ctax-!= ,$1 ,$3))

    ($relational-exp$ ($shift-exp$)
		      $1)
    ($relational-exp$ ($relational-exp$ <<> $shift-exp$)
		      `(ctax-< ,$1 ,$3))
    ($relational-exp$ ($relational-exp$ <>> $shift-exp$)
		      `(ctax-> ,$1 ,$3))
    ($relational-exp$ ($relational-exp$ <<=> $shift-exp$)
		      `(ctax-<= ,$1 ,$3))
    ($relational-exp$ ($relational-exp$ <>=> $shift-exp$)
		      `(ctax->= ,$1 ,$3))

    ($shift-exp$ ($additive-exp$)
		 $1)
    ($shift-exp$ ($shift-exp$ <<<> $additive-exp$)
		 `(ctax-<< ,$1 ,$3))
    ($shift-exp$ ($shift-exp$ <>>> $additive-exp$)
		 `(ctax->> ,$1 ,$3))


    ($additive-exp$ ($multiplicative-exp$)
		    $1)
    ($additive-exp$ ($additive-exp$ <+> $multiplicative-exp$)
		    `(ctax-+ ,$1 ,$3))
    ($additive-exp$ ($additive-exp$ <-> $multiplicative-exp$)
		    `(ctax-- ,$1 ,$3))

    ($multiplicative-exp$ ($cm-exp$)
			  $1)
    ($multiplicative-exp$ ($multiplicative-exp$ <*> $cm-exp$)
			  `(ctax-* ,$1 ,$3))
    ($multiplicative-exp$ ($multiplicative-exp$ </> $cm-exp$)
			  `(ctax-/ ,$1 ,$3))
    ($multiplicative-exp$ ($multiplicative-exp$ <%> $cm-exp$)
			  `(ctax-% ,$1 ,$3))

    ;; What C++ calls "pm" (pointer to member?) we
    ;; call "cm": computed member:
    ;;
    ($cm-exp$ ($unary-exp$)
	      $1)
    ($cm-exp$ ($cm-exp$ <arrow*> $unary-exp$)
		   `(ctax->* ,$3 ,$1))
    ($cm-exp$ ($cm-exp$ <dot*> $unary-exp$)
		   `(ctax-.* ,$3 ,$1))

    ($unary-exp$ ($postfix-exp$)
		 $1)
    ($unary-exp$ (<dollar> $unary-exp$)
		 `(ctax-dollar $2))
    ($unary-exp$ (<++> $unary-exp$)
		 `(ctax-++ ,$2))
    ($unary-exp$ (<--> $unary-exp$)
		 `(ctax--- ,$2))
    ($unary-exp$ (<-> $unary-exp$)
		 `(ctax-- ,$2))
    ($unary-exp$ (<!> $unary-exp$)
		 `(ctax-! ,$2))
    ($unary-exp$ (<lognot> $unary-exp$)
		 `(ctax-lognot ,$2))

    ($postfix-exp$ ($primary-exp$)
		   $1)
    ($postfix-exp$ ($postfix-exp$ <lbracket> $exp$ <rbracket>)
		   `(ctax-array-ref ,$1 ,$3))
    ($postfix-exp$ ($postfix-exp$ <lparen> $opt-exp-list$ <rparen>)
		   `(ctax-apply ,$1 ,$3))
    ($postfix-exp$ ($postfix-exp$ <++>)
		   `(ctax-post-++ ,$1))
    ($postfix-exp$ ($postfix-exp$ <-->)
		   `(ctax-post--- ,$1))

    ($postfix-exp$ ($postfix-exp$ <arrow> <identifier>)
		   `(ctax-> ,$3 ,(car $1)))
    ($postfix-exp$ ($postfix-exp$ <dot> <identifier>)
		   `(ctax-. ,$3 ,(car $1)))

    ($exp-list$ ($assignment-exp$)
		(list $1))
    ($exp-list$ (<keyword>)
		(list (car $1)))
    ($exp-list$ (<keyword> $assignment-exp$)
		(list (car $1) $2))
    
    ($exp-list$ ($exp-list$ <comma> $assignment-exp$)
		(append! $1 (list $3)))
    ($exp-list$ ($exp-list$ <comma> <keyword>)
		(append! $1 (list (car $3))))
    ($exp-list$ ($exp-list$ <comma> <keyword> $assignment-exp$)
		(append! $1 (list (car $3) $4)))

    ($opt-exp-list$ ($exp-list$)
		    $1)
    ($opt-exp-list$ ()
		    '())


    ($primary-exp$ (<identifier>)
		   (car $1))
    ($primary-exp$ (<number>)
		   (car $1))
    ($primary-exp$ (<constant>)
		   `',(car $1))
    ($primary-exp$ (<lparen> $exp$ <rparen>)
		   $2)

    ;; The rest of the primaries have no equivalent in Ctax.
    ;;

    ;; <literal> primaries are Scheme code embedded in Ctax expressions:
    ;;
    ($primary-exp$ (<literal>)
		   `(ctax-literal ,(car $1)))

    ;; <keyword> primaries are Scheme keywords (like ":color")
    ;;
    ($primary-exp$ (<keyword>)
		   (car $1))

    ;; <quasi-constant> primaries are written using backquote
    ;; and behave just like Scheme quasiquote expressions.
    ;;
    ($primary-exp$ (<quasi-constant>)
		   (list 'quasiquote (car $1)))

    ;; <lambda> expressions create anonymous procedures.
    ;;
    ($primary-exp$ (<lambda> <lparen> $arg-list$ <rparen> <lbrace> $declarations$ $statement-list$ <rbrace>)
		   `(ctax-lambda ,$3 ,$6 ,$7))
    ))

