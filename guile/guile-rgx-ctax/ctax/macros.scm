;;; installed-scm-file

;;;; 	Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
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


(define-module #/ctax/macros
  :use-module #/ice-9/common-list)


;;; {The Translator}
;;;
;;; The ctax parser emits syntax trees matching one of 
;;; the following two patterns:
;;;
;;; 	(define <var> (ctax-statement <tree>))
;;;	(ctax-statement <tree>)
;;;
;;; <tree>, in both cases, is an s-expression Ctax parse-tree.
;;; For example, the ctax:
;;;
;;; while (1) { auto z; z = x(); if (\odd? (z)) y(); else break; }
;;;
;;; becomes
;;;
;;; (ctax-while 1
;;;	(ctax-begin ((z #f)) 
;;;	  ((ctax-assign z (ctax-apply x ()))
;;;	   (ctax-if (ctax-apply odd? (z))
;;;	      (ctax-apply y ())
;;;	      (ctax-break)))))
;;;
;;; The procedure ctax-transl-statement rewrites <tree>s into
;;; ordinary Scheme.
;;;
;;; The memoizing macro ctax-statement rewrites its argument
;;; using ctax-transl-statement.  Thus, ctax-statement performs
;;; incremental translation of Ctax to Scheme.
;;;
;;; Expressions translated by ctax-transl-statement use a set of 
;;; user-definable procedures called "ctax operators".  These
;;; include ctax-+, ctax--, ctax-* and so on.
;;;

(defmacro-public ctax-statement (s) (translate-ctax-statement s))

(define-public (translate-ctax-statement s)
  (ctax-transl-statement s #f #f (lambda (translation flags) translation)))


;; ctax-transl-statement tree following exits-ok? return
;;
;; Translate a statement from ctax s-exp notation to Scheme.
;;
;; The arguments are 
;;     tree -- a ctax syntax tree to translate
;;
;;     following -- the name of a label to which to pass the value
;;                  of this statement.  #f if the statment should just
;;                  return it's value.
;;
;;                  For example, the two branches of a conditional are
;;                  (normally) translated with following set to 'fi,
;;                  and that label is given to the statements that
;;                  follow the conditional.
;;
;;                  Some care is taken to not introduce labels
;;                  unecessarily.  For example, if a conditional is
;;                  being compiled with following set to 'break
;;                  (indicating an enclosing while loop), then it
;;                  won't introduce a 'fi label.  Instead, the two
;;                  branches will also have following set to 'break.
;;                  This optimization is a kind of goto compression.
;;
;;     exits-ok? -- #f unless the statement is enclosed in a loop.
;;                  Only if this is true can the statement be `break'
;;                  or `continue'
;;
;;     return -- the return continuation. Takes two arguments.
;;
;;               The first argument is the Scheme form which is the
;;               translation.
;;
;;               The second is a list of flags describing the translation.
;;               At the moment, the flags are either '() or a
;;               one element list.
;;
;;               The flags can be:
;;
;;                   '(return)  -- the statement is a return statement.
;;
;;                   '(fi) -- the statement passes its value to the
;;                            label `fi'.  Presumably the statement
;;                            was a conditional.
;;
;;                   '(break) -- the statement passes its value to the
;;                               label `break'.  The statement was
;;                               some form of loop (while, for or do).
;;
;;
;;		The flags tell the caller something about the lexical
;;		environment expected by the compiled code.  In particular,
;;		it reports what goto-labels are free in the translated 
;;		statement.
;;
;; The statement tree language supports:
;;
;; (ctax-lamba <formals> <declarations> (<statement> ...))
;; (ctax-begin <declarations> (<statement> ...))
;; (ctax-for <init> <condition> <increment> <body>)
;; (ctax-while <condition> <statement>)
;; (ctax-do <condition> <statement>)
;; (ctax-if <condition> <statement> ?<statement>?)
;; (ctax-return <expression>)
;; (ctax-break)
;; (ctax-continue)
;;
;; The expression syntax supports operators
;; 
;; (ctax-neg ctax-log-neg ctax-pos ctax-bit-neg)
;; (ctax-| ctax-^ ctax-& ctax-== ctax-!= ctax-< ctax-> ctax-<= ctax->=
;;	   ctax-<< ctax->> ctax-+ ctax-- ctax-* ctax-/ ctax-% ctax-array-ref
;;	   ctax-|| ctax-&& ctax-! ctax-lognot ctax-?: ctax-comma)
;; (ctax-++ ctax---)
;; (ctax-post-++ ctax-post---)
;;
;; (ctax-assign)
;; (ctax-assign-op)
;;
;;
;;
;; <declarations> are of the form: ((<var> <init>) ...)
;;
;; Blocks and functions have local variable declarations.
;; These denote a let that initializes all of the locals
;; to #f and whose body begins with assignments to those
;; locals that are initialized to a value that is not obviously
;; #f:
;;
;; {
;;   scm a;
;;   scm b = 99;
;;   scm c = #f;
;;   scm d = b + 1;
;;   ...
;; }
;;
;;
;; written as s-exp:
;;
;; (ctax-begin ((a #f) (b 99) (c #f) (d (ctax-+ b 1)) ...)
;;
;; denotes:
;;
;; (let ((a #f) (b #f) (c #f))
;;    (begin (set! b 99)
;;	     (set! d (ctax-+ b 1)))
;;    [...])
;;
;;
;; Conditionals and loops compile by forcing their continuations
;; to have a label.   For example:
;;
;; { if (a) b; else c; d; e; }
;;
;; (ctax-begin () ((ctax-if a b c) d e))
;;
;; denotes:
;;
;; (let ((fi (lambda (return) d e)))
;;    (if a (fi b) (fi c)))
;;
;; and { while (x) y; z; }
;;
;; (let ((break (lambda (return) z)))
;;    (letrec ((continue (lambda (return)
;;			    (if x 
;;				(continue y)
;;				(break #f)))))
;;	(continue #f)))
;;
(define (ctax-transl-statement tree following exits-ok? return)
  (let ((statement-type (ctax-tree-type tree)))

    (case statement-type

      ;; Compound statements.
      ;;
      ;; In the simplest case, a ctax block turns into just a Scheme
      ;; block:
      ;;
      ;;   { a; b; c; }    =>   (begin [a] [b] [c])
      ;;
      ;; That case is handled by translating a to [a], and then
      ;; making (conceptually):
      ;;
      ;;        (begin [a] (begin [{b; c;}]))
      ;;
      ;; To actually build such a scheme form, we use ctax-make-begin!
      ;; which flattens nested begin forms.
      ;;
      ;; In a more complicated case, the first statment might be
      ;; a loop or conditional.  In that case, the rest of the
      ;; statements have to be labeled:
      ;;
      ;;      { if (a) b; else return c;  d; }
      ;;    =>
      ;;      (let ((fi (lambda (return) [d])))
      ;;         (if (ctax-test [a])
      ;;            (fi [b])
      ;;            c))
      ;;
      ;; Note that this translation isn't hygenic: it mixes some
      ;; compiler generated identifiers ("fi" and "return") in with
      ;; the identifiers of the source program.  We get away with that
      ;; by making the labels illegal ctax identifiers.  Slightly more
      ;; sophisticated translations could be hygenic but there is no
      ;; need so long as the compiler can allocate a few variable
      ;; names to itself.
      ;;
      
      
      ((ctax-begin)

       (let* ((formals (cadr tree))
	      (body (caddr tree))
	      (first-stmt (car body))
	      (rest-stmts (cdr body)))

	 (if (null? rest-stmts)

	     ;; If a compound statement only contains one element,
	     ;; just translate that element.
	     ;;
	     (ctax-transl-statement
	      (car body)
	      following
	      exits-ok?
	      (lambda (only-tree only-labels)
		(return
		 ;; Even though the block has only one statement, it
		 ;; may have some local variables.
		 ;;
		 (if (null? formals)
		     only-tree
		     (ctax-enclose-with-formals formals only-tree))
		 only-labels)))


	     ;; Truly compound statemts
	     ;;
	     ;; Start by translating the first statement...
	     ;;
	     (ctax-transl-statement
	      first-stmt

	      ;; We are in the middle of a block, so the first statement
	      ;; is followed directly by other statements.  Therefore,
	      ;; it should just return its value normally and we will
	      ;; ignore it.
	      ;;
	      #f

	      ;; It is only ok for the first statement to be a break
	      ;; or continue iff it was ok for this whole block to have
	      ;; been a break or continue:
	      ;;
	      exits-ok?

	      (lambda (first-tree first-attribs)
		;; A big dispatch on the attributes of the first
		;; statement:
		;;
		(cond
		 ;; If the first statement was simple enough, then there
		 ;; are no free labels to resolve
		 ;;
		 ((null? first-attribs)
		  ;; Just put the statement in a scheme block with the
		  ;; rest of the statements.  First, contruct a ctax block
		  ;; containing only the rest of this block, and compile
		  ;; that:
		  ;;
		  (ctax-transl-statement
		   `(ctax-begin () ,rest-stmts)

		   ;; The subblock containing all statements after the
		   ;; first is followed by whatever follows the block
		   ;; we're working on.
		   ;;
		   following

		   ;; Again, this is inherited:
		   ;;
		   exits-ok?

		   (lambda (rest-tree rest-attribs)
		     ;; This function has the compiled first
		     ;; statement, and then compiled rest of the block.
		     ;;
		     (let ((block-denot
			    (ctax-make-begin! (list first-tree rest-tree))))
		       (return
			(if (null? formals)
			    block-denot
			    (ctax-enclose-with-formals formals block-denot))
			;; The attributes of the tail of the block
			;; become the attributes of the whole block:
			;;
			rest-attribs)))))


		 ;; If the first statement was a return statement,
		 ;; then ignore the remaining statements and consider 
		 ;; this whole block a return statement.
		 ;;
		 ((equal? '(return) first-attribs)
		  (return first-tree '(return)))

		 ;; If the first statement was a conditional or loop,
		 ;; provide the appropriate label for the rest of the block:
		 ;;
		 ((member first-attribs '((fi) (break)))
		  (ctax-transl-statement
		   ;; Compile the rest of the block.
		   ;;
		   `(ctax-begin () ,rest-stmts)

		   ;; The rest of the block inherits the whole
		   ;; block's follow.
		   ;;
		   following

		   ;; Inherit whether we are in a loop:
		   ;;
		   exits-ok?

		   (lambda (rest-tree rest-attribs)
		     ;; Label the rest of the block `fi' or `break'
		     ;; so that the first statement can terminate using
		     ;; branches to that label.
		     ;;
		     (let ((block-denot `(let ((,(car first-attribs)
						(lambda (return) ,rest-tree)))
					   ,first-tree)))
		       (return
			(if (null? formals)
			    block-denot
			    (ctax-enclose-with-formals formals
						       block-denot))

			rest-attribs)))))

		 (else (list 'goof
			     first-attribs
			     first-tree))))))))


      ;; Return statements simply denote their expression's denotation.
      ;; This is different from an expression statement.  An expression
      ;; statement denotes its expression's denotation but wrapped in a
      ;; call to the label implied by `follow'.
      ;;
      ;; To illustrate the difference, consider compiling the return in:
      ;;
      ;; { if (<x>) return <y>; else <z>; ... }
      ;;
      ;; where <y> and <z> both happen to be expressions.
      ;;
      ;; When [<z>] is evaluated it should pass the result to the label
      ;; "fi", which will transfer control to the elided part of the block.
      ;;
      ;; When [<y>] is evaluated, on the other hand, it should ignore "fi"
      ;; and return its value directly.
      ;;
      ((ctax-return)
       (return (ctax-transl-expression (cadr tree)) '(return)))
      

      ;; A conditional is translated to a Scheme conditional.
      ;; Along with returning the translated statement, we inform
      ;; the caller that a label "fi" is needed as a branch target
      ;; for the consequent and anti-consequent of the conditional.
      ;;
      ((ctax-if)
       (let* ((pred (cadr tree))
	      (consequent (caddr tree))
	      (anticons (cadddr tree))

	      (tail-label  (or following 'fi))

	      ;; Translate the predicate trivially...
	      ;;
	      (pred-denot `(ctax-test ,(ctax-transl-expression pred))))
	 
	 (ctax-transl-statement
	  consequent
	  tail-label
	  exits-ok?	
	  (lambda (cons-denot cons-labels)
	    (ctax-transl-statement
	     anticons
	     tail-label
	     exits-ok?
	     (lambda (anticons-denot anticons-labels)
	       (return
		`(if ,pred-denot
		     ,cons-denot
		     ,anticons-denot)
		(if following
		    #f
		    '(fi)))))))))

      ;; A loop is translated to a tail-recursive procedure
      ;; called "continue".
      ;;
      ;; Along with returning the translated statement, we inform
      ;; the caller that a label "break" is needed as a branch target
      ;; for the consequent and anti-consequent of the conditional.
      ;;
      ((ctax-while ctax-do)
       (let* ((pred (cadr tree))
	      (body (caddr tree))
	      (pred-denot `(ctax-test ,(ctax-transl-expression pred)))
	      (tail-label (or following 'break)))

	 (ctax-transl-statement
	  body
	  'continue
	  #t
	  (lambda (body-denot body-labels)
	    (return
	     
	     (let ((w/continue
		    `(letrec ((continue
			       (lambda (return)
				 (if ,pred-denot
				     ,body-denot
				     (break return)))))
		       ;; Does one execution of the body always
		       ;; precede the first evaluation of the predicate?
		       ;;
		       ,(if (eq? statement-type 'ctax-do)
			    body-denot
			    '(continue #f)))))

	       ;; If there is a `following' label, then that
	       ;; label is where calls to `break' should go.
	       ;;
	       (if following
		   `(let ((break ,following))
		      ,w/continue)
		   w/continue))

	     
	     ;; If there is no following label, then 
	     ;; the caller has to provide an appropriate
	     ;; binding for `break'.
	     ;;
	     (if following
		 #f
		 '(break)))))))


      ;; For loops are rewritten as while loops.
      ;;
      ((ctax-for)
       (let* ((init (cadr tree))
	      (pred (caddr tree))
	      (increment (cadddr tree))
	      (body (car (cddddr tree)))
	      (new-body `(ctax-begin () ,(append body increment)))
	      (new-loop `(ctax-while ,pred
				     ,new-body))
	      (easier-form `(ctax-begin () ,(list init new-loop))))

	 (ctax-transl-statement easier-form following exits-ok? return)))

      ;; Translated break and continue statements work by making 
      ;; tail calls to labels of those names.   Here we return the 
      ;; translated statement and inform the caller that a label
      ;; is needed.
      ;;
      ((ctax-break) (return '(break #f) '(break)))
      ((ctax-continue) (return '(continue #f) '(continue)))

      ;; Expressions are translated trivially:
      ;;
      (else
       (let ((exp-denot (ctax-transl-expression tree)))
	 (return
	  (if following
	      (list following exp-denot)
	      exp-denot)
	  '()))))))


;; ctax-transl-expression ctax-tree
;; Return ctax-tree, translated to Scheme.
;;
;; Translate an expression.  This is trivial because tricky flow of control
;; is not an issue.  
;;


(define (ctax-transl-expression tree)
  (case (ctax-tree-type tree)
    ;; Expressions:
    ((ctax-comma)
     (ctax-make-begin! (map ctax-transl-expression (cdr tree))))
     
    ((ctax-constant quasiquote quote) tree)

    ((ctax-variable) tree)

    ((ctax-literal) (cadr tree))

    ((ctax-apply)
     (map ctax-transl-expression (cons (cadr tree) (caddr tree))))

    ((ctax-lambda)
     `(lambda ,(cadr tree)
	,(ctax-transl-statement `(ctax-begin ,(caddr tree) ,(cadddr tree))
				#f #f
				(lambda (tree flags) tree))))

    ((ctax-neg ctax-log-neg ctax-pos ctax-bit-neg)
     (cons (car tree)
	   (map ctax-transl-expression (cdr tree))))

    ((ctax-assign)
     (let* ((dest (cadr tree))
	    (val (caddr tree))
	    (dest-denot (ctax-transl-expression dest))
	    (val-denot (ctax-transl-expression val)))
       (ctax-make-assignment dest-denot val-denot)))
       
    ((ctax-| ctax-^ ctax-& ctax-== ctax-!= ctax-< ctax-> ctax-<= ctax->=
	     ctax-<< ctax->> ctax-+ ctax-- ctax-* ctax-/ ctax-% ctax-array-ref
	     ctax-|| ctax-&& ctax-! ctax-lognot ctax-?: ctax-comma)
     (cons (car tree)
	   (map ctax-transl-expression (cdr tree))))

    ((ctax-++ ctax---)
     (ctax-transl-expression `(ctax-assign-op ,(cadr tree)
					      (,(if (eq? 'ctax-++ (ctax-tree-type tree))
						    ctax-+
						    ctax--)
					       ,(cadr tree)
					       1))))

    ((ctax-post-++ ctax-post---)
     (let ((amt (if (eq? 'ctax-post-++ (ctax-tree-type tree))
		    1
		    -1)))
       (ctax-transl-expression `(ctax-+ ,(- amt) (ctax-assign-op ,(cadr tree) (ctax-+ ,amt ,(cadr tree)))))))

    ((ctax-assign-op)
     (ctax-make-assignment (cadr tree) (caddr tree)))

    (else (error (list 'internal-error tree)))))

  

;; Return a symbol describing a parse tree.
;;
(define (ctax-tree-type tree)
  (cond
   ((pair? tree) (car tree))
   ((memq tree '(ctax-break ctax-continue)) tree)
   ((or (keyword? tree) (symbol? tree)) 'ctax-variable)
   (else 'ctax-constant)))


;; When building up scheme forms like (begin...), collapse
;; nested begin forms destructively.
;;
(define (ctax-make-begin! expressions)

  (define (is-begin form)
    (and (pair? form) (eq? (car form) 'begin)))

  (define (build-list! dest exps)
    (cond
     ((null? exps)
      (set-cdr! dest '()))
     ((is-begin (car exps))
      (set-cdr! dest (cdar exps))
      (build-list! (last-pair dest) (cdr exps)))
     (else
      (set-cdr! dest (cons (car exps) '()))
      (build-list! (cdr dest) (cdr exps)))))

  (let ((answer (cons 'begin '#f)))
    (build-list! answer expressions)
    answer))


;; Translating assignments:
;;
(define (ctax-make-assignment dest val)
  (cond
   ((symbol? dest)
    `(set! ,dest ,val))

   ((eq? (car dest) 'ctax-array-ref)
    `(ctax-array-set! ,(cadr dest)
		      ,(caddr dest)
		      ,val))

   (else
    (error (list 'illegal-assignment dest val)))))


(define (ctax-enclose-with-formals formals scheme-form)
  `(let ,(map (lambda (v) `(,(car v) #f)) formals)
     ,(let ((sets (map (lambda (v) `(set! ,@v))
		       (remove-if (lambda (v) (not (cadr v))) formals))))
	(if sets
	    `(begin
	       (begin ,@ sets)
	       ,scheme-form)
	    scheme-form))))

