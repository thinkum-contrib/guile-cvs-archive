;;;; 	Copyright (C) 1996, 1997, 1998, 2001 Free Software Foundation, Inc.
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
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;;
;;;; The author can be reached at djurfeldt@nada.kth.se
;;;; Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
;;;;

;;; ****************************************
;;; * Prototype for the Guile-Tk interface *
;;; * Experimental hack---be warned!       *
;;; ****************************************

(define-module (tcltk tcltk)
  :use-module (ice-9 debug)
  :use-module (ice-9 threads)
  :use-module (tcltk dynlink))

;; Re-export bindings from the gtcltk module
(export
  tcl-create-interp tcl-global-eval tcl-create-command
  tcl-delete-command tcl-get-int tcl-get-double tcl-get-boolean
  tcl-split-list tcl-merge tcl-trace-var2 tcl-untrace-var2
  tcl-set-var2 tcl-get-var2 tcl-defined? tcl-do-one-event
  tk-init-main-window tk-loop? tk-main-loop tk-num-main-windows)

(if (defined? 'load-extension)
    (load-extension "libguile-tcltk-gtcltk" "scm_init_tcltk_gtcltk")
    (merge-compiled-code "scm_init_gtcltk" "libguile-tcltk-gtcltk"))

(define widgets
  '(button checkbutton radiobutton menubutton menu canvas
	   label entry message listbox text scrollbar
	   scale frame toplevel))

(read-set! keywords 'prefix)

(define-public TCL_VERSION "7.6")
(define-public TCL_MAJOR_VERSION 7)
(define-public TCL_MINOR_VERSION 6)

;;; When a TCL command returns, the string pointer interp->result points to
;;; a string containing return information from the command.  In addition,
;;; the command procedure returns an integer value, which is one of the
;;; following:
;;;
;;; TCL_OK		Command completed normally;  interp->result contains
;;;			the command's result.
;;; TCL_ERROR		The command couldn't be completed successfully;
;;;			interp->result describes what went wrong.
;;; TCL_RETURN		The command requests that the current procedure
;;;			return;  interp->result contains the procedure's
;;;			return value.
;;; TCL_BREAK		The command requests that the innermost loop
;;;			be exited;  interp->result is meaningless.
;;; TCL_CONTINUE		Go on to the next iteration of the current loop;
;;;			interp->result is meaningless.
;;;

(define-public TCL_OK		0)
(define-public TCL_ERROR	1)
(define-public TCL_RETURN	2)
(define-public TCL_BREAK	3)
(define-public TCL_CONTINUE	4)

;;; Flag values passed to variable-related procedures.
;;;

(define-public TCL_GLOBAL_ONLY		1)
(define-public TCL_APPEND_VALUE	2)
(define-public TCL_LIST_ELEMENT	4)
(define-public TCL_TRACE_READS		#x10)
(define-public TCL_TRACE_WRITES	#x20)
(define-public TCL_TRACE_UNSETS	#x40)
(define-public TCL_TRACE_DESTROYED	#x80)
(define-public TCL_INTERP_DESTROYED	#x100)
(define-public TCL_LEAVE_ERR_MSG	#x200)

;;; Flag values to pass to TCL_DoOneEvent to disable searches
;;; for some kinds of events:
;;;

(define-public TCL_DONT_WAIT		2)
(define-public TCL_X_EVENTS		4)
(define-public TCL_FILE_EVENTS		8)
(define-public TCL_TIMER_EVENTS		#x10)
(define-public TCL_IDLE_EVENTS		#x20)
(define-public TCL_ALL_EVENTS		-3)

(define-public (tcl-eval . strings)
  (let* ((cmd (tcl-merge the-interpreter (map ->tcl-arg-string strings)))
	 (status (tcl-global-eval the-interpreter cmd)))
    (if (zero? (car status))
	(cdr status)
	(error (cdr status)))))

(define uniq-command
  (let ((cnt 0))
    (lambda (prefix)
      (set! cnt (+ cnt 1))
      (string-append prefix (number->string cnt)))))

(define (closure->tcl-name p)
  (let ((tcl-name (procedure-property p 'tcl-name)))
    (or tcl-name
	(let ((name (uniq-command ;(or (procedure-property p 'name)
		                       "*__guile#"))
	      ;(template (procedure-property p 'tcl-calling-convention)))
	      )
	  (tcl-create-command the-interpreter name p)
	  ;(set-procedure-property! p 'tcl-name name)
	  name))))

(define (->tcl-arg-string v)
  (cond
   ((symbol? v) v)
   ((keyword? v) (keyword-dash-symbol v))
   ((string? v) v)
   ((number? v) (number->string v))
   ((eq? #f v) "0")
   ((eq? #t v) "1")
   ((closure? v)
    (let ((cc (procedure-property v 'tcl-calling-convention)))
      (if cc
	  (string-append (closure->tcl-name v) " " cc)
	  (closure->tcl-name v))))
   ((null? v) "{}")
   ((list? v)
    (apply string-append
	   (reverse (let loop ((ls (cdr v))
			       (res (list (->tcl-arg-string (car v)))))
		      (if (null? ls)
			  res
			  (loop (cdr ls)
				(cons (->tcl-arg-string (car ls))
				      (cons " " res))))))))
   (else "")))

(define (tcl-args args)
  (cond ((null? args) '())
	((symbol? (car args))
	 (cons (car args) (tcl-args (cdr args))))
	((keyword? (car args))
	 (cons (keyword-dash-symbol (car args)) (tcl-args (cdr args))))
	((string? (car args))
	 (cons (car args) (tcl-args (cdr args))))
	((number? (car args))
	 (cons (number->string (car args)) (tcl-args (cdr args))))
	((eq? #f (car args))
	 (cons "0" (tcl-args (cdr args))))
	((eq? #t (car args))
	 (cons "1" (tcl-args (cdr args))))
	((closure? (car args))
	 (let ((cc (procedure-property (car args) 'tcl-calling-convention)))
	   (if cc
	       (cons (string-append (closure->tcl-name (car args)) " " cc)
		     (tcl-args (cdr args)))
	       (cons (closure->tcl-name (car args))
		     (tcl-args (cdr args))))))
	(else "")))

(define-public (tcl-command interp name)
  (let ((proc
	 (lambda args
	   (let ((status (tcl-global-eval
			  interp
			  (tcl-merge interp (map ->tcl-arg-string
						 (cons name args))))))
	     (if (zero? (car status))
		 (cdr status)
		 (throw 'tcl-error (cdr status)))))))
    (set-procedure-property! proc 'name name)
    proc))

(define-public (tk-widget widget-name)
  (let* ((widget-name-string (symbol->string widget-name))
	 (widget-name-prefix (string-append widget-name-string "#"))
	 (proc
	  (lambda (parent . args)
	    (let* ((name (cond ((or (not parent)
				    (procedure? parent))
				(string-append
				 (if parent
				     (procedure-property parent 'name)
				     "")
				 "."
				 (let ((hit (list-index args '#:name)))
				   (if hit
				       (let ((name (list-ref args (+ hit 1))))
					 (if (zero? hit)
					     (set! args (cddr args))
					     (let ((pred (list-tail args
								    (- hit 1))))
					       (set-cdr! pred (cddr pred))))
					 (symbol->string name))
				       (uniq-command widget-name-prefix)))))
			       ((string? parent) parent)
			       ((symbol? parent) (symbol->string parent))
			       (else (throw 'tcl-error
					    "Parent must be #f, a widget, or a symbol"))))
		   (status (tcl-global-eval
			    the-interpreter
			    (tcl-merge the-interpreter
				       (cons widget-name-string
					     (cons name
						   (map ->tcl-arg-string
							args)))))))
	      (cond ((not (zero? (car status)))
		     (throw 'tcl-error (cdr status)))
		    ((symbol? parent) (cdr status))
		    (else (let ((w (tcl-command the-interpreter name)))
			    (set-procedure-property! w 'tcl-name name)
			    w)))))))
    (set-procedure-property! proc 'name widget-name)
    proc))

;; Reifying a Tcl command as a Scheme procedure.
;;
;(define-public (reify-tcl-command interp name)
;  (let ((command-object (tcl-command interp name)))
;    (and command-object
;	 (let ((reified
;		(lambda args
;		  (let ((answer
;			 (tcl-apply-command command-object
;					    (map (lambda (a)
;						   (or (procedure-property a 'tk-command)
;						       a))
;						 args))))
;		    (if (eq? 0 (car answer))
;			(tcl-string-> (cdr answer))
;			(throw 'tcl-error (cdr answer)))))))
;	   (set-procedure-property! reified 'tk-command name)
;	   reified))))
(define-public reify-tcl-command tcl-command)

(define-public (tcl-command? p)
  (and (procedure? p) (procedure-property 'tcl-command)))

;; Evaluate some code in the scope of a TCL-ERROR handler.
;; The handler returns a conventional Tcl error value (i.e. (cons 1 message))
;; Some type conversion is automaticly done on the return value to put it
;; in a form Tcl will like.
;;
(defmacro-public with-tcl-error-handling body
  `(catch 'tcl-error
	  (lambda () (->tcl-string (begin ,@body)))
	  (lambda (tag . message)
	    (cons 1 (apply errcat message)))))

(define-public (errcat . args)
  (apply string-append
	 (map (lambda (x)
		(call-with-output-string
		     (lambda (p)
		       ((if (string? x) display write) x p)
		       (display " " p))))
	      args)))

;; If this is defined to be an unary function, it gets to extend the 
;; default type conversion rules for arguments (it is passed otherwise
;; unhandled values).
;;
(define-public tcl-type-converter #f)

;; Default conversions from Scheme to Tcl strings.
;;
(define-public (->tcl-string val)
  (cond
   ((string? val) val)
   ((symbol? val) val)
   ((number? val) (number->string val))
   ((eq? #f val) "0")
   ((eq? #t val) "1")
   ((keyword? val) (keyword->symbol val))
   (#t "")))

(define (that x) x)

;; Default conversions from Tcl strings to Scheme.
;;
(define-public (tcl-string-> val)
   (cond
    ((string->number val) => that)
    ((equal? "" val) #f)
    (#t val)))


;;; {Namespace cleaning}
;;;

;; These are the names of procedures already defined 
;; in Scheme but which, in this context, ought to refer
;; to Tcl/Tk commands.

(define override-scheme-list '(bind raise))

(define override-scheme-table (make-vector 137 '()))

(for-each (lambda (name)
	    (hashq-set! override-scheme-table name #t))
	  override-scheme-list)

;;; {An Implicit Default Interpreter}
;;;
;;; For programs like "wish" in which there is one designated default
;;; interpreter.
;;;

(define-public the-interpreter #f)

(define tcltk-module (nested-ref the-root-module
				 '(app modules tcltk tcltk)))
(define tcltk-interface (module-public-interface tcltk-module))

(define-public (new-interpreter)
  (set! the-interpreter (tcl-create-interp))
  (let ((tcl-binder (make-tcl-binder the-interpreter)))
    (set-module-binder! tcltk-interface tcl-binder)))

;; If there is a defined variable called NAME, return it.
;; If not, but there is a Tcl command in the default interpreter
;; called NAME, create a variable an initialize it to point to the
;; reified Tcl command.
;;
;; Finally, always return a variable, perhaps undefined, if DEFINING?
;; is a true value.
;;
(define-public (make-tcl-binder interp)
  (let* ((root-module the-scm-module)
	 (root-binder
	  (or (module-binder root-module)
	      ;; new module system
	      (let ((ec (standard-interface-eval-closure root-module)))
		(lambda (m s d?)
		  (ec s d?))))))
    (lambda (m s define?)
      (cond ((and (not (hashq-ref override-scheme-table s))
		  (root-binder root-module s #f)))
	    ((module-obarray-ref (module-obarray m) s))
	    ((tcl-defined? interp s)
	     (let ((b (make-undefined-variable)))
	       (module-obarray-set! (module-obarray m) s b)
	       (variable-set! b (tcl-command interp s))
	       b))
	    (else (and define?
		       (let ((b (make-undefined-variable)))
			 (module-obarray-set! (module-obarray m) s b)
			 b)))))))

;; Used to define Scheme procedures which are also Tcl commands.
;; The declarations syntax is;
;; 
;; (proc name (?<calling-convention>? ?.? <formals>) <body>)
;;
;; which is expanded in terms of tcl-lambda.
;;
(defmacro-public proc (name . spec)
  `(begin
     (define ,name (tcl-lambda ,@ spec))
     (tcl-create-command the-interpreter ',name ,name)))


;; Used to define an anonymous Scheme procedure which is suitable
;; for use as a Tcl command.
;;
;; The declaration syntax is:
;;
;; (tcl-lambda (?<calling-convention>? ?.? <formals>) <body>)
;;
;; A <calling-convention> is a string that describes how the procedure
;; should be called when it is used as a Tcl command.
;; If the procedure hash the tcl-name PROC, and the calling convention
;; "%x %y", then the procedure will be called as:
;;
;;		PROC %x %y
;;
;; Such calling conventions are useful in cases such as binding a Scheme
;; procedure to Tk event.
;;
;; Formals specifications are as usual except that non-rest parameters
;; can have declarations.  Declarations are arbitrary expressions in which
;; the name of the formal is in the second position.  The expressions are
;; evaluated in the scope of the formals, and may modify the formals by
;; side effect.  Declarations should return a false value to cause an error
;; to be thrown, a true value otherwise.
;;
(defmacro-public tcl-lambda (formals . body)
  (let* ((calling-convention (if (and (pair? formals)
				      (string? (car formals)))
				 (let ((a (car formals)))
				   (set! formals (cdr formals))
				   a)
				 #f))
	 (args (tcl-formals-vars formals))
	 (full-body `(begin
		       ,@(tcl-type-checks formals 1)
		       ,@body)))
    `(let ((proc (lambda ,args
		   (with-tcl-error-handling ,full-body))))
       ,@(if calling-convention
	     `((set-procedure-property! proc
					'tcl-calling-convention
					,calling-convention)
	       proc)
	     `(proc)))))


;; From a list of formals, perhaps with declarations, return the
;; formals <<e.g.  (a (tcl->int b) . c) => (a b . c) >>
;;
(define-public (tcl-formals-vars formals)
  (if (not (pair? formals))
      formals
      (cons (if (pair? (car formals))
		(cadar formals)
		(car formals))
	    (tcl-formals-vars (cdr formals)))))

(define-public (tcl-error . args)
  (apply throw (cons 'tcl-error args)))


(define-public (tcl-type-checks formals pos)
  (cond
   ((not (pair? formals)) '())
   ((not (pair? (car formals)))
    (tcl-type-checks (cdr formals) (+ 1 pos)))
   (#t (cons (tcl-type-check (car formals))
	     (tcl-type-checks (cdr formals) (+ 1 pos))))))

(define-public tcl-type-converters
  `( (number 	. 	,(lambda (x) (tcl->number x)))
     (string	.	,(lambda (x) x)) ))

(define (tcl-type-check x)
  (let ((a (assoc (car x) tcl-type-converters)))
    (if (not a)
	(error "Unsupported declaration" x)
	(list 'set! (cadr x) (cons (cdr a) (cdr x))))))

(define-public (tcl->number x)
  (cond ((string? x) (string->number x))
	((integer? x) x)
	(#t (tcl-error "Expected integer but got" x))))

;;; Get variable from tcl:

(define-public (tcl-get-var name)
  (tcl-get-var2 the-interpreter name #f 0))

(define-public (tcl-get-number name)
  (string->number (get-var name)))

;;; {Widgets}
;;;
(define (make-widget! name)
  (let ((w (tk-widget name))
	(b (make-undefined-variable)))
    (module-obarray-set! (module-obarray tcltk-interface) name b)
    (variable-set! b w)))

(for-each make-widget! widgets)

;;; {The application window}

(define-public top #f)

;; (tk-main-window [name] [geometry])
(define-public (tk-make-main-window . args)
  (if (not the-interpreter)
      (new-interpreter))
  (if (not (tcl-defined? the-interpreter "."))
      (let ((init-status (tk-init-main-window the-interpreter
					      (or (getenv "DISPLAY") ":0")
					      "gwish"
					      "Gwish")))
	(if (not (eq? #t init-status))
	    (error init-status))))
  (set! top (tcl-command the-interpreter "."))
  (if (not (null? args))
      (begin
	(tcl-global-eval the-interpreter
			 (string-append "wm title . " (car args)))
	(if (not (null? (cdr args)))
	    (tcl-global-eval the-interpreter
			     (string-append "wm geometry . " (cadr args)))))))

(define-public (tk-main-window?)
  (and the-interpreter
       (tcl-defined? the-interpreter ".")))

(if (memq 'threads *features*)
    (begin
      (define-public (tk-spawn-handler)
	(if (not (tk-loop?))
	    (begin-thread
	     (error-catching-loop
	      (lambda ()
		(if (or (tk-loop?)
			(= (tk-num-main-windows) 0))
		    (throw 'quit))
		(tk-main-loop))))))

      (define-public (tk-main-window . args)
	(apply tk-make-main-window args)
	(tk-spawn-handler))
      ))
