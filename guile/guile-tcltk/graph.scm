;;; installed-scm-file

;;;; 	Copyright (C) 1998 Free Software Foundation, Inc.
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

;; This demonstration of the tcltk module can be compared to the graph
;; editor demo in this book:
;;
;;
;;    Title:			Tcl and the Tk Toolkit
;;    Author:			John K. Ousterhout
;;    Publisher:		Addison-Wesley
;;    ISBN:			0-201-63337-X
;;

(define-module (tcltk graph)
  :use-module (ice-9 slib)
  :use-module (tcltk tcltk))

;; {Hash tables}

(require 'hash-table)

;; {Tcl interpreter}

(or the-interpreter (new-interpreter))

;;  {Shorthand for Medium-Sized equal?-based Hash Tables}
;;
(define aref (hash-inquirer equal?))
(define aremove (hash-remover equal?))
(define aset! (hash-associator equal?))
(define (make-table) (make-hash-table 64))

(define node-x (make-table))
(define node-y (make-table))
(define edge-first (make-table))
(define edge-second (make-table))

(proc mknode ("%x %y" (number x) (number y))
      (let ((new (string->number (.c 'create 'oval
				     (- x 10) (- y 10)
				     (+ x 10) (+ y 10)
				     :outline 'black
				     :fill 'white
				     :tags 'node))))
	(aset! node-x new x)
	(aset! node-y new y)
	(aset! edge-first new '())
	(aset! edge-second new '())))

(proc mkedge ((number first) (number second))
      (let ((edge (.c 'create 'line
		      (aref node-x first) (aref node-y first)
		      (aref node-x second) (aref node-y second))))
	(.c 'lower edge)
	(aset! edge-first first (cons edge (aref edge-first first)))
	(aset! edge-second second (cons edge (aref edge-second second)))))

(define first-node #f)
(define current-node #f)

(define (move-node node x-dist y-dist)
  (.c 'move node x-dist y-dist)
  (aset! node-x node (+ x-dist (aref node-x node)))
  (aset! node-y node (+ y-dist (aref node-y node)))
  (for-each
   (lambda (edge)
     'bar
     (.c 'coords edge
	 (aref node-x node)  (aref node-y node)
	 (lindex (.c 'coords edge) 2)
	 (lindex (.c 'coords edge) 3)))
   (aref edge-first node))
  (for-each
   (lambda (edge)
     (.c  'coords edge
	  (lindex (.c 'coords edge) 0) (lindex (.c 'coords edge) 1)
	  (aref node-x node) (aref node-y node)))
   (aref edge-second node))
  (update 'idletasks))

(define cur-x #f)
(define cur-y #f)

(define (setup-window)
  (canvas '.c)
  (pack '.c :fill "both" :expand #t)
  (bind '.c "<Button-1>" mknode)

  (.c 'bind 'node "<Any-Enter>"
      (tcl-lambda ()
        (.c 'itemconfigure 'current :fill 'black)))

  (.c 'bind 'node "<Any-Leave>"
      (tcl-lambda ()
        (.c 'itemconfigure 'current :fill 'white)))

  (.c 'bind 'node "<Button-2>"
      (tcl-lambda ("%x %y" (number x) (number y))
        (set! cur-x x)
	(set! cur-y y)
	#t))

  (.c 'bind 'node "<B2-Motion>"
      (tcl-lambda ("%x %y" (number x) (number y))
        (let ((amt-x (- x cur-x))
	      (amt-y (- y cur-y)))
	  (set! cur-x x)
	  (set! cur-y y)
	  (move-node (string->number (.c 'find 'withtag 'current)) amt-x amt-y)
	  #t)))

  (bind '.c 1
	(tcl-lambda () (set! first-node (.c 'find 'withtag 'current))))

  (bind '.c 2
	(tcl-lambda ()
	  (set! current-node (.c 'find 'withtag 'current))
	  (if (and first-node current-node)
	      (mkedge first-node current-node))))

  (focus '.c)

  (bind '.c 'Q (tcl-lambda () (wm 'withdraw ".") (destroy ".")))
  (bind '.c 'q (tcl-lambda () (wm 'withdraw ".") (destroy "."))))

(define-public (graph)
  (if (not (tk-main-window?))
      (begin
	(tk-make-main-window)
	(setup-window)
	(tk-spawn-handler))))

(graph)
