;;; Guile VM assembler

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (system vm assemble)
  :use-syntax (system base syntax)
  :use-module (system il glil)
  :use-module (system vm core)
  :use-module (system vm conv)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module (ice-9 common-list)
  :export (assemble))

(define (assemble glil env . opts)
  (optimizing-dump (codegen (preprocess glil #f) #t)))


;;;
;;; Types
;;;

(define-structure (<vm-asm> venv glil body))
(define-structure (venv parent nexts closure?))
(define-structure (vmod id))
(define-structure (vlink module name))
(define-structure (bytespec nargs nrest nlocs bytes objs))


;;;
;;; Stage 1: Preprocess
;;;

(define (preprocess x e)
  (match x
    (($ <glil-asm> nargs nrest nlocs nexts body)
     (let* ((venv (make-venv e nexts #f))
	    (body (map (lambda (x) (preprocess x venv)) body)))
       (make-<vm-asm> venv x body)))
    (($ <glil-external> op depth index)
     (do ((d depth (1- d))
	  (e e (venv-parent e)))
	 ((= d 0))
       (set-venv-closure?! e #t))
     x)
    (else x)))


;;;
;;; Stage 2: Bytecode generation
;;;

(define (codegen glil toplevel)
  (match glil
    (($ <vm-asm> venv ($ <glil-asm> nargs nrest nlocs nexts _) body)
     (let ((stack '())
	   (label-alist '())
	   (object-alist '())
	   (nvars (+ nargs nlocs -1)))
       (define (push-code! code)
	 (set! stack (optimizing-push code stack)))
       (define (push-object! x)
	 (let ((index (or ((if (vlink? x) assoc-ref assq-ref) object-alist x)
			  (let ((index (length object-alist)))
			    (set! object-alist (acons x index object-alist))
			    index))))
	   (push-code! `(object-ref ,index))))
       (define (label-ref key)
	 (assq-ref label-alist key))
       (define (label-set key)
	 (let ((addr (apply + (map length stack))))
	   (set! label-alist (assq-set! label-alist key addr))))
       (define (generate-code x)
	 (match x
	   (($ <vm-asm> venv)
	    (let ((spec (codegen x #f)))
	      (if toplevel
		  (dump-object! spec push-code!)
		  (push-object! spec)))
	    (if (venv-closure? venv) (push-code! `(make-closure))))

	   (($ <glil-void>)
	    (push-code! `(void)))

	   (($ <glil-const> x)
	    (if toplevel
		(dump-object! x push-code!)
		(cond ((object->code x) => push-code!)
		      (else (push-object! x)))))

	   (($ <glil-argument> op index)
	    (push-code! `(,(symbol-append 'local- op) ,(- nvars index))))

	   (($ <glil-local> op index)
	    (push-code! `(,(symbol-append 'local- op)
			  ,(- nvars (+ nargs index)))))

	   (($ <glil-external> op depth index)
	    (do ((e venv (venv-parent e))
		 (d depth (1- d))
		 (i 0 (+ i (venv-nexts e))))
		((= d 0)
		 (push-code! `(,(symbol-append 'external- op) ,(+ index i))))))

	   (($ <glil-module> op module name)
	    ;; (let ((vlink (make-vlink (make-vmod module) name)))
	    (let ((vlink (make-vlink #f name)))
	      (if toplevel
		  (dump-object! vlink push-code!)
		  (push-object! vlink)))
	    (push-code! (list (symbol-append 'variable- op))))

	   (($ <glil-label> label)
	    (label-set label))

	   (($ <glil-branch> inst label)
	    (let ((setter (lambda (addr) (- (label-ref label) addr))))
	      (push-code! (list inst setter))))

	   (($ <glil-call> inst nargs)
	    (if (instruction? inst)
		(let ((pops (instruction-pops inst)))
		  (cond ((< pops 0)
			 (push-code! (list inst nargs)))
			((= pops nargs)
			 (push-code! (list inst)))
			(else
			 (error "Wrong number of arguments:" inst nargs))))
		(error "Unknown instruction:" inst)))))
       ;;
       ;; main
       (if (> nexts 0) (push-code! `(external ,nexts)))
       (for-each generate-code body)
       (let ((bytes (apply string-append (stack-finalize (reverse! stack))))
	     (objs (map car (reverse! object-alist))))
	 (make-bytespec nargs nrest nlocs bytes objs))))))

(define (stack-finalize stack)
  (let loop ((list '()) (stack stack) (addr 0))
    (if (null? stack)
	(reverse! list)
	(let* ((orig (car stack))
	       (addr (+ addr (length orig)))
	       (code (if (and (pair? (cdr orig)) (procedure? (cadr orig)))
			 `(,(car orig) ,((cadr orig) addr))
			 orig)))
	  (loop (cons (code->bytes code) list) (cdr stack) addr)))))

;; Optimization

(define *optimize-table*
  '((not       (not       . not-not)
	       (eq?       . not-eq?)
	       (null?     . not-null?)
	       (not-not   . not)
	       (not-eq?   . eq?)
	       (not-null? . null?))
    (br-if     (not       . br-if-not)
	       (eq?       . br-if-eq)
	       (null?     . br-if-null)
	       (not-not   . br-if)
	       (not-eq?   . br-if-not-eq)
	       (not-null? . br-if-not-null))
    (br-if-not (not       . br-if)
	       (eq?       . br-if-not-eq)
	       (null?     . br-if-not-null)
	       (not-not   . br-if-not)
	       (not-eq?   . br-if-eq)
	       (not-null? . br-if-null))))

(define (optimizing-push code stack)
  (let ((alist (assq-ref *optimize-table* (car code))))
    (cond ((and alist (pair? stack) (assq-ref alist (caar stack))) =>
	   (lambda (inst) (cons (cons inst (cdr code)) (cdr stack))))
	  (else (cons (code-pack code) stack)))))


;;;
;;; Stage3: Dump optimization
;;;

(define (optimizing-dump bytespec)
  ;; no optimization yet
  (bytespec-bytes bytespec))

(define (dump-object! x push-code!)
  (let dump! ((x x))
    (cond
     ((object->code x) => push-code!)
     ((bytespec? x)
      (let ((nargs (bytespec-nargs x))
	    (nrest (bytespec-nrest x))
	    (nlocs (bytespec-nlocs x))
	    (bytes (bytespec-bytes x))
	    (objs  (bytespec-objs x)))
	;; dump parameters
	(if (and (< nargs 4) (< nlocs 16))
	    (push-code! (object->code (+ (* nargs 32) (* nrest 16) nlocs)))
	    (begin
	      (push-code! (object->code nargs))
	      (push-code! (object->code nrest))
	      (push-code! (object->code nlocs))
	      (push-code! (object->code #f))))
	;; dump object table
	(cond ((not (null? objs))
	       (for-each dump! objs)
	       (push-code! `(vector ,(length objs)))))
	;; dump bytecode
	(push-code! `(load-program ,bytes))))
     ((vlink? x)
      ;; (push-code! `(local-ref ,(object-index (vlink-module x))))
      (dump! (vlink-name x))
      (push-code! `(link/current-module)))
     ;;((vmod? x)
     ;;  (push-code! `(load-module ,(vmod-id x))))
     ((integer? x)
      (let ((str (do ((n x (quotient n 256))
		      (l '() (cons (modulo n 256) l)))
		     ((= n 0)
		      (list->string (map integer->char l))))))
	(push-code! `(load-integer ,str))))
     ((string? x)
      (push-code! `(load-string ,x)))
     ((symbol? x)
      (push-code! `(load-symbol ,(symbol->string x))))
     ((keyword? x)
      (push-code! `(load-keyword ,(symbol->string (keyword-dash-symbol x)))))
     ((list? x)
      (for-each dump! x)
      (push-code! `(list ,(length x))))
     ((pair? x)
      (dump! (car x))
      (dump! (cdr x))
      (push-code! `(cons)))
     ((vector? x)
      (for-each dump! (vector->list x))
      (push-code! `(vector ,(vector-length x))))
     (else
      (error "Cannot dump:" x)))))

;;;(define (dump-table-object! obj+index)
;;;  (let dump! ((x (car obj+index)))
;;;    (cond
;;;     (else
;;;      (for-each push-code! (dump-object! x)))))
;;;  (push-code! `(local-set ,(cdr obj+index))))
