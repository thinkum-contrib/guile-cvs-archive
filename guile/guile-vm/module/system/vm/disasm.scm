;;; Guile VM Disassembler

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

(define-module (system vm disasm)
  :use-module (system vm core)
  :use-module (system vm conv)
  :use-module (ice-9 regex)
  :use-module (ice-9 match)
  :use-module (ice-9 format)
  :use-module (ice-9 receive)
  :use-module (ice-9 and-let-star)
  :export (disassemble-program disassemble-bytecode))

(define (disassemble-program prog . opts)
  (let* ((arity (program-arity prog))
	 (nargs (car arity))
	 (nrest (cadr arity))
	 (nlocs (caddr arity))
	 (bytes (program-bytecode prog))
	 (objs  (program-objects prog)))
    ;; Disassemble this bytecode
    (format #t "Disassembly of ~A:\n\n" prog)
    (format #t "args = ~A  rest = ~A  locals = ~A\n\n" nargs nrest nlocs)
    (format #t "Bytecode:\n\n")
    (disassemble-bytecode bytes objs)
    (if (> (vector-length objs) 0)
	(disassemble-objects objs))
    ;; Disassemble other bytecode in it
    (for-each
     (lambda (x)
       (if (program? x)
	   (begin (display "----------------------------------------\n")
		  (apply disassemble-program x opts))))
     (vector->list objs))))

(define (disassemble-bytecode bytes . opt)
  (let ((decode (make-byte-decoder bytes))
	(rest '()))
    (do ((addr+code (decode) (decode)))
	((not addr+code) (newline))
      (receive (addr code) addr+code
	(match code
	  (('load-program x)
	   (let ((sym (gensym "")))
	     (set! rest (acons sym x rest))
	     (print-info addr (format #f "load-program #~A" sym) #f)))
	  (else
	   (let ((info (list->string code))
		 (extra (original-value code (if (null? opt) #f (car opt)))))
	     (print-info addr info extra))))))
    (for-each (lambda (sym+bytes)
		(format #t "Bytecode #~A:\n\n" (car sym+bytes))
		(disassemble-bytecode (cdr sym+bytes)))
	      (reverse! rest))))

(define (disassemble-objects objs)
  (display "Objects:\n\n")
  (let ((len (vector-length objs)))
    (do ((n 0 (1+ n)))
	((= n len) (newline))
      (let ((info (object->string (vector-ref objs n))))
	(print-info n info #f)))))

(define (disassemble-meta meta)
  (display "Meta info:\n\n")
  (for-each (lambda (data)
	      (print-info (car data) (list->string (cdr data)) #f))
	    meta)
  (newline))

(define (original-value code table)
  (define (branch-code? code)
    (string-match "^(br|jump)" (symbol->string (car code))))
  (let ((code (code-unpack code)))
    (cond ((code->object code) => object->string)
;;;	((branch-code? code)
;;;	 (format #f "-> ~A" (+ addr (cadr code))))
	  (else
	   (let ((inst (car code)) (args (cdr code)))
	     (case inst
	       ((make-false) "#f")
;;;	     ((object-ref)
;;;	      (object->string (vector-ref objs (car args))))
	       ((local-ref local-set)
		;;'(ref x))
		#f)
;;;	     ((module-ref module-set)
;;;	      (let ((var (vector-ref objs (car args))))
;;;		(list (if (eq? inst 'module-ref) 'ref 'set)
;;;		      (if (pair? var) (car var) var))))
	       (else #f)))))))

(define (list->string list)
  (let ((str (object->string list)))
    (substring str 1 (1- (string-length str)))))

(define (print-info addr info extra)
  (if extra
      (format #t "~4@A    ~24A;; ~A\n" addr info extra)
      (format #t "~4@A    ~A\n" addr info)))
