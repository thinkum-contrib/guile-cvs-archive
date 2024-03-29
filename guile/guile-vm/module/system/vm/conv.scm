;;; Guile VM code converters

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

(define-module (system vm conv)
  :use-module (system vm core)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :export (code-pack code-unpack object->code code->object code->bytes
		     make-byte-decoder))

;;;
;;; Code compress/decompression
;;;

(define (code-pack code)
  (match code
    ((inst (? integer? n))
     (cond ((< n 10)
	    (let ((abbrev (string->symbol (format #f "~A:~A" inst n))))
	      (if (instruction? abbrev) (list abbrev) code)))
	   (else code)))
    (else code)))

(define (code-unpack code)
  (let ((inst (symbol->string (car code))))
    (cond
     ((string-match "^([^:]*):([0-9]+)$" inst) =>
      (lambda (data)
	(cons* (string->symbol (match:substring data 1))
	       (string->number (match:substring data 2))
	       (cdr code))))
     (else code))))


;;;
;;; Encoder/decoder
;;;

(define (object->code x)
  (cond ((eq? x #t) `(make-true))
	((eq? x #f) `(make-false))
	((null? x) `(make-eol))
	((and (integer? x) (exact? x))
	 (cond ((and (<= -128 x) (< x 128))
		`(make-int8 ,(modulo x 256)))
	       ((and (<= -32768 x) (< x 32768))
		(let ((n (if (< x 0) (+ x 65536) x)))
		  `(make-int16 ,(quotient n 256) ,(modulo n 256))))
	       (else #f)))
	((char? x) `(make-char8 ,(char->integer x)))
	(else #f)))

(define (code->object code)
  (match code
    (('make-true) #t)
    (('make-false) #f) ;; FIXME: Same as the `else' case!
    (('make-eol) '())
    (('make-int8 n)
     (if (< n 128) n (- n 256)))
    (('make-int16 n1 n2)
     (let ((n (+ (* n1 256) n2)))
       (if (< n 32768) n (- n 65536))))
    (('make-char8 n)
     (integer->char n))
    (('load-string s) s)
    (('load-symbol s) (string->symbol s))
    (('load-keyword s) (make-keyword-from-dash-symbol (string->symbol s)))
    (else #f)))

(define (code->bytes code)
  (let* ((code (code-pack code))
	 (inst (car code))
	 (rest (cdr code))
	 (head (make-string 1 (integer->char (instruction->opcode inst))))
	 (len (instruction-length inst)))
    (cond ((< len 0)
	   ;; Variable-length code
	   (let ((str (car rest)))
	     (string-append head (encode-length (string-length str)) str)))
	  ((= len (length rest))
	   ;; Fixed-length code
	   (string-append head (list->string (map integer->char rest))))
	  (else
	   (error "Invalid code:" code)))))

(define (make-byte-decoder bytes)
  (let ((addr 0) (size (string-length bytes)))
    (define (pop)
      (let ((byte (char->integer (string-ref bytes addr))))
	(set! addr (1+ addr))
	byte))
    (lambda ()
      (if (< addr size)
	  (let* ((start addr)
		 (inst (opcode->instruction (pop)))
		 (n (instruction-length inst))
		 (code (if (< n 0)
			   ;; variable length
			   (let* ((end (+ (decode-length pop) addr))
				  (str (substring bytes addr end)))
			     (set! addr end)
			     (list inst str))
			   ;; fixed length
			   (do ((n n (1- n))
				(l '() (cons (pop) l)))
			       ((= n 0) (cons* inst (reverse! l)))))))
	    (values start code))
	  #f))))


;;;
;;; Variable-length interface
;;;

;; NOTE: decoded in vm_fetch_length in vm.c as well.

(define (encode-length len)
  (define C integer->char)
  (cond ((< len 254) (string (C len)))
	((< len (* 256 256))
	 (string (C 254) (C (quotient len 256)) (C (modulo len 256))))
	((< len most-positive-fixnum)
	 (string (C 255)
		 (C (quotient len (* 256 256 256)))
		 (C (modulo (quotient len (* 256 256)) 256))
		 (C (modulo (quotient len 256) 256))
		 (C (modulo len 256))))
	(else (error "Too long code length:" len))))

(define (decode-length pop)
  (let ((len (pop)))
    (cond ((< len 254) len)
	  ((= len 254) (+ (* (pop) 256) (pop)))
	  (else (+ (* (pop) 256 256 256)
		   (* (pop) 256 256)
		   (* (pop) 256)
		   (pop))))))
