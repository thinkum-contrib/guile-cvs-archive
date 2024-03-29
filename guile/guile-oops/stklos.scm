;;;; 	Copyright (C) 1999 Free Software Foundation, Inc.
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


(define-module (oop goops stklos)
  :use-module (oop goops internal)
  :no-backtrace
  )

;;;
;;; This is the stklos compatibility module.
;;;
;;; WARNING: This module is under construction.  While we expect to be able
;;; to run most stklos code without problems in the future, this is not the
;;; case now.  The current compatibility is only superficial.
;;;
;;; Any comments/complaints/patches are welcome.  Tell us about
;;; your incompatibility problems (bug-guile@gnu.org).
;;;

;; Export all bindings that are exported from (oop goops)...
(module-for-each (lambda (sym var)
		   (module-add! %module-public-interface sym var))
		 (nested-ref the-root-module '(app modules oop goops
						   %module-public-interface)))

;; ...but replace the following bindings:
(export define-class define-method)

;; Also export the following
(export write-object)

;;; Enable keyword support (*fixme*---currently this has global effect)
(read-set! keywords 'prefix)

(define standard-define-class-transformer
  (macro-transformer standard-define-class))

(define define-class
  ;; Syntax
  (let ((name cadr)
	(supers caddr)
	(slots cadddr)
	(rest cddddr))
    (procedure->macro
      (lambda (exp env)
	(standard-define-class-transformer
	 `(define-class ,(name exp) ,(supers exp) ,@(slots exp)
	    ,@(rest exp))
	 env)))))

(define define-method
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((name (cadr exp)))
	(if (and (pair? name)
		 (eq? (car name) 'setter)
		 (pair? (cdr name))
		 (null? (cddr name)))
	    (let ((name (cadr name)))
	      (cond ((not (symbol? name))
		     (goops-error "bad method name: ~S" name))
		    ((defined? name env)
		     `(begin
			(if (not (is-a? ,name <generic-with-setter>))
			    (define-accessor ,name))
			(add-method! (setter ,name) (method ,@(cddr exp)))))
		    (else
		     `(begin
			(define-accessor ,name)
			(add-method! (setter ,name) (method ,@(cddr exp)))))))
	    (cond ((not (symbol? name))
		   (goops-error "bad method name: ~S" name))
		  ((defined? name env)
		   `(begin
		      (if (not (or (is-a? ,name <generic>)
				   (is-a? ,name <primitive-generic>)))
			  (define-generic ,name))
		      (add-method! ,name (method ,@(cddr exp)))))
		  (else
		   `(begin
		      (define-generic ,name)
		      (add-method! ,name (method ,@(cddr exp)))))))))))
