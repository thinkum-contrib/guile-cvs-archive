(define (foreign-source . args) #f)
(defmacro define-foreign args #f)
(defmacro define-record-discloser args #f)

(define ascii->char integer->char)
(define char->ascii char->integer)
(define (make-immutable! thing) thing)
(define (immutable? thing) #f)
(define (unspecific) (if #f #f))
(define (reading-error port message . irritants)
  (apply error message (append irritants (list port))))

;; just pick out the begin forms.
(defmacro define-structure (name interface . body)
  (let loop ((rest body)
	     (result '(begin)))
    (if (null? rest)
	(reverse result)
	(loop (cdr rest)
	      (if (eq? (caar rest) 'begin)
		  (cons (car rest) result)
		  result)))))

(use-modules (ice-9 slib))
(require 'values)
(require 'format)

(load-from-path "scsh/alt-syntax.scm")
(load-from-path "scsh/receive.scm")
(load-from-path "scsh/let-opt.scm")

(load-from-path "scsh/syntax-helpers.scm")

;; "delete" primitive is replaced, but doesn't seem worth saving.
(load-from-path "scsh/utilities.scm")
;; replace procedures in utilities.scm with guile primitives.
(set! index string-index)
;; note the different convention for rindex starting position.
(set! rindex (lambda (str char . start)
	       (apply string-rindex str char 0 start)))

(load-from-path "scsh/scsh-version.scm")
(load-from-path "scsh/fname.scm")
(load-from-path "scsh/errno.scm")
(load-from-path "scsh/defrec.scm")
(load-from-path "scsh/enumconst.scm")
(load-from-path "scsh/weak.scm")
(load-from-path "scsh/fluid.scm")
(load-from-path "scsh/population.scm")
(load-from-path "scsh/stringcoll.scm")
(load-from-path "scsh/bitwise.scm")
(load-from-path "scsh/condition.scm")
(load-from-path "scsh/scsh-condition.scm")
(load-from-path "scsh/re.scm")
(load-from-path "scsh/syscalls.scm")
(load-from-path "scsh/syntax.scm")
(load-from-path "scsh/fileinfo.scm")
(load-from-path "scsh/glob.scm")
(load-from-path "scsh/filemtch.scm")
(load-from-path "scsh/filesys.scm")
(load-from-path "scsh/here.scm")
(load-from-path "scsh/time.scm")
(load-from-path "scsh/newports.scm")
(load-from-path "scsh/rw.scm")
(load-from-path "scsh/char-set.scm")
(load-from-path "scsh/rdelim.scm")
(load-from-path "scsh/awk.scm")
(load-from-path "scsh/fr.scm")
(load-from-path "scsh/netconst.scm")
(load-from-path "scsh/network.scm")
(load-from-path "scsh/sighandlers.scm")
(load-from-path "scsh/procobj.scm")
(load-from-path "scsh/scsh.scm")

(init-scsh-vars #f)
(set! command-line-arguments (cdr (command-line)))
