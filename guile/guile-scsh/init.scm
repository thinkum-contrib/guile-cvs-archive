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

(defmacro structure-ref (structure symb)
  symb)

;; FIXME: check whether slib is still necessary.
(use-modules (ice-9 slib))
(require 'values)
(require 'format)

(load-from-path "scsh/alt-syntax.scm")
(load-from-path "scsh/receive.scm")
(load-from-path "scsh/let-opt.scm")

(load-from-path "scsh/syntax-helpers.scm")

;; "delete" primitive is replaced, but doesn't seem worth saving.
(load-from-path "scsh/utilities.scm")

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
(load-from-path "scsh/jar-defrecord.scm")
(load-from-path "scsh/char-set.scm")

(use-modules (regex spencer))

(define guile-regexp? regexp?)
(load-from-path "scsh/rx/re-low.scm")
(load-from-path "scsh/rx/re-high.scm")
(load-from-path "scsh/rx/let-match.scm")
(load-from-path "scsh/rx/spencer.scm")
(load-from-path "scsh/rx/oldfuns.scm")
(load-from-path "scsh/rx/cond-package.scm")
(load-from-path "scsh/rx/parse.scm")
(load-from-path "scsh/rx/posixstr.scm")
(load-from-path "scsh/rx/re-fold.scm")
(load-from-path "scsh/rx/re-subst.scm")
(load-from-path "scsh/rx/re-syntax.scm")
(load-from-path "scsh/rx/rx-lib.scm")
(load-from-path "scsh/rx/simp.scm")
(load-from-path "scsh/rx/re.scm")

(define-syntax rx expand-rx)
(define-syntax if-sre-form
  (lambda (exp r c)
    (if (sre-form? (cadr exp) r c)
	(caddr exp)
	(cadddr exp))))

(load-from-path "scsh/lib/ccp.scm")
(load-from-path "scsh/lib/list-lib.scm")

;; replaces string-downcase, string-downcase!, string-upcase, string-upcase!
(load-from-path "scsh/lib/string-lib.scm")

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
