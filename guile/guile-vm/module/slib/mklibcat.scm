;"mklibcat.scm" Build catalog for SLIB
;Copyright (C) 1997 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(call-with-output-file (in-vicinity (implementation-vicinity) "slibcat")
  (lambda (op)
    (display ";\"slibcat\" SLIB catalog for " op)
    (display (scheme-implementation-type) op)
    (display (scheme-implementation-version) op)
    (display ".        -*-scheme-*-" op) (newline op)
    (display ";" op) (newline op)
    (display "; DO NOT EDIT THIS FILE -- it is automagically generated" op)
    (newline op) (newline op)

    (display "(" op) (newline op)
    (for-each
     (lambda (asp) (display " " op) (write asp op) (newline op))
     (append
      (list (cons 'schelog
		  (in-vicinity (sub-vicinity (library-vicinity) "schelog")
			       "schelog"))
	    (cons 'portable-scheme-debugger
		  (in-vicinity (sub-vicinity (library-vicinity) "psd")
			       "psd-slib"))
	    (cons 'jfilter
		  (in-vicinity (sub-vicinity (library-vicinity) "jfilter")
			       "jfilter")))
      (map (lambda (p)
	     (if (symbol? (cdr p)) p
		 (cons
		  (car p)
		  (if (pair? (cdr p))
		      (cons
		       (cadr p)
		       (in-vicinity (library-vicinity) (cddr p)))
		      (in-vicinity (library-vicinity) (cdr p))))))
	   '(
	     (rev4-optional-procedures	.	"sc4opt")
	     (rev2-procedures		.	"sc2")
	     (multiarg/and-		.	"mularg")
	     (multiarg-apply		.	"mulapply")
	     (rationalize		.	"ratize")
	     (transcript		.	"trnscrpt")
	     (with-file			.	"withfile")
	     (dynamic-wind		.	"dynwind")
	     (dynamic			.	"dynamic")
	     (fluid-let		defmacro	.	"fluidlet")
	     (alist			.	"alist")
	     (hash			.	"hash")
	     (sierpinski		.	"sierpinski")
	     (soundex			.	"soundex")
	     (hash-table		.	"hashtab")
	     (logical			.	"logical")
	     (random			.	"random")
	     (random-inexact		.	"randinex")
	     (modular			.	"modular")
	     (factor			.	"factor")
	     (primes			.	factor)
	     (charplot			.	"charplot")
	     (sort			.	"sort")
	     (tsort			.	topological-sort)
	     (topological-sort		.	"tsort")
	     (common-list-functions	.	"comlist")
	     (tree			.	"tree")
	     (coerce			.	"coerce")
	     (format			.	"format")
	     (generic-write		.	"genwrite")
	     (pretty-print		.	"pp")
	     (pprint-file		.	"ppfile")
	     (object->string		.	"obj2str")
	     (string-case		.	"strcase")
	     (stdio			.	"stdio")
	     (printf			.	"printf")
	     (scanf			.	"scanf")
	     (line-i/o			.	"lineio")
	     (string-port		.	"strport")
	     (getopt			.	"getopt")
	     (debug			.	"debug")
	     (qp			.	"qp")
	     (break	defmacro	.	"break")
	     (trace	defmacro	.	"trace")
	     (eval			.	"eval")
	     (record			.	"record")
	     (promise			.	"promise")
	     (synchk			.	"synchk")
	     (defmacroexpand		.	"defmacex")
	     (macro-by-example	defmacro	.	"mbe")
	     (syntax-case		.	"scainit")
	     (syntactic-closures	.	"scmacro")
	     (macros-that-work		.	"macwork")
	     (macro			.	macro-by-example)
	     (object			.	"object")
	     (yasos		macro	.	"yasyn")
	     (oop			.	yasos)
	     (collect		macro	.	"collect")
	     (struct	defmacro	.	"struct")
	     (structure	syntax-case	.	"structure")
	     (values			.	"values")
	     (queue			.	"queue")
	     (priority-queue		.	"priorque")
	     (array			.	"array")
	     (array-for-each		.	"arraymap")
	     (repl			.	"repl")
	     (process			.	"process")
	     (chapter-order		.	"chap")
	     (posix-time		.	"psxtime")
	     (common-lisp-time		.	"cltime")
	     (time-zone			.	"timezone")
	     (relational-database	.	"rdms")
	     (database-utilities	.	"dbutil")
	     (database-browse		.	"dbrowse")
	     (html-form			.	"htmlform")
	     (alist-table		.	"alistab")
	     (parameters		.	"paramlst")
	     (getopt-parameters		.	"getparam")
	     (read-command		.	"comparse")
	     (batch			.	"batch")
	     (glob			.	"glob")
	     (filename			.	glob)
	     (make-crc			.	"makcrc")
	     (fft			.	"fft")
	     (wt-tree			.	"wttree")
	     (string-search		.	"strsrch")
	     (root			.	"root")
	     (minimize			.	"minimize")
	     (precedence-parse		.	"prec")
	     (parse			.	precedence-parse)
	     (commutative-ring		.	"cring")
	     (self-set			.	"selfset")
	     (determinant		.	"determ")
	     (byte			.	"byte")
	     (tzfile			.	"tzfile")
	     (schmooz			.	"schmooz")
	     (net-clients		.	"nclients")
	     (db->html			.	"db2html")
	     (http			.	"http-cgi")
	     (cgi			.	http)
	     (uri			.	"uri")
	     (uniform-resource-identifier .	uri)
	     (pnm			.	"pnm")
	     (metric-units		.	"simetrix")
	     (new-catalog		.	"mklibcat")
	     ))))
    (display " " op)

    (let* ((req (in-vicinity (library-vicinity)
			     (string-append "require" (scheme-file-suffix)))))
      (write (cons '*SLIB-VERSION* (or (require:version req) *SLIB-VERSION*))
	     op))
    (newline op)
    (display ")" op) (newline op)

    (let ((load-if-exists
	   (lambda (path)
	     (cond ((not (file-exists? path))
		    (set! path (string-append path (scheme-file-suffix)))))
	     (cond ((file-exists? path)
		    (slib:load-source path))))))
      ;;(load-if-exists (in-vicinity (implementation-vicinity) "mksitcat"))
      (load-if-exists (in-vicinity (implementation-vicinity) "mkimpcat")))

    (let ((catcat
	   (lambda (vicinity name specificity)
	     (let ((path (in-vicinity vicinity name)))
	       (and (file-exists? path)
		    (call-with-input-file path
		      (lambda (ip)
			(newline op)
			(display "; " op)
			(write path op)
			(display " SLIB " op)
			(display specificity op)
			(display "-specific catalog additions" op)
			(newline op) (newline op)
			(do ((c (read-char ip) (read-char ip)))
			    ((eof-object? c))
			  (write-char c op)))))))))
      (catcat (library-vicinity) "sitecat" "site")
      (catcat (implementation-vicinity) "implcat" "implementation")
      (catcat (implementation-vicinity) "sitecat" "site"))
    ))

(set! *catalog* #f)