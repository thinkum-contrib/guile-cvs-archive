(module (ice-9 files)

	(open (ice-9 posix)
	      (ice-9 guile)
	      (ice-9 provide)
	      ((ice-9 strorder) string=?))

	(export scm-line-incrementors 
		read-line
		file-exists?
		file-is-directory?
		has-suffix?
		load-from-path
		(dev (alias stat:dev))
		(ino (alias stat:ino))
		(mode (alias stat:mode))
		(nlink (alias stat:nlink))
		(uid (alias stat:uid))
		(gid (alias stat:gid))
		(rdev (alias stat:rdev))
		(size (alias stat:size))
		(atime (alias stat:atime))
		(mtime (alias stat:mtime))
		(ctime (alias stat:ctime))
		(blksize (alias stat:blksize))
		(blocks (alias stat:blocks))
		(type (alias stat:type))
		(perms (alias stat:perms))
		read-delimited
		read-delimited!
		))
		

;;; {Line and Delimited I/O}

;;; corresponds to SCM_LINE_INCREMENTORS in libguile.
(define scm-line-incrementors "\n")

(define (read-line! string . maybe-port)
  (let* ((port (if (pair? maybe-port)
		   (car maybe-port)
		   (current-input-port))))
    (let* ((rv (%read-delimited! scm-line-incrementors
				 string
				 #t
				 port))
	   (terminator (car rv))
	   (nchars (cdr rv)))
      (cond ((and (= nchars 0)
		  (eof-object? terminator))
	     terminator)
	    ((not terminator) #f)
	    (else nchars)))))

(define (read-delimited! delims buf . args)
  (let* ((num-args (length args))
	 (port (if (> num-args 0)
		   (car args)
		   (current-input-port)))
	 (handle-delim (if (> num-args 1)
			   (cadr args)
			   'trim))
	 (start (if (> num-args 2)
		    (caddr args)
		    0))
	 (end (if (> num-args 3)
		  (cadddr args)
		  (string-length buf))))
    (let* ((rv (%read-delimited! delims
				 buf
				 (not (eq? handle-delim 'peek))
				 port
				 start
				 end))
	   (terminator (car rv))
	   (nchars (cdr rv)))
      (cond ((or (not terminator)	; buffer filled
		 (eof-object? terminator))
	     (if (zero? nchars)
		 (if (eq? handle-delim 'split)
		     (cons terminator terminator)
		     terminator)
		 (if (eq? handle-delim 'split)
		     (cons nchars terminator)
		     nchars)))
	    (else
	     (case handle-delim
	       ((trim peek) nchars)
	       ((concat) (string-set! buf nchars terminator)
			 (+ nchars 1))
	       ((split) (cons nchars terminator))
	       (else (error "unexpected handle-delim value: " 
			    handle-delim))))))))
  
(define (read-delimited delims . args)
  (let* ((port (if (pair? args)
		   (let ((pt (car args)))
		     (set! args (cdr args))
		     pt)
		   (current-input-port)))
	 (handle-delim (if (pair? args)
			   (car args)
			   'trim)))
    (let loop ((substrings ())
	       (total-chars 0)
	       (buf-size 100))		; doubled each time through.
      (let* ((buf (make-string buf-size))
	     (rv (%read-delimited! delims
				   buf
				   (not (eq? handle-delim 'peek))
				   port))
	     (terminator (car rv))
	     (nchars (cdr rv))
	     (join-substrings
	      (lambda ()
		(apply string-append
		       (reverse
			(cons (if (and (eq? handle-delim 'concat)
				       (not (eof-object? terminator)))
				  (string terminator)
				  "")
			      (cons (make-shared-substring buf 0 nchars)
				    substrings))))))
	     (new-total (+ total-chars nchars)))
	(cond ((not terminator)
	       ;; buffer filled.
	       (loop (cons (substring buf 0 nchars) substrings)
		     new-total
		     (* buf-size 2)))
	      ((eof-object? terminator)
	       (if (zero? new-total)
		   (if (eq? handle-delim 'split)
		       (cons terminator terminator)
		       terminator)
		   (if (eq? handle-delim 'split)
		       (cons (join-substrings) terminator)
		       (join-substrings))))
	      (else
	       (case handle-delim
		   ((trim peek concat) (join-substrings))
		   ((split) (cons (join-substrings) terminator))


		   (else (error "unexpected handle-delim value: "
				handle-delim)))))))))

;;; read-line [PORT [HANDLE-DELIM]] reads a newline-terminated string
;;; from PORT.  The return value depends on the value of HANDLE-DELIM,
;;; which may be one of the symbols `trim', `concat', `peek' and
;;; `split'.  If it is `trim' (the default), the trailing newline is
;;; removed and the string is returned.  If `concat', the string is
;;; returned with the trailing newline intact.  If `peek', the newline
;;; is left in the input port buffer and the string is returned.  If
;;; `split', the newline is split from the string and read-line
;;; returns a pair consisting of the truncated string and the newline.

(define (read-line . args)
  (let* ((port		(if (null? args)
			    (current-input-port)
			    (car args)))
	 (handle-delim	(if (> (length args) 1)
			    (cadr args)
			    'trim))
	 (line/delim	(%read-line port))
	 (line		(car line/delim))
	 (delim		(cdr line/delim)))
    (case handle-delim
      ((trim) line)
      ((split) line/delim)
      ((concat) (if (and (string? line) (char? delim))
		    (string-append line (string delim))
		    line))
      ((peek) (if (char? delim)
		  (unread-char delim port))
	      line)
      (else
       (error "unexpected handle-delim value: " handle-delim)))))



;;; {Files}
;;;
;;; If no one can explain this comment to me by 31 Jan 1998, I will
;;; assume it is meaningless and remove it. -twp
;;;   !!!! these should be implemented using Tcl commands, not fports.

;; Using the vector returned by stat directly is probably not a good
;; idea (it could just as well be a record).  Hence some accessors.
(define (dev f) (vector-ref f 0))
(define (ino f) (vector-ref f 1))
(define (mode f) (vector-ref f 2))
(define (nlink f) (vector-ref f 3))
(define (uid f) (vector-ref f 4))
(define (gid f) (vector-ref f 5))
(define (rdev f) (vector-ref f 6))
(define (size f) (vector-ref f 7))
(define (atime f) (vector-ref f 8))
(define (mtime f) (vector-ref f 9))
(define (ctime f) (vector-ref f 10))
(define (blksize f) (vector-ref f 11))
(define (blocks f) (vector-ref f 12))

;; derived from stat mode.
(define (type f) (vector-ref f 13))
(define (perms f) (vector-ref f 14))

(define file-exists?
  (if (feature? 'posix)
      (lambda (str)
	(access? str F_OK))
      (lambda (str)
	(let ((port (catch 'system-error (lambda () (open-file str OPEN_READ))
			   (lambda args #f))))
	  (if port (begin (close-port port) #t)
	      #f)))))

(define file-is-directory?
  (if (feature? 'i/o-extensions)
      (lambda (str)
	(eq? (type (stat str)) 'directory))
      (lambda (str)
	(display str)
	(newline)
	(let ((port (catch 'system-error
			   (lambda () (open-file (string-append str "/.")
						 OPEN_READ))
			   (lambda args #f))))
	  (if port (begin (close-port port) #t)
	      #f)))))

(define (has-suffix? str suffix)
  (let ((sufl (string-length suffix))
	(sl (string-length str)))
    (and (> sl sufl)
	 (string=? (substring str (- sl sufl) sl) suffix))))


;;; {Loading by paths}

;;; Load a Scheme source file named NAME, searching for it in the
;;; directories listed in %load-path, and applying each of the file
;;; name extensions listed in %load-extensions.
(define (load-from-path name)
  (start-stack 'load-stack
	       (primitive-load-path name (interaction-environment))))

