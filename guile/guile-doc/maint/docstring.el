
;;; docstring.el
;;;
;;; Emacs Lisp utilities for synchronising Guile docstrings between
;;; the reference manual and the C or Scheme source files.
;;;
;;; Written by Neil Jerram, July 2000.


;;; {Guile Doc String Utilities}

;;; Doc strings for primitive Guile procedures written in C are
;;; defined in the C source code as an integral part of the C function
;;; definition.  But we also want to incorporate these doc strings in
;;; the reference manual, and in a way that does not require an
;;; additional auto-generation step while building or editing the
;;; manual.  Also, if a doc string is edited in the reference manual,
;;; we want it to be easy to merge the changes back into the
;;; corresponding C source file; and vice versa.

;;; The solution is to give the C source files and the reference
;;; manual equal status in the mastering of doc strings, and to
;;; provide tools that make it easy to keep the two masters in sync.
;;; The design for how we do this is as follows.

;;; 1.  There is a "canonical form" for doc strings, intermediate
;;; between the doc string representations that are required in C and
;;; in the reference manual.  (In practice, the canonical form is the
;;; same as how the doc string appears in the reference manual.)

;;; 2.  Whenever we know that the C source and the reference manual
;;; are in sync, we calculate the MD5 digest for the canonical form of
;;; the doc string, and save it in a comment in the reference manual.

;;; 3.  When asked to resync the C source and the reference manual, we
;;; calculate the MD5 digests for the canonical forms of both the C
;;; doc string and manual doc string, and compare these digests with
;;; the one saved in the manual.  If both match, there is nothing to
;;; do.  If one matches but not the other, we know that the
;;; non-matching doc string is more recent.  If neither matches, we
;;; have a conflict that must be resolved by hand.

;;; 4.  If there is no saved MD5 digest in the reference manual,
;;; behave as though the saved digest is the MD5 value for "".

;;; 5.  If a doc string is missing in either C source or reference
;;; manual, behave as though the calculated digest for the doc string
;;; is the MD5 value for "".

(require 'md5)

(defun split-manual-doc-parts (manual-doc)
  (let (md5 offset canonical)

    ;; Look for saved MD5 digest.
    (if (string-match "^@c docstring md5 \\(\"[^\"]*\"\\)\n" manual-doc)
        (setq md5 (car (read-from-string manual-doc
                                         (match-beginning 1)
                                         (match-end 1)))
              offset (match-end 0)
              manual-doc (substring manual-doc offset))
      (setq md5 (md5 "")
            offset 0))

    ;; Whatever remains is the canonical doc string, except for the
    ;; trailing newline.
    (setq canonical
          (if (and (> (length manual-doc) 0)
                   (string-equal (substring manual-doc (1- (length manual-doc))) "\n"))
              (substring manual-doc 0 (1- (length manual-doc)))
            manual-doc))

    ;; Return a list of all the bits.
    (list md5 offset canonical)))

(defun insert-manual-doc-parts (md5 canonical)

  ;; Insert MD5 digest comment.
  (insert "@c docstring md5 ")
  (prin1 md5 (current-buffer))
  (insert "\n")

  ;; Insert doc string.
  (insert canonical)
  (insert "\n"))

(defvar manual-overlay nil)

(defun query-sync-doc-strings ()
  (interactive)
  (while (query-sync-next-doc-string))
  (message "All doc strings processed"))

(defun query-sync-next-doc-string ()
  (interactive)
  (if (re-search-forward "^@c docstring begin " nil t)

      ;; Evaluate the Lisp expression following begin, which should return
      ;; the canonical form of the appropriate source file doc string.
      (let* ((doc-string-spec (read (current-buffer)))
	     (doc-string-info (save-excursion (eval doc-string-spec)))
	     (doc-string-data (nth 0 doc-string-info))
	     (doc-string-canonical-fn (nth 1 doc-string-info))
	     (doc-string-display-fn (nth 2 doc-string-info))
	     (doc-string-replace-fn (nth 3 doc-string-info))
	     (doc-string-undisplay-fn (nth 4 doc-string-info))
	     (source-canonical (save-excursion (funcall doc-string-canonical-fn doc-string-data))))

	;; Move to the beginning of the following line.
	(forward-line 1)
	(let ((manual-beg (point)))

	  ;; Find the end of the manual doc string.
	  (re-search-forward "^@end deffn$")
	  (forward-line 1)
	  (let* ((manual-end (point))
		 (manual-doc (buffer-substring manual-beg manual-end))
		 (manual-doc-parts (split-manual-doc-parts manual-doc))
		 (manual-saved-md5 (nth 0 manual-doc-parts))
		 (manual-offset    (nth 1 manual-doc-parts))
		 (manual-canonical (nth 2 manual-doc-parts)))

	    ;; Display manual and source buffers.
	    (delete-other-windows)
	    (if manual-overlay
		nil
	      (setq manual-overlay (make-overlay 0 0 (current-buffer)))
	      (overlay-put manual-overlay 'face 'highlight))
	    (move-overlay manual-overlay
			  (+ manual-beg manual-offset)
			  manual-end
			  (current-buffer))
	    (save-excursion (funcall doc-string-display-fn doc-string-data))
        
	    ;; Calculate the MD5 digests of the current doc strings from
	    ;; source and reference manual.
	    (let ((source-md5 (md5 source-canonical))
		  (manual-md5 (md5 manual-canonical)))

	      (cond
	   
	       ;; Case 1: both calculated MD5 digests match the saved
	       ;; value.
	       ((and (string-equal source-md5 manual-saved-md5)
		     (string-equal manual-md5 manual-saved-md5))
		(message "Doc strings for %s match" 
			 doc-string-spec))

	       ;; Case 2: source file MD5 digest matches the saved value.
	       ((string-equal source-md5 manual-saved-md5)
		(if (y-or-n-p "Copy doc string from reference manual to source file? ")
		    (progn
		      (save-excursion
			(funcall doc-string-replace-fn doc-string-data manual-canonical))
		      (goto-char manual-beg)
		      (delete-region manual-beg manual-end)
		      (insert-manual-doc-parts manual-md5 manual-canonical))))

	       ;; Case 3: manual MD5 digest matches the saved value.
	       ((string-equal manual-md5 manual-saved-md5)
		(if (y-or-n-p "Copy doc string from source file to reference manual? ")
		    (progn
		      (goto-char manual-beg)
		      (delete-region manual-beg manual-end)
		      (insert-manual-doc-parts source-md5 source-canonical))))

	       ;; Case 4: neither calculated digest matches the saved
	       ;; value, but they match each other.
	       ((string-equal source-md5 manual-md5)
		(if (y-or-n-p "Doc strings match.  Update saved MD5 digest? ")
		    (progn
		      (goto-char manual-beg)
		      (delete-region manual-beg manual-end)
		      (insert-manual-doc-parts source-md5 source-canonical))))

	       ;; Case 5: neither calculated digest matches the saved
	       ;; value, and they are different.
	       (t
		(cond ((y-or-n-p "CONFLICT!  Copy doc string from source file to reference manual? ")
		       (goto-char manual-beg)
		       (delete-region manual-beg manual-end)
		       (insert-manual-doc-parts source-md5 source-canonical))
		      ((y-or-n-p "CONFLICT!  Copy doc string from reference manual to source file? ")
		       (save-excursion
			 (funcall doc-string-replace-fn doc-string-data manual-canonical))
		       (goto-char manual-beg)
		       (delete-region manual-beg manual-end)
		       (insert-manual-doc-parts manual-md5 manual-canonical))
		      ((y-or-n-p "CONFLICT!  Skip to next docstring? "))
		      (t
		       (message "Doc strings for %s are in conflict"
				doc-string-spec)
		       (error "Doc strings for %s are in conflict"
			      doc-string-spec))))

	       ))

	    (move-overlay manual-overlay 0 0)
	    (save-excursion (funcall doc-string-undisplay-fn doc-string-data))))

	t)

    (message "No more doc strings in this buffer!")
    nil))


;;; {Doc strings from C source files}

(defvar c-doc-string-overlay nil)

(defvar c-doc-string-path
  '("/home/neil/Guile/guile-ssh/guile/guile-core/libguile")
  "*List of directories in which to search for C source files.")

(defun c-doc-string (file name)

  (let ((path c-doc-string-path)
        (c-file-buffer nil))

    ;; Find the specified file in the C doc string path.
    (while (and path (null c-file-buffer))
      (if (file-readable-p (concat (car path) "/" file))
          (setq c-file-buffer
                (find-file-noselect (concat (car path) "/" file))))
      (setq path (cdr path)))

    (or c-file-buffer
        (error "%s not found in C doc string path!" file))

    (save-excursion
      (set-buffer c-file-buffer)

      ;; Return a list containing data for this doc string, and further
      ;; Lisp functions to do various things to that data.
      (list (cons c-file-buffer
                  (c-doc-string-data name))
            'c-doc-string-canonical
            'c-doc-string-display
            'c-doc-string-replace
            'c-doc-string-undisplay))))

;;; c-doc-string-data PROC-NAME
;;;
;;; If the current buffer contains a C style doc string for a
;;; procedure called PROC-NAME, return the region containing the doc
;;; string as a cons (beg . end).  If not, return nil.

(defun c-doc-string-data (proc-name)
  (interactive "sProcedure name: ")
  (save-excursion
    (goto-char (point-min))
    (let ((region nil)
          (deffn nil))

      ;; Search for occurrences of the target procedure name, enclosed
      ;; in double quotes.
      (while (and (null region)
                  (search-forward (concat "\"" proc-name "\"") nil t))
        (let ((name-end (match-end 0)))

	  ;; (Re-)Initialize the deffn summary string.
	  (setq deffn (concat "@deffn primitive " proc-name))

          ;; Find the start of the preceding SCM_DEFINE form.
          (save-excursion
            (if (re-search-backward "^SCM_DEFINE1?" nil t)
                (let (def-end)

                  ;; Auto-generate a @deffn line from the argument list.
                  (save-excursion
                    (let (req opt rst (opttail ""))
		      (cond
		       ((looking-at "SCM_DEFINE1")
			(re-search-forward "SCM_DEFINE1 *(")
			(forward-sexp 2)
			(re-search-forward ", *\\([^,]+\\),[^(]*(")
			(let ((proc-type (buffer-substring (match-beginning 1)
							   (match-end 1))))
			  (cond
			   ((string-equal proc-type "scm_tc7_rpsubr")
			    (setq req 2 opt 0 rst 0))
			   ((string-equal proc-type "scm_tc7_asubr")
			    (setq req 2 opt 0 rst 0))
			   (t
			    (error "Unhandled SCM_DEFINE1 procedure type: %s" proc-type)))))

		       (t ;; SCM_DEFINE
			(re-search-forward "SCM_DEFINE *(")
			(forward-sexp 2)
			(re-search-forward ", *\\([0-9]+\\), *\\([0-9]+\\), *\\([0-9]+\\),[^(]*(")
			(setq req (string-to-int (buffer-substring (match-beginning 1)
								   (match-end 1)))
			      opt (string-to-int (buffer-substring (match-beginning 2)
								   (match-end 2)))
			      rst (string-to-int (buffer-substring (match-beginning 3)
								   (match-end 3))))))

                      (while (> req 0)
                        (re-search-forward " *SCM *\\([^,)]+\\)[,)]")
                        (setq deffn
                              (concat deffn " " (buffer-substring (match-beginning 1)
                                                                  (match-end 1)))
                              req (1- req)))

                      (while (> opt 0)
                        (re-search-forward " *SCM *\\([^,)]+\\)[,)]")
                        (setq deffn
                              (concat deffn " [" (buffer-substring (match-beginning 1)
								   (match-end 1)))
                              opttail
                              (concat opttail "]")
                              opt
                              (1- opt)))
                      (setq deffn (concat deffn opttail))

                      (if (> rst 0)
                          (progn
                            (re-search-forward " *SCM *\\([^,)]+\\)[,)]")
                            (setq deffn
                                  (concat deffn " . " (buffer-substring (match-beginning 1)
                                                                        (match-end 1))))))))

                  ;; Skip over the SCM_DEFINE and its arguments.
                  (forward-sexp 2)
                  (setq def-end (point))
                  ;; Check whether this SCM_DEFINE form encloses the
                  ;; current occurrence of the target procedure name.
                  (if (> def-end name-end)
                      ;; It does, so this is the correct SCM_DEFINE.
                      (progn
                        ;; Find the start of the doc string.  This is
                        ;; the first quotation mark after the end of
                        ;; the Scheme level name.
                        (goto-char name-end)
                        (if (search-forward "\"" def-end t)
                            (progn
                              (forward-char -1)
                              (setq region (cons (point) nil))
                              ;; Skip over sexps as long as they are
                              ;; strings.
                              (while (and (condition-case nil
                                              (progn
                                                (forward-sexp)
                                                (< (point) def-end))
                                            ((error) nil))
                                          (char-equal (char-before) 34))
                                (setcdr region (point)))

                              
                              )))))))))
      (or region
          (error "%s not found in %s" proc-name (buffer-file-name)))
      (cons deffn region))))

(defun c-doc-string-canonical (c-doc-string-data)
  (let ((buf (car c-doc-string-data))
        (deffn (cadr c-doc-string-data))
        (region (cddr c-doc-string-data))
        doc)
    (set-buffer buf)
    (save-excursion
      (goto-char (car region))
      (while (< (point) (cdr region))
        (setq doc
              (concat doc
                      ;; Use read here as a trick to
                      ;; automatically handle newline escape
                      ;; sequences in the doc string.
                      (read buf)))))
    (concat (if (string-match "^@deffn " doc)
                doc
              (concat deffn "\n" doc))
            "\n"
            "@end deffn")))

(defun c-doc-string-display (c-doc-string-data)
  (let ((buf (car c-doc-string-data))
        (region (cddr c-doc-string-data))
        doc)
    (set-buffer buf)
    (if c-doc-string-overlay
        nil
      (setq c-doc-string-overlay (make-overlay 0 0 buf))
      (overlay-put c-doc-string-overlay 'face 'highlight))
    (move-overlay c-doc-string-overlay
                  (car region)
                  (cdr region)
                  buf)
    (goto-char (car region))
    (display-buffer buf)))

(defun c-doc-string-undisplay (c-doc-string-data)
  (set-buffer (car c-doc-string-data))
  (if c-doc-string-overlay
      (move-overlay c-doc-string-overlay 0 0)))

(defun c-doc-string-replace (c-doc-string-data new-canonical)
  (let ((buf (car c-doc-string-data))
        (deffn (cadr c-doc-string-data))
        (region (cddr c-doc-string-data)))

    ;; Trim off the @deffn line if it matches what we would
    ;; autogenerate for this procedure.
    (if (eq (string-match (concat "^" (regexp-quote deffn) "\n")
                          new-canonical) 0)
        (setq new-canonical (substring new-canonical (match-end 0))))

    ;; Trim off the @end deffn line, which should always be there.
    (setq new-canonical 
          (substring new-canonical 
                     0 
                     (string-match "\n@end deffn$" new-canonical)))

    ;; Write what remains to the C file.
    (save-excursion
      (set-buffer buf)
      (goto-char (car region))
      (delete-region (car region) (cdr region))
      (while (string-match "^\\([^\n]*\\)\n+" new-canonical)
        (prin1 (substring new-canonical
                          (match-beginning 1)
                          (match-end 1))
               buf)
        (setq new-canonical
              (substring new-canonical (match-end 0)))
        (forward-char -1)
	(let ((num-newlines (- (match-end 0) (match-end 1))))
	  (while (> num-newlines 0)
	    (insert "\\n")
	    (setq num-newlines (1- num-newlines))))
        (forward-char 1)
        (insert "\n")
        (c-indent-command))
      (prin1 new-canonical buf))))


;;; make-function-list
;;;
;;; Converts the contents of guile-procedures.txt into a
;;; concise summary of all primitive procedures in Guile.

(defun make-function-list ()
  (interactive)
  (let ((buf (get-buffer-create "*Function List*"))
        name
        file
        alist
        entry)
    (goto-char (point-min))
    (while (re-search-forward "\n(\\([^ )]+\\)" nil t)
      (setq name (buffer-substring (match-beginning 1) (match-end 1)))
      (re-search-forward "\\[\\([^:]+\\)")
      (setq file (buffer-substring (match-beginning 1) (match-end 1)))
      (setq entry (assoc file alist))
      (if entry
          (setcdr entry (append (cdr entry) (list name)))
        (setq alist
              (cons (list file name) alist))))
    (set-buffer buf)
    (erase-buffer)
    (mapcar (function (lambda (s)
                        (insert "=== "
                                (car s)
                                " ===\n\n"
                                (car (cdr s)))
                        (mapcar (function (lambda (p)
                                            (insert ", " p)))
                                (cdr (cdr s)))
                        (insert "\n\n")))
            (reverse alist))))


;;; guile-procedures-to-manual-docstring-comments
;;;
;;; Converts the contents of guile-procedures.txt into a set of
;;; reference manual docstring comments.

(defun guile-procedures-to-manual-docstring-comments ()
  (interactive)
  (let ((buf (get-buffer-create "*Manual Docstring Comments*"))
        name
        file
        alist)
    (goto-char (point-min))
    (while (re-search-forward "\n(\\([^ )]+\\)" nil t)
      (setq name (buffer-substring (match-beginning 1) (match-end 1)))
      (re-search-forward "\\[\\([^:]+\\)")
      (setq file (buffer-substring (match-beginning 1) (match-end 1)))
      (setq alist (cons (cons file name) alist)))
    (set-buffer buf)
    (erase-buffer)
    (mapcar (function (lambda (s)
                        (insert "@c docstring begin (c-doc-string \""
                                (car s)
                                "\" \""
                                (cdr s)
				"\")\n\n")))
            (reverse alist))
    (goto-char (point-min))
    (display-buffer buf)))

;;; post-ediff-merge
;;;
;;; This function is useful in the following sequence of events:
;;; (1) grep *.texi for c-doc-string, uniq and sort the output
;;; (2) use guile-procedures-to-manual-docstring-comments to get the
;;;     list of all docstring comments corresponding to guile-procedures.txt,
;;;     sort the output
;;; (3) use Ediff Merge Buffers to merge the results of (1) and (2)
;;; (4) use post-ediff-merge to remove the bits that are common between
;;;     the results of (1) and (2).

(defun post-ediff-merge ()
  (goto-char (point-min))
  (query-replace-regexp "\\(end of combination\\)[^<]*\\(<<<<<<<\\)" "\\1\n\\2" nil))

(provide 'docstring)
