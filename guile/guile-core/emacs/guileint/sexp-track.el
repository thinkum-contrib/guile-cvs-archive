;;; sexp-track.el -- track status of top-level Lisp/Scheme forms

;;;; Copyright (C) 2003 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later
;;;; version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;;; 02111-1307 USA

;;; Commentary:

;; Sexp tracking is maintaining a list of all the top level forms in
;; the buffer, and for each one keeping track of whether it is
;; syntactically complete, whether it has been modified since last
;; sent to a Lisp or Scheme process, etc.

;;; Code:

;; Intended public interface.

(defvar sexp-track-start-regexp "^("
  "*Regexp matching start of each top level form.")

(defvar sexp-track-end-ignore-regexp "^\\(\\s-*\\|\\s-*;.*\\)$"
  "*Regexp matching lines to ignore at end of each top level form.")

(defun sexp-track-toggle (&optional arg)
  "Toggle sexp tracking for the current buffer.
If ARG is specified it should be a number.  It means to turn sexp
tracking on or off according as ARG is positive or negative."
  (interactive "P")
  (if arg
      (if (> arg 0)
	  (sexp-track-scan)
	(sexp-track-killall))
    (if (null sexp-track-whole-buffer-overlay)
	(sexp-track-scan)
      (sexp-track-killall))))

(defun sexp-track-mark-up-to-date (beg end)
  "Mark top level forms in the specified region as up to date."
  (let ((os (sexp-track-overlays-in beg end)))
    (while os
      (if (and (>= (overlay-start (car os)) beg)
	       (<= (overlay-end (car os)) end))
	  (overlay-put (car os) 'after-string sexp-track-up-to-date-indicator))
      (setq os (cdr os)))))

(defgroup sexp-track nil
  "Library for tracking status of top level forms in a Lisp or Scheme buffer."
  :group 'lisp)

(defcustom sexp-track-modified-indicator "[*]"
  "*String used to indicate that the preceding form has been modified."
  :type 'string 
  :group 'sexp-track)

(defcustom sexp-track-up-to-date-indicator "[-]"
  "*String used to indicate that the preceding form is up to date."
  :type 'string
  :group 'sexp-track)

(defcustom sexp-track-broken-indicator "[?]"
  "*String used to indicate that the preceding form is syntactically broken."
  :type 'string
  :group 'sexp-track)

(defcustom sexp-track-face 'default
  "*Face used to show top level forms when sexp tracking is on."
  :type 'face
  :group 'sexp-track)

;; Implementation.

(defvar sexp-track-whole-buffer-overlay nil)

(defun sexp-track-killall ()
  (delete-overlay sexp-track-whole-buffer-overlay)
  (setq sexp-track-whole-buffer-overlay nil)
  (mapcar (function (lambda (o)
		      (if (overlay-get o 'sexp-track)
			  (delete-overlay o))))
	  (overlays-in (point-min) (point-max))))

(defun sexp-track-scan ()
  (or sexp-track-whole-buffer-overlay
      (set (make-local-variable 'sexp-track-whole-buffer-overlay)
	   (make-overlay (point-min) (point-max))))
  (overlay-put sexp-track-whole-buffer-overlay 'modification-hooks
	       (list (function sexp-track-modified)))
  (sexp-track-rescan))

(defun sexp-track-rescan ()
  (save-excursion
    (goto-char (point-min))
    (let ((start (and (re-search-forward sexp-track-start-regexp nil t)
		      (match-beginning 0))))
      (while start
	(setq start (sexp-track-install-overlay start))))))

(defun sexp-track-install-overlay (expr-start)
  (let* ((next-start (and (re-search-forward sexp-track-start-regexp nil t)
			  (match-beginning 0)))
	 (expr-end
	  (save-excursion
	    (goto-char (or next-start (point-max)))
	    (while (save-excursion
		     (forward-line -1)
		     (and (> (point) expr-start)
			  (looking-at sexp-track-end-ignore-regexp)))
	      (forward-line -1))
	    (point))))
    (sexp-track-check-syntax (sexp-track-get-make-overlay expr-start expr-end))
    next-start))

(defun sexp-track-overlays-in (start end)
  ;; Like `overlays-in' but only includes overlays with `sexp-track'
  ;; property `t', and orders the overlays by their starting position
  ;; (with the first in the buffer first).
  (let ((os (delq sexp-track-whole-buffer-overlay (overlays-in start end)))
	(stos '()))
    (while os
      (if (overlay-get (car os) 'sexp-track)
	  (setq stos (cons (car os) stos)))
      (setq os (cdr os)))
    (sort stos (function (lambda (x y)
			   (< (overlay-start x)
			      (overlay-start y)))))))

(defun sexp-track-get-make-overlay (start end)
  (let ((os (sexp-track-overlays-in start end)))
    ;; Delete all but the first overlay found, as they're about to be
    ;; overwritten.
    (while (cdr os)
      (delete-overlay (cadr os))
      (setcdr os (cddr os)))
    ;; Now either move the existing overlay or create a new one.
    (if (car os)
	(progn
	  (move-overlay (car os) start end)
	  (car os))
      (let ((o (make-overlay start end)))
	(overlay-put o 'sexp-track t)
	(overlay-put o 'face sexp-track-face)
	o))))

(defvar sexp-track-modification-overlays nil
  "List of overlays in range of a buffer modification.")

(defun sexp-track-modified (o afterp beg end &optional oldlen)
   (if afterp
       (save-match-data
	 (let (deactivate-mark
	       (rescan-start
		(save-excursion
                  (goto-char beg)
		  (re-search-backward sexp-track-start-regexp nil t)))
	       (rescan-end
		(save-excursion
		  (goto-char end)
		  (or (and (re-search-forward sexp-track-start-regexp nil t)
			   (match-beginning 0))
		      (point-max)))))
	   (while sexp-track-modification-overlays
	     (if (= (overlay-start (car sexp-track-modification-overlays))
		    (overlay-end (car sexp-track-modification-overlays)))
		 (delete-overlay (car sexp-track-modification-overlays)))
	     (setq sexp-track-modification-overlays
		   (cdr sexp-track-modification-overlays)))	  
	   (if rescan-start
	       (save-restriction
		 (narrow-to-region rescan-start rescan-end)
		 (sexp-track-rescan)))
	   ;;(message "Just modified! %S" (list beg end oldlen))
	   ))
     (set (make-local-variable 'sexp-track-modification-overlays)
	  (sexp-track-overlays-in beg end))))

(defun sexp-track-check-syntax (o)
  (overlay-put o 'after-string
	       (if (save-excursion
		     (goto-char (overlay-start o))
		     (condition-case nil
			 (forward-sexp 1)
		       (error))
		     (or (= (point) (overlay-end o))
			 (and (char-equal (char-after (point)) ?\n)
			      (= (+ (point) 1) (overlay-end o)))))
		   sexp-track-modified-indicator
		 sexp-track-broken-indicator)))

;;; sexp-track.el ends here.
