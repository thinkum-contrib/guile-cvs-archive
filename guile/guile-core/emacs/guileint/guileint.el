;;; NAME:	 guileint.el
;;; SYNOPSIS:	 A Guile/Emacs interface prototype
;;; VERSION:	 1.5
;;; LAST CHANGE: 2002-10-19
;;; CREATED:	 1997-07-17
;;; AUTHOR:	 Mikael Djurfeldt <djurfeldt@nada.kth.se>
;;; COPYRIGHT:	 (C) 1997, 2002 Mikael Djurfeldt
;;;
;;;  Verbatim copies of this file may be freely redistributed.
;;;
;;;  Modified versions of this file may be redistributed provided that this
;;;  notice remains unchanged, the file contains prominent notice of
;;;  author and time of modifications, and redistribution of the file
;;;  is not further restricted in any way.
;;;
;;;  This file is distributed `as is', without warranties of any kind.
;;;
;;; REQUIREMENTS:
;;;
;;; USAGE:
;;;
;;; BUGS:
;;;

(require 'cl-19 "cl")

(setq scheme-program-name
      (let ((v (getenv "SCHEME_PROGRAM_NAME")))
	(or v
	    (concat "guile"
		    (and window-system " --emacs")))))

;;; Select buffers to pop up as separate windows
(if window-system
    (progn
      (defvar default-special-display-buffer-names
	special-display-buffer-names)
      (setq special-display-buffer-names
	    (union default-special-display-buffer-names '("*scheme*")))

      (setq same-window-buffer-names
	    (delete "*scheme*" same-window-buffer-names))

      (setq special-display-frame-alist
	    '((height . 24) (width . 80) (unsplittable . t)))
      ))

;;; Menus
;;;

(require 'defmenu)

;(setq menu-bar-final-items
;      '(completion inout signals scheme help-menu))
(setq menu-bar-final-items
      '(interpret scheme help-menu))

;; The global menu
;;
(define-menu global-map 'interpret "Interpret"
  '(("Guile" run-scheme (not (comint-check-proc "*scheme*")))
    ("Switch to *scheme*" guile-switch-to-scheme
     (comint-check-proc "*scheme*"))))

(load "inda-scheme")

(provide 'guileint)
