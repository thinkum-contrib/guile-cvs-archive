(defun lookup-face-create (face &optional force)
  "Get a FACE, or create it if it doesn't exist.  In order for it to
properly create the face, the following naming convention must be used:
    [reverse-](fgcolor[/bgcolor])[-bold][-italic][-underline]

Each color is either the name of an X color (see .../X11/lib/X11/rgb.txt),
a hexadecimal specification of the form \"hex-[0-9A-Fa-f]+\", or \"default\".

An optional argument, FORCE, will cause the face to be recopied from the
default...which is probably of use only if you've changed fonts."

  ;; make the face if we need to...
  (let* ((fn (symbol-name face))
	 (frame (selected-frame))
	 (basefont (cdr (assq 'font (frame-parameters frame))))
	 error fgcolor bgcolor)
    (cond
     ((or (null face)			
	  ;;(memq face hilit-predefined-face-list)
	  )
      ;; do nothing if the face is nil or if it's predefined.
      )
     ((or force
	  (not (memq face (face-list)))
	  (not (string= (get face 'basefont) basefont)))
      (copy-face 'default 'scratch-face)
      (if (string-match "^reverse-?" fn)
	  (progn (invert-face 'scratch-face)
		 (setq fn (substring fn (match-end 0)))))

      ;; parse foreground color
      (if (string-match "^\\(hex-\\)?\\([A-Za-z0-9]+\\)" fn)
	  (setq fgcolor (concat
			 (if (match-beginning 1) "#")
			 (substring fn (match-beginning 2) (match-end 2)))
		fn (substring fn (match-end 0)))
	(error "bad face name %S" face))

      ;; parse background color
      (if (string-match "^/\\(hex-\\)?\\([A-Za-z0-9]+\\)" fn)
	  (setq bgcolor (concat
			 (and (match-beginning 1) "#")
			 (substring fn (match-beginning 2) (match-end 2)))
		fn (substring fn (match-end 0))))
      
      (and (string= "default" fgcolor) (setq fgcolor nil))
      (and (string= "default" bgcolor) (setq bgcolor nil))
      
      ;; catch errors if we can't allocate the color(s)
      (condition-case nil
	  (progn (set-face-foreground 'scratch-face fgcolor)
		 (set-face-background 'scratch-face bgcolor)
		 (copy-face 'scratch-face face)
		 (put face 'basefont basefont))
	(error (message "couldn't allocate color for '%s'"
			(symbol-name face))
	       (setq face 'default)
	       (setq error t)))
      (or error
	  ;; don't bother w/ bold or italic if we didn't get the color
	  ;; we wanted, but ignore errors making the face bold or italic
	  ;; if the font isn't available, there's nothing to do about it...
	  (progn
	    ;(set-face-font face nil frame) MDJ 020919 <djurfeldt@nada.kth.se>
	    (set-face-underline-p face (string-match "underline" fn))
	    (if (string-match ".*bold" fn)
		;; make face bold in all frames
		(make-face-bold face nil 'noerr))
	    (if (string-match ".*italic" fn)
		;; make face italic in all frames
		(make-face-italic face nil 'noerr))
	    ))
      )))
  face)

(provide 'fcreate)
