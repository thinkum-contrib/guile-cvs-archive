;;;; url.scm: URL manipulation tools.

(define-module (www url)
  :use-module (ice-9 regex))

;;;; 	Copyright (C) 1997 Free Software Foundation, Inc.
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

;;;; TODO:
;;;;   * support `user:password@' strings in URLs.
;;;;   * make URL parsing smarter.  This is good for most TCP/IP-based
;;;;	 URL schemes, but parsing is actually specific to each URL scheme.
;;;;   * fill out url:encode, include facilities for URL-scheme-specific
;;;;     encoding methods (e.g. a url-scheme-reserved-char-alist)

(define url-regexp (make-regexp "([^:]+):(//([^:/]+))?(:([0-9]+)?)?(/(.*))?"))

(define-public (url:parse url)
  (let ((url-match (regexp-exec url-regexp url)))
    (let ((url-scheme (string->symbol (match:substring url-match 1)))
	  (host (match:substring url-match 3))
	  (port (cond ((match:substring url-match 5) => string->number)
		      (else #f)))
	  (path (match:substring url-match 7)))
      (vector url-scheme host port path))))

;; `url:scheme' is an unfortunate term, but it is the technical
;; name for that portion of the URL according to RFC 1738. Sigh.

(define-public (url:scheme url) (vector-ref url 0))
(define-public (url:host url)   (vector-ref url 1))
(define-public (url:port url)   (vector-ref url 2))
(define-public (url:path url)   (vector-ref url 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (url-decode STR)
;; Turn + into space, and hex-encoded %XX strings into their
;; eight-bit characters.  Is a regexp faster than character
;; scanning?  Does it incur more overhead (which may be more
;; important for code that frequently gets restarted)?

(define-public (url:decode str)
  (regexp-substitute/global #f "\\+|%([0-9A-Fa-f][0-9A-Fa-f])" str
    'pre
    (lambda (m)
      (cond ((string=? "+" (match:substring m 0)) " ")
	    (else (integer->char
		   (string->number
		    (match:substring m 1)
		    16)))))
    'post))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (url-encode STR)
;; The inverse of url-decode.  Can't be done easily with
;; a regexp: we would have to construct a regular expression
;; like "[\277-\377]", for example, and Guile strings don't
;; let you interpolate character literals.  Pity.
;;   URL-encode any characters in STR that are not safe: these
;; include any character not in the SAFE-CHARS list and any
;; character that *is* in the RESERVED-CHARS list argument.

(define-public (url:encode str reserved-chars)
  (with-output-to-string
    (lambda ()
      (for-each (lambda (ch)
		  (if (and (safe-char? ch)
			   (not (memv ch reserved-chars)))
		      (display ch)
		      (begin
			(display #\%)
			(display (number->string (char->integer ch) 16)))))
		(string->list str)))))

(define safe-chars
  '(#\$ #\- #\_ #\. #\+ #\! #\* #\' #\( #\) #\, #\; #\/ #\? #\: #\@ #\& #\=))

(define (safe-char? ch)
  ;; ``Thus, only alphanumerics, the special characters "$-_.+!*'(),", and
  ;; reserved characters used for their reserved purposes may be used
  ;; unencoded within a URL.'' RFC 1738, #2.2.
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (memv ch safe-chars)))
