;;; installed-scm-file

;;;; 	Copyright (C) 1998, 2002 Free Software Foundation, Inc.
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

(define-module (tcltk breakout)
  :use-module (ice-9 slib)
  :use-module (ice-9 threads)
  :use-module (tcltk tcltk))

(require 'random)

(tk-main-window)

;; Make sure that bind is tk-bind (in case we're in the root module)
;(define bind (nested-ref the-root-module
;			 '(app modules tcltk tcltk %module-public-interface bind)))

;; How big a canvas?
;;
(define play-w 520)
(define play-h 520)

;; Where does the play area start
;;
(define bounds-x 4)
(define bounds-y 4)

;; Where is the paddle ul cornder?
;;
(define paddle-x 0)
(define paddle-y 375)

;; Where is the puck center?
;;
(define puck-x 0)
(define puck-r 5)
(define puck-y (- paddle-y puck-r 1))

;; Paddle size:
;;
(define paddle-height 10)
(define paddle-width 64)

;; How big is the in-bounds area for the puck?
;;
(define bounds-w 512)
(define bounds-h puck-y)

;; How many blocks per row?
;;
(define n-row 16)

;; Row y positions
;;
(define row0-y 32)
(define row1-y 64)

;; Individual block size
;;
(define row-height 16)
(define row-width (/ bounds-w n-row))

;; Each entry either the name of a canvas
;; item for the block or #f if the block
;; has been eliminated:
;;
(define row0 (make-vector n-row #f))
(define row1 (make-vector n-row #f))


;; Puck dynamic
;;
(define puck-max-vx 4.4)
(define puck-max-vy 4.4)
(define puck-init-init-vx 1.0)
(define puck-init-init-vy -1.0)
(define puck-init-vx 1.0)
(define puck-init-vy -1.0)

(define puck-vx puck-init-vx)
(define puck-vy puck-init-vy)
(define (coin-toss . from)
  (list-ref from (random (length from))))
(define (puck-tick)
  (let ((old-x puck-x)
	(old-y puck-y))
    (set! puck-x (+ puck-x puck-vx))
    (set! puck-y (+ puck-y puck-vy))
    (cond

     ((or (and (< puck-y (+ row1-y row-height))
	       (>= puck-y row1-y)
	       (hit-puck-at-game-x!? row1 puck-x)
	       row1-y)
	  (and (< puck-y (+ row0-y row-height))
	       (>= puck-y row0-y)
	       (hit-puck-at-game-x!? row0 puck-x)
	       row0-y)
	  (and (< puck-y 0)
	       0))
      => (lambda (yref)
	   (report-score)
	   (set! puck-vy (- puck-vy))
	   (set! puck-y (+ yref (- puck-y yref)))
	   (if (= 0 n-blocks)
	       (begin
		 (win-level)
		 (set! old-y puck-y)
		 (set! old-x puck-x)))))

     ((<= bounds-h puck-y)
      (cond
       ((paddle-sweet? puck-x)
	(begin
	  (set! puck-vy (- puck-vy))
	  (set! puck-y (+ bounds-h (- bounds-h puck-y)))))
       ((paddle-sour? puck-x)
	(begin
	  (set! puck-vy (- puck-vy))
	  (let ((total (+ (* puck-vx puck-vx) (* puck-vy puck-vy))))
	    (set! puck-vx ((coin-toss + -) (random puck-init-vx)))
	    (set! puck-vy (- (sqrt (- total (* puck-vx puck-vx))))))
	  (set! puck-y (+ bounds-h (- bounds-h puck-y)))))
       (else
	(lose-level)
	(set! old-y puck-y)
	(set! old-x puck-x))))
       

     ((< puck-x 0)
      (begin
	(set! puck-vx (- puck-vx))
	(set! puck-x (- puck-x))))

     ((<= bounds-w puck-x)
      (begin
	(set! puck-vx (- puck-vx))
	(set! puck-x (+ bounds-w (- bounds-w puck-x))))))
     
    (move-puck  (- puck-x old-x) (- puck-y old-y))))



(define (paddle-sweet? x)
  (and (> (abs puck-vx) .00001)
       (let ((r (/ paddle-width 2)))
	 (< (abs (- x (+ r paddle-x)))
	    r))))

(define (paddle-sour? x)
  (let ((r (/ paddle-width 2)))
    (< (abs (- x (+ r paddle-x)))
       (+ (* 3 puck-r) r))))


;; Hooks

(define n-blocks #f)
(define (new-level)
  (.game.c 'delete 'all)
  (set! score-report #f)
  (set! puck-report #f)
  (set! game-over-report #f)
  (make-row! row0 row0-y 'green)
  (make-row! row1 row1-y 'blue)
  (set! n-blocks (* 2 n-row))
  (new-puck)
  (new-paddle)
  (report-game-state))

(define (new-game)
  (set! game-playable #t)
  (set! game-playing #f)
  (set! puck-init-vx puck-init-init-vx)
  (set! puck-init-vy puck-init-init-vy)
  (new-level)
  (set! n-pucks pucks-per-game)
  (set! score 0))


;; Make the window and playing area
;;
(define play-geom (string-append (number->string play-w)
				 "x"
				 (number->string play-h)))
(define (setup-geom)
  (wm 'minsize '.game play-w play-h)
  (wm 'maxsize '.game play-w play-h)
  (wm 'geometry '.game play-geom)
  (canvas '.game.c)
  (pack '.game.c :fill "both" :expand #t))


;; Make the two rows:
;;
(define (make-row! v y color)
  (let loop ((n 0))
    (if (= n 16)
	v
	(begin
	  (vector-set! v n (.game.c 'create 'rectangle
				    (+ bounds-x (* n row-width))
				    (+ bounds-y y)
				    (+ bounds-x (* (+ 1 n) row-width))
				    (+ bounds-y (+ y row-height))
				    :fill color
				    :width 3))
	  (loop (+ n 1))))))

(define (hit-puck-at-game-x!? row x-game)
  (let* ((x (- x-game bounds-x))
	 (i (inexact->exact (floor (/ x row-width)))))
    (and (>= i 0)
	 (< i (vector-length row))
	 (vector-ref row i)
	 (begin
	   (.game.c 'delete (vector-ref row i))
	   (vector-set! row i #f)
	   (set! n-blocks (- n-blocks 1))
	   (set! score (+ 1 score))
	   #t))))


;; Drawing the paddle:
;;
(define (paddle-x-max) (+ paddle-x paddle-width))
(define (paddle-y-max) (+ paddle-y paddle-height))
(define paddle-color 'red)
(define paddle #f)
(define (new-paddle)
  (set! paddle
	(.game.c 'create 'rectangle
		 (+ bounds-x paddle-x)
		 (+ bounds-y paddle-y)
		 (+ bounds-x (paddle-x-max))
		 (+ bounds-y (paddle-y-max))
		 :fill paddle-color)))

(define (center-paddle-at-canvas-coord x)
  (let ((old-x paddle-x))
    (set! paddle-x (- x bounds-x (/ paddle-width 2)))
    (.game.c 'move paddle (- paddle-x old-x) 0)))


;; Drawing the puck:
;;

(define puck-color 'purple)
(define puck #f)
(define (new-puck)
  (set! puck-y (- paddle-y puck-r 1))
  (set! puck-x 0)
  (set! puck-vx puck-init-vx)
  (set! puck-vy puck-init-vy)
  (set! puck 
	(.game.c 'create 'oval
		 (+ bounds-x (- puck-x puck-r))
		 (+ bounds-y (- puck-y puck-r))
		 (+ bounds-x (+ puck-x puck-r))
		 (+ bounds-y (+ puck-y puck-r))
		 :fill puck-color)))

(define (move-puck dx dy)
  (.game.c 'move puck dx dy))

(define (remove-puck)
  (.game.c 'delete puck)
  (new-puck))


(define (loop)
  (let loop ()
    (puck-tick)
    (usleep 1000)
    (if game-playing
	(loop))))




(define game-playable #f)
(define game-playing #f)
(define game-quit #f)
(define score 0)
(define pucks-per-game 3)
(define n-pucks pucks-per-game)
(define score-report #f)
(define puck-report #f)
(define game-over-report #f)

(define (lose-level)
  (remove-puck)
  (set! game-playing #f)
  (if (> n-pucks 0)
      (begin
	(set! n-pucks (+ -1 n-pucks)))
      (set! game-playable #f)))

(define (win-level)
  (set! n-pucks (+ 1 n-pucks))
  (remove-puck)
  (set! game-playing #f)
  (set! score (+ 25 score))
  (set! puck-init-vy (* puck-init-vy 2))
  (set! puck-init-vx (* puck-init-vx 2))
  (if (< puck-init-vx puck-max-vx)
      (new-level)
      (set! game-playable #f)))


(define (report-score)
  (and score-report (.game.c 'delete score-report))
  (set! score-report
	(.game.c 'create 'text 10 (+ 64 paddle-y)
		 :font "-adobe-helvetica-bold-r-normal-*-24-*-*-*-*-*-*-*"
		 :anchor 'w))
  (.game.c 'insert score-report 0
	   (string-append "Score: " (number->string score))))


(define (report-game-state)  
  (report-score)
  (and puck-report (.game.c 'delete puck-report))
  (set! puck-report
	(.game.c 'create 'text 200 (+ 64 paddle-y)
		 :font "-adobe-helvetica-bold-r-normal-*-24-*-*-*-*-*-*-*"
		 :fill (cond
			((not game-playable) 'thistle4)
			((eq? n-pucks 0) 'red)
			(else 'navy))
		 :anchor 'w))
  (.game.c 'insert puck-report 0
	   (if (not game-playable)
	       "GAME OVER"
	       (string-append "Pucks remaining: " (number->string n-pucks))))
  (and game-over-report (.game.c 'delete game-over-report))
  (set! game-over-report
	(.game.c 'create 'text 10 (+ 96 paddle-y)
		 :font "-adobe-helvetica-bold-o-normal-*-18-*-*-*-*-*-*-*"
		 :fill (if (not game-playable) 'red 'ForestGreen)
		 :anchor 'w))
  (.game.c 'insert game-over-report 0
	   (cond
	    ((not game-playable) "`P' to start a new game;  `Q' to quit")
	    (game-playing "`P' to pause;  `Q' to quit this game")
	    (else  "`P' to play;  `Q' to quit"))))


(define cv (make-condition-variable))

(define-public (play-game)
  (if (not (tcl-defined? the-interpreter ".game"))
      (begin
	(toplevel '.game)
	(setup-geom)
	(setup-bindings)))
  (new-game)
  (set! game-quit #f)
  (let ((mu (make-mutex)))
    (lock-mutex mu)
    (let game-loop ()
      (report-game-state)
      (wait-condition-variable cv mu)
      (if (not game-quit)
	  (begin
	    (set! game-playing #t)
	    (loop)
	    (game-loop)))))
  (destroy '.game))



(proc game-q ignored
      (cond
       (game-playing (set! game-playing #f) (new-game))
       ((not game-playable)
	(new-game)
	(report-game-state))
       (else (set! game-quit #t)
	     (signal-condition-variable cv))))

(proc game-p ignored
      (cond
       (game-playing (set! game-playing #f))
       (else (signal-condition-variable cv))))

(define (setup-bindings)
  (bind '.game.c "<Motion>"
	(tcl-lambda ("%x %y" (number x) (number y))
		    (center-paddle-at-canvas-coord x)
		    ""))
  (bind '.game.c '<q> 'game-q)
  (bind '.game.c '<Q> 'game-q)
  
  (bind '.game.c '<p> 'game-p)
  (bind '.game.c '<P> 'game-p)
  (focus '.game.c))

;(play-game)
