;;;; Hex tetris game experiment
; (C)2007 Justin Heyes-Jones

(in-package #:cl-user)  

; window or screen height TODO this must go somewhere better
(defparameter *WINDOW-WIDTH* 640)
(defparameter *WINDOW-HEIGHT* 480)

;;;; UTIL

(load "util.lisp")

;;;; SYSTEMS
(load "sprites.lisp") ; sprite drawing and management

;;;; GAME 
(load "gameobjects.lisp") ; generic game objects

(load "hexboard.lisp") ; managing the data and drawing of a hex board

;;;; DATA 

; a list of game objects
; todo this should be part of the engine maybe?
(defparameter *game-objects* nil)

(defun show-frame-rate()
  (sdl:draw-string-centered-* (format nil "fps: ~a" (sdl:frame-rate)) (screen-center-x) (screen-center-y)
					:surface sdl:*default-display*))

(defparameter *mouse-click-x* -1)
(defparameter *mouse-click-y* -1)

(defun show-debug-info()
  (let ((hb (car *game-objects*)))
    (sdl:draw-string-centered-* (format nil "ring: ~a index: ~a." 
					(hb-screen-x-y-to-ring hb
							       *mouse-click-x* *mouse-click-y*)
					(hb-screen-x-y-to-ring-index hb
							       *mouse-click-x* *mouse-click-y*))
				100 15 :surface sdl:*default-display*)))

; specific objects for this game

; todo gameobjects
(defun add-object(object)
  (push object *game-objects*))

; todo gameobjects
(defun init-game-objects()
  ; add game objects to update loop
  (add-object (make-hex-board 15 6 15 (screen-center-x) (screen-center-y)))
)

(defun update-game-objects()
  (loop for game-object in *game-objects* do
	(update game-object (/ 1.0 (sdl:frame-rate)))))

(defun draw-game-objects()
  (loop for game-object in *game-objects* do
	(draw game-object)))

(defun hexgame()
  "hex game"
  (sdl:with-init ()			;Initialise SDL
      (setf (sdl:frame-rate) 30) ; Set target framerate (or 0 for unlimited)
      (sdl:window *WINDOW-WIDTH* *WINDOW-HEIGHT* :title-caption "Hex" :icon-caption "Hex")
      (progn
	;; init your game
	(sdl:initialise-default-font)
	(init-game-objects)
	(sdl:with-events  ()
	  (:quit-event () t)
	  (:mouse-button-down-event (:x x :y y)
				    (setf *mouse-click-x* x)
				    (setf *mouse-click-y* y))
	  (:key-down-event (:key key)
			   (if (sdl:key= key :SDL-KEY-ESCAPE)
			       (sdl:push-quit-event)))
	  (:idle () ;; redraw screen on idle
		 ;; fill the background
		 (sdl:clear-display (sdl:color :r #x00 :g #x00 :b #x00))
		 ;; Do stuff
;		 (show-frame-rate)
		 (show-debug-info)
		 (update-game-objects)
		 (draw-game-objects)
		 ;; Update the whole screen 
		 (sdl:update-display)))
	(setf *game-objects* nil))))



