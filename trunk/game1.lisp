;;;; Simple game engine
; (C)2007 Justin Heyes-Jones

(in-package #:cl-user)  

; TEMP load in other files here instead of as package
; TODO load in a loader file 

; window or screen height TODO this must go somewhere better
(defparameter *WINDOW-WIDTH* 640)
(defparameter *WINDOW-HEIGHT* 480)

;;;; SYSTEMS
(load "sprites.lisp") ; sprite drawing and management

;;;; GAME 
(load "gameobjects.lisp") ; generic game objects

;;;; DATA 
(load "spritedefs1.lisp") ; sample sprite definitions

; a list of game objects
(defparameter *game-objects* nil)

(defun screen-center-x() (ash *window-width* -1))
(defun screen-center-y() (ash *window-height* -1))

;;; todo this should go in a util file
(defun random-range(min max)
  (let ((diff (abs (- min max))))
    (+ min (random diff))))

(defun init-game-objects()
  ; add some game objects
  (let ((num-objects 5))
    (loop for count from 1 to num-objects do
	  (push
	   (make-instance 'physics-sprite 
			  :x (random-range 0.0 640.0) :y (random-range 0.0 480.0) 
			  :vx 0.0 :vy 0.0
			  :ax (random-range -0.001 0.001) :ay (random-range -0.001 0.001)
			  :ax 0.0 :ay 0.0
			  :sprite-def waddle-sprite :current-frame 'waddle-idle-2
			  :speed 2.0)
	   *game-objects*))))

(defun update-game-objects()
  (loop for game-object in *game-objects* do
	(update game-object (/ 1.0 (sdl:frame-rate)))))

(defun draw-game-objects()
  (loop for game-object in *game-objects* do
	(draw game-object)))

(defun game1()
  "example of game engine"
  (sdl:with-init ()			;Initialise SDL
      (setf (sdl:frame-rate) 60) ; Set target framerate (or 0 for unlimited)
      (sdl:window *WINDOW-WIDTH* *WINDOW-WIDTH* :title-caption "simple game engine" :icon-caption "simple game engine")
      (progn 
	;; init your game
	(sdl:initialise-default-font)
	(init-game-objects)
	(sdl:with-events  ()
	  (:quit-event () t)
	  (:keydown (:key key)
		    (if (sdl:key= key :SDLK_ESCAPE)
			(sdl:push-quit-event)))
	  (:idle () ;; redraw screen on idle
		 ;; fill the background
		 (sdl:clear-display (sdl:color :r #x22 :g #x22 :b #x44))
		 ;; Do stuff
		 (show-frame-rate)
		 (update-game-objects)
		 (draw-game-objects)
		 ;; Update the whole screen 
		 (sdl:update-display)))
	(sprites:flush-image-cache)
	(setf *game-objects* nil))))

(defun show-frame-rate()
  (sdl:draw-string-centered-* (format nil "fps: ~a" (sdl:frame-rate)) (screen-center-x) (screen-center-y)
					:surface sdl:*default-display*))


