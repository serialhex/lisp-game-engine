;;;; Simple game engine
; (C)2006 Justin Heyes-Jones

(in-package #:cl-user)  

; set font load path
(defvar *font-path* (merge-pathnames "font.bmp" (or *load-truename* *default-pathname-defaults*)))

; TEMP load in other files here instead of as package
; TODO load in a loader file 

; window or screen height TODO this must go somewhere better
(defparameter *WINDOW-WIDTH* 640)
(defparameter *WINDOW-HEIGHT* 480)
(defparameter *display-surface* nil)

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
  (let ((num-objects 500))
    (loop for count to num-objects do
	  (push
	   (make-instance 'game-sprite-object :x (random-range 0.0 640.0) :y (random-range 0.0 480.0) 
			  :vx 0.0 :vy 0.0
			  :ax (random-range -0.01 0.01) :ay (random-range -0.01 0.01)
			  :sprite-def waddle-sprite :current-frame 'waddle-idle-2)
	   *game-objects*))))

(defun update-game-objects()
  (loop for game-object in *game-objects* do
	(update game-object (/ 1.0 (sdl:get-framerate)))))

(defun draw-game-objects()
  (loop for game-object in *game-objects* do
	(draw game-object)))

(defun game1()
  "example of simple game"
  (sdl:with-init ()			;Initialise SDL
      (sdl:set-framerate 30) ; Set target framerate (or 0 for unlimited)
      (sdl:with-display (*WINDOW-WIDTH* *WINDOW-HEIGHT* :flags sdl:SDL_ANYFORMAT)
      (let* ((small-font 
	      (sdl-simple-font:initialise-font (namestring *font-path*) 4 5 
					       "abcdefghijklmnopqrstuvwxyz:'!?_-,.()#~0123456789" #(99 0 0)))
	     (text-image (sdl-simple-font:make-text-image small-font "draw text image")))
	;; init your game
	(init-game-objects)
	(sdl:with-events  ()
	  (:quit () t)
	  (:keydown (:key key)
		    (if (sdl:key= key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle () ;; redraw screen on idle
	   ;; fill the background
	   (sdl:clear-display :color (vector #x22 #x22 #x44))
	   ;; Do stuff
	   (sdl-simple-font:draw-string-centered "basic game engine demo"
						 :surface sdl:*default-display*
						 :font small-font 
						 :position (sdl:point (screen-center-x) (screen-center-y)))
	   (update-game-objects)
	   (draw-game-objects)
	   ;; Update the whole screen 
	   (sdl:update-display)))
	(sdl-simple-font:free-text-image text-image) ; clean up
	(sdl-simple-font:close-font small-font)
	(sprites:flush-image-cache))))
  t)
