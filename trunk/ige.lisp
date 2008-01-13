;;;; A component based, data driven, interactive game engine
; (C)2008 Justin Heyes-Jones

(in-package :cl-user)  

; Window or screen height and screen mode (full screen or not)
; Note that these are set up with defparameter which creates
; variables with global extent, and will reset them if you reload.
; Using defvar here would enable you to keep the existing values
; on reloads if you wanted that behaviour instead.
(defparameter *WINDOW-WIDTH* 640)
(defparameter *WINDOW-HEIGHT* 480)
(defparameter *FULL-SCREEN-P* nil)
(defparameter *BG-COLOR* (sdl:color :r #x22 :g #x22 :b #x44))

;;;; temp - this is global data for now that will be part of the input 
;;;; input system, or the message system

(defparameter *player-1-key-up-held* nil)
(defparameter *player-1-key-down-held* nil)

;;; This should be platform independent somehow
;;; for now this will work for me
#+sbcl (defparameter *bmp-path* "/home/justinhj/lisp-game-engine/")
#-sbcl (defparameter *bmp-path* nil)

;;;; Totally generic systems
(load "util")
(load "sprites") ; sprite drawing, animation and file handling

;;; A bit more game engine specific systems

;(load "gameobjects") ; generic game objects
(load "components")

;;;; Game specific data - eventually this should be elsewhere

(load "spritedefs1") ; sample sprite definitions
(load "pong") ; components and code for being a pong game

; this variable ensures proper use of engine-init
; engine-run and engine-quit
(defparameter *engine-active* nil)

(defun make-left-pong-player()
  (let ((phys (make-instance '2d-physics
			     :collide-type 'paddle))
	(anim (make-instance 'animated-sprite
			     :sprite-def left-bat-sprite :current-frame 'frame-1
			     :speed 8.0)) ; frames per second
	(pong (make-instance 'player-paddle-logic
			     :control-type 'human-keyboard
			     :side 'left))
	(obj (make-instance 'composite-object
			    :name "human player 1")))
    (add-component obj phys)
    (add-component obj anim)
    (add-component obj pong)
    obj))

(defun make-right-pong-player()
  (let ((phys (make-instance '2d-physics
			     :collide-type 'paddle))
	(anim (make-instance 'animated-sprite
			     :sprite-def right-bat-sprite :current-frame 'frame-1
			     :speed 16.0)) ; frames per second
	(pong (make-instance 'player-paddle-logic
			     :control-type 'ai-hard
			     :side 'right))
	(obj (make-instance 'composite-object
			    :name "hard ai player")))
    (add-component obj phys)
    (add-component obj anim)
    (add-component obj pong)
    obj))

(defun make-ball()
  (let ((phys (make-instance '2d-physics
			     :x (random-range 0.0 640.0) :y (random-range 0.0 480.0) 
			     :vx (random-range 4.0 10.0) :vy (random-range -8.0 8.0)
			     :collide-type 'ball
			     :collide-with-types '(paddle left-goal right-goal wall)))
	(anim (make-instance 'animated-sprite
			     :sprite-def ball-sprite :current-frame 'frame-1
			     :speed 4.0))
	(ball (make-instance 'ball-logic))
	(obj (make-instance 'composite-object
			    :name "ball")))
    (add-component obj phys)
    (add-component obj anim)
    (add-component obj ball)
    obj))

(defun send-message-to-all-objects(message &rest args)
  "Send the message to every component of every object"
  (dolist (obj *active-game-objects*)
    (dolist (comp (slot-value obj 'components))
      (apply #'handle-message comp message args))))
  
;;;; need to use with- pattern here...
;;;; todo also move to components
;;;; eg: (defun update-components(message &rest rest)
(defun update-game-objects()
  (send-message-to-all-objects 'update (/ 1.0 (sdl:frame-rate))))

(defun draw-game-objects()
  (send-message-to-all-objects 'collide)
  (send-message-to-all-objects 'draw))

(defun engine-init()
  "sets up the game engine and returns"
  (if *engine-active*
      (error "engine-init was already called"))
  (sdl:init-sdl)  
  (sdl:init-sub-systems)
  (setf (sdl:frame-rate) 60) ; Set target framerate (or 0 for unlimited)
  ;;; TODO handle full screen here
  (sdl:window *WINDOW-WIDTH* *WINDOW-HEIGHT* :title-caption "Game engine" :icon-caption "Game engine")
  ;;; init other stuff we want available
  (sdl:initialise-default-font)
  (setf (sdl:frame-rate) 60) ; Set target framerate (or 0 for unlimited)
  (setf *engine-active* t))

(defun engine-quit()
  "shut sdl down and clean up everything that engine-init did"
  (if (null *engine-active*)
      (error "engine-init was not called"))
  (sprites:flush-image-cache)
  (setf *active-game-objects* nil) ; free all game objects to the garbage collector
  (sdl:quit-sub-systems)
  (sdl:quit-sdl)
  (setf *engine-active* nil))

#+lispworks 
(defun engine-run-as-lisporks-new-process()
  (mp:process-run-function "Game engine" '(:priority 42) #'engine-run))

(defun engine-run()
  "The main game engine loop, it updates objects then draws them"
  (if (null *engine-active*)
      (error "engine-init was not called"))
  (sdl:with-events  ()
    (:quit-event () t)
    (:key-down-event (:key key)
		     (cond
		       ((sdl:key= key :SDL-KEY-ESCAPE)
			(sdl:push-quit-event))
		       ((sdl:key= key :SDL-KEY-UP)
			(setf *player-1-key-up-held* t))
		       ((sdl:key= key :SDL-KEY-DOWN)
			(setf *player-1-key-down-held* t))))
    (:key-up-event (:key key)
		     (cond
		       ((sdl:key= key :SDL-KEY-UP)
			(setf *player-1-key-up-held* nil))
		       ((sdl:key= key :SDL-KEY-DOWN)
			(setf *player-1-key-down-held* nil))))
    (:idle () ;; redraw screen on idle
	   ;; fill the background
	   (sdl:clear-display *BG-COLOR*)
	   ;; Do stuff
	   (update-game-objects)
	   (draw-game-objects)
	   (show-frame-rate) ; todo could be component
	   ;; Update the whole screen 
	   (sdl:update-display))))
    
(defun show-frame-rate()
  "Show the current desired framerate
Warning, this doesn't show the actual frame rate, but it probably should"
  (sdl:draw-string-solid-* 
   (format nil "fps: ~a" (sdl:frame-rate))
   10 10 :color (sdl:color :r #xff :b #xff :g #xff)))

; a simple test run
; the data driven engine will look something like this
; but with a nicer syntax for creating and adding objects
(defun start-pong()
  (engine-init)
  (add-object-to-active-list (make-left-pong-player))
  (add-object-to-active-list (make-right-pong-player))
  (add-object-to-active-list (make-ball)))

; (engine-run)