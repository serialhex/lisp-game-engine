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

;;; This should be platform independent somehow
;;; for now this will work for me
#+sbcl (defparameter *bmp-path* "/home/justinhj/lisp-game-engine/")
#-sbcl (defparameter *bmp-path* nil)

;;;; Totally generic systems
;(load "gameobjects") ; generic game objects
(load "util")
(load "sprites") ; sprite drawing, animation and file handling
(load "input") ; joystick, mouse and keyboard

(load "components")

;;;; Game specific data - eventually this should be elsewhere

(load "pong") ; components and code for being a pong game

; this variable ensures proper use of engine-init
; engine-run and engine-quit
(defparameter *engine-active* nil)

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
  (input:initialise)
  (setf *engine-active* t))

(defun engine-quit()
  "shut sdl down and clean up everything that engine-init did"
  (if (null *engine-active*)
      (error "engine-init was not called"))
  (sprites:flush-image-cache)
  (setf *active-game-objects* nil) ; free all game objects to the garbage collector
  (input:quit)
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
    ; todo replace this with a simple input system
    (:key-down-event (:key key)
		     (input:handle-key-down key)
		     (if (sdl:key= key :SDL-KEY-ESCAPE)
			 (sdl:push-quit-event)))
    (:key-up-event (:key key)
		     (input:handle-key-up key))
    (:idle () ;; redraw screen on idle
	   ;; fill the background
	   (sdl:clear-display *BG-COLOR*)
	   ;; Do stuff
	   (update-game-objects)
	   (draw-game-objects)
	   (show-frame-rate) ; todo could be component
	   ;; Update the whole screen 
	   (sdl:update-display)
	   (input:update (/ 1.0 (sdl:frame-rate))))))
    
(defun show-frame-rate()
  "Show the current desired framerate
Warning, this doesn't show the actual frame rate, but it probably should"
  (sdl:draw-string-solid-* 
   (format nil "fps: ~2$" (coerce (sdl:average-fps) 'single-float))
   300 10 
   :justify :center
   :color (sdl:color :r #xff :g #xff :b #xff)))

   

