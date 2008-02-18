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
(defparameter *FULL-SCREEN-P* nil) ; unused
(defparameter *BG-COLOR* (sdl:color :r #x22 :g #x22 :b #x44))

; Set the default bitmap path
(defparameter *bmp-path* nil)

;;;; Game/Media systems - all games can use these
(load "util")
(load "sprites") ; sprite drawing, animation and file handling
(load "input") ; joystick, mouse and keyboard

(load "components") ; generic components and objects
(load "game") ; game and level management

;;;; Game specific data - eventually this should be elsewhere

(load "pong") ; components and code for being a pong game

(defparameter *engine-game* nil
  "Current game which is the levels, game specific data and objects")

(defparameter *engine-active* nil 
  "Monitors the status of the engine to ensure init and quit correct use")

(defun engine-init()
  "sets up the game engine and returns"
  (if *engine-active*
      (error "engine-init was already called"))
  (sdl:init-sdl)  
  (sdl:init-sub-systems)
  (setf (sdl:frame-rate) 60) ; Set target framerate (or 0 for unlimited)
  ;;; TODO handle full screen here
  (sdl:window *WINDOW-WIDTH* *WINDOW-HEIGHT* 
	      :title-caption "Game engine" :icon-caption "Game engine")
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
  (input:quit)
  (sdl:quit-sub-systems)
  (sdl:quit-sdl)
  (setf *engine-active* nil))

; Note this works in lispworks, kinda, you can run the main loop in a seperate 
; process
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
	   (engine-update-game)
	   (sdl:update-display)
	   (input:update (/ 1.0 (sdl:frame-rate))))))

(defun engine-get-game()
  *engine-game*)
  
(defun engine-set-game(game)
  "Current game the engine is running"
  (setf *engine-game* game))

(defun engine-update-game()
  "Update the current game"
  (game-update *engine-game*))


   

