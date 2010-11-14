;;;; IGE - Interactive Game Engine
;;;; An object oriented compoment based, data driven multimedia and game framework
;;;; (C)2008 Justin Heyes-Jones

; todo
; add objects to the currently running level
; framerate - add update delta to engine instead of calculating it everywhere
; that is, the current frame update time 

(in-package :cl-user)  

; Set up global vars used throughout the engine

(defvar *WINDOW-WIDTH* 640 "Width of window")
(defvar *WINDOW-HEIGHT* 480 "Height of window")
(defvar *FULL-SCREEN-P* nil "Whether or not this is a full screen application") 
(defvar *BG-COLOR* (sdl:color :r #x22 :g #x22 :b #x44) "Screen is filled with this color each frame")
(defvar *FRAME-RATE* 30 "Target frame rate")

; Set the default bitmap path
; TODO this should be a variable in the engine
#-unix(defparameter *bmp-path* nil)
#+unix(defparameter *bmp-path* "/home/justinhj/lisp-game-engine/")

(defparameter *engine-game* nil
  "Current game which is the levels, game specific data and objects")

(defparameter *engine-active* nil 
  "Monitors the status of the engine to ensure init and quit correct use")

;;;; Game/Audio and graphics systems - all games can use these
(load "util")
(load "sprites") ; sprite drawing, animation and file handling

(load "components") ; generic components and objects

(load "2dphysics")
(load "animatedsprites")
(load "text")
(load "rectangles")
(load "menu") ; menu system
(load "game") ; game and level management

;;;; Game specific data - eventually this should be elsewhere

(defun engine-get-window-flags(full-screen-p)
  "return appropriate window flags depending on full screen or not"
  (let ((flags sdl-cffi::SDL-SW-SURFACE))
    (if full-screen-p
	(logior flags sdl-cffi::SDL-FULLSCREEN)
	flags)))

(defun engine-init(&key
		   (window-width *WINDOW-WIDTH*)
		   (window-height *WINDOW-HEIGHT*)
		   (full-screen-p *FULL-SCREEN-P*))
  "sets up the game engine and returns"
  (if *engine-active*
      (error "engine-init was already called"))
  ; copy user params to globals
  (setf *WINDOW-HEIGHT* window-height)
  (setf *WINDOW-WIDTH* window-width)
  (setf *FULL-SCREEN-P* full-screen-p)
  ; init game engine
  (sdl:init-sdl :flags SDL:SDL-INIT-AUDIO)  
  ;(sdl:init-subsystems)
  (sdl:window window-width window-height 
	      :flags (engine-get-window-flags full-screen-p)
	      :title-caption "Game engine" :icon-caption "Game engine")
  (setf (sdl:frame-rate) *FRAME-RATE*)
  (sdl:initialise-default-font)
  (sdl:initialise-input-util) 
  (setf *engine-active* t))

(defun engine-quit()
  "shut sdl down and clean up everything that engine-init did"
  (if (null *engine-active*)
      (error "engine-init was not called"))
  (sprites:flush-image-cache)
  (sdl:quit-input-util)
  ;(sdl:quit-subsystems)
  (sdl:quit-sdl)
  (setf *engine-active* nil)
  t)

; Note this works in lispworks, kinda, you can run the main loop in a seperate 
; process
#+lispworks 
(defun engine-run-as-lispworks-new-process()
  (mp:process-run-function "Game engine" '(:priority 42) #'engine-run))

(defun engine-run()
  "The main game engine loop, it updates objects then draws them"
  (if (null *engine-active*)
      (error "engine-init was not called"))

  (sdl:with-events  ()
    (:quit-event () t)

    (:key-down-event (:key key)
		     (if (sdl:key= key :SDL-KEY-ESCAPE)
			 (sdl:push-quit-event)))

    (:idle () ;; redraw screen on idle
	   
           ; set frame rate and cursor visiblity
	   (setf (sdl:frame-rate) *FRAME-RATE*)
	   (sdl:show-cursor nil)

	   ;; fill the background
	   (sdl:clear-display *BG-COLOR*)

	   (if (sdl:key-pressed-p :SDL-KEY-UP)
	       (format t "chips~%"))

	   (engine-update-game)
	   (sdl:update-display))))
	   ;(sdl:update-input-util (sdl:frame-time)

(defun engine-get-game()
  *engine-game*)
  
(defun engine-set-game(game)
  "Current game the engine is running"
  (setf *engine-game* game))

(defun engine-update-game()
  "Update the current game"
  (when *engine-game*
    (game-update *engine-game*)))



   

