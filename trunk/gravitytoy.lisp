;;;; Simple game engine
; (C)2007 Justin Heyes-Jones

(in-package #:cl-user)  

; TEMP load in other files here instead of as package
; TODO load in a loader file 

; window or screen height TODO this must go somewhere better
(defparameter *WINDOW-WIDTH* 640)
(defparameter *WINDOW-HEIGHT* 480)

;;;; UTIL

(load "util.lisp")

;;;; SYSTEMS
(load "sprites.lisp") ; sprite drawing and management

;;;; GAME 
(load "gameobjects.lisp") ; generic game objects

(load "circular-queue.lisp")

(load "trail-physics-rectangle.lisp")

;;;; DATA 

; a list of game objects
; todo this should be part of the engine maybe?
(defparameter *game-objects* nil)

(defun show-frame-rate()
  (sdl:draw-string-centered-* (format nil "fps: ~a" (sdl:frame-rate)) (screen-center-x) (screen-center-y)
					:surface sdl:*default-display*))


; specific objects for this game

; todo physical constants
(defconstant *max-gravity* 1.3)
(defconstant *start-speed* 0.6)

; todo own file
(defclass gravity-source(physics-rectangle)
  ((gravity :initform 0.0 :initarg :gravity)))

(defmethod update((object gravity-source) time-elapsed)
  "Apply gravity to all nearby objects"
  (dolist (item *game-objects*)
    (unless (eql (type-of item) 'gravity-source)
      (progn
	(let* ((b (* time-elapsed (get-acceleration-due-to-gravity object item)))
	       (a (clamp b (- *max-gravity*) (+ *max-gravity*))))
	  (with-slots ((x1 x) (y1 y)) item
		      (with-slots ((x2 x) (y2 y)) object
				  (let* ((dx (- x2 x1)) (dy (- y2 y1))
					 (h (sqrt (+ (sqr dx) (sqr dy))))
					 (scale (/ a h)))
				    (setf (slot-value item 'ax) (* scale dx))
				    (setf (slot-value item 'ay) (* scale dy))))))))))

(defmethod get-distance((object1 physics) (object2 physics))
  (with-slots ((x1 x) (y1 y)) object1
    (with-slots ((x2 x) (y2 y)) object2
      (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))) )

(defmethod get-acceleration-due-to-gravity((source gravity-source) (object physics))
  (let ((d (get-distance source object)))
    (if (= d 0.0)
	0.0
      (* 10000.0 (/ 1.0 (sqr d))))))

; todo gameobjects
(defmacro add-object(object)
  `(push ,object *game-objects*))

; todo gameobjects
(defun init-game-objects()
  ; add game objects to update loop
  (let ((num-objects 50))
  ; add some gravity to screen center
    (add-object (make-instance 'gravity-source :x 320.0 :y 240.0
		    :w 1 :h 1 :vx 0.0 :vy 0.0 :ax 0.0 :ay 0.0
		    :color (sdl:color :r 244 :g 9 :b 9) :gravity 10.0))
    (loop for count from 1 to num-objects do
	  ; add a bunch of other stuff
	  (push
	   (make-trail-physics-rectangle (random-range 0.0 640.0) (random-range 0.0 480.0)  
					 (random-range -3.0 3.0) (random-range -3.0 3.0)
					 (sdl:color :r 244 :g 244 :b 244) (sdl:color :r 14 :g 14 :b 14) 10)
	   *game-objects*))))

(defun update-game-objects()
  (loop for game-object in *game-objects* do
	(update game-object (/ 1.0 (sdl:frame-rate)))))

(defun draw-game-objects()
  (loop for game-object in *game-objects* do
	(draw game-object)))

(defun gravitytoy()
  "gravity toy"
  (sdl:with-init ()			;Initialise SDL
      (setf (sdl:frame-rate) 60) ; Set target framerate (or 0 for unlimited)
      (sdl:window *WINDOW-WIDTH* *WINDOW-HEIGHT* :title-caption "Gravity" :icon-caption "Gravity")
      (progn
	;; init your game
	(sdl:initialise-default-font)
	(init-game-objects)
	(sdl:with-events  ()
	  (:quit-event () t)
	  (:key-down-event (:key key)
			   (if (sdl:key= key :SDL-KEY-ESCAPE)
			       (sdl:push-quit-event)))
	  (:idle () ;; redraw screen on idle
		 ;; fill the background
		 (sdl:clear-display (sdl:color :r #x22 :g #x22 :b #x44))
		 ;; Do stuff
;		 (show-frame-rate)
		 (update-game-objects)
		 (draw-game-objects)
		 ;; Update the whole screen 
		 (sdl:update-display)))
	(setf *game-objects* nil))))



