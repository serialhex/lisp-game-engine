;;;; Gravity Toy
; (C)2007 Justin Heyes-Jones

(in-package #:cl-user)  

; window or screen height TODO this must go somewhere better
(defparameter *WINDOW-WIDTH* 640)
(defparameter *WINDOW-HEIGHT* 480)

;;;; UTIL

(load "util")

;;;; SYSTEMS
(load "sprites") ; sprite drawing and management

;;;; GAME 
(load "gameobjects") ; generic game objects

(load "circular-queue")

(load "trail-physics-rectangle")

;;;; DATA 

; a list of game objects
; todo this should be part of the engine maybe?
(defparameter *game-objects* nil)

(defun show-frame-rate()
  (sdl:draw-string-centered-* (format nil "fps: ~a" (sdl:frame-rate)) (screen-center-x) (screen-center-y)
					:surface sdl:*default-display*))


; specific objects for this game

; todo physical constants
(defparameter *max-gravity* 4.0)
(defparameter *max-gravity-distance* 200.0)

(defconstant *start-speed* 4.0)

; todo own file
(defclass gravity-source(physics-rectangle)
  ((gravity :initform 0.0 :initarg :gravity)))

(defmethod update((object gravity-source) time-elapsed)
  "Apply gravity to all nearby objects"
  (dolist (item *game-objects*)
    (unless (eql (type-of item) 'gravity-source)
      (progn
	(let* ((a (* time-elapsed (get-acceleration-due-to-gravity object item))))
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
    (if (> d *max-gravity-distance*)
	0.0
      (* *max-gravity* (/ 1.0 (sqr d))))))

; todo gameobjects
(defmacro add-object(object)
  `(push ,object *game-objects*))

; todo gameobjects
(defun init-game-objects()
  ; add game objects to update loop
  (let ((num-objects 20))
  ; add some gravity to screen center
    (add-object (make-instance 'gravity-source :x 320.0 :y 240.0
		    :w 1 :h 1 :vx 0.0 :vy 0.0 :ax 0.0 :ay 0.0
		    :color (sdl:color :r 244 :g 9 :b 9) :gravity 10.0))
    (loop for count from 1 to num-objects do
	  ; add a bunch of other stuff
	  (push
	   (make-trail-physics-rectangle (random-range 0.0 640.0) (random-range 0.0 480.0)  
					 (random-range (- *start-speed*) *start-speed*)
					 (random-range (- *start-speed*) *start-speed*)
					 (sdl:color :r 255 :g 0 :b 0) (sdl:color :r 0 :g 0 :b 0) 15)
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
		 (sdl:clear-display (sdl:color :r #x00 :g #x00 :b #x00))
		 ;; Do stuff
;		 (show-frame-rate)
		 (update-game-objects)
		 (draw-game-objects)
		 ;; Update the whole screen 
		 (sdl:update-display)))
	(setf *game-objects* nil))))



