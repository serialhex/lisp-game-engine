;;;; Generic game objects
;;;; Reusable game components

(defclass object()
   ((name :initform (format nil "name_~4,'0D" (random 100)) :initarg :name)))

(defgeneric initialise((object object)))
(defgeneric update((object object) time-elapsed))
(defgeneric draw((object object)))
(defgeneric shutdown((object object)))

;;;; an object that knows about physics
; handles collisions, acceleration, position and shape
(defclass physics (object)
  ((x :initform 0.0 :initarg :x)
   (y :initform 0.0 :initarg :y)
   (vx :initform 0.0 :initarg :vx)
   (vy :initform 0.0 :initarg :vy)
   (ax :initform 0.0 :initarg :ax)
   (ay :initform 0.0 :initarg :ay)))
 
;;;; animated sprite object with bmp surface
(defclass sprite (object)
  ((sprite-def :initform nil :initarg :sprite-def)
   (current-frame :initform nil :initarg :current-frame)
   (speed :initform 0.0 :initarg :speed)
   (time-playing :initform 0.0 :initarg :time-playing)))

;;;; a sprite with physics
(defclass physics-sprite (sprite physics)
  ())

(defmethod initialise((object sprite))
  (sprites:load-sprite-image (slot-value object 'sprite-def)))

; UPDATE METHODS

(defmethod update((object sprite) time-elapsed)
  "Determine the current animation frame based on time elapsed"
  (with-slots (current-frame speed time-playing sprite-def) object
    (incf time-playing time-elapsed)
    (let* ((num-frames (length (sprites::sprite-def-frames sprite-def)))
	   (frame-num (sprites:get-frame-from-time-and-speed num-frames speed time-playing)))
      (setf current-frame
	    (sprites:get-sprite-frame-with-index 
	     (sprites::sprite-def-frames sprite-def) frame-num))))
  (call-next-method))

; TODO *window-height* etc needs to access a global object of some kind
; not just global variables, i think luke already did...

(defmethod update((object physics) time-elapsed)
  (with-slots (x y vx vy ax ay) object
  ; accelerate
	      (incf vx ax)
	      (incf vy ay)
  ; move
	      (incf x vx)
	      (incf y vy)

  ; handle collisions
  ; TODO
  ; wrap on screen
	      (if (> x *WINDOW-WIDTH*)
		  (setf x 0.0))
	      (if (> y *WINDOW-HEIGHT*)
		  (setf y 0.0))
		
	      (if (< x 0.0)
		  (setf x *WINDOW-WIDTH*))
	      (if (< y 0.0)
		  (setf y *WINDOW-HEIGHT*))
  ;(call-next-method)))
))

(defmethod draw((object sprite))
  (with-slots (x y sprite-def current-frame ) object
	      (sprites:draw-sprite sdl:*default-display* 
				   x y 
				   sprite-def 
				   current-frame)))

(defmethod draw((object physics))
  (with-slots (x y name) object 
    (sdl:draw-string-centered-* name    
				:surface sdl:*default-display*
				:position (sdl:point x y))))

(defmethod shutdown((object object))
  )

(defmethod shutdown((object sprite))
  )


