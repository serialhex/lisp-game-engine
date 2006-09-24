;;;; Generic game objects
;;;; Reusable game components

(defclass game-object()
   ((name :initform 'unnamed :initarg :name)))

(defgeneric initialise((object game-object)))
(defgeneric update((object game-object) time-elapsed))
(defgeneric draw((object game-object)))
(defgeneric shutdown((object game-object)))

;;;; sprite object
(defclass game-sprite-object (game-object)
  ((sprite-def :initform nil :initarg :sprite-def)
   (current-frame :initform nil :initarg :current-frame)
   (x :initform 0.0 :initarg :x)
   (y :initform 0.0 :initarg :y)
   (vx :initform 0.0 :initarg :vx)
   (vy :initform 0.0 :initarg :vy)
   (ax :initform 0.0 :initarg :ax)
   (ay :initform 0.0 :initarg :ay)))

(defmethod initialise((object game-sprite-object))
  (sprites:load-sprite-image (slot-value object 'sprite-def)))

(defmethod update((object game-sprite-object) time-elapsed)
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
))

(defmethod draw((object game-sprite-object))
  (with-slots (x y sprite-def current-frame ) object
	      (sprites:draw-sprite sdl:*default-display* 
				   x y 
				   sprite-def 
				   current-frame)))


(defmethod shutdown((object game-object))
  )




