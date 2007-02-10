;;;; a physics-rectangle with a trail
;;;; all hail, trail-physics-rectangle
;;;; requires that circular-queue.lisp was loaded



;;;; a colored rectangle with phsyics
(defclass trail-physics-rectangle (colored-rectangle physics) 
  ((trail :initform nil :initarg :trail)
   (end-color :initform (sdl:color :r 255 :g 255 :b 255) :initarg :end-color)))

(defun make-trail-physics-rectangle(x y vx vy start-color end-color trail-length)
  (let ((trail (cq-make trail-length)))
    (make-instance 'trail-physics-rectangle
		   :x x :y y :w 1 :h 1 :vx vx :vy vy :ax 0.0 :ay 0.0
		   :color start-color :end-color end-color :trail trail)))

(defmethod update((object trail-physics-rectangle) time-elapsed)
  "add current position to trail"
  (cq-add-back (list (slot-value object 'x) (slot-value object 'y)) (slot-value object 'trail))
  (call-next-method))

(defmethod draw((object trail-physics-rectangle))
  "draw trail"
  (with-slots (trail color end-color) object
    (cq-iterate (pos trail)
      (sdl:draw-box (sdl:rectangle :x (first pos) :y (second pos) :w 1 :h 1)
		    :color color
		    :surface sdl:*default-display*)))
  (call-next-method))





