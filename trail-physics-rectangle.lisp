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
  (cq-add-back (list (slot-value object 'x) (slot-value object 'y)) (slot-value object 'trail)))

(defmethod draw((object trail-physics-rectangle))
  "draw trail"
  (with-slots (trail color end-color) object
    (let ((ndx 0))
      (cq-iterate-pairs (pos1 pos2 trail)
	(sdl:draw-line-* (first pos1) (second pos1) (first pos2) (second pos2) 
			 :color (interp-sdl-color end-color color (/ ndx (circular-queue-count trail)))
			 :surface sdl:*default-display*)
	(incf ndx))))
  (call-next-method))





