; rectangle component 

(defclass rectangle(component)
  ((color :initform (sdl:color :r 255 :g 255 :b 255) :initarg :color)
   (x :initform 0 :initarg :x)
   (y :initform 0 :initarg :y)
   (w :initform 0 :initarg :w)
   (h :initform 0 :initarg :h)))

(defmethod handle-message((comp rectangle) message-type &rest rest)
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('draw
       (with-slots 
	     (color w h) comp

	 (let ((phys-comp (find-component-with-type owner '2d-physics)))
	   (with-slots (x y) phys-comp 

	     (sdl:draw-box (sdl:rectangle-from-edges-* x y (+ x w -1) (+ y h -1))
			   :surface sdl:*default-display*
			   :color color
			   :clipping nil))))))))





