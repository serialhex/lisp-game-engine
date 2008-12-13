; text component

(defclass text (component)
  ((string :initform "" :initarg :string)
   (justification :initform :left :initarg :justification)
   (color :initform (sdl:color :r #xff :g #xff :b #xff) :initarg :color)))

(defmethod handle-message((comp text) message-type &rest rest)
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('change-text
       (with-slots (string) comp
	   (setf string (first rest))))
      ('draw
       (with-slots (string justification color) comp
	 (let ((phys-comp (find-component-with-type owner '2d-physics)))
	   (with-slots (x y) phys-comp 
	     (sdl:draw-string-solid-* 
	      (format nil string)
	      (sx x) (sy y)
	      :justify justification
	      :color color)))))
      (otherwise
       nil))))
