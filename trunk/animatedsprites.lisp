; animated sprite component 


;;;; TODO need positional info (where to draw the sprite 
;;;; relative to the 2d-physics-component)
(defclass animated-sprite(component)
  ((sprite-def :initform nil :initarg :sprite-def)
   (current-frame :initform nil :initarg :current-frame)
   (speed :initform 0.0 :initarg :speed)
   (time-playing :initform 0.0 :initarg :time-playing)))

(defmethod handle-message((comp animated-sprite) message-type &rest rest)
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('initialise
       (sprites:load-sprite-image (slot-value comp 'sprite-def)))
      ('update
       (let ((time-elapsed (first rest)))
	 ;;Determine the current animation frame based on time elapsed"
	 (with-slots 
	       (current-frame speed time-playing sprite-def) 
	     comp
	   (incf time-playing time-elapsed)
	   (let* ((num-frames (length (sprites:sprite-def-frames sprite-def)))
		  (frame-num (sprites:get-frame-from-time-and-speed num-frames speed time-playing)))
	     (setf current-frame
		   (sprites:get-sprite-frame-with-index 
		    (sprites:sprite-def-frames sprite-def) frame-num)))
	   ;; now inform the physics object of our dimensions based on the sprite
	   (multiple-value-bind (spr-width spr-height)
	       (sprites:get-sprite-frame-width-and-height sprite-def current-frame)
	     (let ((phys-comp (find-component-with-type owner '2d-physics)))
	       (with-slots (width height) phys-comp
		 (setf width spr-width)
		 (setf height spr-height)))))))
      ('draw
       (with-slots (sprite-def current-frame) comp
	 (let ((phys-comp (find-component-with-type owner '2d-physics)))
	   (with-slots (x y) phys-comp 
	     (sprites:draw-sprite sdl:*default-display* 
				  (sx x) (sy y) 
				  sprite-def 
				  current-frame)))))
      (otherwise
       nil))))

