; 2d physics component

;;;; TODO add rotational velocity and
;;;; acceleration here when needed
;;;; and different collision responses
(defclass 2d-physics (component)
  ((x :initform 0.0 :initarg :x)
   (y :initform 0.0 :initarg :y)
   (vx :initform 0.0 :initarg :vx)
   (vy :initform 0.0 :initarg :vy)
   (ax :initform 0.0 :initarg :ax)
   (ay :initform 0.0 :initarg :ay)
   (width :initform 1.0 :initarg :width)
   (height :initform 1.0 :initarg :height)
   (collide-type :initform 'no-collision :initarg :collide-type)
   (collide-with-types :initform nil :initarg :collide-with-types)
   (collision-response :initform nil :initarg :collision-response)
   (collision-list :initform nil)))

;;;; Use this to view collision rectangles 
(defparameter *debug-collision* nil)

(defun 2d-physics-collide-p(c1 c2)
  "Test two 2d-physics components for collison"
  (with-slots ((x1 x) (w1 width) (y1 y) (h1 height)) c1
    (with-slots ((x2 x) (w2 width) (y2 y) (h2 height)) c2
      (let ((result 
	     (not
	      (or
	       (> x1 (+ x2 (1- w2))) ; any of these true means no collision
	       (< (+ x1 (1- w1)) x2)
	       (> y1 (+ y2 (1- h2)))
	       (< (+ y1 (1- h1)) y2)))))
	result))))

;;;; simple O(n^2) collision check for a certain object
;;;; todo some kind of caching or spatial data structure like a kd-tree
(defun get-2d-physics-collisions(comp)
  ; only collide if we have some types to collide with
  (if (slot-value comp 'collide-with-types)

      (let ((candidates 
	     (game-get-components-of-type '2d-physics)))

	; filter the list to include only valid types
	(let ((filtered-candidates
               
               ; first remove self from the list
	       (remove comp 
 
                       ; then remove collision types we don't care about
		       (remove-if-not 
			(lambda (comp2)
			  (member 
			   (slot-value comp2 'collide-type)
			   (slot-value comp 'collide-with-types)))
			candidates))))

	  ; filtered-candidates is now the list of objects we are 
	  ; prepared to collide with
	  (remove-if-not 
	    (lambda (comp2)
	      (2d-physics-collide-p comp comp2)) filtered-candidates)))
      nil))

(defmethod handle-message((comp 2d-physics) message-type &rest rest)
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ; do game logic type think step
      ('update
       (with-slots (x y vx vy ax ay) comp
	 ;accelerate
	 (incf vx ax)
	 (incf vy ay)
         
         ; move
	 (incf x vx)
	 (incf y vy)))

      ; handle collisions
      ('collide
       (setf (slot-value comp 'collision-list) (get-2d-physics-collisions comp)))
      
      ; draw this item to screen
      ('draw
       ; show debug collision data
       (if (and *debug-collision* (slot-value comp 'collision-list))
;	   (let ((color 
;		  (if (slot-value comp 'collision-list)
;		      (sdl:color :r #xff)
;		      (sdl:color :g #xff))))
	   (let ((color (sdl:color :r #xff)))
	     (with-slots (x y width height) comp
	       (sdl:draw-box
		(sdl:rectangle
		 :x (sx x) :y (sy y) :w width :h height)
		:color color
		:surface sdl:*default-display*))))))))


