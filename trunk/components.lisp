;;;; This file contains all you need to make objects that 
;;;; are composed of generic components

;;;; Base class for components
(defclass component()
  ((owner :initform nil :initarg :owner)))

;;;; Base class for objects composed of components
(defclass composite-object()
  ((name :initform (format nil "anon_~4,'0D" (get-next-uid)) :initarg :name)
   (components :initform nil :initarg :components)))

; a list of game objects
; everything that makes up the game must be a game object
(defparameter *active-game-objects* nil)
; true when the engine has been init'd but not quit'd

(defun add-object-to-active-list(obj)
  "Add an object to the game"
  (push obj *active-game-objects*))

(defun find-object-with-name-in-active-list(name)
  "find object with name"
  (find-if 
   (lambda (obj) (string-equal name (slot-value obj 'name))) 
   *active-game-objects*))


(defun remove-object-with-name-from-active-list(name)
  "remove any objects with name"
  (setf *active-game-objects*
	(remove-if 
	 (lambda (obj) (string-equal name (slot-value obj 'name)))
	 *active-game-objects*)))

(defun debug-view-active-object-names()
  (dolist (obj *active-game-objects*) 
    (format t "name ~a~%" (slot-value obj 'name))))

(defun get-components-of-type-from-active-list(type)
  (let ((found-components nil))
    (dolist (obj *active-game-objects*)
      (dolist (comp (slot-value obj 'components))
	(if (equal type (type-of comp))
	    (setf found-components (cons comp found-components)))))
    found-components))

(defgeneric add-component(obj component))
(defgeneric handle-message(component message-type &rest rest))

(defmethod handle-message((comp component) message-type &rest rest)
())

(defmethod add-component((obj composite-object) (comp component))
  (setf (slot-value comp 'owner) obj)
  (push comp (slot-value obj 'components)))

;;; Functions for locating components by name, type and so on

(defgeneric find-component-with-type(object type))

(defmethod find-component-with-type((obj composite-object) type)
  (with-slots (components) obj
    (if components
	(dolist (comp components)
	  (if (equal (type-of comp) type)
	      (return-from find-component-with-type comp)))
	nil)))

;;; Some useful generic components, this may move

;;;; TODO add rotational velocity and
;;;; acceleration here when needed
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
;;;; todo some kind of caching
(defun get-2d-physics-collisions(comp)
  ; only collide if we have some types to collide with
  (if (slot-value comp 'collide-with-types)
      (let ((candidates 
	     (get-components-of-type-from-active-list '2d-physics)))

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
	 (incf y vy)


	 ))
      ; handle collisions
      ('collide
       (setf (slot-value comp 'collision-list) (get-2d-physics-collisions comp)))
      
      ; draw this item to screen
      ('draw
       ; show debug collision data
       (if *debug-collision*
	   (let ((color 
		  (if (slot-value comp 'collision-list)
		      (sdl:color :r #xff)
		      (sdl:color :g #xff))))
	     (with-slots (x y width height) comp
	       (sdl:draw-box
		(sdl:rectangle
		 :x (sx x) :y (sy y) :w width :h height)
		:color color
		:surface sdl:*default-display*))))))))
  
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
	   (let* ((num-frames (length (sprites::sprite-def-frames sprite-def)))
		  (frame-num (sprites:get-frame-from-time-and-speed num-frames speed time-playing)))
	     (setf current-frame
		   (sprites:get-sprite-frame-with-index 
		    (sprites::sprite-def-frames sprite-def) frame-num)))
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
