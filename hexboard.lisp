;;;; represent a board of hexes
;;;; 

(load "util.lisp")

(defclass hex-board (object)
  ((num-rings :initform nil :initarg :num-rings)
   (rings :initform nil :initarg :rings)
   (ring-radius :initform nil :initarg :ring-radius)
   (shapes :initform nil :initarg :shapes)
   (sides :initform nil :initarg :sides)
   (cx :initform 0.0 :initarg :cx)
   (cy :initform 0.0 :initarg :cy)))

(defstruct hb-shape 
  ring
  index
  offsets
  imprinted)

(defmethod update ((object hex-board) time-elapsed)
  "handle mouse clicks, set that bit of the board"
  (with-slots (rings num-rings shapes) object
	      (loop for r from 1 to (1- num-rings) do
		    (hb-clear-full-ring object r))
	      (loop for shape in shapes do
; descend shape (TODO needs to be time synched)
		    (hb-descend-shape object shape)) 
 ; remove any shapes that got imprinted on the board
	      (setf shapes 
		    (remove-if #'hb-shape-imprinted-p shapes))
	      (if (and (>= *mouse-click-x* 0)
		       (>= *mouse-click-y* 0))
		  (progn
		    (let ((ring (hb-screen-x-y-to-ring object *mouse-click-x* *mouse-click-y*))
			  (index (hb-screen-x-y-to-ring-index object *mouse-click-x* *mouse-click-y*)))
		      (if (< ring num-rings)
			  (if (< index (length (aref rings ring)))
			      (let ((new-shape (make-hb-shape :ring ring :index index 
							      :offsets nil :imprinted nil)))
				(hb-add-shape object new-shape)))))
		    (setf *mouse-click-x* -1)
		    (setf *mouse-click-y* -1)
		    ))))

(defparameter *debug-hb-draw* nil)

(defun hb-draw-ring-index-color(hb ring index color &optional (size 1))
  (with-slots (cx cy ring-radius rings sides) hb
	      (let* ((frac (* PI 2.0 (/ index sides)))
		     (x (* (sin frac) ring ring-radius))
		     (y (* (cos frac) ring ring-radius -1.0))
		     (val (hb-get-ring-index hb ring index)))
		(if *debug-hb-draw*
		    (sdl:draw-string-centered-* (format nil "r: ~a i: ~a." ring index)
						(+ cx x) (+ cy y)
						:surface sdl:*default-display*)
		  (sdl:draw-box (sdl:rectangle 
				 :x (+ cx x (/ size -2))
				 :y (+ cy y (/ size -2))
				 :w size :h size)
				:color color :surface sdl:*default-display*)))))

(defmethod draw ((object hex-board))
  (with-slots (cx cy num-rings rings shapes sides) object
	      (loop for ring from 0 to (1- num-rings) do
		    (let* ((num-items sides)); todo remove
		      (loop for index from 0 to (1- num-items) do 
			    (let* ((val (hb-get-ring-index object ring index))
				   (color (if val 
					      (sdl:color :r 255 :g 0 :b 0) 
					    (sdl:color :r 64 :g 64 :b 64))))
			      (hb-draw-ring-index-color object ring index color 2)))))
	      ; now draw the activae shapes
	      (loop for shape in shapes do
		    (hb-draw-shape object shape))))

(defun make-hex-board(rings sides radius x y)
  (let ((board (make-instance 'hex-board :num-rings rings :ring-radius radius
			      :rings (make-array rings :adjustable nil)
			      :cx x :cy y :sides sides)))
    (loop for r from 0 to (1- rings) do
	  (setf (aref (slot-value board 'rings) r)
		(make-array sides :adjustable nil :initial-element nil)))
    (loop for i from 0 to (1- sides) do 
	  (hb-set-ring-index board 0 i t)) ; set the center to filled already
    board))

(defun hb-screen-x-y-to-ring(hb x y)
  (with-slots (cx cy ring-radius) hb
	      (let ((dist (sqrt (+ (sqr (- cx x)) (sqr (- cy y))))))
		(round (/ dist ring-radius)))))
  
(defun hb-screen-x-y-to-ring-index(hb x y)
  (with-slots (cx cy ring-radius num-rings rings sides) hb
	      (let ((dx (- x cx))
		    (dy (- y cy))
		    (ring (hb-screen-x-y-to-ring hb x y)))
		(if (< ring num-rings)
		    (let* ((angle-degs (rads-degs (atan2 dx (- dy))))
			  (items sides)
			  (item-arc (/ 360.0 items)))
		      (round (/ angle-degs item-arc)))
		  -1))))

;;;; todo for these two functions, validate the ring index to the vertex length

(defun hb-set-ring-index(hb ring index value)
  "set the specified ring and index in that ring with the specified value"
  (with-slots (rings num-rings sides) hb 
	      (if (>= ring num-rings)
		  (error "invalid ring")
		(let ((ring-items (aref rings ring)))
		  (if (>= index sides)
		      (error "invalid ring index")
		    (setf (aref ring-items index) value))))))

(defun hb-get-ring-index(hb ring index)
  "get the value stored at the specified ring and index"
  (with-slots (rings num-rings sides) hb 
	      (if (>= ring num-rings)
		  (error "invalid ring")
		(let ((ring-items (aref rings ring)))
		  (if (>= index sides)
		      (error "invalid ring index")
		    (aref ring-items index))))))

;;;; Shapes - an active shape (there may be more than one) is alive on the board 
;;;; these are handled differently to the board itself so they can 
;;;; a) move as shape and be blocked if part of the shape is blocked
;;;; b) be moved, rotated as a unit by the player

(defun hb-shape-imprinted-p(shape)
  (hb-shape-imprinted shape))

(defun hb-imprint-shape(hb shape)
  (hb-set-ring-index hb (hb-shape-ring shape) (hb-shape-index shape) t)
  (setf (hb-shape-imprinted shape) t))
; TODO also do the offsets

(defun hb-descend-shape(hb shape)
  "move the shape down until it would collide, then imprint it"
  (if (hb-shape-imprinted-p shape)
      (error "descending an imprinted shape"))
  (if (hb-shape-collides-p hb shape (1- (hb-shape-ring shape)))
      (hb-imprint-shape hb shape) 
    (decf (hb-shape-ring shape) 1)))

(defun hb-shape-collides-p(hb shape ring)
  "returns true if it did not collide in the new ring position"
  (let ((index (hb-shape-index shape)))
    (if (hb-get-ring-index hb ring index)
	t
      nil)))
; TODO same for offsets

(defun hb-draw-shape(hb shape)
  "draw a shape"
  ; first draw the position part
  (let ((ring (hb-shape-ring shape))
	(index (hb-shape-index shape)))
    (hb-draw-ring-index-color hb ring index (sdl:color :r 0 :b 255 :g 0) 3)))
; todo the offsets part

(defun hb-add-shape(hb shape)
  "add a shape to the board"
  (push shape (slot-value hb 'shapes)))

(defun hb-clear-full-ring(hb ring)
  "determine if a ring can be removed and clear it"
  (with-slots (rings sides) hb 
	      (let ((all-set t))
		(loop for index from 0 to (1- sides) do
		      (if (null (hb-get-ring-index hb ring index))
; todo use loop body for this
			  (setf all-set nil)))
		(if all-set
		    (loop for index from 0 to (1- sides) do
			  (hb-set-ring-index hb ring index nil))))))

; rotate a shape

; accelerate a shape






