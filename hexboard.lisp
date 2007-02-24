;;;; represent a board of hexes
;;;; 

(load "util.lisp")

(defclass hex-board (object)
  ((num-rings :initform nil :initarg :num-rings)
   (rings :initform nil :initarg :rings)
   (ring-radius :initform nil :initarg :ring-radius)
   (cx :initform 0.0 :initarg :cx)
   (cy :initform 0.0 :initarg :cy)))

(defmethod update ((object hex-board) time-elapsed)
  "handle mouse clicks, set that bit of the board"
  (with-slots (rings num-rings) object
	      (let ((ring (hb-screen-x-y-to-ring object *mouse-click-x* *mouse-click-y*))
		    (index (hb-screen-x-y-to-ring-index object *mouse-click-x* *mouse-click-y*)))
		(if (< ring num-rings)
		    (if (< index (length (aref rings ring)))
			(hb-set-ring-index object ring index t))))))

(defparameter *debug-hb-draw* nil)

(defun hb-draw-ring-index(hb ring index ring-items)
  (with-slots (cx cy ring-radius) hb
	      (let* ((frac (* PI 2.0 (/ index ring-items)))
		     (x (* (sin frac) ring ring-radius))
		     (y (* (cos frac) ring ring-radius -1.0))
		     (val (hb-get-ring-index hb ring index)))
		(if *debug-hb-draw*
		    (sdl:draw-string-centered-* (format nil "r: ~a i: ~a." ring index)
						(+ cx x) (+ cy y)
						:surface sdl:*default-display*)
		  (sdl:draw-box (sdl:rectangle :x (+ cx x) :y (+ cy y) :w 2 :h 2)
				:color (if val 
					   (sdl:color :r 255 :g 0 :b 0) 
					 (sdl:color :r 0 :g 255 :b 0)) 
				:surface sdl:*default-display*)))))

(defmethod draw ((object hex-board))
  (with-slots (cx cy num-rings rings) object
	      (loop for ring from 0 to (1- num-rings) do
		    (let* ((items (aref rings ring))
			   (num-items (length items)))
		      (loop for index from 0 to (1- num-items) do 
			    (hb-draw-ring-index object ring index num-items))))))


(defun make-hex-board(rings sides radius x y)
  (let ((board (make-instance 'hex-board :num-rings rings :ring-radius radius
			      :rings (make-array rings :adjustable nil)
			      :cx x :cy y)))
    (loop for r from 0 to (1- rings) do
	  (setf (aref (slot-value board 'rings) r)
		(make-array (1+ (* (1- sides) r)) :adjustable nil :initial-element nil)))
    board))

(defun hb-screen-x-y-to-ring(hb x y)
  (with-slots (cx cy ring-radius) hb
	      (let ((dist (sqrt (+ (sqr (- cx x)) (sqr (- cy y))))))
		(round (/ dist ring-radius)))))
  
(defun hb-screen-x-y-to-ring-index(hb x y)
  (with-slots (cx cy ring-radius num-rings rings) hb
	      (let ((dx (- x cx))
		    (dy (- y cy))
		    (ring (hb-screen-x-y-to-ring hb x y)))
		(if (< ring num-rings)
		    (let* ((angle-degs (rads-degs (atan2 dx (- dy))))
			  (items (length (aref rings ring)))
			  (item-arc (/ 360.0 items)))
		      (round (/ angle-degs item-arc)))
		  -1))))

;;;; todo for these two functions, validate the ring index to the vertex length

(defun hb-set-ring-index(hb ring index value)
  "set the specified ring and index in that ring with the specified value"
  (with-slots (rings num-rings) hb 
	      (if (>= ring num-rings)
		  (error "invalid ring")
		(let ((ring-items (aref rings ring)))
		  (if (>= index (length ring-items))
		      (error "invalid ring index")
		    (setf (aref ring-items index) value))))))


(defun hb-get-ring-index(hb ring index)
  "get the value stored at the specified ring and index"
  (with-slots (rings num-rings) hb 
	      (if (>= ring num-rings)
		  (error "invalid ring")
		(let ((ring-items (aref rings ring)))
		  (if (>= index (length ring-items))
		      (error "invalid ring index")
		    (aref ring-items index))))))

