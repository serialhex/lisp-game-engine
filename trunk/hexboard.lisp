;;;; represent a board of hexes
;;;; 

(load "util.lisp")

(defclass hex-board (object)
  ((num-rings :initform nil :initarg :num-rings)
   (rings :initform nil :initarg :rings)
   (ring-radius :initform nil :initarg :ring-radius)
   (cx :initform 0.0 :initarg :cx)
   (cy :initform 0.0 :initarg :cy)
))

(defmethod update ((object hex-board) time-elapsed)
  )

(defmethod draw ((object hex-board))
  (with-slots (cx cy) object
	      (sdl:draw-box (sdl:rectangle :x cx :y cy :w 3 :h 3)
			    :color (sdl:color :r 255 :g 255 :b 255) ; color
			    :surface sdl:*default-display*)))


(defun make-hex-board(rings radius x y)
  (let ((board (make-instance 'hex-board :num-rings rings :ring-radius radius
			      :rings (make-array rings :adjustable nil)
			      :cx x :cy y)))
    (loop for r from 0 to (1- rings) do
	  (setf (aref (slot-value board 'rings) r)
		(make-array (1+ r) :adjustable nil :initial-element nil)))
    board))

(defun hb-screen-x-y-to-ring(hb x y)
  (with-slots (cx cy ring-radius) hb
	      (let ((dist (sqrt (+ (sqr (- cx x)) (sqr (- cy y))))))
		(round (/ dist ring-radius)))))
  
(defun hb-screen-x-y-to-ring-index(hb x y)
  (with-slots (cx cy ring-radius num-rings) hb
	      (let ((dx (- x cx))
		    (dy (- y cy)))
		(let ((angle-degs (rads-degs (atan2 dx (- dy)))))
		  (round (/ angle-degs num-rings)))))) ; now return the number elements in ring index
; round divided by the angle






               




                         