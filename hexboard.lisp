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

(defun make-hex-board(rings radius x y)
  (let ((board (make-instance 'hex-board :num-rings rings :ring-radius radius
			      :rings (make-array rings :adjustable nil)
			      :cx x :cy y)))
    (loop for r from 0 to (1- rings) do
	  (setf (aref (slot-value board 'rings) r)
		(make-array (1+ r) :adjustable nil :initial-element nil)))
    board))

(defun hb-screen-x-y-to-ring-index(hb x y)
  (with-slots (cx cy ring-radius) hb
	      (let ((dist (sqrt (+ (sqr (- cx x)) (sqr (- cy y))))))
		(round (/ dist ring-radius)))))
  
(defun hb-screen-x-y-to-ring-index-index(hb x y)
  (with-slots (cx cy ring-radius) hb
	      (let ((x (- cx x) (- cy y)))
		(let (angle-degs (rads-degs (atan2 x y)))
		  1.0)))) ; now return the number elements in ring index
; round divided by the angle






               




                         