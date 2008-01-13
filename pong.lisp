;;;; components needed to play pong

;; some data for pong 

(defparameter *paddle-side-offset* 30.0)

;(defparameter *paddle-hoz-offset* 30.0)
;(defparameter *paddle-left-offset* *paddle-hoz-offset*)
;(defparameter *paddle-right-offset* (- 640 70))

(defparameter *paddle-start-y* (/ 480 2))

;;;; the brains behind the player paddle
(defclass player-paddle-logic(component)
  ((score :initform 0 :initarg :score)
   (hits-this-rally :initform 0 :initarg :hits-this-rally)
   (control-type :initform 'none :initarg :control-type) ; none, ai-easy, ai-hard, player etc
   (side :initform 'left :initarg :side)))

(defparameter *player-paddle-speed* 5.0)
(defparameter *hard-ai-paddle-speed* 7.0)

(defun reflect-off-screen-y-to-screen(y height)
  "allows for size of ball"
  (let ((edge (- 480 height)))
    (if (< y 0)
	(- y)
	(if (>= y edge)
	    (setf y (- edge (- y edge)))
	    y))))

(defun get-ball-y-intersect-at-x(bx by bvx bvy paddle-x height)
  "given a ball pos and speed, a paddle x coord and ball height, get the y intersect"
  (let ((dydx (/ bvy bvx))
	(diff-x (abs (- bx paddle-x))))
    (reflect-off-screen-y-to-screen 
     (+ by (* diff-x dydx))
     height)))

(defun do-hard-pong-ai-update(paddle-obj paddle-logic-comp paddle-phys-comp)
  "Do AI update for hard level AI"
  (with-slots (x y vx vy) paddle-phys-comp 
    (setf vy 0.0)
    (let* ((ball-obj (find-object-with-name-in-active-list "ball"))
	   (ball-phys (find-component-with-type ball-obj '2d-physics)))
      (with-slots ((bx x) (by y) (bvx vx) (bvy vy) height) ball-phys
	(if (or 
	     (and (< bx x) (> bvx 0))
	     (and (> bx x) (< bvx 0)))
	    (let* ((target-y 
		    (get-ball-y-intersect-at-x bx by bvx bvy x height))
		   (sign (if (< target-y y) -1 1)))
	      (setf vy (* sign 
			  (min *hard-ai-paddle-speed*
			       (abs (- target-y y)))))))))))

(defun set-paddle-x-position(paddle-logic physics)
  "given a paddle logic component and its physics component
locate it correctly horizontally"
  (with-slots (x width) physics
    (if (equal (slot-value paddle-logic 'side) 'left)
	(setf x *paddle-side-offset*)
	(setf x (- 640 1 width *paddle-side-offset*)))))

(defmethod handle-message((comp player-paddle-logic) message-type &rest rest)  
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('update
       (let ((phys (find-component-with-type owner '2d-physics)))
	 (set-paddle-x-position comp phys)
	 ; update differently based on ai type
	 ; in a more complex game the ai or player stuff can be broken into more
         ; components, or use more advanced ones like a state machine, planning 
	 ; rule based systems and so on.
	 (case (slot-value comp 'control-type)
	   ('ai-hard
	    (do-hard-pong-ai-update owner comp phys))
	   ('human-keyboard
             ; basic input 
	    (setf (slot-value phys 'vy) 0.0)
	    (when *player-1-key-up-held* 
	      (setf (slot-value phys 'vy) (* -1 *player-paddle-speed*)))
	    (when *player-1-key-down-held* 
	      (setf (slot-value phys 'vy) *player-paddle-speed*)))
	   (otherwise
	    (error "~a is not a known control type for player-paddle-logic component"))))))))

;;;; This is the component that manages the ball

(defclass ball-logic(component)
  ())

(defmethod handle-message((comp ball-logic) message-type &rest rest)  
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('update
       (let ((phys (find-component-with-type owner '2d-physics)))
	 (with-slots (x y vx vy ax ay width height collision-list) phys
	   ; simple screen boundary handling
	   (when (< x 0)
	     (setf vx (abs vx)))
	   (when (> (+ x width) 639)
	     (setf vx (* -1 (abs vx))))
	   (when (< y 0)
	     (setf vy (abs vy)))
	   (when (> (+ y height) 479)
	     (setf vy (* -1 (abs vy))))
	   ; on collision with player 
           (if collision-list 
	       (if (< x (screen-center-x))
		   (setf vx (abs vx))
		   (setf vx (* -1 (abs vx)))))))))))

       