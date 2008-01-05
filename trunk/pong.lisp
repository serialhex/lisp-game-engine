;;;; components needed to play pong

;;;; some data for pong 

(defparameter *paddle-hoz-offset* 30.0)
(defparameter *paddle-left-offset* *paddle-hoz-offset*)
(defparameter *paddle-right-offset* (- 640 70))

(defparameter *paddle-start-y* (/ 480 2))

;;;; the brains behind the player paddle
(defclass player-paddle-logic(component)
  ((score :initform 0 :initarg :score)
   (hits-this-rally :initform 0 :initarg :hits-this-rally)
   (control-type :initform 'none :initarg :control-type) ; none, ai-easy, ai-hard, player etc
   (side :initform 'left :initarg :side)))

(defparameter *player-paddle-speed* 5.0)

(defmethod handle-message((comp player-paddle-logic) message-type &rest rest)  
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('update
       (let ((phys (find-component-with-type owner '2d-physics)))
	 ; update differently based on ai type
	 ; in a more complex game the ai or player stuff can be broken into more
         ; components, or use more advanced ones like a state machine, planning 
	 ; rule based systems and so on.
	 (case (slot-value comp 'control-type)
	   ('ai-hard
	    (with-slots (x y vx vy) phys 
	      (setf vy 0.0)
	      (when (< y 20.0)
		  (setf vy *player-paddle-speed*))
	      (when (> y 400.0)
		  (setf vy (* -1 *player-paddle-speed*)))))
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
; this deletes the object
;	     (remove-object-with-name-from-active-list (slot-value owner 'name)))
	   (when (> (+ x width) 639)
	     (setf vx (* -1 (abs vx))))
	   (when (< y 0)
	     (setf vy (abs vy)))
	   (when (> (+ y height) 479)
	     (setf vy (* -1 (abs vy))))
	   ; on collision with player 
           (if collision-list 
	       ; reverse horizontal speed
	       (setf vx (abs vx)))))))))

       