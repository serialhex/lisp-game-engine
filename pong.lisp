;;;; components needed to play pong

;;;; the brains behind the player paddle
(defclass player-paddle-logic(component)
  ())

(defparameter *player-paddle-speed* 5.0)

(defmethod handle-message((comp player-paddle-logic) message-type &rest rest)  
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('update
       (let ((phys (find-component-with-type owner '2d-physics)))
	 ; basic input 
	 (setf (slot-value phys 'vy) 0.0)
	 (when *player-1-key-up-held* 
	   (setf (slot-value phys 'vy) (* -1 *player-paddle-speed*)))
	 (when *player-1-key-down-held* 
	   (setf (slot-value phys 'vy) *player-paddle-speed*))
	 )))))

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
	   (when (< (+ x width) 0)
	     (remove-object-with-name-from-active-list (slot-value owner 'name)))
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

       