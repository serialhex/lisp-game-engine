;;;; components needed to play pong

(load "spritedefs1") ; sample sprite definitions

;; some data for pong 

(defparameter *paddle-side-offset* 50.0)

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
    (let* ((ball-obj (game-find-object-with-name "ball"))
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
	    (when (input:key-held-p :SDL-KEY-UP)
	      (setf (slot-value phys 'vy) 
		    (* -1.0 *player-paddle-speed* (+ 0.75 (input:key-held-time :SDL-KEY-UP)))))
	    (when (input:key-held-p :SDL-KEY-DOWN)
	      (setf (slot-value phys 'vy) 
		    (* 1.0 *player-paddle-speed* (+ 0.75 (input:key-held-time :SDL-KEY-DOWN))))))
	   (otherwise
	    (error "~a is not a known control type for player-paddle-logic component"))))))))

;;;; This is the component that manages the ball

(defclass ball-logic(component)
  ((pause :initform 3.0 :initarg :pause)))

(defmethod handle-message((comp ball-logic) message-type &rest rest)  
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('update
       (let ((phys (find-component-with-type owner '2d-physics))
	     (dt (/ 1.0 (sdl:frame-rate))))
	 (with-slots (x y vx vy ax ay width height collision-list) phys
	   (with-slots (pause) comp

	     ; pause between goals handling 
	   
	     (if (> pause 0.0)
		 (progn
		   (decf pause dt)
		 (if (< pause 0.0)
		     (progn 
		       (setf pause 0.0)
		       (setf vx (random-range 4.0 10.0))
		       (setf vy (random-range -8.0 8.0))))))

	     ; hit goal handling	      

	     (when (< x -20)
	       (progn
		 (setf vx 0.0)
		 (setf vy 0.0)
		 (setf x (screen-center-x))
		 (setf y (screen-center-y))
		 (setf pause 3.0)))
	     
	     (when (> (+ x width) (+ 20 639))
	       (progn
		 (setf vx 0.0)
		 (setf vy 0.0)
		 (setf x (screen-center-x))
		 (setf y (screen-center-y))
		 (setf pause 3.0)))

  	     ; top of screen handling - just bounce

	     (when (< y 0)
	       (setf vy (abs vy)))
	     
	     (when (> (+ y height) 479)
	       (setf vy (* -1 (abs vy))))

	     ; on collision with player 
	     (if collision-list 
		 (if (< x (screen-center-x))
		     (setf vx (abs vx))
		     (setf vx (* -1 (abs vx))))))))))))

;;;; This is the component that manages the high level game logic

(defclass pong-logic(component)
  ((left-score :initform 0 :initarg :left-score)
   (right-score :initform 0 :initarg :right-score)
   (win-score :initform 1 :initarg :win-score)
   (last-scorer :initform nil :initarg :last-scorer)))

(defmethod handle-message((comp pong-logic) message-type &rest rest)  
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('update
       t))))

(defun make-game-logic()
  "constructs an object with no visible components that
has the responsiblity for managing a single game of pong"
  (let ((obj (make-instance 'composite-object :name "game logic"))
	(logic (make-instance 'pong-logic
			    :left-score 0
			    :right-score 0
			    :win-score 10
			    :last-scorer (random-nth '(left right)))))
    (add-component obj logic)
    obj))

(defun make-left-pong-player()
  (let ((phys (make-instance '2d-physics
			     :collide-type 'paddle))
	(anim (make-instance 'animated-sprite
			     :sprite-def left-bat-sprite :current-frame 'frame-1
			     :speed 8.0)) ; frames per second
	(pong (make-instance 'player-paddle-logic
			     :control-type 'human-keyboard
			     :side 'left))
	(obj (make-instance 'composite-object
			    :name "human player 1")))
    (add-component obj phys)
    (add-component obj anim)
    (add-component obj pong)
    obj))

(defun make-text-object(string x y justification color &optional name)
  "constructs an object with a text and physics components
and the specified text properties"
  (let ((phys (make-instance '2d-physics :x x :y y))
	(text (make-instance 'text :justification justification :string string :color color))
	(obj (make-instance 'composite-object
			    :name (or name (random-unique-name)))))
    (add-component obj phys)
    (add-component obj text)
    obj))

(defun make-right-pong-player()
  (let ((phys (make-instance '2d-physics
			     :collide-type 'paddle))
	(anim (make-instance 'animated-sprite
			     :sprite-def right-bat-sprite :current-frame 'frame-1
			     :speed 16.0)) ; frames per second
	(pong (make-instance 'player-paddle-logic
			     :control-type 'ai-hard
			     :side 'right))
	(obj (make-instance 'composite-object
			    :name "hard ai player")))
    (add-component obj phys)
    (add-component obj anim)
    (add-component obj pong)
    obj))

(defun make-ball()
  (let ((phys (make-instance '2d-physics
			     :x (random-range 0.0 640.0) :y (random-range 0.0 480.0) 
			     :vx (random-range 4.0 10.0) :vy (random-range -8.0 8.0)
			     :collide-type 'ball
			     :collide-with-types '(paddle left-goal right-goal wall)))
	(anim (make-instance 'animated-sprite
			     :sprite-def ball-sprite :current-frame 'frame-1
			     :speed 4.0))
	(ball (make-instance 'ball-logic))
	(obj (make-instance 'composite-object
			    :name "ball")))
    (add-component obj phys)
    (add-component obj anim)
    (add-component obj ball)
    obj))


(defun action-main-menu()
  (format t "Main menu (todo)~%"))

(defun action-do-something()
  (format t "do something !~%"))

(defun action-quit-engine()
  (format t "Quitter!~%"))

(defun action-start-game()
  "starts the game by going to the gameplay level"
  (game-request-level (engine-get-game) "level 1"))

(defun std-text-color()
  (sdl:color :r #xe0 :g #xe0 :b #xe0))

(defun make-title-level()
  "creates title screen"
  (let ((level (make-instance 'level :name "title")))
    (level-add-object level (make-text-object "Pong ..." 20 200 :left (std-text-color)))
    (level-add-object level (make-text-object "... rocks" 620 200 :right (std-text-color)))
    (level-add-objects level 
		       (make-menu  '("Start game" 
				    "Quit")  
				    '(action-start-game 
				      action-quit-engine)
				    0 
				    320 100 
				    20 :center 
				    (sdl:color :r #xe0 :g #xe0 :b #xe0)
				    (sdl:color :r #xff :g #xff :b #xff)))
    level))

(defun make-player-select()
  "creates player select screen"
  (let ((level (make-instance 'level :name "player select")))
    (level-add-object level (make-text-object "Choose number of players" 320 200 :center (std-text-color)))
    (level-add-object level (make-text-object "And/Or Ai level" 320 300 :center (std-text-color)))
    level))

(defun make-frame-rate-display()
  "make an object that displays the average frame rate"
  (let ((obj (make-text-object "fps" 300 10 :center (std-text-color))))
    (add-component obj (make-instance 'frame-rate-to-text))
    obj))

(defun make-gameplay-level()
  "creates the pong gameplay level"
  (let ((level (make-instance 'level :name "level 1")))
    (level-add-object level (make-left-pong-player))
    (level-add-object level (make-right-pong-player))
    (level-add-object level (make-ball))
    (level-add-object level (make-game-logic))
    (level-add-object level (make-frame-rate-display))
    level))

(defun make-pong()
  "create the objects for the game and start it up"
  (let ((game (make-instance 'game :name "Pong")))
    (game-add-level game (make-player-select))
    (game-add-level game (make-gameplay-level))
    (game-add-level game (make-title-level) t)
    (engine-set-game game)))





