;;;; Pong implemented with IGE

; TODO

; limit paddles to court bounds... court bounds just globals

; DONE animated sprites need a handle. an offset to draw them at. for example, the balls physics position should be the
; center of the bitmap. so the physics works as it does now, but the draw position is at a user specified offset
; DONE set it up so paddles work. 

; DONE add message system so ball is dummer.. reset message, serve message (direction)
; DONE game manages the pause

; draw everything at relative coordinates - define court with center at screen center - add basic 2d camera
; functionality

; DONE use offset for ball

; reset scores on game over

; quit game does not exit engine (bug?)

; rework collision so it works properly, not just reflect

; ball prediction not working

; DONE ball speed (need to launch at range of angles, same speed all the time)

;; load sprite info

(load "pongspritedefs") ; 

; parameters

(defparameter *paddle-side-offset* 30.0 "How far from the edges the paddles are")

(defparameter *paddle-start-y* (/ *WINDOW-HEIGHT* 2))
(defparameter *between-play-pause* 3.0)
(defparameter *ball-serve-speed* 250.0)
(defparameter *ball-serve-max-angle* (degs-rads 90.0))
(defparameter *player-start-paddle-speed* 200.0)
(defparameter *player-max-paddle-speed* 800.0)
(defparameter *player-paddle-accel* 2600.0)
(defparameter *hard-ai-paddle-speed* 60.0)
(defparameter *win-score* 20)

(defparameter *left-player-up-key* :SDL-KEY-q)
(defparameter *left-player-down-key* :SDL-KEY-z)

(defparameter *right-player-up-key* :SDL-KEY-UP)
(defparameter *right-player-down-key* :SDL-KEY-DOWN)

; court - This defines the court relative to the screen 

(defparameter *court-screen-top-offset* 40)
(defparameter *court-screen-bottom-offset* 10)
(defparameter *court-screen-left-offset* 10)
(defparameter *court-screen-right-offset* 10)

(defun court-x-center()
  (+ *court-screen-left-offset*
     (/ (- (- *WINDOW-WIDTH* *court-screen-right-offset*) 
	   *court-screen-left-offset*) 2.0)))

(defun court-y-center()
  (+ *court-screen-top-offset*
     (/ (- (- *WINDOW-HEIGHT* *court-screen-bottom-offset*) 
	   *court-screen-top-offset*) 2.0)))

;; The pong game subclass the game object to make your game
;; this is not a composite-object or a component
;; it's a subclass of game, which has level management 
;; You put your high level game logic in here.
;; it's up to you what the game update does, but it should
;; at the very least update and draw the current level 
;; objects and components
;; In this case the update sends update, collide and draw
;; and before that it updates itself, managing the current 
;; level. 
;; In addition it has a message-handler which objects
;; can talk to using 
;; (handle-message (game message-type &rest rest))
;; This is where you store data that all components 
;; need to look at.

; todo a lot of these slots need hooking up
(defclass pong-game(game)
  ((left-score :initform 0 :initarg :left-score)
   (right-score :initform 0 :initarg :right-score)
   (hits-this-rally :initform 0 :initarg :hits-this-rally)
   (win-score :initform *win-score* :initarg :win-score)
   (last-scorer :initform nil :initarg :last-scorer)
   (pause :initform *between-play-pause* :initarg :pause :documentation "Pauses in the game between scored points")))

(defmethod update((game pong-game))
  (with-slots (current-level pause) game

    (if (string= (slot-value current-level 'name) "level1")

        ; handle game pause

	(with-slots (pause) game
	  (when (> pause 0.0)
	    (decf pause (sdl:frame-time))
	    (when (< pause 0.0) 

	        ; launch ball
		(setf pause 0.0)
		(let* ((ball-obj (game-find-object-with-name "ball"))
		       (ball-logic (find-component-with-type ball-obj 'ball-logic)))
	          ; todo must get serve direction from last scorer
		  (handle-message ball-logic 'serve 'left))))))))

(defun handle-game-over(left-score right-score win-score)
  "check for game over, and go to the game over level"
  (when (or (= win-score left-score) (= win-score right-score))
    (game-request-level (engine-get-game) "game over")
    (setf (slot-value (engine-get-game) 'left-score) 0)
    (setf (slot-value (engine-get-game) 'right-score) 0)))

; todo, the players should be an array so you can halve the amount of code here

(defmethod handle-message((game pong-game) message-type &rest rest)  
  (with-slots (active-objects left-score right-score win-score pause) game
    (case message-type
      ('reset
       (send-message-to-object-component active-objects "left score"
					 'text 'change-text 
					 (format nil "~2,'0d" 0))
       (send-message-to-object-component active-objects "right score"
					 'text 'change-text 
					 (format nil "~2,'0d" 0))
       )
      ('left-scored
       (incf left-score)
       (send-message-to-object-component active-objects "left score"
					 'text 'change-text 
					 (format nil "~2,'0d" left-score))
       (handle-game-over left-score right-score win-score)
       (setf pause *between-play-pause*)
       (send-message-to-object-component active-objects "ball" 'ball-logic 'reset))

      ('right-scored
       (incf right-score)
       (send-message-to-object-component active-objects "right score"
					 'text 'change-text 
					 (format nil "~2,'0d" right-score))
       (handle-game-over left-score right-score win-score)
       (setf pause *between-play-pause*)
       (send-message-to-object-component active-objects "ball" 'ball-logic 'reset)))))

; todo - optimise sending messages to all components of certain types
; for example populate lists based on what messages they receive,
; which would require changing handle-message to store the messages 
; in an accessible way...

(defmethod game-update((game pong-game))
  (with-slots (current-level requested-level start-level active-objects) game

    ; first update the pong-game objects
    ; a subclass of game
    ; does all the game high level logic
    (update game)

    ; now send various messages to the game objects
    ; this way the objects know that update is done before collide
    ; which is done before draw ...
    (send-message-to-all-objects active-objects 'update (sdl:frame-time))
    (send-message-to-all-objects active-objects 'collide)
    (send-message-to-all-objects active-objects 'draw)))

(defun get-player-paddle-speed(time)
  "get the player paddle speed based on how long it's been moving"
  (min *player-max-paddle-speed*
       (+ *player-start-paddle-speed* 
	  (* 0.5 *player-paddle-accel* (* time time)))))

;;;; the brains behind the player paddle
(defclass player-paddle-logic(component)
  ((control-type :initform 'none :initarg :control-type) ; none, ai-easy, ai-hard, player etc
   (side :initform 'left :initarg :side)))

(defun reflect-off-screen-y-to-screen(y height)
  "allows for size of ball"
  (let ((edge (- *WINDOW-HEIGHT* height)))
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
	     (and (> bx x) (< bvx 0))
	     (= bvx 0))
	    (let* ((target-y 
		    (if (= 0 bvx)
			(screen-center-y) ; if ball not moving, go to center
			(get-ball-y-intersect-at-x bx by bvx bvy x height)))
		   (sign (if (< target-y y) -1 1)))
	      (setf vy (* sign 
			  (min *hard-ai-paddle-speed*
			       (abs (- target-y y)))))
	      ; debug
	      (sdl:draw-pixel-* (- *WINDOW-WIDTH* 2) (floor target-y) :color (sdl:color :r 255 :g 255 :b 0))
))))))

(defun set-paddle-x-position(paddle-logic physics)
  "given a paddle logic component and its physics component
locate it correctly horizontally"
  (with-slots (x width) physics
    (if (equal (slot-value paddle-logic 'side) 'left)
	(setf x (+ *court-screen-left-offset* *paddle-side-offset*))
	(setf x (- *WINDOW-WIDTH* *court-screen-right-offset* *paddle-side-offset*)))))

; message handler for player paddle
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

             ; basic input - the up down arrows move the paddle
	     ; which moves more rapidly the longer the key is held
	    (setf (slot-value phys 'vy) 0.0)

	    (let* ((up-key (if (eq (slot-value comp 'side) 'left) 
			       *left-player-up-key*
			       *right-player-up-key*))
		   (down-key (if (eq (slot-value comp 'side) 'left) 
			       *left-player-down-key*
			       *right-player-down-key*))
		   (move-up (sdl:key-held-p up-key))
		   (move-down (sdl:key-held-p down-key)))

	      (when move-up
		(setf (slot-value phys 'vy) 
		      (* -1.0 (get-player-paddle-speed (sdl:key-time-in-current-state up-key)))))
	      (when move-down
		(setf (slot-value phys 'vy) 
		      (* 1.0 (get-player-paddle-speed (sdl:key-time-in-current-state down-key))))))

	    ; print out the velocity for debugging
;	    (format t "velocity ~2$ ~a ~%" (slot-value phys 'vy) (slot-value comp 'side) )

	    ; now clip to the court top
	    (if (and
		 (< (slot-value phys 'y) *court-screen-top-offset*)
		 (< (slot-value phys 'vy) 0.0))
		(progn
		  (setf (slot-value phys 'y) *court-screen-top-offset*)
		  (setf (slot-value phys 'vy) 0.0))))
	   (otherwise
	    (error "~a is not a known control type for player-paddle-logic component"))))))))

;;;; This is the component that manages the ball

(defclass ball-logic(component)
  ())

(defun pong-serve(phys)
  "Set up the x and y speed of the ball on a serve"
  (with-slots (vx vy) phys
    ; rotate by max angle about horizontal 
    (let ((rot (+ (degs-rads 90.0)
		  (random-range (/ *ball-serve-max-angle* -2.0) (/ *ball-serve-max-angle* 2.0)))))
      (setf vx (* *ball-serve-speed* (sin rot)))
      (setf vy (* *ball-serve-speed* (cos rot))))))

(defmethod handle-message((comp ball-logic) message-type &rest rest)  
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('reset
       ; set the ball to initial state

       (let ((phys (find-component-with-type owner '2d-physics)))
	 (with-slots (x y vx vy) phys
	   
	   (setf vx 0.0)
	   (setf vy 0.0)
	   (setf x (court-x-center))
	   (setf y (court-y-center)))))

      ('serve
       ; todo don't ignore direction to serve argument
       (let ((phys (find-component-with-type owner '2d-physics)))
	 (pong-serve phys)))

      ('update
       (let ((phys (find-component-with-type owner '2d-physics))
	     (dt (sdl:frame-time)))

	 (with-slots (x y vx vy ax ay width height collision-list) phys
	   (with-slots (pause) comp

	     (when (< x *court-screen-left-offset*)
	       (handle-message (engine-get-game) 'right-scored))
	     
	     (when (> x (- *WINDOW-WIDTH* *court-screen-right-offset*))
	       (handle-message (engine-get-game) 'left-scored))

  	     ; top of court handling - just bounce

	     (when (< y *court-screen-top-offset*)
	       (setf vy (abs vy)))
	     
	     (when (> y (- *WINDOW-HEIGHT* *court-screen-bottom-offset*))
	       (setf vy (* -1 (abs vy))))

	     ; collisions with other objects mean we hit the paddles
             ; just reverse the x velocity
	     ; todo would be nice to handle the paddles having rounded
	     ; corners to ping the ball off in more random directions
	     (if collision-list 
		 (if (< x (screen-center-x))
		     (setf vx (abs vx))
		     (setf vx (* -1 (abs vx))))))))))))

(defun make-pong-player(side human sprite-def control-type name)
  (let ((phys (make-instance '2d-physics
			     :collide-type 'paddle :y *paddle-start-y*))
	(anim (make-instance 'animated-sprite :sprite-def sprite-def
			     :current-frame 'frame-1 :speed 5.0))
	(pong (make-instance 'player-paddle-logic
			     :control-type control-type :side side))
	(obj (make-instance 'composite-object :name name)))
    (add-component obj phys)
    (add-component obj anim)
    (add-component obj pong)
    obj))

(defun make-left-pong-player()
  (make-pong-player 'left t left-bat-sprite 'human-keyboard "left player"))

(defun make-right-pong-player()
  (make-pong-player 'right t right-bat-sprite 'human-keyboard "right player"))

(defun make-ball()
  (let ((phys (make-instance '2d-physics
			     :x (random-range 0.0 *WINDOW-WIDTH*) :y (random-range 0.0 *WINDOW-HEIGHT*) 
			     :vx (random-range 4.0 10.0) :vy (random-range -8.0 8.0)
			     :collide-type 'ball
			     :collide-with-types '(paddle)))
	(anim (make-instance 'animated-sprite
			     :sprite-def ball-sprite :current-frame 'frame-1
			     :speed 8.0))
	(ball (make-instance 'ball-logic))
	(obj (make-instance 'composite-object
			    :name "ball")))
    (add-component obj phys)
    (add-component obj anim)
    (add-component obj ball)
    obj))

(defun action-quit-engine()
  (format t "Goodbye~%")
  (sdl:push-quit-event))

(defun action-start-game()
  "starts the game by going to the gameplay level"
  (game-request-level (engine-get-game) "level1"))

(defun std-text-color()
  (sdl:color :r #xe0 :g #xe0 :b #xe0))

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

(defun make-title-level()
  "creates title screen"
  (let ((level (make-instance 'level :name "title")))
    (level-add-object level (make-text-object "Pong ..." 20 200 :left (std-text-color)))
    (level-add-object level (make-text-object "... in Common Lisp" 620 200 :right (std-text-color)))
    (level-add-objects level 
		       (make-menu '("Start game" 
				    "Quit")  
				    '(action-start-game 
				      action-quit-engine)
				    0 
				    320 100 
				    20 :center 
				    (sdl:color :r #xe0 :g #xe0 :b #xe0)
				    (sdl:color :r #x00 :g #xff :b #x00)))
    level))

(defun action-main-menu()
  (game-request-level (engine-get-game) "title"))

(defun make-game-over-level()
  "creates game over screen"
  (let ((level (make-instance 'level :name "game over")))
    (level-add-objects level
		       (make-menu  '("GAME OVER") 
				   '(action-main-menu)
				   0 
				   320 200 
				   20 :center 
				   (sdl:color :r #xe0 :g #xe0 :b #xe0)
				   (sdl:color :r #xff :g #xff :b #xff)))

    level))

(defun make-player-select-level()
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

(defun score-x-position(side)
  (if (eq side 'left)
      20
      (- *WINDOW-WIDTH* 20)))

(defun score-text-justify(side)
  (if (eq side 'left)
      :left
      :right))

(defun score-string(score)
  "generate the score string"
  (format nil "~2,'0d" score))

(defun make-score-text(side)
  "make a text object for displaying the left or right score"
  (let ((name (string-downcase (format nil "~a score" side))))
    (make-text-object (score-string 0) 
		      (score-x-position side)
		      20
		      (score-text-justify side)
		      (sdl:color :r 255 :g 255 :b 255)
		      name)))

(defun make-background()
  (let ((phys (make-instance '2d-physics
			     :x 0 :y 0))
	(anim (make-instance 'animated-sprite
			     :sprite-def background-sprites :current-frame 'level-1))
	(obj (make-instance 'composite-object
			    :name "background")))
    (add-component obj phys)
    (add-component obj anim)
    obj))

; Handles drawing the court and court collision

(defclass court(component)
  ((top-offset :initform 0.0 :initarg :top-offset)
   (bottom-offset :initform 0.0 :initarg :bottom-offset)
   (left-offset :initform 0.0 :initarg :left-offset)
   (right-offset :initform 0.0 :initarg :right-offset)))

; court message handler

(defmethod handle-message((court-comp court) message-type &rest rest)  
  (let ((owner (slot-value court-comp 'owner)))
    (case message-type 
      ('update
       t)
      ('draw
       ; draw the screen bounds
       (with-slots (left-offset right-offset top-offset bottom-offset) court-comp
	 (sdl:draw-hline left-offset (- *WINDOW-WIDTH* right-offset) (- *WINDOW-HEIGHT* bottom-offset) :color (sdl:color :r 255 :g 255 :b 255))
	 (sdl:draw-vline left-offset top-offset (- *WINDOW-HEIGHT* bottom-offset) :color (sdl:color :r 255 :g 255 :b 255))
	 (sdl:draw-vline (- *WINDOW-WIDTH* right-offset) top-offset (- *WINDOW-HEIGHT* bottom-offset) :color (sdl:color :r 255 :g 255 :b 255))
	 (sdl:draw-hline left-offset (- *WINDOW-WIDTH* right-offset) top-offset :color (sdl:color :r 255 :g 255 :b 255))
	 (let ((center (round (+ left-offset (/ (- (- *WINDOW-WIDTH* right-offset) left-offset) 2.0)))))
	   (sdl:draw-vline center top-offset (- *WINDOW-HEIGHT* bottom-offset) :color (sdl:color :r 255 :g 255 :b 255))))))))

(defun make-court()
  (let ((court (make-instance 'court
			      :top-offset *court-screen-top-offset*
			      :bottom-offset *court-screen-bottom-offset*
			      :left-offset *court-screen-left-offset*
			      :right-offset *court-screen-right-offset*))
	(obj (make-instance 'composite-object
			    :name "court")))
    (add-component obj court)
    obj))

(defun make-gameplay-level()
  "creates the pong gameplay level"
  (let ((level (make-instance 'level :name "level1")))
    (level-add-object level (make-left-pong-player))
    (level-add-object level (make-right-pong-player))
    (level-add-object level (make-ball))
    (level-add-object level (make-frame-rate-display))
    (level-add-object level (make-score-text 'left))
    (level-add-object level (make-score-text 'right))
    (level-add-object level (make-court))
    (level-add-object level (make-background))
    level))

(defun make-pong()
  "create the objects for the game and start it up"
  (let ((game (make-instance 'pong-game 
			     :name "Pong")))
    (game-add-level game (make-player-select-level))
    (game-add-level game (make-gameplay-level))
    (game-add-level game (make-title-level) t)
    (game-add-level game (make-game-over-level))
    (engine-set-game game)))

(defun play-pong(&optional (full-screen nil))
  (engine-init :window-height 480 :window-width 640 :full-screen-p full-screen)
  (make-pong)
  (unwind-protect ; make sure any aborts call quit before returning
       (engine-run)
    (and
     (engine-quit)
     (setf *FULL-SCREEN-P* nil))))








