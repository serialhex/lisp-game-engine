;;;; Code which manages games and levels

(defclass level()
  ((name :initform nil :initarg :name)
   (objects :initform nil :initarg :objects))
  (:documentation "Levels are just named lists of objects"))

(defclass game()
  ((name :initform nil :initarg :name)
   (current-level :initform nil :initarg :current-level)
   (levels :initform (make-hash-table :test #'equal))
   (requested-level :initform nil :initarg :requested-level)
   (active-objects :initform nil :initarg :active-objects)
   (start-level :initform nil :initarg :current-level))
  (:documentation "Game manages a set of levels and a list of active objects"))

(defun game-add-level(game level &optional (start-level-p nil))
  "Add a level to the game hashed by name"
  (let ((levels (slot-value game 'levels))
	(level-name (slot-value level 'name)))
    (setf (gethash level-name levels) level))
  (if start-level-p
      (setf (slot-value game 'start-level) level)))

(defun game-change-level(game)
  "Set the game active-objects to be the ones in the new level."
  (with-slots '(current-level active-objects requested-level) game
    (setf current-level requested-level)
    (setf active-objects (slot-value current-level objects))))

(defun game-update(game)
 "Update game. Manages level changes and updates the active list of objects.
I may make this a method and ensure that this is called but let the user code
do game specific update"
 (when game
   (with-slots '(current-level requested-level start-level) game
     (if (null current-level)
	 (setf requested-level start-level)))))




       (unless (equal current-level requested-level)
	 (game-change-level game))
       ; Send the user define messages and arguments for each update
       ; TODO this will actually be user defined, not determined here
       (send-message-to-all-objects active-objects 'update (/ 1.0 (sdl:frame-rate)))
       (send-message-to-all-objects active-objects 'collide)
       (send-message-to-all-objects active-objects 'draw))))

(defun game-update(game)
 "Update game. Manages level changes and updates the active list of objects.
I may make this a method and ensure that this is called but let the user code
do game specific update"
 (with-slots '(start-level) game
   (describe start-level)))
     (describe start-level))))


(defun game-request-level(game level-name)
  "Ask the game to change levels next update"
  (with-slots '(requested-level levels) game
    (let ((level (gethash level-name levels)))
      (setf requested-level level))))










