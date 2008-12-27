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

(defun level-add-object(level obj)
  "Add an object to the object list"
  (setf (slot-value level 'objects) (cons obj (slot-value level 'objects)))) 

(defun level-add-objects(level objs)
  "Add a list of objects to the object list"
  (setf (slot-value level 'objects) (append objs (slot-value level 'objects)))) 

(defun game-add-level(game level &optional (start-level-p nil))
  "Add a level to the game hashed by name"
  (let ((levels (slot-value game 'levels))
	(level-name (slot-value level 'name)))
    (setf (gethash level-name levels) level))
  (if start-level-p
      (setf (slot-value game 'start-level) level)))

(defun game-change-level(game)
  "Set the game active-objects to be the ones in the new level."
  (with-slots (current-level active-objects requested-level) game
    (setf current-level requested-level)
    (setf active-objects (slot-value current-level 'objects))

    ; todo this may need to be an option. for example you want to be
    ; able to have some objects move between levels, and not get reset 
    ; messaqes
    (send-message-to-all-objects active-objects 'reset)))

(defgeneric game-update(game))

(defmethod game-update :before ((game game))
 "Update game. Manages level changes and updates the active list of objects.
Note that the user needs a game update that will be called after this, which 
must update the objects by sending appropriate update messages."
 (with-slots (current-level requested-level start-level active-objects) game
   (if (null current-level)
       (setf requested-level start-level))
   (unless (equal current-level requested-level)
     (game-change-level game))))

(defun game-request-level(game level-name)
  "Ask the game to change levels next update"
  (with-slots (requested-level levels) game
    (let ((level (gethash level-name levels)))
      (setf requested-level level))))

(defun game-find-object-with-name(name)
  (let ((game (engine-get-game))
	(level (slot-value *engine-game* 'current-level)))
    (find-object-with-name (slot-value level 'objects) name)))

(defun game-find-components-of-type(type)
  (let* ((found-components nil)
	 (game (engine-get-game))
	 (level-objects (slot-value (slot-value game 'current-level) 'objects)))
    (get-components-of-type level-objects type)))

(defun game-add-object-to-active-objects(obj)
  "Add an object to the active object list"
  (push obj (slot-value (engine-get-game) 'active-objects)))










