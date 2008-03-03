;;;; Objects and components for doing a menu

(defclass menu-item(component)
  ((prev-item :initform nil :initarg :prev-item)
   (next-item :initform nil :initarg :next-item)
   (action :initform nil :initarg :action :documentation "What to do when selected and activated")
   (active-p :initform nil :initarg :active-p :documentation "True if active")
   (selected-p :initform nil :initarg :selected-p :documentation "True if selected (highlighted)")))

(defmethod handle-message((comp menu-item) message-type &rest rest)  
  (with-slots (prev-item next-item action active-p selected-p) comp
    (case message-type 
      ('update
       (cond 
	 ((input:key-pressed-p :SDL-KEY-UP)
	  (format t "menu item key up~%"))
	 ((input:key-pressed-p :SDL-KEY-DOWN)
	  (format t "menu key down~%"))
	 ((input:key-pressed-p :SDL-KEY-RETURN)
	  (if action
	      (funcall action))))))))
	  

(defun make-menu-item(text x y justification action &key prev-item next-item)
  "returns a menu item made up of a text object and a menu component"
  (let ((gui-item (make-text-object text x y justification)))
    (add-component gui-item (make-instance 'menu-item :action action :prev-item prev-item :next-item next-item))
    gui-item))

(defun menu-fix-up-prev-next(list-of-menu-objects &optional (wrap-p nil))
  "given a list of objects with gui-item components, set up the prev
next item fields so that each item points to the one following it in the 
list and vice versa. If wrap-p is true then the last item goes to the first
and again, the opposite is true"
  (let ((first-item (first list-of-menu-objects)))
    (loop for item-index from 0 to (1- (length list-of-menu-objects)) do
)))
	 

(defun make-menu(list-of-text x y y-spacing justification list-of-actions)
  "returns a list of objects, which together make up a menu"
  (menu-fix-up-prev-next
   (loop 
      for text in list-of-text 
      for action in list-of-actions
      for y-pos = y then (+ y-pos y-spacing) collect
	(make-menu-item text x y-pos justification action))))
