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
	  (if selected-p
	      (if prev-item
		  (progn
		    (setf selected-p nil)
		    (with-component-of-type-slots (prev-item 'menu-item (selected-p))
		      (setf selected-p t))))))	      
	 ((input:key-pressed-p :SDL-KEY-DOWN)
	  (if selected-p
	      (if next-item
		  (progn
		    (setf selected-p nil)
		    (with-component-of-type-slots (next-item 'menu-item (selected-p))
		      (setf selected-p t))))))	      
	 ((input:key-pressed-p :SDL-KEY-RETURN)
	  (if (and selected-p action)
	      (funcall action))))))))
	  

(defun make-menu-item(text x y justification action &key prev-item next-item)
  "returns a menu item made up of a text object and a menu component"
  (let ((menu-item (make-text-object text x y justification)))
    (add-component menu-item (make-instance 'menu-item :action action :prev-item prev-item :next-item next-item))
    menu-item))

(defun menu-fix-up-prev-next(list-of-menu-objects &optional (wrap-p nil))
  "given a list of objects with menu-item components, set up the prev
next item fields so that each item points to the one following it in the 
list and vice versa. If wrap-p is true then the last item goes to the first
and again, the opposite is true"
  (offset-list-iterator (((prev -1) (cur 0) (next 1)) list-of-menu-objects)
    (with-component-of-type-slots (cur 'menu-item (prev-item next-item))
      (setf prev-item prev)
      (setf next-item next)))
  list-of-menu-objects)
	 
(defun make-menu(list-of-text list-of-actions initial-selected x y y-spacing justification)
  "returns a list of objects, which together make up a menu"
  (let ((objects (loop 
		    for text in list-of-text 
		    for action in list-of-actions
		    for y-pos = y then (+ y-pos y-spacing) collect
		      (make-menu-item text x y-pos justification action))))
    (with-component-of-type-slots ((nth initial-selected objects) 'menu-item (selected-p))
      (setf selected-p t))
    (menu-fix-up-prev-next objects)))




   
