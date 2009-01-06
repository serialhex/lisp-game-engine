;;;; Objects and components for doing a menu

(defclass menu-item(component)
  ((prev-item :initform nil :initarg :prev-item)
   (next-item :initform nil :initarg :next-item)
   (action :initform nil :initarg :action :documentation "What to do when selected and activated")
   (active-p :initform nil :initarg :active-p :documentation "True if active")
   (selected-p :initform nil :initarg :selected-p :documentation "True if selected (highlighted)")
   (default-color :initform (sdl:color :r #xff :g #xff :b #xff) :initarg :default-color)
   (selected-color :initform (sdl:color :r #xff :g #xff :b #xff) :initarg :selected-color)
   (time-selected :initform 0.0 :initarg :time-selected)))

(defmethod handle-message((comp menu-item) message-type &rest rest)  
  (let ((owner (slot-value comp 'owner)))
    (with-slots (prev-item next-item action active-p selected-p 
			   selected-color default-color
			   time-selected) comp
      (case message-type 
	('update
	 (cond 

	   ; handle the up arrow key
	   ((sdl:key-pressed-p :SDL-KEY-UP)
	    (if (and selected-p (> time-selected 0.0))
		(if prev-item
		    (progn
		      (setf selected-p nil)
		      (setf time-selected 0.0)
		      (with-component-of-type-slots (owner 'text (color))
			(setf color default-color))
		      (with-component-of-type-slots 
			  (prev-item 'menu-item (selected-p time-selected))
			(setf selected-p t)
			(setf time-selected 0.0))
		      (with-component-of-type-slots (prev-item 'text (color))
			(setf color selected-color))))))

	   ; handle the down arrow key
	   ((sdl:key-pressed-p :SDL-KEY-DOWN)
	    (if (and selected-p (> time-selected 0.0))
		(if next-item
		    (progn
		      (setf selected-p nil)
		      (setf time-selected 0.0)
		      (with-component-of-type-slots (owner 'text (color))
			(setf color default-color))
		      (with-component-of-type-slots 
			  (next-item 'menu-item (selected-p time-selected))
			     (setf selected-p t)
			     (setf time-selected 0.0))
		      (with-component-of-type-slots (next-item 'text (color))
			(setf color selected-color))))))

	        ; handle return/enter key to trigger menu option
	   
		((sdl:key-pressed-p :SDL-KEY-RETURN)
		 (if (and selected-p action)
		     (funcall action))))
	 (setf time-selected (+ (sdl:frame-time))))))))
	  
(defun make-menu-item(text x y justification action default-color selected-color)
  "returns a menu item made up of a text object and a menu component"
  (let ((menu-item (make-text-object text x y justification default-color)))
    (add-component menu-item (make-instance 'menu-item :action action 
					    :default-color default-color :selected-color selected-color))
    menu-item))

(defun menu-set-prev-next(list-of-menu-objects &optional (wrap-p nil))
  "given a list of objects with menu-item components, set up the prev
next item fields so that each item points to the one following it in the 
list and vice versa. If wrap-p is true then the last item goes to the first
and again, the opposite is true"
  (offset-list-iterator (((prev -1) (cur 0) (next 1)) list-of-menu-objects)
    (with-component-of-type-slots (cur 'menu-item (prev-item next-item))
      (setf prev-item prev)
      (setf next-item next)))
  list-of-menu-objects)
	 
(defun make-menu(list-of-text list-of-actions initial-selected x y y-spacing justification default-color selected-color)
  "returns a list of objects, which together make up a menu"
  (let ((objects (loop 
		    for text in list-of-text 
		    for action in list-of-actions
		    for y-pos = y then (+ y-pos y-spacing) collect
		      (make-menu-item text x y-pos justification action default-color selected-color))))
    (with-component-of-type-slots ((nth initial-selected objects) 'menu-item (owner selected-p))
      (with-component-of-type-slots (owner 'text (color))
	(setf selected-p t)
	(setf color selected-color)))
    (menu-set-prev-next objects)))




   
