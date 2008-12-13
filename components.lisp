;;;; This file contains all you need to make objects that 
;;;; are composed of generic components

;;;; Base class for components
(defclass component()
  ((owner :initform nil :initarg :owner)))

;;;; Base class for objects composed of components
(defclass composite-object()
  ((name :initform (format nil "anon_~4,'0D" (get-next-uid)) :initarg :name)
   (components :initform nil :initarg :components)))

(defun send-message-to-all-objects(objects message &rest args)
  "Send the message to every component of every object"
  (dolist (obj objects)
    (dolist (comp (slot-value obj 'components))
      (apply #'handle-message comp message args))))

(defun send-message-to-object-component(objects name component message &rest args)
  "used to send a message to a named objects component"
  (let ((obj (find-object-with-name objects name)))
    (if obj
	(let ((comp (find-component-with-type obj component)))
	  (if comp
	      (apply #'handle-message comp message args)
	      (error "component not found")))
	(error "object not found"))))

(defun find-object-with-name(objects name)
  "find object with name"
  (find-if 
   (lambda (obj) (string-equal name (slot-value obj 'name))) 
   objects))

(defun remove-object-with-name(objects name)
  "remove any objects with name"
  (setf objects
	(remove-if 
	 (lambda (obj) (string-equal name (slot-value obj 'name)))
	 objects)))

(defun game-debug-view-active-objects()
  (let ((objects (slot-value (engine-get-game) 'active-objects)))
    (dolist (obj objects) 
      (format t "name ~a~%" (slot-value obj 'name)))))

(defun get-components-of-type(objects type)
  (let ((found-components nil))
    (dolist (obj objects              )
      (dolist (comp (slot-value obj 'components))
	(if (equal type (type-of comp))
	    (setf found-components (cons comp found-components)))))
    found-components))

(defgeneric add-component(obj component))
(defgeneric handle-message(component message-type &rest rest))

(defmethod handle-message((comp component) message-type &rest rest)
())

; note: should this be generic? it only really has this behaviour
; note: this returns an obj because thats useful to me at a point in 
; the code but probably not a great reason
(defmethod add-component((obj composite-object) (comp component))
  (setf (slot-value comp 'owner) obj)
  (push comp (slot-value obj 'components))
  obj)

;;; Functions for locating components by name, type and so on

(defgeneric find-component-with-type(object type))

(defmethod find-component-with-type((obj composite-object) type)
  (with-slots (components) obj
    (if components
	(dolist (comp components)
	  (if (equal (type-of comp) type)
	      (return-from find-component-with-type comp)))
	nil)))

(defmacro with-component-of-type-slots((object type slots) &body body)
  `(let ((comp (find-component-with-type ,object ,type)))
     (when comp
       (with-slots ,slots comp
	 ,@body))))
     
   
;;;; add this component to a text object to show the frame rate 

(defclass frame-rate-to-text(component)
  ())

(defmethod handle-message((comp frame-rate-to-text) message-type &rest rest)
  (let ((owner (slot-value comp 'owner)))
    (case message-type 
      ('update
	 (let ((text-comp (find-component-with-type owner 'text)))
	   (with-slots (string) text-comp 
	     (setf string (format nil "fps: ~2$" (coerce (sdl:average-fps) 'single-float))))))))) 
       









