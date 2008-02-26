;;;; Input - Joystick and keyboard
; Provides some utilities for managing key presses
; joystick and mouse input, for the IGE

(defpackage :input
  (:use #:cl #:lispbuilder-sdl)
  (:export 
   #:initialise
   #:key-pressed-p 
   #:key-released-p 
   #:key-held-p
   #:key-held-time
   #:key-released-time
   #:handle-key-up
   #:handle-key-down
   #:update
   #:quit))

(in-package :input)

(defparameter *initialised* nil)
(defparameter *key-status-table* nil)

(defstruct key-status status time prev-time)

(defun get-or-create-key-status(key)
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	status
	(setf (gethash key *key-status-table*) 
	      (make-key-status :status 'unknown :time 0.0 :prev-time 0.0)))))

(defun initialise()
  "set up the input system"
  (unless *initialised*
    (setf *initialised* t)
    (setf *key-status-table* (make-hash-table))))

(defun debug-view-keys()
  (loop for key being the hash-keys of *key-status-table* do
       (let ((status (gethash key *key-status-table*)))
	 (format t "key ~a status ~a time ~a prev time ~a~%" key (key-status-status status) 
		 (key-status-time status) (key-status-prev-time status)))))

(defun update(time)
  ;(debug-view-keys)
  (loop for key-status being the hash-values of *key-status-table* do
       (incf (key-status-time key-status) time)))

(defun quit()
  (setf *initialised* nil)
  (setf *key-status-table* nil))

(defun handle-key-up(key)
  (let ((status (get-or-create-key-status key)))
    (setf (key-status-status status) 'released)
    (setf (key-status-prev-time status) (key-status-time status))
    (setf (key-status-time status) 0.0)))

(defun handle-key-down(key)
  (let ((status (get-or-create-key-status key)))
    (setf (key-status-status status) 'pressed)
    (setf (key-status-prev-time status) (key-status-time status))
    (setf (key-status-time status) 0.0)))

(defun key-held-p(key)
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	(equal 'pressed (key-status-status status))
	nil)))

(defun key-held-time(key)
  (let ((status (gethash key *key-status-table*)))
    (if (key-status-p status)
	(key-status-time status)
	0.0)))

(defun key-pressed-p(key)
  (let ((status (gethash key *key-status-table*)))
    (if (and (key-status-p status)
	     (equ (key-status-status status) 'pressed)
	     (= 0.0 (key-status-time status)))
	t
	nil)))


	
	     
