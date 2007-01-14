;;;; SDL Drawing and low level data management for colour keyed sprites

(defpackage :sprites
  (:use #:cl #:lispbuilder-sdl)
  (:export 
   #:flush-image-cache 
   #:load-sprite-image 
   #:draw-sprite 
   #:get-sprite-frame-with-index
   #:get-frame-from-time-and-speed
   #:time-per-frame))

(in-package :sprites)

; hash table of surfaces (to image filenames)
(defparameter *bmp-surfaces* (make-image-cache))

(defstruct sprite-def-frame name x1 y1 x2 y2)
(defstruct sprite-def bmp-file background-colour frames)

;;;; The image cache is a hash table of bmp filenames for each sprite 
;;;; and sdl surface associated with them. 

(defun make-image-cache()
  "Initialise the image cache"
  (make-hash-table :test #'equal))

(defun flush-image-cache()
  "Flushes the image cache, freeing all the surfaces"
  (loop for v being the hash-values of *bmp-surfaces* do 
	(sdl-cffi::SDL-Free-Surface (sdl:fp v)))
  (setf *bmp-surfaces* (make-image-cache)))

(defun load-sprite-image(sprite-def-1)
  "Given a sprite def load in it's source bmp and make a surface with the appropriate colour key"
  (let ((surface (gethash (sprite-def-bmp-file sprite-def-1) *bmp-surfaces*)))
    (unless surface ; if the surface exists just exit
      (let ((surface (sdl:convert-surface
		      :surface (sdl:load-image (sprite-def-bmp-file sprite-def-1) #P".") 
		      :key-color (sprite-def-background-colour sprite-def-1))))
	(if surface
	    (progn 
	      (setf (gethash (sprite-def-bmp-file sprite-def-1) *bmp-surfaces*) surface)
	      (format t "loaded ~a ptr ~a~%" (sprite-def-bmp-file sprite-def-1) (sdl:fp surface)))
	  (error "Unable to load imagefile ~a~%" (sprite-def-bmp-file sprite-def-1)))))))

(defun get-sprite-frame-with-index(frame-list index)
  "Given a list of frames return the one with index provided"
  (sprite-def-frame-name (nth index frame-list)))

(defun get-sprite-frame(frame-list frame)
  "Find a frame with the name given"
  (if (null frame-list)
      (error "could not find frame ~a~%" frame))
  (if (eql (sprite-def-frame-name (car frame-list)) frame)
      (car frame-list)
    (get-sprite-frame(cdr frame-list) frame)))

(defun draw-sprite(screen x y sprite-def target-frame)
  "Draw a sprite"
  (load-sprite-image sprite-def) ; ensure bmp is loaded (will only load once)
  (let* ((surface (gethash (sprite-def-bmp-file sprite-def) *bmp-surfaces*))
	 (frame (get-sprite-frame (sprite-def-frames sprite-def) target-frame))
	 (sw (1+ (- (sprite-def-frame-x2 frame) (sprite-def-frame-x1 frame))))
	 (sh (1+ (- (sprite-def-frame-y2 frame) (sprite-def-frame-y1 frame)))))
    (if surface
	(sdl-base::with-rectangles ((src-rect) (dst-rect))
	  (setf (sdl-base::rect-x src-rect) (sprite-def-frame-x1 frame)
		(sdl-base::rect-y src-rect) (sprite-def-frame-y1 frame)
		(sdl-base::rect-w src-rect) sw
		(sdl-base::rect-h src-rect) sh)
	  (setf (sdl-base::rect-x dst-rect) x
		(sdl-base::rect-y dst-rect) y)
	  (sdl-base:blit-surface (sdl:fp surface) (sdl:fp screen)
				 src-rect  dst-rect))
	(error "no surface"))))

(defun get-frame-from-time-and-speed(num-frames speed time)
  "Given the number of frames in an animation and the speed, figure out which frame to be playing at a given time"
  (mod (floor (/ time (time-per-frame speed))) num-frames))
  
(defun time-per-frame(speed) 
  "How long each animation frame takes"
  (/ 1.0 speed))
    
