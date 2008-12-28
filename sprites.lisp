;;;; SDL Drawing, file operations and animation for images

(defpackage :sprites
  (:use #:cl #:lispbuilder-sdl)
  (:export 
   #:flush-image-cache 
   #:load-sprite-image 
   #:draw-sprite 
   #:get-sprite-frame-with-index
   #:get-frame-from-time-and-speed
   #:time-per-frame
   #:get-sprite-frame-width-and-height
   #:sprite-def-frames))

(in-package :sprites)

(defstruct sprite-def-frame name x1 y1 x2 y2 xoff yoff)
(defstruct sprite-def bmp-file background-colour frames)

;;;; The image cache is a hash table of bmp filenames for each sprite 
;;;; and sdl surface associated with them. 

(defun make-image-cache()
  "Initialise the image cache"
  (make-hash-table :test #'equal))

; hash table of surfaces (to image filenames)
(defparameter *bmp-surfaces* (make-image-cache))

(defun flush-image-cache()
  "Flushes the image cache, freeing all the surfaces, uses sdl::free to avoid messing up weak pointer handling"
  (loop for v being the hash-values of *bmp-surfaces* do 
	(sdl::free v))
  (setf *bmp-surfaces* (make-image-cache)))

(defun load-sprite-image(sprite-def)
  "Given a sprite def load in it's source bmp and make a surface with the appropriate colour key"
  (let ((surface (gethash (sprite-def-bmp-file sprite-def) *bmp-surfaces*)))
    (unless surface
      (let ((image-surface 
	     (sdl:load-image (sprite-def-bmp-file sprite-def) :color-key (sprite-def-background-colour sprite-def))))
	(if (null image-surface)
	    (error "failed to get a surface from the bmp ~a" image-surface))
	(if image-surface
	    (setf (gethash (sprite-def-bmp-file sprite-def) *bmp-surfaces*) image-surface)
	    (error "Unable to load imagefile ~a~%" (sprite-def-bmp-file sprite-def)))))))

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

(defun get-sprite-frame-width-and-height(sprite-def sprite-frame)
  "given a sprite definition and a frame number find the width 
and height and return them as a a list"
  (let ((frame (get-sprite-frame (sprite-def-frames sprite-def) sprite-frame)))
    (values
     (1+ (- (sprite-def-frame-x2 frame) (sprite-def-frame-x1 frame)))
     (1+ (- (sprite-def-frame-y2 frame) (sprite-def-frame-y1 frame))))))

(defun round-coords-and-add-offset(x y xoff yoff)
  "When drawing sprites, this rounds the coords to make sure they are integer
and then subtracts the offset. For example if the offset is in the center of 
the sprite, then drawing it at 0,0 actualy draws it - xoff, - yoff"
  (values 
   (- (round x) xoff)
   (- (round y) yoff)))
  
(defun draw-sprite(screen x y sprite-def target-frame)
  "Draw a sprite"
  (load-sprite-image sprite-def) ; ensure bmp is loaded (will only load once)
  (let ((surface (gethash (sprite-def-bmp-file sprite-def) *bmp-surfaces*))
	(frame (get-sprite-frame (sprite-def-frames sprite-def) target-frame)))
    (multiple-value-bind (xnew ynew)
	(round-coords-and-add-offset x y (sprite-def-frame-xoff frame) (sprite-def-frame-yoff frame))
      (multiple-value-bind (sw sh)
	  (get-sprite-frame-width-and-height sprite-def target-frame)
	(if surface
	    (let ((src-rect (rectangle :x (sprite-def-frame-x1 frame)
				       :y (sprite-def-frame-y1 frame)
				       :w sw :h sh))
		  (dst-rect (rectangle :x xnew :y ynew)))
	      (sdl-base:blit-surface (sdl:fp surface) (sdl:fp screen) (sdl:fp src-rect) (sdl:fp dst-rect)))
	    (error "no surface"))))))

(defun get-frame-from-time-and-speed(num-frames speed time)
  "Given the number of frames in an animation and the speed, figure out which frame to be playing at a given time"
  (if (= speed 0.0)
      0
      (mod (floor (/ time (time-per-frame speed))) num-frames)))
  
(defun time-per-frame(speed) 
  "How long each animation frame takes"
  (if (> speed 0.0)
      (/ 1.0 speed)
      0.0))
    
