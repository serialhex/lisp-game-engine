;;;; SDL Drawing and low level data management for colour keyed sprites

(defpackage :sprites
  (:use #:cl #:lispbuilder-sdl)
  (:export 
   #:flush-image-cache #:load-sprite-image #:draw-sprite))

(in-package :sprites)

(defstruct sprite-def-frame name x1 y1 x2 y2)
(defstruct sprite-def bmp-file background-colour frames)

;;;; hash table of bmp filenames for each sprite and sdl surfaces
;;;; associated with them. 
(defun make-image-cache()
  (make-hash-table :test #'equal))

(defparameter *bmp-surfaces* (make-image-cache))

(defun flush-image-cache()
  (loop for v being the hash-values of *bmp-surfaces* do 
	(sdl:SDL_FreeSurface v))
  (setf *bmp-surfaces* (make-image-cache)))

(defun load-sprite-image(sprite-def-1)
  "given a sprite def load in it's source bmp and make a surface with the appropriate colour key"
  (let ((surface (gethash (sprite-def-bmp-file sprite-def-1) *bmp-surfaces*)))
    (unless surface
      (let ((surface (sdl:convert-surface-to-display-format 
		      :surface (sdl:load-bmp (sprite-def-bmp-file sprite-def-1)) 
		      :key-color (sprite-def-background-colour sprite-def-1))))
	(when surface
	  (setf (gethash (sprite-def-bmp-file sprite-def-1) *bmp-surfaces*) surface))))))

(defun get-sprite-frame(frame-list frame)
  (if (null frame-list)
      (error "could not find frame ~a~%" frame))
  (if (eql (sprite-def-frame-name (car frame-list)) frame)
      (car frame-list)
    (get-sprite-frame(cdr frame-list) frame)))

(defun draw-sprite(screen x y sprite-def target-frame)
  (load-sprite-image sprite-def) ; ensure bmp is loaded (will only load once)
  (let* ((surface (gethash (sprite-def-bmp-file sprite-def) *bmp-surfaces*))
	 (frame (get-sprite-frame (sprite-def-frames sprite-def) target-frame))
	 (sw (1+ (- (sprite-def-frame-x2 frame) (sprite-def-frame-x1 frame))))
	 (sh (1+ (- (sprite-def-frame-y2 frame) (sprite-def-frame-y1 frame)))))
    (if surface
	(sdl:blit-surface :src surface :dst screen 
			  :src-rect (vector (sprite-def-frame-x1 frame) 
					    (sprite-def-frame-y1 frame) 
					    sw sh) 
			  :dst-rect (sdl:point x y))
      (error "no surface"))))
    
