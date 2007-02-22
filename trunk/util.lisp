;;;; Various utilities

; math

(defun random-range(min max)
  "a random value between min and max"
  (let ((diff (abs (- min max))))
    (+ min (random diff))))

(defun clamp(item min max)
  (if (> item max)
      max
    (if (< item min)
	min
      item)))

(defun sqr(n) 
  (* n n))

(defun rads-degs(rads) 
  (* rads (/ 360.0 (* 2 PI))))

(defun degs-rads(degs) 
  (* degs (/ (* 2 PI) 360.0)))

(defun atan2(x y) 
  "somewhat awful but tested and working atan2"
  (if (= y 0.0)
      (if (< x 0.0)
	  (+ PI (/ PI 2.0))
	(/ PI 2.0))
    (if (= x 0.0) 
	(if (> y 0.0)
	    0.0
	  PI)
      (let ((at (atan (/ x y))))
	(if (> x 0.0) 
	    (if (> y 0.0)
		at
	      (+ PI at))
	  (if (> y 0.0)
	      (+ PI PI at)
	    (+ PI at)))))))

;;;; graphics/sdl

(defun screen-center-x() 
  (ash *window-width* -1))

(defun screen-center-y() 
  (ash *window-height* -1))

;;;; color manipulation

(defun interp-sdl-color(c1 c2 scale)
  "interpolate between two sdl colors"
  (let ((cnew (sdl:color))
	(dr (- (sdl:r c2) (sdl:r c1)))
	(dg (- (sdl:g c2) (sdl:g c1)))
	(db (- (sdl:b c2) (sdl:b c1))))
    (setf (sdl:r cnew) (+ (sdl:r c1) (* scale dr)))
    (setf (sdl:g cnew) (+ (sdl:g c1) (* scale dg)))
    (setf (sdl:b cnew) (+ (sdl:b c1) (* scale db)))
    cnew))






