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


(defun scale-sdl-color(color scale)
  "scale a color from 0 to 1"
  (let ((new-color (sdl:color)))
    (setf (sdl:r new-color) (* scale (sdl:r color)))
    (setf (sdl:g new-color) (* scale (sdl:g color)))
    (setf (sdl:b new-color) (* scale (sdl:b color)))
    new-color))

(defun half(x)
  "halve the value of x"
  (/ x 2))

(defun integer-in-range(num start end)
  (let ((int (floor num)))
    (max (min end int) start)))

;;;; todo this should go somewhere that should know about screen width and height

(defun sx(x)
  (integer-in-range x 0 640))

(defun sy(y)
  (integer-in-range y 0 480))

;;;; random unique name

(defun random-unique-name()
  (format nil "anon ~a" (get-next-uid)))

;;;; manage unique id's
(let ((next-id 0))
  (defun get-next-uid()
    "get the next unique id"
    (incf next-id)
    (1- next-id)))

(defun safe-nth(n lst)
  "get the nth member of a list. safely return nil
if n is out of bounds"
  (let ((len (length lst)))
    (if (or
	 (>= n len)
	 (< n 0))
	nil
	(nth n lst))))

(defun random-nth(lst) 
  "return a random element from the list"
  (let ((l (length lst))) 
    (nth (random l) lst)))

; (bind-var-fn (a 3 #'(lambda (n) (* 2 n))) (format t "a ~a~%" a))
;a 6
;NIL

(defmacro bind-var-fn((var param fn) &body body)
  `(let ((,var (funcall ,fn ,param)))
     ,@body))

;(bind-list-var-fn (((a 1) (b 2) (c 3)) 
;		   #'(lambda (n) (* 3 n))) 
;  (format t "~a ~a ~a~%" a b c))
; 3 6 9

(defmacro bind-list-var-fn((lst fn) &body body)
  (if (null lst)
      `(progn ,@body)
      `(let ((,(first (car lst))
	      (funcall ,fn ,(second (car lst)))))
	 (bind-list-var-fn (,(cdr lst) ,fn)
	   ,@body))))

; (bind-vars ((a b c) (1 (+ 4 5) 3)) (format t "~a ~a ~a" a b c))
;1 9 3

(defmacro bind-vars((vars vals) &body body)
  `(let ,(mapcar 
	  #'(lambda(var val) 
	      `(,var ,val)) vars vals)
     ,@body))


;(offset-list-iterator (((prev -1) (cur 0) (next 1)) '(1 2 3 5 6 7)) 
; (format t "prev ~a cur ~a next ~a~%" prev cur next))

(defmacro offset-list-iterator((name-offset-list lst) &body body)
"iterate over a list using an array of indices
which are offsets to the current item. out of bounds
indices return nil. at each iteration the symbols provided
are initialised with the item in the list offset by 
the amount you specify"
`(loop 
    for item in ,lst
    for count from 0 
    do
      (bind-list-var-fn (,name-offset-list 
			 #'(lambda(n) (safe-nth (+ n count) ,lst)))
	,@body)))

(defmacro test1(apple)
  (let ((var (gensym)))
    `(let ((,var ,apple))
       (1+ ,var))))

;;;; debugging utils

; This is from onlisp, prints out a macro 
; pretty handy when developing one
(defmacro mac (expr) 
  `(pprint (macroexpand-1 ',expr)))


