;;;; a simple circular queue
;;;; can add items to the end 
;;;; can get the first guy (first in first out)
;;;; if it fills up you lose the guys at the front if you add more

(defstruct circular-queue
  :data
  :first
  :next-free
  :size
  :count)

(defun cq-get-ndx(n cq)
  "given an index and a circular queue wraps the index if required"
  (mod n (circular-queue-size cq)))

(defun cq-get-nth(n cq)
  "return the nth item of the queue, starting from the first item"
  (if (< n (circular-queue-count cq))
      (aref (circular-queue-data cq) 
	    (cq-get-ndx 
	     (+ n (circular-queue-first cq)) cq))
      nil))

(defun cq-get-next-ndx(n cq)
  "increment an index accounting for wrapping"
  (cq-get-ndx (1+ n) cq))

(defun cq-make(size)
  (make-circular-queue :data (make-array size :adjustable nil)
		       :first 0 :count 0 :size size :next-free 0))

(defun cq-full-p(q)
  "true if queue is full - additions will be lost"
  (if (= (circular-queue-size q) (circular-queue-count q))
      t
      nil))

(defun cq-add-back(item q)
  (setf (aref (circular-queue-data q) (circular-queue-next-free q)) item) ; add the item at the free point
  (setf (circular-queue-next-free q) (cq-get-next-ndx (circular-queue-next-free q) q)) ; increment the next-free point
  (if (cq-full-p q)
      (setf (circular-queue-first q) (cq-get-next-ndx (circular-queue-first q) q)) ; adjust first count
      (incf (circular-queue-count q))))

(defun cq-pop-first(q)
  (if (= (circular-queue-count q) 0)
      nil
      (let ((item (aref (circular-queue-data q) (circular-queue-first q))))
	(setf (circular-queue-first q) (cq-get-next-ndx (circular-queue-first q) q)) ; increment first
	(incf (circular-queue-count q) -1) ; decrement count
	item)))

(defmacro cq-iterate((var queue) &body body)
  "iterate across a circular queue"
  (let ((iter (gensym)))
    `(loop for ,iter from 0 to (1- (circular-queue-count ,queue)) do
	(let ((,var (aref (circular-queue-data ,queue) 
			  (cq-get-ndx 
			   (+ ,iter (circular-queue-first ,queue)) ,queue))))
	  ,@body))))

(defmacro cq-iterate-pairs((var1 var2 queue) &body body)
  "iterate across a circular queue two at a time"
  (let ((iter1 (gensym)) 
	(iter2 (gensym)))
    `(let ((count (circular-queue-count ,queue)))
      (loop for ,iter1 from 0 to (- count 2) 
       for ,iter2 from 1 to (- count 1) do
       (let ((,var1 (cq-get-nth ,iter1 ,queue))
	     (,var2 (cq-get-nth ,iter2 ,queue)))
	 ,@body)))))

(defun cq-print(q)
  (cq-iterate (item q)
    (format t "~a " item))
  (format t "~%"))

  


  
    
