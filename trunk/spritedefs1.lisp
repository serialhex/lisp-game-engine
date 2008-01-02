;;;; Sprite defs for walking
; a sprite def tells the game everything it needs to know about a sprite. what file it comes
; from, what the transparent colour is, and a set of numbered or named frames with x1y1 and  
; x2y2settings (ie where it starts and where it ends on the image)

(defparameter waddle-frames
  (list 
   (sprites::make-sprite-def-frame :name 'waddle-idle-1 :x1 13 :y1 17 :x2 36 :y2 39)
   (sprites::make-sprite-def-frame :name 'waddle-idle-2 :x1 39 :y1 17 :x2 62 :y2 39)))

(defparameter fireball-frames
  (list 
   (sprites::make-sprite-def-frame :name 'waddle-fireball-right :x1 39 :y1 222 :x2 62 :y2 248)
   (sprites::make-sprite-def-frame :name 'waddle-fireball-left :x1 276 :y1 235 :x2 305 :y2 260
)))   

;;;; 324, 236, 356, 261

(defparameter waddle-sprite
  (sprites::make-sprite-def 
   :bmp-file (sdl:create-path "waddle.bmp" cl-user::*bmp-path*) 
   :background-colour (sdl:color :r 255 :g 255 :b 255) 
   :frames waddle-frames))


