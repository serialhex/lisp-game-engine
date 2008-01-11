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


;;;; New pong sprites

(defparameter left-bat-frames
  (list (sprites::make-sprite-def-frame :name 'frame-1 :x1 135 :y1 55 :x2 151 :y2 103)))

(defparameter right-bat-frames
  (list (sprites::make-sprite-def-frame :name 'frame-1 :x1 184 :y1 55 :x2 200 :y2 103)))

(defparameter ball-sprite-frames
  (list (sprites::make-sprite-def-frame :name 'frame-1 :x1 47 :y1 35 :x2 64 :y2 50)))

(defparameter ball-sprite
  (sprites::make-sprite-def 
   :bmp-file (sdl:create-path "pongsprites.bmp" cl-user::*bmp-path*) 
   :background-colour (sdl:color :g 255) 
   :frames ball-sprite-frames))

(defparameter left-bat-sprite
  (sprites::make-sprite-def 
   :bmp-file (sdl:create-path "pongsprites.bmp" cl-user::*bmp-path*) 
   :background-colour (sdl:color :g 255) 
   :frames left-bat-frames))

(defparameter right-bat-sprite
  (sprites::make-sprite-def 
   :bmp-file (sdl:create-path "pongsprites.bmp" cl-user::*bmp-path*) 
   :background-colour (sdl:color :g 255) 
   :frames right-bat-frames))



