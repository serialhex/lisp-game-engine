;;;; New pong sprites

; A sprite def tells the engine everything it needs to know about a sprite. 
; What bmp file it comes from, what the transparent colour is, and a set of 
; numbered or named frames with x1y1 and x2y2settings (ie where it starts 
; and where it ends on the image)

(defparameter left-bat-frames
  (list (sprites::make-sprite-def-frame :name 'frame-1 :x1 135 :y1 55 :x2 151 :y2 103)
	(sprites::make-sprite-def-frame :name 'frame-2 :x1 157 :y1 55 :x2 173 :y2 103)))

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
