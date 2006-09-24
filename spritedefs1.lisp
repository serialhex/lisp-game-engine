;;;; Sprite defs for walking
; a sprite def tells the game everything it needs to know about a sprite. what file it comes
; from, what the transparent colour is, and a set of numbered or named frames with x1y1 and  
; x2y2settings (ie where it starts and where it ends on the image)

; filename : waddle.bmp

(defparameter waddle-frames
  (list 
   (sprites::make-sprite-def-frame :name 'waddle-idle-1 :x1 13 :y1 17 :x2 36 :y2 39)
   (sprites::make-sprite-def-frame :name 'waddle-idle-2 :x1 39 :y1 17 :x2 62 :y2 39)))

(defparameter waddle-sprite
  (sprites::make-sprite-def :bmp-file "waddle.bmp" :background-colour (vector 255 255 255) :frames waddle-frames))

