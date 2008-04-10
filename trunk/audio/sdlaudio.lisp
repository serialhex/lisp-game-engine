; An SDL audio mixer

(declaim (optimize (debug 3) (speed 2) (space 1))) 

; todo package set up 
;(defpackage :audioplay 
;  (:use :cl-user :cffi :lispbuilder-sdl-cffi))
;(in-package :audioplay)

(cffi:defcstruct wave
  (spec sdl-cffi::SDL-Audio-Spec)
  (sound :pointer)
  (soundlen :unsigned-int)
  (soundpos :int))

(defparameter *wave* nil)
(defparameter *pause-sample* nil)
(defparameter *audio-buffer-size* 4096)
(defparameter default-sample-name "/home/justinhj/lisp/sample.wav")

; (setf *pause-sample* t)
; (setf *pause-sample* nil)

; From On Lisp
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

; This is a simple version of memcpy(dst, src, num)
; it takes two pointers and copies num bytes from 1 to the other
; there's no bounds checking, this is only for use when you know
; exactly what you're copying
(defun my-memcpy(dst src num)
  (loop for b from 0 to (1- num) do 
       (setf (cffi:mem-aref dst :unsigned-char b) (cffi:mem-aref src :unsigned-char b))))

; Define callback function for managing the audio buffer
; void SDLCALL fillerup(void *unused, Uint8 *stream, int len)
(cffi:defcallback fillerup :void ((unused :pointer) (stream :pointer) (len :int))

;  (format t "fillerup ~a ~a~%" stream len)

  (if *pause-sample* 
      (return-from fillerup))

  (let ((waveptr (cffi:inc-pointer 
		  (cffi:foreign-slot-value *wave* 'wave 'sound)
		  (cffi:foreign-slot-value *wave* 'wave 'soundpos))) ; pointer into sound data at play pos
	(waveleft (- (cffi:foreign-slot-value *wave* 'wave 'soundlen)
		     (cffi:foreign-slot-value *wave* 'wave 'soundpos)))) ; how much sound left

;    (format t "waveptr ~a waveleft ~a len ~a 1 ~a 2 ~a~%" waveptr waveleft len
;		  (cffi:foreign-slot-value *wave* 'wave 'sound)
;		  (cffi:foreign-slot-value *wave* 'wave 'soundpos))

;    (return-from fillerup)

    (while (<= waveleft len) ; while enough sound to fill len bytes of the audio stream buffer

 ;     (format t "stream ~a waveptr ~a waveleft ~a len ~a~%" stream waveptr waveleft len)

      ; send all the audio data you can
      (sdl-cffi::SDL-Mix-Audio stream waveptr waveleft sdl-cffi::SDL-MIX-MAXVOLUME)

      ; increment the stream pointer by what you wrote
      (cffi:incf-pointer stream waveleft)
      (decf len waveleft) ; reduce the amount of data we still need to right

      ; reset the sound pointer to point at the beginning
      (setf waveptr (cffi:foreign-slot-value *wave* 'wave 'sound)) 
      (setf waveleft (cffi:foreign-slot-value *wave* 'wave 'soundlen))
      (setf (cffi:foreign-slot-value *wave* 'wave 'soundpos) 0))

  ;  (format t "2 stream ~a waveptr ~a waveleft ~a len ~a~%" stream waveptr waveleft len)

    ; at this point we have more than enough sound, so just copy len bytes of it
    ; and increment the playing sounds position by len
    (sdl-cffi::SDL-Mix-Audio stream waveptr len sdl-cffi::SDL-MIX-MAXVOLUME)
    (incf (cffi:foreign-slot-value *wave* 'wave 'soundpos) len)))

; allocate a SDL_AudioSpec with the specified parameters

(defun allocate-sdl-audiospec(des-freq des-format des-samples des-callback des-userdata des-channels)
  (let ((spec (cffi:foreign-alloc 'sdl-cffi::SDL-Audio-Spec)))
    (cffi:with-foreign-slots ((sdl-cffi::freq 
			       sdl-cffi::format 
			       sdl-cffi::samples 
			       sdl-cffi::callback 
			       sdl-cffi::channels 
			       sdl-cffi::userdata) 
			      spec sdl-cffi::SDL-Audio-Spec)
      (setf sdl-cffi::freq des-freq)
      (setf sdl-cffi::format des-format)
      (setf sdl-cffi::samples des-samples)
      (setf sdl-cffi::callback des-callback)
      (setf sdl-cffi::channels des-channels)
      (setf sdl-cffi::userdata des-userdata))
    spec))

(defun allocate-desired-audiospec()
  (allocate-sdl-audiospec 44100 sdl-cffi::AUDIO-S16LSB *audio-buffer-size* (cffi:get-callback 'fillerup) 
			  (cffi:null-pointer) 2))



(defun debug-print-audio-spec(spec)
  "display information about the provided sdl-audio-spec"
  (cffi:with-foreign-slots ((sdl-cffi::freq 
			     sdl-cffi::format 
			     sdl-cffi::channels
			     sdl-cffi::callback
			     sdl-cffi::userdata)
			    spec
			    sdl-cffi::SDL-Audio-Spec)
    (format t "audio spec: freq ~a format ~x channels ~a callback ~a userdata ~a~%"
	    sdl-cffi::freq 
	    sdl-cffi::format 
	    sdl-cffi::channels
	    sdl-cffi::callback
	    sdl-cffi::userdata)))

(defun sdlaudio(&optional (samplename default-sample-name))
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-cdrom sdl:sdl-init-audio)
    (sdl:window 200 120)
    (setf (sdl:frame-rate) 60)

; lispworks - need this to enable callbacks from foreign code into lisp
#+lispworks (sys:setup-for-alien-threads)

    ; audio init

    (let ((wave-ptr (cffi:foreign-alloc 'wave)))
      (let ((load-wav-result (sdl-cffi::SDL-Load-WAV samplename
					(cffi:foreign-slot-pointer wave-ptr 'wave 'spec)
					(cffi:foreign-slot-pointer wave-ptr 'wave 'sound)
					(cffi:foreign-slot-pointer wave-ptr 'wave 'soundlen))))
	(setf (cffi:foreign-slot-value wave-ptr 'wave 'soundpos) 0)

	(if (sdl-cffi::null-pointer-p load-wav-result)
	    (error "unable to load wav error: ~a~%" (sdl-cffi::sdl-get-error)))

	(cffi:with-foreign-slots ((sdl-cffi::freq sdl-cffi::channels sdl-cffi::format)
				  (cffi:foreign-slot-value wave-ptr 'wave 'spec)
				  sdl-cffi::SDL-Audio-Spec)
	  (format t "loaded sample freq ~a channels ~a format ~x~%" sdl-cffi::freq sdl-cffi::channels sdl-cffi::format))
	
	(if (cffi:null-pointer-p wave-ptr)
	    (error "cannot load wave file")
	    (setf *wave* wave-ptr))

	; set the callback 
	(setf (cffi:foreign-slot-value 
	       (cffi:foreign-slot-value wave-ptr 'wave 'spec) 
	       'sdl-cffi::sdl-audio-spec 
	       'sdl-cffi::callback)
	      (cffi:get-callback 'fillerup))

	; initialise audio

	(let ((desired-audio-spec (allocate-desired-audiospec))
	      (obtained-audio-spec (cffi:foreign-alloc  'sdl-cffi::SDL-Audio-Spec)))

          ; print out information about the desired audio spec
;	  (format t "1 desired:~%")
;	  (debug-print-audio-spec desired-audio-spec)

	  ; set the mixer callback 
	  (setf (cffi:foreign-slot-value 
		 desired-audio-spec
		 'sdl-cffi::sdl-audio-spec 
		 'sdl-cffi::callback)
		(cffi:get-callback 'fillerup))

	  (let ((open-audio-result
		 (sdl-cffi::SDL-Open-Audio desired-audio-spec obtained-audio-spec))
		(rstring
		 (cffi:foreign-string-alloc "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"))
		(audio-converter 
		 (cffi:foreign-alloc 'sdl-cffi::SDL-Audio-CVT)))

            ; print out information about the desired audio spec
;	    (format t "desired:~%")
;	    (debug-print-audio-spec desired-audio-spec)

            ; print out information about the obtained audio spec
	    (format t "obtained audio spec:~%")
	    (debug-print-audio-spec obtained-audio-spec)

   	    ; and the wave file spec
;	    (format t "wave:~%")
;	    (debug-print-audio-spec (cffi:foreign-slot-value wave-ptr 'wave 'spec))

	    (if (< open-audio-result 0)
		(error "Open audio failed: ~a~%" (sdl-cffi::sdl-get-error)))

	    (format t "Using audio driver: ~a~%" 
		    (sdl-cffi::SDL-Audio-Driver-Name rstring 52))

	    (cffi:with-foreign-slots ((sdl-cffi::freq 
				       sdl-cffi::format 
				       sdl-cffi::channels)
				      (cffi:foreign-slot-pointer wave-ptr 'wave 'spec)
				      sdl-cffi::SDL-Audio-Spec)

	      (when (< (sdl-cffi::SDL-Build-Audio-CVT audio-converter sdl-cffi::format sdl-cffi::channels sdl-cffi::freq
		     (cffi:foreign-slot-value obtained-audio-spec 'sdl-cffi::sdl-audio-spec 'sdl-cffi::format)
		     (cffi:foreign-slot-value obtained-audio-spec 'sdl-cffi::sdl-audio-spec 'sdl-cffi::channels)
		     (cffi:foreign-slot-value obtained-audio-spec 'sdl-cffi::sdl-audio-spec 'sdl-cffi::freq)) 0)
		  (error "failed to make audio converter ~a~%" (sdl-cffi::sdl-get-error))))

	    ; we now have the converter, now need to allocate space for the sample

            ; wav_cvt.buf = malloc(wav_len * wav_cvt.len_mult);	    
	    (let ((sample-buffer
		   (cffi:foreign-alloc :unsigned-char
				       :count (* (cffi:foreign-slot-value wave-ptr 'wave 'soundlen)
					  (cffi:foreign-slot-value audio-converter 
								   'sdl-cffi::SDL-Audio-CVT 'sdl-cffi::len-mult)))))

	      (setf (cffi:foreign-slot-value audio-converter 'sdl-cffi::SDL-Audio-CVT 'sdl-cffi::buf)
		    sample-buffer)

              ; wav_cvt.len = wav_len;
	      
	      ; set up the converter
	      (setf (cffi:foreign-slot-value audio-converter 'sdl-cffi::SDL-Audio-CVT 'sdl-cffi::len)
		    (cffi:foreign-slot-value wave-ptr 'wave 'soundlen))

	      ; memcpy(wav_cvt.buf, wav_buf, wav_len);

	      (my-memcpy sample-buffer
			 (cffi:foreign-slot-value wave-ptr 'wave 'sound)
			 (cffi:foreign-slot-value wave-ptr 'wave 'soundlen))

	      (let ((sample-length 0)
		    (convert-result 
		     (sdl-cffi::SDL-Convert-Audio audio-converter)))

		(setf sample-length 		
		      (coerce 
		       (round 
			(* (cffi:foreign-slot-value audio-converter 
						    'sdl-cffi::SDL-Audio-CVT 'sdl-cffi::len)
			   (cffi:foreign-slot-value audio-converter 
						    'sdl-cffi::SDL-Audio-CVT 'sdl-cffi::len-ratio)))
		       'fixnum))

		(format t "sample-length ~a~%" sample-length)
		
		(when (< 0 convert-result)
		    (error "unable to convert sample: ~a~%" (sdl-cffi::sdl-get-error)))

		; set the wave structure to represent the new sample

;		(setf (cffi:foreign-slot-value wave-ptr 'wave 'spec) obtained-audio-spec)
		(setf (cffi:foreign-slot-value wave-ptr 'wave 'sound) sample-buffer)
		(setf (cffi:foreign-slot-value wave-ptr 'wave 'soundlen) sample-length)
		(setf (cffi:foreign-slot-value wave-ptr 'wave 'soundpos) 0)

		(sdl-cffi::SDL-Pause-Audio 0) ; unpause the audio

		(if (null nil)
		    (sdl:with-events ()
		      (:quit-event () t)
		      (:idle ()
			     (sdl:draw-pixel-* (random 200) (random 120)
					       :color (sdl:color :r 0
								 :g (random 255)
								 :b 0)
					       :surface sdl:*default-display*)
			     (sdl:update-display))))

		(sdl-cffi::SDL-Close-Audio)
		(sdl-cffi::SDL-Free-WAV (cffi:foreign-slot-value wave-ptr 'wave 'sound))))))))))







