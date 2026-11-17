;;;; "Mode X": 320x240 with square pixels and several pages, a
;;;; mode the BIOS does not offer.  It is made by setting mode 13h and then
;;;; reprogramming the VGA's sequencer and CRT controller through their I/O
;;;; ports.  Video memory becomes four planes (a pixel's plane is x mod 4),
;;;; and there is room for two full pages, so one is drawn while the other
;;;; is shown and they are swapped by changing the display start address.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.modex
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.modex)

(defun sequencer (index value) (outb #x3C4 index) (outb #x3C5 value))
(defun crtc (index value) (outb #x3D4 index) (outb #x3D5 value))
(defun mode-x ()
  (video-mode #x13)
  (sequencer 4 #x06)                    ; unchain the four planes
  (sequencer 0 #x01) (outb #x3C2 #xE3) (sequencer 0 #x03)   ; 480-line timing
  (outb #x3D4 #x11) (outb #x3D5 (logand (inb #x3D5) #x7F))  ; unlock CRTC 0-7
  (loop for (index value) in '((#x06 #x0D) (#x07 #x3E) (#x09 #x41) (#x10 #xEA) (#x11 #xAC)
                               (#x12 #xDF) (#x14 #x00) (#x15 #xE7) (#x16 #x06) (#x17 #xE3))
        do (crtc index value))
  (sequencer 2 #x0F)                    ; all planes: clear both pages
  (dotimes (i 65536) (setf (sap-ref-8 *vga* i) 0)))
(defun paint (page function)            ; one plane at a time
  (dotimes (plane 4)
    (sequencer 2 (ash 1 plane))
    (dotimes (y 240)
      (loop for x from plane below 320 by 4
            do (setf (sap-ref-8 *vga* (+ (* page 19200) (* 80 y) (ash x -2)))
                     (funcall function x y))))))
(defun show-page (page)                 ; display start address = page * 19200
  (crtc #x0C (ash (* page 19200) -8)) (crtc #x0D (logand (* page 19200) #xFF)))
(defun rings (x y)                      ; circles are round here: pixels are square
  (let ((dx (- x 160)) (dy (- y 120)))
    (1+ (mod (* 23 (floor (sqrt (+ (* dx dx) (* dy dy))) 6)) 254))))
(defun weave (x y)
  (1+ (mod (+ (logxor x y) (floor (* x y) 64)) 254)))
(defun modex ()
  (mode-x)
  (rainbow-palette)
  (paint 0 #'rings)
  (paint 1 #'weave)
  (loop for page = 0 then (- 1 page)    ; flip pages once a second until a key
        until (key-pressed-p)
        do (show-page page) (wait-tick 18))
  (finish))

(defun main ()
  "Mode X: 320x240 with square pixels and several pages."
  (modex))

(doslib-examples:define-example "modex" 'main
  :category :graphics
  :description "Mode X: 320x240 with square pixels and several pages.")
