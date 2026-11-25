;;;; "Mode X": 320x240 with square pixels and several pages, a
;;;; mode the BIOS does not offer.  It is made by setting mode 13h and then
;;;; reprogramming the VGA's sequencer and CRT controller through their I/O
;;;; ports.  Video memory becomes four planes (a pixel's plane is x mod 4),
;;;; and there is room for two full pages, so one is drawn while the other
;;;; is shown and they are swapped by changing the display start address.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.modex
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.modex)

;;; (The mode itself is the library's: MODE-X, src/modex.lisp, which says how it is made.)
(defun paint (page function)            ; one plane at a time
  (dotimes (plane 4)
    (vga-planes (ash 1 plane))
    (dotimes (y 240)
      (loop for x from plane below 320 by 4
            do (setf (sap-ref-8 *vga* (+ (* page 19200) (* 80 y) (ash x -2)))
                     (funcall function x y))))))
(defun show-page (page)                 ; display start address = page * 19200
  (vga-display-start (* page 19200)))
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
