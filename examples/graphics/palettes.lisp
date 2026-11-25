;;;; The library's palettes, one after another, on one picture
;;;;
;;;; A picture in 256 colours is numbers; what the numbers look like is the
;;;; palette's business, and changing the palette changes every pixel at
;;;; once without drawing any of them.  This draws one picture -- a plasma
;;;; above, the 256 colours in order below -- and then shows it in each of
;;;; the palettes USE-PALETTE knows by name, the colours flowing along
;;;; (ROTATE-PALETTE) while it waits: SPACE for the next, any other key to
;;;; leave.  (demo palettes fire) starts at one.  A new palette fades in
;;;; (FADE-PALETTE).
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.palettes
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.palettes)

(defun draw-picture ()
  ;; the plasma: colours 1 to 255, never 0
  (dotimes (y 150)
    (dotimes (x 320)
      (let ((v (+ (sin (* x .045)) (sin (* y .06)) (sin (* (+ x y) .03))
                  (sin (* (sqrt (+ (* (- x 160) (- x 160)) (* (- y 75) (- y 75)))) .07)))))
        (setf (sap-ref-8 *vga* (+ x (* 320 y))) (1+ (mod (round (* 32 (+ v 4))) 255))))))
  ;; the colours in order
  (loop for y from 160 below 188
        do (dotimes (x 320)
             (setf (sap-ref-8 *vga* (+ x (* 320 y))) (min 255 (floor (* x 256) 320))))))

(defun show (name)
  "Fade the palette NAME in, with its name under the picture."
  (use-palette name)
  ;; (the caption is drawn in the last colour, so each palette writes its own name)
  (loop for y from 190 below 200 do (dotimes (x 320) (setf (sap-ref-8 *vga* (+ x (* 320 y))) 0)))
  (graphics-text 4 191 (format nil "~(~A~)   SPACE: the next" name) 255)
  (let ((saved (save-palette)))
    (loop for level from 0 to 64 by 8
          do (wait-retrace) (fade-palette saved level))))

(defun palettes (&optional start)
  (let* ((names (palette-names))
         (n (or (position start names :test #'string-equal) 0)))
    (video-mode #x13)
    (draw-picture)
    (loop
      (let ((name (nth (mod n (length names)) names)))
        (show name)
        (loop until (key-pressed-p)
              ;; (the cube is for asking for colours by their parts: it does not flow)
              do (wait-tick 2)
                 (unless (eq name :cube) (rotate-palette 1 255))))
      (if (eql (get-key) #\Space)
          (incf n)
          (return)))
    (video-mode 3)))

(defun main (&optional start)
  "The library's palettes on one picture: SPACE for the next, any other key leaves."
  (palettes start))

(doslib-examples:define-example "palettes" 'main
  :category :graphics
  :description "The library's palettes by name on one picture: grey, fire, ice, heat and the rest.")
