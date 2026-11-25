;;;; A plasma: sine waves written straight into VGA memory
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.plasma
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.plasma)

(defun plasma ()
  (video-mode #x13)                     ; 320x200, 256 colours
  (rainbow-palette)
  (dotimes (y 200)
    (dotimes (x 320)
      (flet ((wave (v k) (sin (* v k))))
        (plot x y (1+ (mod (round (* 40 (+ (wave x .04) (wave y .06) (wave (+ x y) .03)
                                         (wave (sqrt (+ (* x x) (* y y))) .05))))
                           255))))))
  (finish))

(defun main ()
  "A plasma: sine waves written straight into VGA memory."
  (plasma))

(doslib-examples:define-example "plasma" 'main
  :category :graphics
  :description "A plasma: sine waves written straight into VGA memory.")
