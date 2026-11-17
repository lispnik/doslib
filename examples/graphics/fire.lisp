;;;; The classic fire effect, mode 13h, animated until a key is hit
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.fire
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.fire)

(defun fire-palette ()
  (dotimes (i 256)
    (cond ((< i 86) (set-palette i (floor (* i 63) 85) 0 0))
          ((< i 171) (set-palette i 63 (floor (* (- i 85) 63) 85) 0))
          (t (set-palette i 63 63 (floor (* (- i 170) 63) 85))))))
(defun fire-frame (buffer)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (optimize speed (safety 0)))
  (loop for i fixnum from (* 320 100) below (* 320 102)     ; the embers
        do (setf (aref buffer i) (if (< (random 100) 55) 255 0)))
  (dotimes (y 100)
    (declare (fixnum y))
    (let ((row (* 320 y)))
      (declare (fixnum row))
      (dotimes (x 320)
        (declare (fixnum x))
        (let* ((below (+ row 320 x))
               (sum (+ (aref buffer (+ row 320 (mod (1- x) 320)))
                       (aref buffer below)
                       (aref buffer (+ row 320 (mod (1+ x) 320)))
                       (aref buffer (+ below 320))))
               (v (floor (* sum 32) 131)))
          (declare (fixnum below sum v))
          (setf (aref buffer (+ row x)) v)
          (setf (sap-ref-8 *vga* (+ x (* 640 y))) v
                (sap-ref-8 *vga* (+ x 320 (* 640 y))) v))))))
(defun fire ()
  (let ((buffer (make-array (* 320 102) :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
    (video-mode #x13)
    (fire-palette)
    (loop until (key-pressed-p) do (fire-frame buffer))
    (finish)))

(defun main ()
  "The classic fire effect, mode 13h, animated until a key is hit."
  (fire))

(doslib-examples:define-example "fire" 'main
  :category :graphics
  :description "The classic fire effect, mode 13h, animated until a key is hit.")
