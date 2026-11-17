;;;; 640x480 in 256 colours through the VESA BIOS, with bank
;;;; switching: only 64KB of the picture is visible at A000:0000 at a time
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.vesa
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.vesa)

(defvar *bank* -1)
(declaim (inline plot-hi))
(defun plot-hi (x y colour)
  (declare (fixnum x y colour))
  (let* ((offset (+ x (* 640 y))) (bank (ash offset -16)))
    (declare (fixnum offset bank))
    (unless (= bank (the fixnum *bank*))
      (int86 #x10 :ax #x4F05 :bx 0 :dx bank)        ; move the window
      (setf *bank* bank))
    (setf (sap-ref-8 *vga* (logand offset #xFFFF)) colour)))
(defun julia-time (zr zi cr ci limit)
  (declare (double-float zr zi cr ci) (fixnum limit) (optimize speed (safety 0)))
  (let ((i 0))
    (declare (fixnum i))
    (loop (let ((r2 (* zr zr)) (i2 (* zi zi)))
            (when (or (> (+ r2 i2) 4d0) (>= i limit)) (return i))
            (setq zi (+ (* 2d0 zr zi) ci)
                  zr (+ (- r2 i2) cr))
            (incf i)))))
(defun julia (&key (cr -0.8d0) (ci 0.156d0) (limit 200))
  (unless (= (word16 (int86 #x10 :ax #x4F02 :bx #x101)) #x004F)
    (error "No VESA mode 101h (640x480x256) here"))
  (setf *bank* -1)
  (rainbow-palette)
  (dotimes (py 480)
    (dotimes (px 640)
      (let ((n (julia-time (* (- px 320) (/ 1.6d0 320)) (* (- py 240) (/ 1.2d0 240))
                           cr ci limit)))
        (plot-hi px py (if (= n limit) 0 (1+ (mod (* n 5) 255)))))))
  (finish))

(defun main ()
  "640x480 in 256 colours through the VESA BIOS."
  (julia))

(doslib-examples:define-example "vesa" 'main
  :category :graphics
  :description "640x480 in 256 colours through the VESA BIOS.")
