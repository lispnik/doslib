;;;; The Mandelbrot set in mode 13h; the inner loop is
;;;; double-float code compiled natively by SBCL on DOS
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.mandel
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.mandel)

(defun escape-time (cr ci limit)
  (declare (double-float cr ci) (fixnum limit) (optimize speed (safety 0)))
  (let ((zr 0d0) (zi 0d0) (i 0))
    (declare (double-float zr zi) (fixnum i))
    (loop (let ((r2 (* zr zr)) (i2 (* zi zi)))
            (when (or (> (+ r2 i2) 4d0) (>= i limit)) (return i))
            (setq zi (+ (* 2d0 zr zi) ci)
                  zr (+ (- r2 i2) cr))
            (incf i)))))
(defun mandelbrot (&key (x0 -2.25d0) (x1 0.95d0) (y0 -1.0d0) (y1 1.0d0) (limit 128))
  (video-mode #x13)
  (rainbow-palette)
  (dotimes (py 200)
    (dotimes (px 320)
      (let ((n (escape-time (+ x0 (* px (/ (- x1 x0) 320)))
                            (+ y0 (* py (/ (- y1 y0) 200))) limit)))
        (plot px py (if (= n limit) 0 (1+ (mod (* n 6) 255)))))))
  (finish))

(defun main ()
  "The Mandelbrot set in mode 13h."
  (mandelbrot))

(doslib-examples:define-example "mandel" 'main
  :category :graphics
  :description "The Mandelbrot set in mode 13h.")
