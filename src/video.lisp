;;;; VGA: video modes, the palette, pixels and lines in mode 13h.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; Video
(defvar *vga* (int-sap #xA0000))
(defun video-mode (mode) (int86 #x10 :ax mode))
(defun set-palette (index r g b)        ; 6 bits per component
  (outb #x3C8 index) (outb #x3C9 r) (outb #x3C9 g) (outb #x3C9 b))
(defun rainbow-palette (&optional (first 1) (last 255))
  (set-palette 0 0 0 0)
  (loop for i from first to last
        for a = (* 2 pi (/ (- i first) (- last first -1)))
        do (flet ((v (phase) (round (* 31.5 (1+ (sin (+ a phase)))))))
             (set-palette i (v 0) (v 2.094) (v 4.189)))))
(declaim (inline plot))
(defun plot (x y colour)                ; mode 13h
  (declare (fixnum x y colour))
  (when (and (< -1 x 320) (< -1 y 200))
    (setf (sap-ref-8 *vga* (+ x (* 320 y))) colour)))
(defun draw-line (x0 y0 x1 y1 colour)   ; Bresenham, mode 13h
  (let* ((dx (abs (- x1 x0))) (dy (- (abs (- y1 y0))))
         (sx (if (< x0 x1) 1 -1)) (sy (if (< y0 y1) 1 -1)) (err (+ dx dy)))
    (loop (plot x0 y0 colour)
          (when (and (= x0 x1) (= y0 y1)) (return))
          (let ((e2 (* 2 err)))
            (when (>= e2 dy) (incf err dy) (incf x0 sx))
            (when (<= e2 dx) (incf err dx) (incf y0 sy))))))
