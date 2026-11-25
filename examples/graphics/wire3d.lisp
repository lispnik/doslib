;;;; Rotating wireframe solids: a cube with an octahedron inside
;;;; it, rotated about two axes and drawn with perspective.  Any key stops it.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.wire3d
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.wire3d)

(defvar *cube* (loop for i below 8 collect (list (if (logbitp 0 i) 1.0 -1.0)
                                                  (if (logbitp 1 i) 1.0 -1.0)
                                                  (if (logbitp 2 i) 1.0 -1.0))))
(defvar *cube-edges*                    ; vertices differing in exactly one bit
  (loop for i below 8 append (loop for b below 3 for j = (logxor i (ash 1 b))
                                   when (< i j) collect (list i j))))
(defvar *octahedron* '((1.5 0 0) (-1.5 0 0) (0 1.5 0) (0 -1.5 0) (0 0 1.5) (0 0 -1.5)))
(defvar *octa-edges* (loop for i below 6 append (loop for j from (1+ i) below 6
                                                      unless (= (floor i 2) (floor j 2))
                                                        collect (list i j))))
(defun project (point a b)              ; rotate about Y then X, then perspective
  (destructuring-bind (x y z) point
    (let* ((x1 (- (* x (cos a)) (* z (sin a)))) (z1 (+ (* x (sin a)) (* z (cos a))))
           (y2 (- (* y (cos b)) (* z1 (sin b)))) (z2 (+ (* y (sin b)) (* z1 (cos b))))
           (scale (/ 260.0 (+ z2 5.0))))
      (list (round (+ 160 (* x1 scale))) (round (- 100 (* y2 scale 0.83)))))))
(defun frame-lines (a b)
  (flet ((solid (points edges colour)
           (let ((p (mapcar (lambda (pt) (project pt a b)) points)))
             (loop for (i j) in edges for k from 0
                   collect (append (nth i p) (nth j p) (list (+ colour (* 7 k))))))))
    (append (solid *cube* *cube-edges* 40) (solid *octahedron* *octa-edges* 150))))
(defun wire3d ()
  (video-mode #x13)
  (rainbow-palette)
  (let ((old '()))
    (loop for a from 0.0 by 0.07 for b from 0.3 by 0.045
          until (key-pressed-p)
          do (let ((new (frame-lines a b)))
               (wait-tick)
               (loop for (x0 y0 x1 y1) in old do (draw-line x0 y0 x1 y1 0))
               (loop for (x0 y0 x1 y1 c) in new do (draw-line x0 y0 x1 y1 c))
               (setf old new))))
  (finish))

(defun main ()
  "Rotating wireframe solids."
  (wire3d))

(doslib-examples:define-example "wire3d" 'main
  :category :graphics
  :description "Rotating wireframe solids.")
