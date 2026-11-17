;;;; A small ray tracer: three spheres over a chequered floor,
;;;; one light, shadows and mirror reflections.  The picture is computed in
;;;; floating point and shown in mode 13h with a 3-3-2 bit RGB palette and
;;;; ordered dithering to hide the coarse colour steps.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.raytrace
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.raytrace)

(defun v+ (a b) (mapcar #'+ a b))
(defun v- (a b) (mapcar #'- a b))
(defun v* (a k) (mapcar (lambda (x) (* x k)) a))
(defun dot (a b) (reduce #'+ (mapcar #'* a b)))
(defun unit (a) (v* a (/ 1.0 (sqrt (dot a a)))))
(defvar *spheres*                       ; centre, radius, colour, mirror
  '(((-1.3 0.0 1.0) 1.0 (1.0 0.25 0.2) 0.25)
    ((1.0 -0.2 0.2) 0.8 (0.2 0.5 1.0) 0.45)
    ((0.1 -0.55 -1.0) 0.45 (1.0 0.85 0.2) 0.15)))
(defvar *light* (unit '(-0.6 1.0 -0.7)))
(defun hit-sphere (origin direction sphere)
  (let* ((oc (v- origin (first sphere))) (b (dot oc direction))
         (disc (- (* b b) (- (dot oc oc) (* (second sphere) (second sphere))))))
    (when (> disc 0)
      (let ((d (- (- b) (sqrt disc)))) (when (> d 0.001) d)))))
(defun nearest (origin direction)       ; => distance, point, normal, colour, mirror
  (let ((best nil) (best-d 1e9))
    (dolist (s *spheres*)
      (let ((d (hit-sphere origin direction s)))
        (when (and d (< d best-d)) (setf best s best-d d))))
    (let ((dy (second direction)))      ; the floor is the plane y = -1
      (when (< dy -0.0001)
        (let ((d (/ (- -1.0 (second origin)) dy)))
          (when (and (> d 0.001) (< d best-d))
            (let ((p (v+ origin (v* direction d))))
              (return-from nearest
                (values d p '(0.0 1.0 0.0)
                        (if (evenp (+ (floor (first p)) (floor (third p))))
                            '(0.9 0.9 0.9) '(0.15 0.15 0.2))
                        0.3)))))))
    (when best
      (let ((p (v+ origin (v* direction best-d))))
        (values best-d p (unit (v- p (first best))) (third best) (fourth best))))))
(defun trace-ray (origin direction depth)
  (multiple-value-bind (d point normal colour mirror) (nearest origin direction)
    (if (null d)
        (let ((sky (max 0.0 (second direction)))) (list (- 0.55 (* 0.4 sky)) (- 0.7 (* 0.35 sky)) 1.0))
        (let* ((lit (if (nearest point *light*) 0.0 (max 0.0 (dot normal *light*))))
               (local (v* colour (+ 0.18 (* 0.82 lit)))))
          (if (and (plusp depth) (plusp mirror))
              (let ((bounce (v- direction (v* normal (* 2 (dot direction normal))))))
                (v+ (v* local (- 1.0 mirror))
                    (v* (trace-ray point bounce (1- depth)) mirror)))
              local)))))
(defvar *bayer* #(0 8 2 10 12 4 14 6 3 11 1 9 15 7 13 5))
(defun raytrace ()
  (video-mode #x13)
  (dotimes (i 256)                      ; palette index = RRRGGGBB
    (set-palette i (* 9 (ldb (byte 3 5) i)) (* 9 (ldb (byte 3 2) i)) (* 21 (ldb (byte 2 0) i))))
  (dotimes (py 200)
    (dotimes (px 320)
      (let* ((direction (unit (list (/ (- px 160) 200.0) (/ (- 100 py) 200.0) 1.0)))
             (rgb (trace-ray '(0.0 0.4 -4.5) direction 3))
             (dither (/ (- (aref *bayer* (+ (mod px 4) (* 4 (mod py 4)))) 7.5) 16.0)))
        (flet ((q (v levels) (max 0 (min (1- levels) (round (+ (* v (1- levels)) dither))))))
          (plot px py (+ (* 32 (q (first rgb) 8)) (* 4 (q (second rgb) 8)) (q (third rgb) 4)))))))
  (finish))

(defun main ()
  "A small ray tracer."
  (raytrace))

(doslib-examples:define-example "raytrace" 'main
  :category :graphics
  :description "A small ray tracer.")
