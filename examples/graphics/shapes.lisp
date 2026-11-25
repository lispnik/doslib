;;;; The library's drawing functions, each one at work
;;;;
;;;; Rectangles, ellipses, circles, polygons and a flood fill, outlined and
;;;; filled, in mode 13h -- with the colour cube for a palette, so that a
;;;; colour is asked for by its red, green and blue (CUBE-COLOUR).  Shapes
;;;; that run off the screen are clipped: the sun here is mostly off it.
;;;; A key ends it.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.shapes
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.shapes)

(defun star (cx cy outer inner points)
  "The corners of a star: a polygon that is not convex."
  (loop for i below (* 2 points)
        for angle = (- (* i (/ pi points)) (/ pi 2))
        for radius = (if (evenp i) outer inner)
        collect (list (round (+ cx (* radius (cos angle)))) (round (+ cy (* radius (sin angle) 5/6))))))

(defun scene ()
  (clear-screen (cube-colour 0 1 3))                            ; the sky
  (fill-circle 300 10 60 (cube-colour 5 4 0))                   ; the sun, mostly off the screen
  (fill-rectangle 0 140 320 60 (cube-colour 0 3 1))             ; the ground
  ;; a house: a filled rectangle, a roof that is a polygon, outlined windows
  (fill-rectangle 40 90 90 60 (cube-colour 4 3 2))
  (draw-rectangle 40 90 90 60 (cube-colour 1 1 0))
  (fill-polygon '((32 90) (85 55) (138 90)) (cube-colour 4 1 0))
  (fill-rectangle 76 115 18 35 (cube-colour 2 1 0))
  (dolist (x '(50 105))
    (fill-rectangle x 100 16 14 (cube-colour 3 5 5))
    (draw-rectangle x 100 16 14 0)
    (horizontal-line x (+ x 15) 107 0))
  ;; a pond, and its outline; a tree
  (fill-ellipse 215 165 60 16 (cube-colour 1 3 5))
  (draw-ellipse 215 165 60 16 (cube-colour 0 1 3))
  (fill-rectangle 196 95 8 50 (cube-colour 2 1 0))
  (fill-circle 200 85 24 (cube-colour 0 4 0))
  (draw-circle 200 85 24 (cube-colour 0 2 0))
  ;; stars: outlined, and one of them filled in afterwards by FILL-AREA
  (draw-polygon (star 40 30 16 7 5) (cube-colour 5 5 3))
  (draw-polygon (star 150 28 20 9 5) (cube-colour 5 5 3))
  (fill-area 150 28 (cube-colour 5 5 0))
  (graphics-text 4 190 "rectangles, ellipses, polygons, a fill" (cube-colour 5 5 5)))

(defun main ()
  "The library's drawing functions: a small scene of rectangles, ellipses, polygons and a fill."
  (video-mode #x13)
  (cube-palette)
  (scene)
  (finish))

(doslib-examples:define-example "shapes" 'main
  :category :graphics
  :description "The drawing functions at work: rectangles, ellipses, circles, polygons and a flood fill.")
