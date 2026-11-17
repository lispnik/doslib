;;;; A textured 3-D maze in the style of early-1990s shooters.
;;;; For each of the 320 screen columns one ray is stepped through a grid
;;;; map until it meets a wall; the distance gives the height of the wall
;;;; slice, and the exact spot hit picks a column of a 64x64 texture.
;;;; Arrow keys walk and turn, Esc quits.  The renderer is ordinary Lisp,
;;;; compiled natively, writing bytes straight into video memory.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.raycast
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.raycast)

(defvar *map*                           ; 16 x 16; digits are wall kinds
  (coerce (loop for row in '("1111111111111111" "1000000020000001" "1033300020004401"
                             "1030000020000401" "1030000000000401" "1000002220000001"
                             "1000002020033001" "1444000020030001" "1004000000030001"
                             "1004000111030001" "1000000100000001" "1022200100444401"
                             "1000200000400001" "1000200330400001" "1000000000000001"
                             "1111111111111111")
                append (map 'list #'digit-char-p row))
          '(simple-array (unsigned-byte 8) (*))))
;;; A texel is ramp*16 + brightness; palette entry = ramp*16 + shade.
(defvar *ramps* '((20 20 22) (60 22 16) (40 46 60) (58 40 18) (20 52 24)   ; grey brick slate wood moss
                  (30 30 34) (44 34 24) (50 50 52)))
                       ; ceiling floor mortar
(defun make-textures ()
  (let ((textures (make-array (* 5 4096) :element-type '(unsigned-byte 8))))
    (dotimes (v 64)
      (dotimes (u 64)
        (flet ((put (kind ramp brightness)
                 (setf (aref textures (+ (* kind 4096) (* v 64) u))
                       (+ (* ramp 16) (max 4 (min 15 brightness))))))
          (let ((noise (random 3)))
            (put 1 (if (or (< (mod v 16) 2)       ; bricks, staggered rows
                           (< (mod (+ u (if (evenp (floor v 16)) 0 16)) 32) 2)) 7 1)
                 (+ 11 noise))
            (put 2 2 (+ 8 noise (if (or (< (mod u 32) 2) (< (mod v 32) 2)) -4 3)))      ; slate blocks
            (put 3 3 (+ 9 (mod (floor (+ u (* 3 (sin (/ v 5.0)))) 4) 4) noise))         ; wood grain
            (put 4 4 (+ 8 noise (if (< (abs (- (mod (+ u v) 32) 16)) 5) 5 0)))))))      ; mossy diagonal
    textures))
(defun set-ramps ()
  (loop for (r g b) in *ramps* for ramp from 0
        do (dotimes (shade 16)
             (flet ((level (c) (floor (* c (1+ shade)) 16)))
               (set-palette (+ (* 16 ramp) shade) (level r) (level g) (level b))))))
(defun render (px py angle textures)
  (declare (single-float px py angle)
           (type (simple-array (unsigned-byte 8) (*)) textures)
           (optimize speed (safety 0)))
  (let* ((map *map*) (vga *vga*)
         (dir-x (cos angle)) (dir-y (sin angle))
         (plane-x (* -0.66 dir-y)) (plane-y (* 0.66 dir-x)))
    (declare (type (simple-array (unsigned-byte 8) (*)) map)
             (single-float dir-x dir-y plane-x plane-y))
    (dotimes (x 320)
      (let* ((camera (- (/ (* 2.0 x) 320.0) 1.0))
             (ray-x (+ dir-x (* plane-x camera))) (ray-y (+ dir-y (* plane-y camera)))
             (map-x (truncate px)) (map-y (truncate py))
             (delta-x (if (zerop ray-x) 1e9 (abs (/ 1.0 ray-x))))
             (delta-y (if (zerop ray-y) 1e9 (abs (/ 1.0 ray-y))))
             (step-x (if (minusp ray-x) -1 1)) (step-y (if (minusp ray-y) -1 1))
             (side-x (* delta-x (if (minusp ray-x) (- px map-x) (- (+ map-x 1.0) px))))
             (side-y (* delta-y (if (minusp ray-y) (- py map-y) (- (+ map-y 1.0) py))))
             (side 0) (kind 0))
        (declare (single-float camera ray-x ray-y delta-x delta-y side-x side-y)
                 (fixnum map-x map-y step-x step-y side kind))
        (loop                           ; walk the grid one cell boundary at a time
          (if (< side-x side-y)
              (setf side-x (+ side-x delta-x) map-x (+ map-x step-x) side 0)
              (setf side-y (+ side-y delta-y) map-y (+ map-y step-y) side 1))
          (setf kind (aref map (+ map-x (* 16 map-y))))
          (when (plusp kind) (return)))
        (let* ((distance (max 0.05 (if (= side 0) (- side-x delta-x) (- side-y delta-y))))
               (height (truncate (/ 200.0 distance)))
               (top (- 100 (ash height -1))) (bottom (+ top height))
               (hit (if (= side 0) (+ py (* distance ray-y)) (+ px (* distance ray-x))))
               (u (logand (truncate (* 64.0 (- hit (ffloor hit)))) 63))
               (light (max 2 (- (if (= side 0) 16 12) (truncate (* distance 1.3)))))
               (texture (+ (* kind 4096) u)))
          (declare (single-float distance hit) (fixnum height top bottom u light texture))
          (dotimes (y 200)
            (setf (sap-ref-8 vga (+ x (* 320 y)))
                  (cond ((< y top) (+ 80 (max 1 (- 12 (ash y -3)))))           ; ceiling
                        ((>= y bottom) (+ 96 (max 1 (- (ash y -3) 11))))       ; floor
                        (t (let ((texel (aref textures
                                              (+ texture (* 64 (logand (truncate (* 64 (- y top)) height)
                                                                       63))))))
                             (+ (logand texel #xF0)
                                (ash (* (logand texel 15) light) -4))))))))))))
(defun wall-p (x y) (plusp (aref *map* (+ (truncate x) (* 16 (truncate y))))))
(defun raycast ()
  (let ((textures (make-textures)) (x 1.5) (y 14.5) (angle 0.0))
    (video-mode #x13)
    (set-ramps)
    (loop
      (render x y angle textures)
      (let* ((key (hi (wait-key)))
             (dx (* 0.3 (cos angle))) (dy (* 0.3 (sin angle))))
        (flet ((move (sign)
                 (let ((nx (+ x (* sign dx))) (ny (+ y (* sign dy))))
                   (unless (wall-p (+ nx (* sign 0.2 (signum dx))) y) (setf x nx))
                   (unless (wall-p x (+ ny (* sign 0.2 (signum dy)))) (setf y ny)))))
          (case key
            (#x48 (move 1)) (#x50 (move -1))
            (#x4B (decf angle 0.12)) (#x4D (incf angle 0.12))
            (#x01 (return))))))
    (video-mode 3)))

(defun main ()
  "A textured 3-D maze in the style of early-1990s shooters."
  (raycast))

(doslib-examples:define-example "raycast" 'main
  :category :graphics
  :description "A textured 3-D maze in the style of early-1990s shooters.")
