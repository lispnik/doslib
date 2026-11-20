;;;; A rotozoomer: a picture turned and zoomed over the whole screen, in mode
;;;; 13h (320 by 200, a byte a pixel), the way the demos of the early 1990s
;;;; did it.
;;;;
;;;;   (doslib-examples:run "rotozoom")
;;;; Esc leaves.
;;;;
;;;; The picture (the "texture") is 256 by 256 bytes, made here: a pattern
;;;; with two words on it.  It repeats in both directions, like wallpaper,
;;;; and the screen is a window on that wallpaper, turned by some angle and
;;;; held nearer or further away.
;;;;
;;;; The trick is to go from the screen to the texture and not the other way
;;;; about.  One pixel to the right on the screen is always the same step
;;;; across the texture, whatever the angle: (cos a, sin a) times the zoom.
;;;; One pixel down is that step turned a quarter circle.  So nothing is
;;;; rotated pixel by pixel.  Two texture co-ordinates, U and V, are worked
;;;; out for the top left corner; then for every pixel
;;;;
;;;;   colour = texture[V, U];  U = U + dU;  V = V + dV
;;;;
;;;; and that is all: two additions and a table look-up, no multiplication
;;;; and no sines.  The sines are looked up once a frame.
;;;;
;;;; The co-ordinates are fixed-point numbers: whole 32-bit integers that
;;;; stand for fractions, here with the top 8 bits the texel (texture pixel,
;;;; 0 to 255) and the 24 bits below it the fraction.  The texture being 256
;;;; wide is the other half of the trick: when U runs off the right edge the
;;;; 32-bit addition overflows, and what is left is the co-ordinate on the
;;;; wallpaper's next repeat.  The wrap costs nothing.  (Lisp integers do not
;;;; overflow, so the code says LOGAND with FFFFFFFFh; the compiler knows
;;;; that this is what a 32-bit ADD does anyway and emits just that.)
;;;;
;;;; The pixels of mode 13h are taller than they are wide (200 lines on a
;;;; screen 3/4 as high as it is wide), so the step for one pixel down is
;;;; made 6/5 as long: the picture turns without squashing.
;;;;
;;;; Each frame is made in a Lisp vector of 64,000 bytes and copied to video
;;;; memory in one go.  The top line shows frames a second (paced by
;;;; WAIT-FRAME: 70 or so if the machine keeps up) and how long one frame
;;;; took to draw, timed with the timer chip.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.rotozoom
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.rotozoom)

(defconstant +width+ 320) (defconstant +height+ 200)
(defconstant +screen-bytes+ (* +width+ +height+))
(defconstant +black+ 254) (defconstant +white+ 255)

(deftype bytes () '(simple-array (unsigned-byte 8) (*)))
(deftype word32 () '(unsigned-byte 32))
(defun make-bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;;; ---- the texture: 256 by 256, colours 0 to 191 in three ramps of 64
(defun texture-palette ()
  (dotimes (i 64)
    (set-palette i (floor i 4) (floor i 2) (+ 15 (floor (* i 3) 4)))            ; blues
    (set-palette (+ 64 i) 63 (+ 16 (floor (* i 3) 4)) (floor i 3))              ; orange to yellow
    (set-palette (+ 128 i) (floor i 2) (+ 30 (floor i 2)) (floor i 3)))         ; greens
  (set-palette +black+ 0 0 0)
  (set-palette +white+ 63 63 63))

(defun write-word (texture string left top scale ramp)
  "STRING in the BIOS's 8 by 8 letters, each pixel of them SCALE texels
square, shaded down the letter in the 64 colours from RAMP, with a shadow."
  (flet ((put (x y colour) (setf (aref texture (+ (logand x 255) (* 256 (logand y 255)))) colour)))
    (dotimes (pass 2)                   ; the shadow first, then the letters over it
      (dotimes (i (length string))
        (dotimes (row 8)
          (dotimes (column 8)
            (when (logbitp (- 7 column) (glyph-row (char string i) row))
              (dotimes (dy scale)
                (dotimes (dx scale)
                  (let ((x (+ left (* scale (+ column (* 8 i))) dx))
                        (y (+ top (* scale row) dy)))
                    (if (zerop pass)
                        (put (+ x 3) (+ y 3) 2)
                        (put x y (+ ramp (floor (* 63 (+ (* scale row) dy))
                                                (* 8 scale)))))))))))))))

(defun make-texture ()
  "Diamonds in blue (the exclusive or of the co-ordinates, folded), LISP in
orange and DOS in green."
  (let ((texture (make-bytes (* 256 256))))
    (dotimes (y 256)
      (dotimes (x 256)
        (flet ((fold (n) (if (logbitp 6 n) (- 63 (logand n 63)) (logand n 63))))
          (setf (aref texture (+ x (* 256 y)))
                (+ 6 (floor (* 7 (logxor (fold x) (fold y))) 8))))))
    (write-word texture "LISP" 0 28 8 64)
    (write-word texture "DOS" 32 156 8 128)
    texture))

;;; ---- sines: 1024 steps to the circle, 65,536 for 1
(defun make-sines ()
  (let ((sines (make-array 1024 :element-type 'fixnum)))
    (dotimes (i 1024 sines)
      (setf (aref sines i) (round (* 65536 (sin (* i (/ pi 512)))))))))

;;; ---- the inner loop
(defun draw-frame (buffer texture u v du dv row-du row-dv)
  "Fill BUFFER, a screenful, from TEXTURE.  U, V is where the top left pixel
is on the texture; DU, DV the step for one pixel to the right; ROW-DU,
ROW-DV the step for one pixel down.  All are fixed-point: the texel in the
top 8 bits of 32."
  (declare (type bytes buffer texture) (type word32 u v du dv row-du row-dv)
           (optimize speed (safety 0)))
  (let ((i 0))
    (declare (fixnum i))
    (dotimes (y +height+)
      (let ((u u) (v v))                ; along this row
        (declare (type word32 u v))
        (dotimes (x +width+)
          ;; the texel's row is V's top 8 bits, its column U's: row * 256 + column
          (setf (aref buffer i)
                (aref texture (logior (logand (ash v -16) #xFF00) (ash u -24))))
          (incf i)
          (setf u (logand (+ u du) #xFFFFFFFF)
                v (logand (+ v dv) #xFFFFFFFF))))
      (setf u (logand (+ u row-du) #xFFFFFFFF)
            v (logand (+ v row-dv) #xFFFFFFFF)))))

;;; ---- once a frame: the angle and the zoom become the six numbers above
(defun frame-steps (sines frame)
  "For frame number FRAME: U, V of the top left pixel, and the steps."
  (flet ((sine (n) (aref sines (logand n 1023))))
    (let* ((angle (+ frame (ash (* 200 (sine (* 3 frame))) -16)))    ; turns, and swings
           ;; texels to the pixel, in 256ths: from a quarter (close up) to nearly three
           (zoom (+ 384 (ash (* 320 (sine (* 5 frame))) -16)))
           ;; 65,536ths times 256ths: 2^24 is one texel, which is the format wanted
           (du (* (sine (+ angle 256)) zoom))                        ; cosine
           (dv (* (sine angle) zoom))
           (row-du (- (floor (* dv 6) 5)))                 ; a quarter turn on, 6/5 as long
           (row-dv (floor (* du 6) 5))
           ;; the middle of the screen looks at a point that wanders over the texture
           (centre-u (* 96 256 (sine (* 2 frame))))
           (centre-v (* 96 256 (sine (+ 300 (* 3 frame))))))
      (flet ((word (n) (logand n #xFFFFFFFF)))
        (values (word (- centre-u (* 160 du) (* 100 row-du)))
                (word (- centre-v (* 160 dv) (* 100 row-dv)))
                (word du) (word dv) (word row-du) (word row-dv))))))

(defun status-line (frames-a-second clocks)
  "CLOCKS is the time one frame took to draw, in the timer chip's clocks."
  (coerce (format nil "~40A" (format nil "Rotozoom ~3D frames/s ~4,1F ms a frame"
                                     frames-a-second (/ (* clocks 1000.0) +timer-hz+)))
          'simple-string))

(defun rotozoom ()
  (let ((texture (make-texture)) (sines (make-sines)) (buffer (make-bytes +screen-bytes+))
        (frame 0) (frames 0) (frames-a-second 0) (drawing 0) (since (timer-count))
        (status (status-line 0 0)) (vga *vga*))
    (with-video-mode (#x13)
      (texture-palette)
      (with-pinned-objects (buffer)     ; it must stay where it is while its address is in use
        (loop
          (when (and (key-pressed-p) (eql (get-key) #\Escape)) (return))
          (let ((start (timer-count)))
            (multiple-value-call #'draw-frame buffer texture (frame-steps sines frame))
            (incf drawing (max 0 (- (timer-count) start))))
          (draw-glyphs (vector-sap buffer) 0 +width+ status +white+ +black+)
          (wait-frame)
          (sb-kernel:system-area-ub8-copy (vector-sap buffer) 0 vga 0 +screen-bytes+)
          (incf frame) (incf frames)
          (let ((elapsed (- (timer-count) since)))
            (when (or (>= elapsed +timer-hz+) (< elapsed 0))         ; a second (or midnight)
              (when (> elapsed 0)
                (setf frames-a-second (round (* frames +timer-hz+) elapsed)
                      status (status-line frames-a-second (round drawing frames))))
              (setf frames 0 drawing 0 since (timer-count)))))))
    (format t "~&rotozoom: ~D frames a second, ~:D pixels a frame, each two additions and a ~
               look-up.~%"
            frames-a-second +screen-bytes+)))

(defun main ()
  "A texture turned and zoomed over the whole screen."
  (rotozoom)
  (values))

(doslib-examples:define-example "rotozoom" 'main
  :category :graphics
  :description "A rotozoomer: a texture turned and zoomed, two additions a pixel.")
