;;;; The tunnel: flying down an endless tiled pipe, in mode 13h (320 by 200,
;;;; a byte a pixel), with no 3-D arithmetic at all while it runs.
;;;;
;;;;   (doslib-examples:run "tunnel")
;;;; Esc leaves.
;;;;
;;;; Look straight down a pipe and every point of its wall is at some angle
;;;; round the middle of the view and at some distance from it; and the
;;;; further down the pipe a piece of wall is, the nearer the middle it
;;;; appears: its depth goes as 1 / distance on the screen.  The wall is
;;;; papered with a texture, 256 by 256, that repeats: across it goes round
;;;; the pipe, down it goes along the pipe.  So the colour of a pixel is
;;;;
;;;;   texture[depth of this pixel, angle of this pixel]
;;;;
;;;; and neither the angle (an arctangent) nor the depth (a square root and a
;;;; division) of a pixel ever changes.  They are worked out once, before
;;;; the picture starts, and kept in a table: for each pixel 16 bits, the
;;;; depth in the top 8 and the angle in the bottom 8, both reckoned so that
;;;; 256 is once round the texture.  To fly forward, add something to every
;;;; depth; to roll, add something to every angle.  Each pixel of a frame is
;;;; then two additions, two table look-ups, and a little masking, in whole
;;;; numbers of at most 17 bits.
;;;;
;;;; Two refinements.  A second table has a brightness for each pixel, less
;;;; towards the middle, so the pipe fades into the dark where the texture
;;;; would be too small to see anyway (it is repeated many times over in a
;;;; few pixels there).  The palette is arranged for it: 16 colours of
;;;; texture at each of 16 brightnesses, the colour number being brightness
;;;; * 16 + texture colour, so shading a pixel is one OR.  And the tables are
;;;; made for a picture 640 by 400, four screens, of which the screen shows a
;;;; window that moves about: the pipe seems to bend as the view swings.
;;;;
;;;; The tables take a moment to make (256,000 arctangents and square roots:
;;;; this is what the trick saves doing every frame, and even here the
;;;; arctangents come from a table of 257) and three quarters of a megabyte
;;;; to keep.  Each frame is made in a Lisp vector of 64,000 bytes
;;;; and copied to video memory in one go.  The top line shows frames a
;;;; second (paced by WAIT-FRAME: 70 or so if the machine keeps up) and how
;;;; long one frame took to draw, timed with the timer chip.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.tunnel
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.tunnel)

(defconstant +width+ 320) (defconstant +height+ 200)
(defconstant +screen-bytes+ (* +width+ +height+))
(defconstant +table-width+ 640) (defconstant +table-height+ 400)
(defconstant +white+ 255)               ; and colour 0 is black

(deftype bytes () '(simple-array (unsigned-byte 8) (*)))
(deftype words () '(simple-array (unsigned-byte 16) (*)))
(defun make-bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;;; ---- the texture: 256 by 256, sixteen colours.  Tiles 32 texels square,
;;; alternately of colours 0 to 7 (stone) and 8 to 14 (rust), raised in the
;;; middle and dark in the joints.
(defun make-texture ()
  (let ((texture (make-bytes (* 256 256))))
    (dotimes (y 256 texture)
      (dotimes (x 256)
        (let* ((tx (logand x 31)) (ty (logand y 31))
               (edge (min tx ty (- 31 tx) (- 31 ty)))                 ; 0 at the joint, to 15
               (grain (logand (logxor (ash x -1) (ash y -1) (ash (* x y) -6)) 1))
               (level (cond ((zerop edge) 0)
                            ((< edge 4) (+ 1 edge))                   ; the bevel
                            (t (+ 5 grain)))))
          (setf (aref texture (+ x (* 256 y)))
                (if (evenp (+ (ash x -5) (ash y -5))) level (+ 8 level))))))))

(defun tunnel-palette ()
  "Colour brightness * 16 + c is texture colour c at that brightness."
  (dotimes (light 16)
    (dotimes (c 16)
      (let ((level (logand c 7)))
        (flet ((shade (dark bright)     ; 6 bits, for this level and this brightness
                 (round (* light (+ dark (/ (* level (- bright dark)) 7))) 15)))
          (if (< c 8)
              (set-palette (+ c (* 16 light)) (shade 6 44) (shade 10 54) (shade 16 63))
              (set-palette (+ c (* 16 light)) (shade 14 63) (shade 4 46) (shade 2 20)))))))
  (set-palette +white+ 63 63 63))

;;; ---- the tables: for each pixel of a picture 640 by 400, its depth and
;;; angle on the pipe's wall, and its brightness.  The square root is the only
;;; floating-point arithmetic done for each pixel, and the arctangent is itself
;;; looked up: that of a ratio from 0 to 1 (an eighth of a circle) is in a small
;;; table, and the other seven eighths are that one mirrored.
(defun make-arctangents ()
  "The angle whose tangent is i/256, for i from 0 to 256, in the tunnel's
units: 512 to the circle, so that the texture goes twice round the pipe (16
tiles)."
  (let ((table (make-bytes 257)))
    (dotimes (i 257 table)
      (setf (aref table i) (round (* (atan i 256) (/ 256 pi)))))))

(defun make-tables ()
  (let ((where (make-array (* +table-width+ +table-height+) :element-type '(unsigned-byte 16)))
        (light (make-bytes (* +table-width+ +table-height+)))
        (arctangents (make-arctangents))
        (i 0))
    (declare (type words where) (type bytes light arctangents) (fixnum i) (optimize speed))
    (dotimes (y +table-height+)
      (dotimes (x +table-width+)
        ;; In half pixels from the middle, which is between four pixels, so neither
        ;; is ever 0; DY in fifths of those, the pixels being 6/5 as tall as wide.
        (let* ((dx (* 5 (- (* 2 x) (1- +table-width+))))
               (dy (* 6 (- (* 2 y) (1- +table-height+))))
               (ax (abs dx)) (ay (abs dy))
               (eighth (if (<= ay ax)
                           (aref arctangents (truncate (* 256 ay) ax))
                           (- 128 (aref arctangents (truncate (* 256 ax) ay)))))
               (quarter (if (minusp dx) (- 256 eighth) eighth))
               (angle (logand (if (minusp dy) (- quarter) quarter) 255))
               (distance (* 0.1f0 (sqrt (float (+ (* ax ax) (* ay ay)) 1f0))))    ; in pixels
               (depth (logand (truncate (+ 0.5f0 (/ 8000f0 distance))) 255)))
          (declare (type (integer -8000 8000) dx dy) (type (integer 1 8000) ax ay)
                   (type (single-float 0.4f0 1000f0) distance))
          (setf (aref where i) (logior (ash depth 8) angle)
                (aref light i) (* 16 (min 15 (truncate distance 9f0)))))
        (incf i)))
    (values where light)))

;;; ---- the inner loop
(defun draw-frame (buffer texture where light left top forward roll)
  "Fill BUFFER, a screenful, with the part of the tunnel whose top left
corner is at LEFT, TOP in the tables, having flown FORWARD texels down it
and rolled ROLL texels round (each 0 to 255)."
  (declare (type bytes buffer texture light) (type words where)
           (type (unsigned-byte 8) forward roll) (type (unsigned-byte 16) left top)
           (optimize speed (safety 0)))
  (let ((i 0) (forward (ash forward 8)))
    (declare (fixnum i))
    (dotimes (y +height+)
      (let ((j (+ left (* +table-width+ (+ top y)))))
        (declare (fixnum j))
        (dotimes (x +width+)
          (let ((w (aref where j)))
            (setf (aref buffer i)
                  (logior (aref texture (logior (logand (+ w forward) #xFF00)    ; the row: depth
                                                (logand (+ w roll) #x00FF)))     ; the column: angle
                          (aref light j))))
          (incf i) (incf j))))))

(defun make-sines ()
  "1024 steps to the circle, 256 for 1."
  (let ((sines (make-array 1024 :element-type 'fixnum)))
    (dotimes (i 1024 sines)
      (setf (aref sines i) (round (* 256 (sin (* i (/ pi 512)))))))))

(defun status-line (frames-a-second clocks)
  "CLOCKS is the time one frame took to draw, in the timer chip's clocks."
  (coerce (format nil "~40A" (format nil "Tunnel ~3D frames/s ~4,1F ms a frame"
                                     frames-a-second (/ (* clocks 1000.0) +timer-hz+)))
          'simple-string))

(defun tunnel ()
  (format t "~&tunnel: working out the tables...~%")
  (multiple-value-bind (where light) (make-tables)
    (let ((texture (make-texture)) (sines (make-sines)) (buffer (make-bytes +screen-bytes+))
          (frame 0) (frames 0) (frames-a-second 0) (drawing 0) (since (timer-count))
          (status (status-line 0 0)) (vga *vga*))
      (with-video-mode (#x13)
        (tunnel-palette)
        (with-pinned-objects (buffer)   ; it must stay where it is while its address is in use
          (loop
            (when (and (key-pressed-p) (eql (get-key) #\Escape)) (return))
            (flet ((sine (n) (aref sines (logand n 1023))))
              (let ((start (timer-count)))
                (draw-frame buffer texture where light
                            ;; the window swings about the middle of the tables
                            (+ 160 (ash (* 150 (sine (* 3 frame))) -8))
                            (+ 100 (ash (* 90 (sine (+ 256 (* 4 frame)))) -8))
                            (logand (* 2 frame) 255)                      ; flying forward
                            (logand (ash (* 96 (sine (* 2 frame))) -8) 255))  ; rolling to and fro
                (incf drawing (max 0 (- (timer-count) start)))))
            (draw-glyphs (vector-sap buffer) 0 +width+ status +white+ 0)
            (wait-frame)
            (sb-kernel:system-area-ub8-copy (vector-sap buffer) 0 vga 0 +screen-bytes+)
            (incf frame) (incf frames)
            (let ((elapsed (- (timer-count) since)))
              (when (or (>= elapsed +timer-hz+) (< elapsed 0))       ; a second (or midnight)
                (when (> elapsed 0)
                  (setf frames-a-second (round (* frames +timer-hz+) elapsed)
                        status (status-line frames-a-second (round drawing frames))))
                (setf frames 0 drawing 0 since (timer-count)))))))
      (format t "~&tunnel: ~D frames a second, ~:D pixels a frame, each two additions and three ~
                 look-ups.~%"
              frames-a-second +screen-bytes+))))

(defun main ()
  "Flying down a textured tunnel, from tables of each pixel's depth and angle."
  (tunnel)
  (values))

(doslib-examples:define-example "tunnel" 'main
  :category :graphics
  :description "The tunnel effect: tables of each pixel's depth and angle, a moving texture.")
