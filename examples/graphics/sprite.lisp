;;;; A sprite editor, worked with the mouse: a 16 by 16 picture drawn large,
;;;; a palette to choose colours from, and the sprite itself at its real
;;;; size, tiled, and flying about.
;;;;
;;;;   (doslib-examples:run "sprite")
;;;; Left button: paint a cell, or choose a colour from the palette.
;;;; Right button: pick up the colour of a cell.  Keys: Space paints the
;;;; cell under the pointer, C clears, M mirrors, S saves the sprite in
;;;; SPRITE.DAT (256 bytes, a colour each), L loads it, Esc leaves.
;;;;
;;;; The mouse is the library's (src/mouse.lisp): MOUSE-PIXEL says where it
;;;; is in this mode's pixels.  The driver draws no pointer in mode 13h, so
;;;; the program draws one: the pixels under it are kept, and put back
;;;; before anything else is drawn.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.sprite
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.sprite)

(defconstant +size+ 16)                 ; the sprite is this many pixels square
(defconstant +cell+ 10)                 ; and each is drawn this large
(defconstant +grid-x+ 8) (defconstant +grid-y+ 8)
(defconstant +palette-x+ 184) (defconstant +palette-y+ 8)      ; 16 by 16 swatches of 8 by 6
(defconstant +stage-x+ 184) (defconstant +stage-y+ 116)        ; where the sprite flies
(defconstant +stage-width+ 128) (defconstant +stage-height+ 52)

(defvar *sprite* (make-array (* +size+ +size+) :element-type '(unsigned-byte 8)))
(defvar *colour* 15)

(defun pixel (x y) (sap-ref-8 *vga* (+ x (* 320 y))))
(defun box (x y width height colour)
  (dotimes (j height)
    (dotimes (i width) (plot (+ x i) (+ y j) colour))))
(defun frame (x y width height colour)
  (draw-line x y (+ x width -1) y colour)
  (draw-line x (+ y height -1) (+ x width -1) (+ y height -1) colour)
  (draw-line x y x (+ y height -1) colour)
  (draw-line (+ x width -1) y (+ x width -1) (+ y height -1) colour))

(defun cell (column row) (aref *sprite* (+ column (* +size+ row))))
(defun draw-cell (column row)
  (box (+ +grid-x+ (* +cell+ column)) (+ +grid-y+ (* +cell+ row)) (1- +cell+) (1- +cell+)
       (cell column row)))
(defun set-cell (column row colour)
  (setf (aref *sprite* (+ column (* +size+ row))) colour)
  (draw-cell column row))
(defun draw-sprite (x y)
  "The sprite at its real size; colour 0 is drawn too, as black."
  (dotimes (row +size+)
    (dotimes (column +size+) (plot (+ x column) (+ y row) (cell column row)))))
(defun draw-current-colour ()
  (box 8 176 40 16 *colour*)
  (frame 7 175 42 18 15))
(defun draw-all ()
  (box 0 0 320 200 1)
  (box (1- +grid-x+) (1- +grid-y+) (1+ (* +cell+ +size+)) (1+ (* +cell+ +size+)) 8)
  (dotimes (row +size+) (dotimes (column +size+) (draw-cell column row)))
  (dotimes (colour 256)
    (box (+ +palette-x+ (* 8 (mod colour 16))) (+ +palette-y+ (* 6 (floor colour 16))) 8 6 colour))
  (frame (1- +palette-x+) (1- +palette-y+) 130 98 15)
  (frame (1- +stage-x+) (1- +stage-y+) (+ 2 +stage-width+) (+ 2 +stage-height+) 15)
  (draw-current-colour))

;;; ---- the pointer: an arrow, with what was under it kept
(defparameter *arrow* #(#b10000000 #b11000000 #b11100000 #b11110000
                        #b11111000 #b11100000 #b10110000 #b00011000))
(defvar *under* (make-array 64 :element-type '(unsigned-byte 8)))
(defvar *pointer* nil "Where the pointer is drawn: (x . y), or NIL when it is not.")
(defun hide-pointer ()
  (when *pointer*
    (destructuring-bind (x . y) *pointer*
      (dotimes (j 8)
        (dotimes (i 8)
          (when (and (< (+ x i) 320) (< (+ y j) 200))
            (setf (sap-ref-8 *vga* (+ x i (* 320 (+ y j)))) (aref *under* (+ i (* 8 j))))))))
    (setf *pointer* nil)))
(defun show-pointer (x y)
  (dotimes (j 8)
    (dotimes (i 8)
      (when (and (< (+ x i) 320) (< (+ y j) 200))
        (setf (aref *under* (+ i (* 8 j))) (pixel (+ x i) (+ y j)))
        (when (logbitp (- 7 i) (aref *arrow* j)) (plot (+ x i) (+ y j) 15)))))
  (setf *pointer* (cons x y)))

;;; ---- what the mouse is over
(defun grid-cell (x y)
  "The column and row of the cell at pixel X, Y, or NIL."
  (let ((column (floor (- x +grid-x+) +cell+)) (row (floor (- y +grid-y+) +cell+)))
    (and (< -1 column +size+) (< -1 row +size+) (values column row))))
(defun palette-colour (x y)
  (let ((column (floor (- x +palette-x+) 8)) (row (floor (- y +palette-y+) 6)))
    (and (< -1 column 16) (< -1 row 16) (+ column (* 16 row)))))
(defun paint-at (x y)
  (multiple-value-bind (column row) (grid-cell x y)
    (if column
        (set-cell column row *colour*)
        (let ((colour (palette-colour x y)))
          (when colour (setf *colour* colour) (draw-current-colour))))))
(defun pick-at (x y)
  (multiple-value-bind (column row) (grid-cell x y)
    (when column (setf *colour* (cell column row)) (draw-current-colour))))

;;; ---- keys
(defun mirror ()
  (dotimes (row +size+)
    (dotimes (column (floor +size+ 2))
      (let ((left (cell column row)) (right (cell (- +size+ 1 column) row)))
        (set-cell column row right)
        (set-cell (- +size+ 1 column) row left)))))
(defun save-sprite (&optional (file "SPRITE.DAT"))
  (with-open-file (out file :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence *sprite* out)))
(defun load-sprite (&optional (file "SPRITE.DAT"))
  (with-open-file (in file :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when in
      (read-sequence *sprite* in)
      (dotimes (row +size+) (dotimes (column +size+) (draw-cell column row))))))
(defun starting-sprite ()
  "Something to look at: a ball."
  (dotimes (row +size+)
    (dotimes (column +size+)
      (let ((d (+ (expt (- column 7.5) 2) (expt (- row 7.5) 2))))
        (setf (aref *sprite* (+ column (* +size+ row)))
              (cond ((> d 56) 0) ((> d 42) 4) ((< (+ (expt (- column 5) 2) (expt (- row 5) 2)) 5) 15)
                    (t 12)))))))

(defun sprite ()
  (unless (mouse-init)
    (format t "~&There is no mouse driver.  Load CTMOUSE and try again.~%")
    (return-from sprite nil))
  (with-video-mode (#x13)
    (mouse-init)                        ; again, for the new mode's size
    (mouse-range 0 0 639 199)
    (mouse-move (* 2 (+ +grid-x+ (* +cell+ 8) 4)) (+ +grid-y+ (* +cell+ 8) 4))   ; the middle of the grid
    (setf *colour* 15 *pointer* nil)
    (starting-sprite)
    (draw-all)
    (let ((fly-x 0) (fly-y 0) (dx 2) (dy 1) (tick (ticks)))
      (loop
        (multiple-value-bind (x y buttons) (mouse-pixel)
          (let ((x (min 319 x)) (y (min 199 y)))
            (when (or (null *pointer*) (/= x (car *pointer*)) (/= y (cdr *pointer*))
                      (/= buttons 0) (/= tick (ticks)) (key-pressed-p))
              (hide-pointer)
              (cond ((logtest buttons 1) (paint-at x y))
                    ((logtest buttons 2) (pick-at x y)))
              (when (key-pressed-p)
                (case (char-downcase (code-char (lo (wait-key))))
                  (#\Escape (return))
                  (#\Space (paint-at x y))
                  (#\c (fill *sprite* 0) (draw-all))
                  (#\m (mirror))
                  (#\s (save-sprite))
                  (#\l (load-sprite))))
              (unless (= tick (ticks))  ; 18 times a second: the sprite flies on
                (setf tick (ticks))
                (box (+ +stage-x+ fly-x) (+ +stage-y+ fly-y) +size+ +size+ 1)
                (when (or (< (+ fly-x dx) 0) (> (+ fly-x dx) (- +stage-width+ +size+))) (setf dx (- dx)))
                (when (or (< (+ fly-y dy) 0) (> (+ fly-y dy) (- +stage-height+ +size+))) (setf dy (- dy)))
                (incf fly-x dx) (incf fly-y dy)
                (draw-sprite (+ +stage-x+ fly-x) (+ +stage-y+ fly-y))
                (dotimes (i 4) (draw-sprite (+ 60 (* +size+ i)) 176)))     ; and tiled
              (show-pointer x y)))))))
  t)

(defun main ()
  "A sprite editor, worked with the mouse."
  (sprite)
  (values))

(doslib-examples:define-example "sprite" 'main
  :category :graphics
  :description "A sprite editor, worked with the mouse.")
