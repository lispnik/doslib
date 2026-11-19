;;;; A paint program for the mouse, in mode 13h.
;;;;
;;;; The mouse comes from the DOS mouse driver through INT 33h (load
;;;; CTMOUSE first); the picture is VGA memory itself, so "undo" is a copy
;;;; of 58,880 bytes and "save" writes them out as a BMP file that the
;;;; bmpview example -- or anything else -- can show.
;;;;
;;;;   left button    paint (or fill, in fill mode); pick a colour from the bar
;;;;   right button   pick up the colour under the pointer
;;;;   1 2 3 4        brush size        P  pencil       F  flood fill
;;;;   U              undo              C  clear        S  save PAINT.BMP
;;;;   Esc            leave
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.paint
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.paint)

(defconstant +height+ 184)              ; the canvas is rows 0..183; the bar is below
(defparameter *swatches*                ; the colours on offer: indices in the standard palette
  (append (loop for i below 16 collect i)
          (loop for i from 32 below 56 by 2 collect i)
          '(20 24 28 31)))
(defvar *colour* 0)
(defvar *size* 3)
(defvar *tool* :pencil)
(defvar *undo* (make-array (* 320 +height+) :element-type '(unsigned-byte 8)))

;;; ---- the mouse
(defun mouse ()                         ; x, y and the buttons (1 left, 2 right)
  (multiple-value-bind (x y buttons) (mouse-pixel)
    (values (min 319 x) (min 199 y) (logand buttons 3))))
(defun toggle-pointer (x y)             ; a cross, XORed in: doing it twice removes it
  (flet ((flip (px py)
           (when (and (< -1 px 320) (< -1 py 200))
             (let ((offset (+ px (* 320 py))))
               (setf (sap-ref-8 *vga* offset) (logxor (sap-ref-8 *vga* offset) 15))))))
    (loop for d from -4 to 4
          unless (zerop d) do (flip (+ x d) y) (flip x (+ y d)))))

;;; ---- drawing
(defun pixel (x y) (sap-ref-8 *vga* (+ x (* 320 y))))
(defun dab (x y)                        ; a square of the brush size, clipped to the canvas
  (let ((half (floor *size* 2)) (colour *colour*))
    (loop for py from (max 0 (- y half)) to (min (1- +height+) (+ y half))
          do (loop for px from (max 0 (- x half)) to (min 319 (+ x half))
                   do (setf (sap-ref-8 *vga* (+ px (* 320 py))) colour)))))
(defun stroke (x0 y0 x1 y1)             ; dabs along a line (Bresenham)
  (let* ((dx (abs (- x1 x0))) (dy (- (abs (- y1 y0))))
         (sx (if (< x0 x1) 1 -1)) (sy (if (< y0 y1) 1 -1)) (err (+ dx dy)))
    (loop (dab x0 y0)
          (when (and (= x0 x1) (= y0 y1)) (return))
          (let ((e2 (* 2 err)))
            (when (>= e2 dy) (incf err dy) (incf x0 sx))
            (when (<= e2 dx) (incf err dx) (incf y0 sy))))))
(defun flood-fill (x y)
  "Fill the area of one colour around X, Y with the current colour, a row at a time."
  (let ((target (pixel x y)) (colour *colour*)
        (stack (make-array 256 :adjustable t :fill-pointer 0)))
    (declare (fixnum target colour))
    (unless (= target colour)
      (vector-push-extend (+ x (* 320 y)) stack)
      (loop until (zerop (fill-pointer stack))
            do (multiple-value-bind (py px) (floor (vector-pop stack) 320)
                 (declare (fixnum px py))
                 (when (= (pixel px py) target)
                   (loop while (and (> px 0) (= (pixel (1- px) py) target)) do (decf px))
                   (let ((above nil) (below nil))
                     (loop while (and (< px 320) (= (pixel px py) target))
                           do (setf (sap-ref-8 *vga* (+ px (* 320 py))) colour)
                              (flet ((look (row seen)   ; push the start of each run to fill
                                       (if (and (< -1 row +height+) (= (pixel px row) target))
                                           (progn (unless seen (vector-push-extend (+ px (* 320 row)) stack))
                                                  t)
                                           nil)))
                                (setf above (look (1- py) above)
                                      below (look (1+ py) below)))
                              (incf px)))))))))
(defun save-undo ()
  (dotimes (i (* 320 +height+)) (setf (aref *undo* i) (sap-ref-8 *vga* i))))
(defun undo ()                          ; swap, so that U twice is "redo"
  (dotimes (i (* 320 +height+))
    (rotatef (aref *undo* i) (sap-ref-8 *vga* i))))
(defun clear-canvas ()
  (dotimes (i (* 320 +height+)) (setf (sap-ref-8 *vga* i) 15)))

;;; ---- the bar at the bottom: colours to pick, and what is selected
(defun say (text)                       ; a few characters at the right, by the BIOS
  (int86 #x10 :ax #x0200 :bx 0 :dx (logior (ash 24 8) 33))
  ;; (not as far as the last column: writing there makes the BIOS scroll the screen)
  (loop for c across (format nil "~6A" (subseq text 0 (min 6 (length text))))
        do (int86 #x10 :ax (logior #x0E00 (char-code c)) :bx 15)))
(defun draw-bar ()
  (loop for y from +height+ below 200
        do (dotimes (x 320) (setf (sap-ref-8 *vga* (+ x (* 320 y))) 0)))
  (loop for colour in *swatches* for i from 0
        do (loop for y from 186 below 197
                 do (loop for x from (* 8 i) below (+ 7 (* 8 i))
                          do (setf (sap-ref-8 *vga* (+ x (* 320 y))) colour)))
           (when (= colour *colour*)    ; a mark under the one in use
             (loop for x from (* 8 i) below (+ 7 (* 8 i))
                   do (setf (sap-ref-8 *vga* (+ x (* 320 198))) 15))))
  (loop for y from 186 below 191        ; the colour in use, larger
        do (loop for x from 264 below 318
                 do (setf (sap-ref-8 *vga* (+ x (* 320 y))) *colour*)))
  (say (format nil "~A ~D" (if (eq *tool* :fill) "fill" "pen") *size*)))

;;; ---- saving: an 8-bit BMP of the canvas, with the palette read back from the VGA
(defun save-bmp (name)
  (let* ((width 320) (height +height+) (offset (+ 14 40 1024)) (size (+ offset (* width height))))
    (with-open-file (out name :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (flet ((u16 (n) (write-byte (ldb (byte 8 0) n) out) (write-byte (ldb (byte 8 8) n) out))
             (u32 (n) (dotimes (i 4) (write-byte (ldb (byte 8 (* 8 i)) n) out))))
        (write-byte 66 out) (write-byte 77 out)                 ; "BM"
        (u32 size) (u32 0) (u32 offset)
        (u32 40) (u32 width) (u32 height) (u16 1) (u16 8) (u32 0)
        (u32 (* width height)) (u32 2835) (u32 2835) (u32 256) (u32 0)
        (dotimes (i 256)                                        ; the palette: B G R 0
          (outb #x3C7 i)
          (let* ((r (inb #x3C9)) (g (inb #x3C9)) (b (inb #x3C9)))
            (write-byte (ash b 2) out) (write-byte (ash g 2) out) (write-byte (ash r 2) out)
            (write-byte 0 out)))
        (loop for y from (1- height) downto 0                   ; rows, bottom first
              do (dotimes (x width) (write-byte (pixel x y) out)))))
    size))

(defun paint ()
  (video-mode #x13)
  (unless (mouse-init)
    (video-mode 3)
    (format t "~&There is no mouse driver.  Load CTMOUSE and try again.~%")
    (return-from paint nil))
  (setf *colour* 0 *size* 3 *tool* :pencil)
  (clear-canvas)
  (save-undo)
  (draw-bar)
  (multiple-value-bind (x y) (mouse)
    (let ((px x) (py y) (down nil))
      (toggle-pointer px py)
      (loop
        (when (key-pressed-p)
          (let ((key (code-char (lo (wait-key)))))
            (toggle-pointer px py)
            (case (char-downcase key)
              (#\Escape (return))
              ((#\1 #\2 #\3 #\4) (setf *size* (nth (digit-char-p key) '(1 1 3 5 9))))
              (#\p (setf *tool* :pencil))
              (#\f (setf *tool* :fill))
              (#\u (undo))
              (#\c (save-undo) (clear-canvas))
              (#\s (draw-bar)
               (say (handler-case (progn (save-bmp "paint.bmp") "saved")
                      (error () "failed")))))
            (unless (char-equal key #\s) (draw-bar))
            (toggle-pointer px py)))
        (multiple-value-bind (x y buttons) (mouse)
          (let ((left (logtest buttons 1)) (right (logtest buttons 2)))
            (when (or (/= x px) (/= y py) (not (eq left down)) right)
              (toggle-pointer px py)                    ; off, while we draw
              (cond ((and right (< y +height+))         ; pick up a colour
                     (setf *colour* (pixel x y)) (draw-bar))
                    ((and left (>= y (1+ +height+)))    ; choose one from the bar
                     (let ((colour (nth (floor x 8) *swatches*)))
                       (when (and colour (not down)) (setf *colour* colour) (draw-bar))))
                    ((and left (eq *tool* :fill))
                     (unless down (save-undo) (flood-fill x (min y (1- +height+)))))
                    (left
                     (unless down (save-undo) (setf px x py y))
                     (stroke px (min py (1- +height+)) x (min y (1- +height+)))))
              (setf px x py y down left)
              (toggle-pointer px py)))))))
  (video-mode 3)
  t)

(defun main ()
  "A paint program for the mouse."
  (paint))

(doslib-examples:define-example "paint" 'main
  :category :graphics
  :description "A paint program for the mouse; saves PAINT.BMP.")
