;;;; VGA: video modes, the palette, pixels and lines in mode 13h; VESA modes.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; Video
(defvar *vga* (int-sap #xA0000) "Video memory in the graphics modes: a byte a pixel in mode 13h.")
(defun video-mode (mode)
  "Set BIOS video mode MODE: 3 is 80x25 text, #x13 is 320x200 in 256 colours."
  (int86 #x10 :ax mode))
(defun current-video-mode ()
  "The BIOS's video mode: 3 is 80x25 text, #x13 is 320x200 in 256 colours."
  (peek #x449))
(defmacro with-video-mode ((mode) &body body)
  "Run BODY in video mode MODE, and go back to 80x25 text however it ends."
  `(progn (video-mode ,mode)
          (unwind-protect (progn ,@body) (video-mode 3))))
(defun wait-retrace ()
  "Wait for the start of the next vertical retrace: the moment to change
the palette or the picture without it showing."
  (loop while (logbitp 3 (inb #x3DA)))
  (loop until (logbitp 3 (inb #x3DA))))
(defun vesa-set-mode (mode &key linear keep)
  "Set a VESA BIOS mode (#x101 is 640x480 in 256 colours).  True if the BIOS
did.  With LINEAR the mode is set for its linear framebuffer (see
VESA-MODE-INFO) and not for the 64K window at A0000h that VESA-SET-BANK
moves; with KEEP video memory is not cleared, and the picture stays."
  (= (word16 (int86 #x10 :ax #x4F02
                         :bx (logior mode (if linear #x4000 0) (if keep #x8000 0))))
     #x004F))
(defun vesa-mode-info (mode)
  "What the VESA BIOS says of MODE (INT 10h, AX=4F01h), as a property list,
or NIL if it knows no such mode.  :WIDTH and :HEIGHT are in pixels, :BITS the
bits in a pixel, :LINE-BYTES the bytes from one row to the next;
:FRAMEBUFFER is the physical address of the mode's linear framebuffer, for
MAP-PHYSICAL, or NIL if it has none; :GRANULARITY is how many kilobytes one
step of VESA-SET-BANK moves the window; :ATTRIBUTES is the mode's attribute
word (bit 7: a linear framebuffer, bit 4: graphics)."
  ;; ES:DI is a real-mode pointer to 256 bytes for the BIOS to fill in: so
  ;; they are in DOS memory, and the call is REAL-INT.
  (with-dos-memory (segment 256)
    (let ((block (linear segment)))
      (dotimes (i 256) (setf (sap-ref-8 (int-sap block) i) 0))
      (when (= (word16 (real-int #x10 :ax #x4F01 :cx mode :es segment :di 0)) #x004F)
        (let ((attributes (peek16 block)) (framebuffer (peek32 (+ block #x28))))
          (list :width (peek16 (+ block #x12)) :height (peek16 (+ block #x14))
                :bits (peek (+ block #x19)) :line-bytes (peek16 (+ block #x10))
                :framebuffer (and (logbitp 7 attributes) (plusp framebuffer) framebuffer)
                :granularity (peek16 (+ block 4))
                :attributes attributes))))))
(defun vesa-set-bank (bank &optional (window 0))
  "Move the 64K window at A0000h to BANK of a VESA mode's video memory."
  (int86 #x10 :ax #x4F05 :bx window :dx bank))
(defun set-palette (index r g b)
  "Make colour INDEX the mixture R, G, B: each from 0 to 63."
  (outb #x3C8 index) (outb #x3C9 r) (outb #x3C9 g) (outb #x3C9 b))
(defun rainbow-palette (&optional (first 1) (last 255))
  "Make colours FIRST to LAST a rainbow, and colour 0 black."
  (set-palette 0 0 0 0)
  (loop for i from first to last
        for a = (* 2 pi (/ (- i first) (- last first -1)))
        do (flet ((v (phase) (round (* 31.5 (1+ (sin (+ a phase)))))))
             (set-palette i (v 0) (v 2.094) (v 4.189)))))
(declaim (inline plot))
(defun plot (x y colour)
  "Set the pixel at X, Y in mode 13h; off the screen, nothing happens."
  (declare (fixnum x y colour))
  (when (and (< -1 x 320) (< -1 y 200))
    (setf (sap-ref-8 *vga* (+ x (* 320 y))) colour)))
(defun draw-line (x0 y0 x1 y1 colour)
  "Draw a line in mode 13h (Bresenham's algorithm)."
  (let* ((dx (abs (- x1 x0))) (dy (- (abs (- y1 y0))))
         (sx (if (< x0 x1) 1 -1)) (sy (if (< y0 y1) 1 -1)) (err (+ dx dy)))
    (loop (plot x0 y0 colour)
          (when (and (= x0 x1) (= y0 y1)) (return))
          (let ((e2 (* 2 err)))
            (when (>= e2 dy) (incf err dy) (incf x0 sx))
            (when (<= e2 dx) (incf err dx) (incf y0 sy))))))
