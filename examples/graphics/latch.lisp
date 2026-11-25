;;;; Latch copies: moving pictures about inside video memory, four pixels to
;;;; the byte.  In Mode X (256 colours with the VGA's memory unchained into
;;;; four planes) the four pixels x = 4n to 4n+3 are at one address, one in
;;;; each plane.  When the processor reads an address the VGA reads all four
;;;; planes into its four latches, though the processor is given only one of
;;;; them.  In write mode 1 (bits 0-1 of graphics controller register 5, at
;;;; port 3CEh) a write puts the latches, not the processor's byte, into the
;;;; planes.  So "read a byte there, write a byte here" copies four pixels,
;;;; and the byte itself does not matter.
;;;;
;;;; Sixteen 16 by 16 tiles are kept in video memory beyond the end of the
;;;; picture (a 320x240 page is 19,200 addresses of the 65,536), and a screen
;;;; of 280 tiles is drawn over and over in two ways:
;;;;
;;;;   - from Lisp's memory, a plane at a time: 256 bytes written for a tile;
;;;;   - with the latches, from the tiles kept in video memory: 64 bytes read
;;;;     and 64 written for a tile.
;;;;
;;;; Each is timed, in tiles a second, and the tiles are shuffled a little
;;;; before every screen so that the drawing can be seen.
;;;;
;;;;   (doslib-examples:run "latch")
;;;; The two ways take turns, two seconds each; Space changes at once; Esc
;;;; leaves, and prints the two rates.
;;;;
;;;; On a real VGA the latches are about four times as fast, because the
;;;; time goes on getting at video memory.  An emulator's accounts differ --
;;;; every access to planar memory costs it much the same, whatever it
;;;; moves -- so expect a smaller gain there.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.latch
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.latch)

(defconstant +tiles+ 16)                ; different tiles
(defconstant +columns+ 20) (defconstant +rows+ 14)      ; tiles on the screen: 280
(defconstant +top+ 16)                  ; the pixel row the tiles start at, below the text
(defconstant +store+ 19200)             ; where the tiles are kept: just after the picture

;;; ---- the mode
;;; (MODE-X, 320x240 in four planes, is the library's, with VGA-PLANES for the map mask and
;;; VGA-GRAPHICS for the graphics controller's registers: src/modex.lisp.)

;;; ---- the tiles: in Lisp's memory, a plane at a time as video memory wants them
(defvar *tiles* (make-array (* 256 +tiles+) :element-type '(unsigned-byte 8))
  "Each tile is 256 bytes: four planes of 16 rows of 4.")
(defun tile-pixel (tile x y)
  "A bevelled square in one of sixteen hues of the BIOS's palette, with its number on it
(in the BIOS's 8 by 8 characters: the library's GLYPH-ROW)."
  (let ((hue (+ 32 (floor (* tile 3) 2))))
    (cond ((and (<= 4 x 11) (<= 4 y 11)
                (logbitp (- 11 x) (glyph-row (char-code (digit-char tile 16)) (- y 4))))
           15)
          ((or (= x 15) (= y 15)) (+ hue 144))  ; the dark edges
          ((or (= x 0) (= y 0)) (+ hue 24))     ; the light ones
          ((and (<= 3 x 12) (<= 3 y 12)) (+ hue 72))
          (t hue))))
(defun make-tiles ()
  (dotimes (tile +tiles+)
    (dotimes (y 16)
      (dotimes (x 16)
        (setf (aref *tiles* (+ (* 256 tile) (* 64 (mod x 4)) (* 4 y) (floor x 4)))
              (tile-pixel tile x y))))))
(defun store-tiles ()
  "Put the tiles in video memory after the picture: 64 addresses each, 4 for each row."
  (dotimes (plane 4)
    (vga-planes (ash 1 plane))
    (dotimes (tile +tiles+)
      (dotimes (i 64)
        (setf (sap-ref-8 *vga* (+ +store+ (* 64 tile) i))
              (aref *tiles* (+ (* 256 tile) (* 64 plane) i)))))))

;;; ---- the screen of tiles, and the two ways to draw it
(defvar *screen* (make-array (* +columns+ +rows+) :element-type '(unsigned-byte 8)))
(defun draw-from-lisp ()
  "Every tile from Lisp's memory: for each plane, 64 bytes a tile."
  (declare (optimize speed))
  (let ((vga *vga*) (tiles *tiles*) (screen *screen*))
    (declare (type system-area-pointer vga)
             (type (simple-array (unsigned-byte 8) (*)) tiles screen))
    (dotimes (plane 4)
      (vga-planes (ash 1 plane))        ; map mask: this plane only
      (dotimes (row +rows+)
        (dotimes (column +columns+)
          (let ((source (+ (* 256 (aref screen (+ column (* +columns+ row)))) (* 64 plane)))
                (target (+ (* 80 (+ +top+ (* 16 row))) (* 4 column))))
            (declare (type fixnum source target))
            (dotimes (line 16)
              (dotimes (i 4)
                (setf (sap-ref-8 vga (+ target i)) (aref tiles (+ source i))))
              (incf source 4)
              (incf target 80))))))))
(defun draw-with-latches ()
  "Every tile from the copies in video memory: 64 reads and 64 writes a tile."
  (declare (optimize speed))
  (let ((vga *vga*) (screen *screen*))
    (declare (type system-area-pointer vga) (type (simple-array (unsigned-byte 8) (*)) screen))
    (vga-planes #x0F)                   ; map mask: all four planes
    (vga-graphics 5 #x41)               ; write mode 1 (bit 6 is the 256-colour mode's)
    (dotimes (row +rows+)
      (dotimes (column +columns+)
        (let ((source (+ +store+ (* 64 (aref screen (+ column (* +columns+ row))))))
              (target (+ (* 80 (+ +top+ (* 16 row))) (* 4 column))))
          (declare (type fixnum source target))
          (dotimes (line 16)
            (dotimes (i 4)              ; the read fills the latches; the write empties them
              (setf (sap-ref-8 vga (+ target i)) (sap-ref-8 vga (+ source i))))
            (incf source 4)
            (incf target 80)))))
    (vga-graphics 5 #x40)))             ; write mode 0 again

(defun shuffle ()
  "Change the screen a little: slide one row along, and swap some tiles."
  (let ((row (random +rows+)))
    (loop for i from (* +columns+ row) below (1- (* +columns+ (1+ row)))
          do (rotatef (aref *screen* i) (aref *screen* (1+ i)))))
  (dotimes (i 6)
    (rotatef (aref *screen* (random (length *screen*)))
             (aref *screen* (random (length *screen*))))))

;;; ---- text: the library's, for planes (MODE-X-TEXT)
(defun draw-status (latches lisp-rate latch-rate)
  (flet ((line (y mine name rate)
           (mode-x-text 0 y (format nil "~C ~A ~5@A tiles/s  "
                                    (if mine (code-char 16) #\Space) name (or rate "?"))
                        (if mine 15 7) 0)))
    (line 0 (not latches) "From Lisp, by planes: " lisp-rate)
    (line 8 latches "Latches, write mode 1:" latch-rate)))

(defun latch ()
  (let ((lisp-rate nil) (latch-rate nil))
    (mode-x)
    (unwind-protect
         (let ((latches nil))
           (make-tiles)
           (store-tiles)
           (dotimes (i (length *screen*)) (setf (aref *screen* i) (mod i +tiles+)))
           (draw-status latches lisp-rate latch-rate)
           (loop with mark = (ticks) with screens = 0 with turn = (ticks)
                 do (when (key-pressed-p)
                      (let ((key (get-key)))
                        (cond ((eql key #\Escape) (return))
                              ((eql key #\Space) (setf turn (- (ticks) 36))))))
                    (shuffle)
                    (if latches (draw-with-latches) (draw-from-lisp))
                    (incf screens)
                    (let ((elapsed (- (ticks) mark)) (now (ticks)))
                      (when (or (>= elapsed 18) (>= (- now turn) 36))
                        (when (>= elapsed 9)    ; long enough to be worth saying
                          (let ((rate (round (* screens +columns+ +rows+ 18.2) elapsed)))
                            (if latches (setf latch-rate rate) (setf lisp-rate rate))))
                        (when (>= (- now turn) 36)
                          (setf latches (not latches) turn now))
                        (draw-status latches lisp-rate latch-rate)
                        (setf mark (ticks) screens 0)))))
      (vga-graphics 5 #x40)
      (video-mode 3))
    (when (and lisp-rate latch-rate)
      (format t "~&From Lisp's memory, a plane at a time: ~D tiles a second.~%" lisp-rate)
      (format t "With the latches, in write mode 1: ~D tiles a second, ~,1F times as many.~%"
              latch-rate (/ latch-rate (max 1 lisp-rate))))))

(defun main ()
  "Latch copies: tiles moved inside video memory four pixels to the byte."
  (latch)
  (values))

(doslib-examples:define-example "latch" 'main
  :category :graphics
  :description "Latch copies: tiles moved inside video memory four pixels to the byte.")
