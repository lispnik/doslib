;;;; A sprite blitter with a back buffer: forty sprites over a tiled
;;;; background in mode 13h (320 by 200, a byte a pixel), without flicker.
;;;;
;;;;   (doslib-examples:run "blit")
;;;; Space: draw into the back buffer, or straight to the screen.  Esc leaves.
;;;;
;;;; Every frame the whole picture is made again: the background first, then
;;;; each sprite on top of it, leaving out the sprite's pixels of colour 0
;;;; (that is all "transparency" is: a colour that is not copied).  Done in
;;;; video memory, the monitor's beam shows whatever is there as it passes:
;;;; sometimes the bare background where a sprite is about to be, sometimes
;;;; half a sprite.  That is flicker.  So the picture is made somewhere the
;;;; beam does not look -- a vector of 64,000 bytes in Lisp's memory, the
;;;; back buffer -- and copied to video memory in one go at the vertical
;;;; retrace, while the beam is on its way back to the top.  The screen only
;;;; ever shows finished frames.
;;;;
;;;; The top line says which way it is being done and how many frames a
;;;; second are drawn (the monitor's 70, if the machine keeps up).  Drawing
;;;; straight to the screen does not wait for the retrace: a fast machine
;;;; that started there could finish before the beam came round, and hide
;;;; what this is here to show.  An emulator shows video memory at moments
;;;; of its own choosing rather than as a beam passes, so there the flicker
;;;; is sprites missing here and there for a frame; and QEMU's retrace bit
;;;; is not timed at all (it flips every time it is read), so the frames
;;;; are also paced by the timer chip.
;;;;
;;;; If the sprite editor (the example "sprite") has saved a sprite in
;;;; SPRITE.DAT in the current directory (256 bytes: 16 by 16, a colour
;;;; each), that is the sprite drawn; otherwise it is a ball.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.blit
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.blit)

(defconstant +width+ 320) (defconstant +height+ 200)
(defconstant +screen-bytes+ (* +width+ +height+))
(defconstant +size+ 16)                 ; a sprite is this many pixels square
(defconstant +sprites+ 40)
(defconstant +shades+ 192)              ; the background's colours: 64 from here

(deftype bytes () '(simple-array (unsigned-byte 8) (*)))
(defun make-bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;;; ---- time, finer than the BIOS's 18.2 ticks a second
(defconstant +clocks-a-second+ 1193182)
(defconstant +frame-clocks+ 16000)      ; a little less than a frame at 70 a second
(defvar *clock* 0 "The last time CLOCK gave.")
(defun clock ()
  "The time in the timer chip's clocks, 1,193,182 to the second: the BIOS's
count of ticks, and how far counter 0 has got through the 65,536 clocks of
this one."
  (loop
    (let ((tick (ticks)))
      (outb #x43 #xC2)                  ; read back counter 0: its status, then its count
      (let* ((status (inb #x40)) (low (inb #x40))
             (gone (logand (- (+ low (* 256 (inb #x40)))) #xFFFF))
             (now (+ (* tick 65536)
                     (if (= (logand status 6) 6)
                         ;; mode 3, a square wave: it counts down twice a tick, by twos,
                         ;; and its output (bit 7) is high the first time
                         (+ (if (logbitp 7 status) 0 32768) (floor gone 2))
                         gone))))       ; mode 2: once a tick, by ones
        ;; The counter can start again a moment before the BIOS has counted the
        ;; tick, and then the time seems to go back by one tick: read again.
        (unless (or (/= tick (ticks)) (< -65537 (- now *clock*) 0))
          (return (setf *clock* now)))))))
(defvar *last-frame* 0)
(defun wait-frame (&optional retrace)
  "Wait until a frame's time has passed since the last one -- and then, if
RETRACE, for the vertical retrace."
  (loop while (< -1 (- (clock) *last-frame*) +frame-clocks+))
  (when retrace (wait-retrace))
  (setf *last-frame* (clock)))

;;; ---- the sprite
(defun ball ()
  "The built-in sprite: a red ball with a highlight, colour 0 around it."
  (let ((sprite (make-bytes (* +size+ +size+))))
    (dotimes (row +size+ sprite)
      (dotimes (column +size+)
        (let ((d (+ (expt (- column 7.5) 2) (expt (- row 7.5) 2))))
          (setf (aref sprite (+ column (* +size+ row)))
                (cond ((> d 56) 0) ((> d 42) 4)
                      ((< (+ (expt (- column 5) 2) (expt (- row 5) 2)) 5) 15)
                      (t 12))))))))
(defun load-sprite (&optional (file "SPRITE.DAT"))
  "The sprite saved by the sprite editor, or the ball if there is none (or
it is empty)."
  (let ((sprite (make-bytes (* +size+ +size+))))
    (if (and (ignore-errors
              (with-open-file (in file :element-type '(unsigned-byte 8) :if-does-not-exist nil)
                (and in (= (read-sequence sprite in) (length sprite)))))
             (find 0 sprite :test #'/=))
        sprite
        (ball))))

;;; ---- the background: bevelled tiles, lighter towards the bottom
(defun background-palette ()
  (dotimes (i 64)
    (set-palette (+ +shades+ i) (floor i 4) (floor i 2) (+ 14 (floor (* i 3) 4)))))
(defun make-background ()
  (let ((background (make-bytes +screen-bytes+)))
    (dotimes (y +height+ background)
      (dotimes (x +width+)
        (let ((tx (mod x 32)) (ty (mod y 32)))
          (setf (aref background (+ x (* +width+ y)))
                (+ +shades+
                   (floor y 7)                                        ; 0 to 28
                   (if (evenp (+ (floor x 32) (floor y 32))) 8 0)     ; a chequer board
                   (cond ((or (= tx 0) (= ty 0)) 14)                  ; lit edges
                         ((or (= tx 31) (= ty 31)) -10 )              ; and shaded ones
                         (t 0))
                   10)))))))

;;; ---- drawing, into video memory or the back buffer alike: DEST is the
;;; address of 64,000 bytes
(defun blit (dest sprite x y)
  "Draw SPRITE with its corner at X, Y, leaving out colour 0."
  (declare (type system-area-pointer dest) (type bytes sprite) (type (unsigned-byte 16) x y)
           (optimize speed (safety 0)))
  (let ((at (+ x (* +width+ y))) (i 0))
    (declare (fixnum at i))
    (dotimes (row +size+)
      (dotimes (column +size+)
        (let ((colour (aref sprite i)))
          (unless (zerop colour) (setf (sap-ref-8 dest (+ at column)) colour)))
        (incf i))
      (incf at +width+))))

(defun draw-text (dest x y string colour)
  "STRING in the BIOS's 8 by 8 characters (they are in the ROM, at FFA6Eh),
on a black bar."
  (declare (type system-area-pointer dest) (type (unsigned-byte 16) x y) (fixnum colour)
           (simple-string string)
           (optimize speed (safety 0)))
  (let ((font (int-sap #xFFA6E)))
    (dotimes (i (min (length string) 40))
      (let ((code (logand (char-code (char string i)) 127)))
        (dotimes (row 8)
          (let ((bits (sap-ref-8 font (+ (* 8 code) row)))
                (at (+ x (* 8 i) (* +width+ (+ y row)))))
            (declare (fixnum at))
            (dotimes (column 8)
              (setf (sap-ref-8 dest (+ at column))
                    (if (logbitp (- 7 column) bits) colour 0)))))))))

;;; ---- the sprites: where each is, and which way it is going
(defstruct (flock (:constructor %make-flock))
  (x (make-array +sprites+ :element-type 'fixnum) :type (simple-array fixnum (*)))
  (y (make-array +sprites+ :element-type 'fixnum) :type (simple-array fixnum (*)))
  (dx (make-array +sprites+ :element-type 'fixnum) :type (simple-array fixnum (*)))
  (dy (make-array +sprites+ :element-type 'fixnum) :type (simple-array fixnum (*))))
(defun make-flock ()
  (let ((flock (%make-flock)))
    (dotimes (i +sprites+ flock)
      (setf (aref (flock-x flock) i) (random (- +width+ +size+))
            (aref (flock-y flock) i) (+ 10 (random (- +height+ +size+ 10)))
            (aref (flock-dx flock) i) (* (1+ (random 3)) (if (zerop (random 2)) 1 -1))
            (aref (flock-dy flock) i) (* (1+ (random 2)) (if (zerop (random 2)) 1 -1))))))
(defun move-flock (flock)
  "Everything a step on, turning back at the edges (and below the top line)."
  (let ((xs (flock-x flock)) (ys (flock-y flock)) (dxs (flock-dx flock)) (dys (flock-dy flock)))
    (dotimes (i +sprites+)
      (let ((x (+ (aref xs i) (aref dxs i))) (y (+ (aref ys i) (aref dys i))))
        (unless (<= 0 x (- +width+ +size+))
          (setf (aref dxs i) (- (aref dxs i)) x (+ (aref xs i) (aref dxs i))))
        (unless (<= 10 y (- +height+ +size+))
          (setf (aref dys i) (- (aref dys i)) y (+ (aref ys i) (aref dys i))))
        (setf (aref xs i) x (aref ys i) y)))))

(defun draw-frame (dest background sprite flock status)
  "The whole picture, from nothing, at DEST."
  (declare (type system-area-pointer dest) (type bytes background))
  (sb-kernel:system-area-ub8-copy (vector-sap background) 0 dest 0 +screen-bytes+)
  (dotimes (i +sprites+)
    (blit dest sprite (aref (flock-x flock) i) (aref (flock-y flock) i)))
  (draw-text dest 0 0 status 15))

(defun status-line (buffered frames-a-second)
  (coerce (format nil "~40A" (format nil "~A ~3D frames/s  Space Esc"
                                     (if buffered "Back buffer    " "Straight to VGA")
                                     frames-a-second))
          'simple-string))

(defun blit-demo ()
  (let ((background (make-background)) (buffer (make-bytes +screen-bytes+))
        (sprite (load-sprite)) (flock (make-flock))
        (buffered t) (frames 0) (frames-a-second 0) (since (clock)) (vga *vga*))
    (with-video-mode (#x13)
      (background-palette)
      (let ((status (status-line buffered frames-a-second)))
        ;; the vectors must stay where they are while their addresses are in use
        (with-pinned-objects (background buffer)
          (loop
            (when (key-pressed-p)
              (case (get-key)
                (#\Escape (return))
                (#\Space (setf buffered (not buffered)
                               status (status-line buffered frames-a-second)))))
            (move-flock flock)
            (cond (buffered         ; make the picture out of sight, then show it all at once
                   (draw-frame (vector-sap buffer) background sprite flock status)
                   (wait-frame t)
                   (sb-kernel:system-area-ub8-copy (vector-sap buffer) 0 vga 0 +screen-bytes+))
                  (t                ; make it where the beam is looking
                   (wait-frame)
                   (draw-frame vga background sprite flock status)))
            (incf frames)
            (let ((elapsed (- (clock) since)))
              (when (or (>= elapsed +clocks-a-second+) (< elapsed 0))   ; a second (or midnight)
                (when (> elapsed 0)
                  (setf frames-a-second (round (* frames +clocks-a-second+) elapsed)
                        status (status-line buffered frames-a-second)))
                (setf frames 0 since (clock))))))))
    (format t "~&blit: ~D sprites, ~D frames a second ~A.~%" +sprites+ frames-a-second
            (if buffered "through the back buffer" "straight to video memory"))))

(defun main ()
  "Forty sprites over a background, drawn in a back buffer and copied to the screen."
  (blit-demo)
  (values))

(doslib-examples:define-example "blit" 'main
  :category :graphics
  :description "Sprites with transparency, drawn in a back buffer and copied at the retrace.")
