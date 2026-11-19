;;;; Dirty rectangles: sprites over a background in mode 13h, redrawing
;;;; and copying to the screen only the parts of the picture that changed.
;;;;
;;;;   (doslib-examples:run "dirty")
;;;; Space: dirty rectangles, or the whole screen every frame.
;;;; + and -: more sprites, or fewer (10 to 200).  Esc leaves.
;;;;
;;;; A back buffer (see the example "blit") cures flicker, but copying all
;;;; 64,000 bytes of it to video memory every frame is the slowest thing a
;;;; program of the time did: video memory is on the far side of the bus,
;;;; and a VGA card on an ISA bus takes about a microsecond a byte.  Most of
;;;; the picture is the same from one frame to the next, though.  Here three
;;;; pictures are kept: the background alone, which never changes; the back
;;;; buffer, which is the background with the sprites on it; and the screen.
;;;; For each frame,
;;;;   - the 16 by 16 square where each sprite was is put back in the back
;;;;     buffer from the background;
;;;;   - the sprites are moved, and drawn in the back buffer where they now
;;;;     are;
;;;;   - and, at the retrace, for each sprite the one rectangle that covers
;;;;     both where it was and where it is goes from the back buffer to the
;;;;     screen.
;;;; Nothing else is touched.
;;;;
;;;; The second line counts the bytes copied to video memory in a frame
;;;; (averaged over a second), against the 64,000 of the whole screen.  With
;;;; 40 sprites it is about a fifth.  Add sprites and it grows: rectangles
;;;; that overlap are copied twice, and somewhere near 200 sprites copying
;;;; the whole screen is the cheaper way again.  (An emulator's video memory
;;;; is as quick as any other, so the frames a second will not show the
;;;; difference there; the bytes do.  QEMU's retrace bit flips every time it
;;;; is read, so the frames are also paced by the timer chip.)
;;;;
;;;; If the sprite editor (the example "sprite") has saved a sprite in
;;;; SPRITE.DAT in the current directory, that is the sprite drawn;
;;;; otherwise it is a ball.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.dirty
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.dirty)

(defconstant +width+ 320) (defconstant +height+ 200)
(defconstant +screen-bytes+ (* +width+ +height+))
(defconstant +size+ 16)                 ; a sprite is this many pixels square
(defconstant +most+ 200)                ; sprites, at most
(defconstant +top+ 16) (defconstant +bottom+ 192)    ; the sprites stay between these lines
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

;;; ---- drawing: FROM, TO and DEST are addresses of 64,000 bytes
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


(defun copy-rectangle (from to x y width height)
  "Copy the rectangle at X, Y from one picture to the same place in another:
four bytes at a time, then the odd ones."
  (declare (type system-area-pointer from to) (type (unsigned-byte 16) x y width height)
           (optimize speed (safety 0)))
  (let ((at (+ x (* +width+ y))) (words (logand width -4)))
    (declare (fixnum at words))
    (dotimes (row height)
      (do ((i 0 (+ i 4)))
          ((>= i words))
        (declare (fixnum i))
        (setf (sap-ref-32 to (+ at i)) (sap-ref-32 from (+ at i))))
      (do ((i words (1+ i)))
          ((>= i width))
        (declare (fixnum i))
        (setf (sap-ref-8 to (+ at i)) (sap-ref-8 from (+ at i))))
      (incf at +width+))))

;;; ---- the sprites: where each is, where it was in the frame now on the
;;; screen, and which way it is going
(deftype numbers () '(simple-array fixnum (*)))
(defun make-numbers () (make-array +most+ :element-type 'fixnum :initial-element 0))
(defstruct flock
  (x (make-numbers) :type numbers) (y (make-numbers) :type numbers)
  (old-x (make-numbers) :type numbers) (old-y (make-numbers) :type numbers)
  (dx (make-numbers) :type numbers) (dy (make-numbers) :type numbers))
(defun scatter (flock)
  (dotimes (i +most+ flock)
    (setf (aref (flock-x flock) i) (random (- +width+ +size+))
          (aref (flock-y flock) i) (+ +top+ (random (- +bottom+ +top+ +size+)))
          (aref (flock-dx flock) i) (* (1+ (random 3)) (if (zerop (random 2)) 1 -1))
          (aref (flock-dy flock) i) (* (1+ (random 2)) (if (zerop (random 2)) 1 -1)))))
(defun move-flock (flock count)
  "The first COUNT sprites a step on, turning back at the edges; where they
were is remembered."
  (let ((xs (flock-x flock)) (ys (flock-y flock)) (dxs (flock-dx flock)) (dys (flock-dy flock)))
    (dotimes (i count)
      (let ((x (+ (aref xs i) (aref dxs i))) (y (+ (aref ys i) (aref dys i))))
        (unless (<= 0 x (- +width+ +size+))
          (setf (aref dxs i) (- (aref dxs i)) x (+ (aref xs i) (aref dxs i))))
        (unless (<= +top+ y (- +bottom+ +size+))
          (setf (aref dys i) (- (aref dys i)) y (+ (aref ys i) (aref dys i))))
        (setf (aref (flock-old-x flock) i) (aref xs i) (aref (flock-old-y flock) i) (aref ys i)
              (aref xs i) x (aref ys i) y)))))

;;; ---- a frame.  BACKGROUND, BUFFER and SCREEN are addresses.
(defun compose (background buffer sprite flock count)
  "Bring the back buffer up to date: rub the sprites out where they were,
move them, and draw them where they are."
  (declare (type system-area-pointer background buffer))
  (dotimes (i count)
    (copy-rectangle background buffer (aref (flock-x flock) i) (aref (flock-y flock) i)
                    +size+ +size+))
  (move-flock flock count)
  (dotimes (i count)
    (blit buffer sprite (aref (flock-x flock) i) (aref (flock-y flock) i))))

(defun show-dirty (buffer screen flock count)
  "Copy to the screen what COMPOSE changed.  Returns how many bytes that was."
  (declare (type system-area-pointer buffer screen) (fixnum count))
  (let ((xs (flock-x flock)) (ys (flock-y flock))
        (old-xs (flock-old-x flock)) (old-ys (flock-old-y flock)) (bytes 0))
    (declare (fixnum bytes))
    (dotimes (i count bytes)
      (let* ((x (min (aref xs i) (aref old-xs i))) (y (min (aref ys i) (aref old-ys i)))
             (width (+ +size+ (abs (- (aref xs i) (aref old-xs i)))))
             (height (+ +size+ (abs (- (aref ys i) (aref old-ys i))))))
        (declare (fixnum x y width height))
        (copy-rectangle buffer screen x y width height)
        (incf bytes (the fixnum (* width height)))))))

(defun text (control &rest arguments)
  "A line of text, exactly as long as the screen is wide: 40 characters."
  (coerce (subseq (format nil "~40A" (apply #'format nil control arguments)) 0 40)
          'simple-string))

(defun repaint (background buffer sprite flock count)
  "The back buffer made again from nothing, with the keys on its last line."
  (declare (type system-area-pointer background buffer))
  (sb-kernel:system-area-ub8-copy background 0 buffer 0 +screen-bytes+)
  (dotimes (i count)
    (blit buffer sprite (aref (flock-x flock) i) (aref (flock-y flock) i)))
  (draw-text buffer 0 +bottom+ (text "Space: method  +/-: sprites  Esc: leave") 14))

(defun dirty-demo ()
  (let ((background (make-background)) (buffer (make-bytes +screen-bytes+))
        (sprite (load-sprite)) (flock (scatter (make-flock))) (count 40)
        (dirty t) (repaint t) (new-text t)
        (frames 0) (bytes 0) (since (clock)) (frames-a-second 0) (bytes-a-frame 0))
    (with-video-mode (#x13)
      (background-palette)
      ;; the vectors must stay where they are while their addresses are in use
      (with-pinned-objects (background buffer)
        (let ((background (vector-sap background)) (buffer (vector-sap buffer)) (screen *vga*))
          (loop
            (when (key-pressed-p)
              (case (get-key)
                (#\Escape (return))
                (#\Space (setf dirty (not dirty) repaint t))
                ((#\+ #\=) (setf count (min +most+ (+ count 10)) repaint t))
                ((#\- #\_) (setf count (max 10 (- count 10)) repaint t))))
            (cond (repaint
                   (repaint background buffer sprite flock count)
                   (setf new-text t frames 0 bytes 0 since (clock)
                         frames-a-second 0 bytes-a-frame 0))
                  (t (compose background buffer sprite flock count)))
            (when new-text              ; the two lines at the top change about once a second
              (draw-text buffer 0 0 (text "~A  ~3D sprites  ~3D fps"
                                          (if dirty "Dirty rectangles" "The whole screen")
                                          count frames-a-second)
                         15)
              (draw-text buffer 0 8 (text "~6:D of 64,000 bytes a frame (~D%)" bytes-a-frame
                                          (round (* 100 bytes-a-frame) +screen-bytes+))
                         (if (< bytes-a-frame +screen-bytes+) 10 12)))
            (wait-frame t)
            (cond ((or repaint (not dirty))
                   (sb-kernel:system-area-ub8-copy buffer 0 screen 0 +screen-bytes+)
                   (incf bytes +screen-bytes+))
                  (t (incf bytes (show-dirty buffer screen flock count))
                     (when new-text     ; they are a dirty rectangle too
                       (copy-rectangle buffer screen 0 0 +width+ 16)
                       (incf bytes (* +width+ 16)))))
            (setf repaint nil new-text nil)
            (incf frames)
            (let ((elapsed (- (clock) since)))
              (when (or (>= elapsed +clocks-a-second+) (< elapsed 0))   ; a second (or midnight)
                (when (> elapsed 0)
                  (setf frames-a-second (round (* frames +clocks-a-second+) elapsed)
                        bytes-a-frame (round bytes frames)
                        new-text t))
                (setf frames 0 bytes 0 since (clock))))))))
    (format t "~&dirty: ~D sprites, ~:D bytes a frame to video memory with ~A, ~
               ~D frames a second.~%"
            count bytes-a-frame (if dirty "dirty rectangles" "whole-screen copies")
            frames-a-second)))

(defun main ()
  "Sprites over a background, redrawn with dirty rectangles."
  (dirty-demo)
  (values))

(doslib-examples:define-example "dirty" 'main
  :category :graphics
  :description "Dirty rectangles: only what changed is redrawn and copied to the screen.")
