;;;; The VGA's split screen, in Mode X: a playfield twice as wide as the
;;;; screen scrolls about, and under it is a status bar that stays where it
;;;; is -- and can be pulled up over the playfield and let down again.
;;;;
;;;; The VGA shows video memory from the start address (CRT controller
;;;; registers 0Ch and 0Dh) onward, and scrolling is changing that address:
;;;; see the example "scroll".  On its way down the screen it compares the
;;;; number of the scan line it is on with the line compare register, and
;;;; after the line where they are equal it starts again from address 0 of
;;;; video memory, whatever the start address is.  So the screen is in two
;;;; parts: the upper shows memory from the start address, and scrolls; the
;;;; lower always shows the beginning of memory, and does not.
;;;;
;;;;   - Line compare has ten bits, in three registers of the CRT controller
;;;;     (port 3D4h): bits 0 to 7 are register 18h, bit 8 is bit 4 of the
;;;;     overflow register 07h, and bit 9 is bit 6 of the maximum scan line
;;;;     register 09h.  (Registers 0 to 7 can be protected from writing by
;;;;     bit 7 of register 11h, and Mode X leaves them so: but that one bit
;;;;     of 07h can always be written.)
;;;;   - It counts scan lines, not rows of pixels.  Mode X has 480 scan
;;;;     lines, each row of pixels being shown twice: for the bar to begin at
;;;;     pixel row R the register is 2R - 1, and 1023, a line that never
;;;;     comes, for no bar at all.
;;;;   - The horizontal pixel panning (attribute controller register 13h),
;;;;     which moves the picture by the pixels between two addresses, would
;;;;     move the bar as well.  Bit 5 of the attribute controller's mode
;;;;     register (10h) says that below the split there is to be no panning.
;;;;
;;;; Here video memory is laid out in rows of 160 addresses (640 pixels: the
;;;; CRT controller's offset register, 13h).  The first 80 rows are the bar;
;;;; the other 329 are the playfield, of which the screen shows a window 320
;;;; pixels wide.  Each frame the program writes the start address, the
;;;; panning, and, when the split is moving, line compare: a few bytes to
;;;; ports, and nothing to the playfield in video memory.  As the bar comes
;;;; up, more of its 80 rows are shown; it is not moved, only uncovered.
;;;;
;;;;   (doslib-examples:run "split")
;;;; The playfield wanders by itself.  Up and Down arrows move the split, 8
;;;; rows at a time, between row 160 and the bottom of the screen (no bar);
;;;; P turns the pixel panning off and on; B turns bit 5 of the mode
;;;; register off and on (off, and with panning, the bar shakes sideways as
;;;; the playfield goes by: what the bit is for); Esc leaves.
;;;;
;;;; Under QEMU (version 11.1 was tried, and looked at in screenshots): the
;;;; split is drawn at the right scan line, the bar stays put while the
;;;; playfield scrolls behind it, and it is uncovered and covered as line
;;;; compare changes.  But QEMU moves a 256-colour picture four pixels for
;;;; each one asked of the panning register, so there the panning starts
;;;; turned off, as in "scroll", and the playfield moves sideways four pixels
;;;; at a time.  Turned on with P, the playfield jumps about; the bar keeps
;;;; still while bit 5 is set, and with B pressed it was seen to jump
;;;; sideways with the playfield: QEMU does honour the bit.  None of this
;;;; has been tried on a real VGA.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.split
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.split)

(defconstant +row-bytes+ 160)           ; addresses from one row to the next: 640 pixels
(defconstant +bar-rows+ 80)             ; the status bar: the first rows of video memory
(defconstant +base+ (* +bar-rows+ +row-bytes+))  ; the address where the playfield begins
(defconstant +field-rows+ (floor (- 65536 +base+) +row-bytes+))   ; 329: what is left of 64K
(defconstant +field-width+ (* 4 +row-bytes+))

;;; ---- the registers
(defun set-split (row)
  "Begin the status bar at pixel row ROW of the screen; 240 or more is no bar."
  (let ((line (if (< row 240) (1- (* 2 row)) 1023)))     ; the scan line to compare with
    (vga-crtc #x18 (ldb (byte 8 0) line))
    (vga-crtc #x07 (dpb (ldb (byte 1 8) line) (byte 1 4) (vga-crtc #x07)))
    (vga-crtc #x09 (dpb (ldb (byte 1 9) line) (byte 1 6) (vga-crtc #x09)))
    line))
(defun split-mode ()
  (mode-x)                              ; the library's: 320 by 240 in four planes
  (vga-crtc #x13 (floor +row-bytes+ 2)) ; the offset register counts in pairs of addresses
  (set-split 240)
  (vga-attribute #x10 #x61))            ; 256 colours, and no panning below the split
(defun qemu-p ()
  "Is this QEMU?  Its configuration device says so: item 0, read at port 511h."
  (outw #x510 0)
  (equal (loop repeat 4 collect (inb #x511)) '(81 69 77 85)))    ; "QEMU"

;;; ---- the picture: squares of 32 pixels, each with its column and row written on it
(defvar *written* 0 "Bytes written to video memory.")
(declaim (inline field-pixel))
(defun field-pixel (x y)
  (declare (type fixnum x y))
  (let ((column (ash x -5)) (row (ash y -5)))
    (cond ((or (zerop (logand x 31)) (zerop (logand y 31))) 0)             ; the lines between
          ((evenp (+ column row)) (+ 32 (mod (+ (* 3 column) (* 5 row)) 32)))  ; a rainbow
          (t (+ 18 row)))))                                                ; greys
(defun draw-field ()
  (declare (optimize speed))
  (let ((vga *vga*))
    (declare (type system-area-pointer vga))
    (dotimes (plane 4)
      (vga-planes (ash 1 plane))        ; the pixels X with X mod 4 = PLANE
      (dotimes (y +field-rows+)
        (dotimes (i +row-bytes+)
          (setf (sap-ref-8 vga (+ +base+ (* +row-bytes+ y) i))
                (field-pixel (+ (* 4 i) plane) y))))))
  (dotimes (row (ceiling +field-rows+ 32))
    (dotimes (column (floor +field-width+ 32))
      (when (<= (+ (* 32 row) 20) +field-rows+)
        (mode-x-text (+ 4 (* 32 column)) (+ 12 (* 32 row)) (format nil "~D" column)
                     (if (evenp (+ column row)) 0 15) nil +base+ +row-bytes+)
        (mode-x-text (+ 20 (* 32 column)) (+ 12 (* 32 row)) (string (code-char (+ 65 row)))
                     (if (evenp (+ column row)) 0 14) nil +base+ +row-bytes+))))
  (incf *written* (* +field-width+ +field-rows+)))
(defun bar-text (line text &optional (colour 15))
  (mode-x-text 4 (+ 6 (* 12 line)) (format nil "~39A" text) colour 1 0 +row-bytes+)
  (incf *written* (* 39 64)))
(defun draw-bar ()
  (vga-planes 15)                       ; four pixels at a write
  (dotimes (i (* +bar-rows+ +row-bytes+))
    (setf (sap-ref-8 *vga* i) (if (< i (* 2 +row-bytes+)) 15 1)))  ; blue, under a white edge
  (incf *written* (* 4 +bar-rows+ +row-bytes+))
  (bar-text 1 "Up/Down move the split   Esc leaves" 14)
  (bar-text 2 "This bar is the start of video memory:" 7)
  (bar-text 3 "after the line compared with, the VGA" 7)
  (bar-text 4 "begins again from address 0.  P: pixel" 7)
  (bar-text 5 "panning.  B: bit 5 of attribute 10h." 7))

(defun split ()
  (let ((frames 0) (start (ticks)) (elapsed 0) (drawn 0) (updates 0))
    (setf *written* 0)
    (split-mode)
    (unwind-protect
         (let ((x 0) (y 0) (dx 1) (dy 1)
               (row 240) (target 200)   ; where the bar begins, and where it is going
               (panning (not (qemu-p))) (bit-5 t) (stale t))
           (rainbow-palette 32 63)
           (draw-field)
           (draw-bar)
           (setf drawn *written* start (ticks))
           (loop
             (when (key-pressed-p)
               (case (get-key)
                 (#\Escape (return))
                 (:up (setf target (max 160 (- target 8))))
                 (:down (setf target (min 240 (+ target 8))))
                 ((#\p #\P) (setf panning (not panning) stale t))
                 ((#\b #\B) (setf bit-5 (not bit-5) stale t)
                  (vga-attribute #x10 (if bit-5 #x61 #x41)))))
             ;; The playfield goes from side to side and up and down; there is more of
             ;; it to see below when the bar is up.
             (let ((limit-x (- +field-width+ 320)) (limit-y (- +field-rows+ row)))
               (incf x dx) (incf y dy)
               (cond ((<= x 0) (setf x 0 dx 1)) ((>= x limit-x) (setf x limit-x dx -1)))
               (cond ((<= y 0) (setf y 0 dy 1)) ((>= y limit-y) (setf y limit-y dy -1))))
             (wait-frame)
             (vga-display-start (+ +base+ (* +row-bytes+ y) (ash x -2)))  ; taken at the retrace
             (wait-retrace)
             (vga-attribute #x13 (if panning (* 2 (logand x 3)) 0))       ; used at once
             (unless (= row target)     ; the bar slides: two rows a frame
               (setf row (if (< row target) (min target (+ row 2)) (max target (- row 2)))
                     stale t)
               (set-split row))
             (incf frames)
             (when (and stale (or (= row target) (zerop (mod frames 8))))
               (bar-text 0 (format nil "Split: row ~3D  line compare ~4D ~:[  ~;P ~]~:[b~;B~]"
                                   row (if (< row 240) (1- (* 2 row)) 1023) panning bit-5))
               (incf updates)
               (setf stale nil)))
           (setf elapsed (- (ticks) start)))
      (video-mode 3))
    (when (plusp frames)
      (format t "~&~D frames~@[, ~D a second~].  The playfield moved in every one, and ~
                 none of~%its ~D bytes was written again: each frame wrote three ~
                 registers, the start~%address (two) and the pixel panning.~%"
              frames (and (plusp elapsed) (round (* frames 18.2) elapsed))
              (* +field-width+ +field-rows+))
      (format t "The numbers in the status bar were rewritten ~D times: ~D bytes in all.~%"
              updates (- *written* drawn)))))

(defun main ()
  "The VGA split screen: a status bar that stays put under a scrolling playfield."
  (split)
  (values))

(doslib-examples:define-example "split" 'main
  :category :graphics
  :description "The VGA split screen: a status bar that stays put under a scrolling playfield.")
