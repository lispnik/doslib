;;;; Mode 12h: the VGA's 640 by 480 in 16 colours, and the graphics controller
;;;; that stands between the processor and its four bit planes.
;;;;
;;;; In this mode a byte of video memory is eight pixels side by side, the
;;;; leftmost in bit 7, and there are four such bytes at every address, one
;;;; in each plane: plane 0 holds bit 0 of each pixel's colour, plane 3 bit 3.
;;;; Pixel X, Y is bit 7 - X mod 8 of address 80 Y + X / 8.  To set one pixel
;;;; a program would have to read four bytes and write four, a plane at a
;;;; time, were it not for the graphics controller (ports 3CEh and 3CFh, the
;;;; library's VGA-GRAPHICS), which does the work for all four planes at once:
;;;;
;;;;   - Latches.  When the processor reads an address, all four planes are
;;;;     read into four latches (the processor sees one of them: register 4
;;;;     says which).  Every write is put together from the latches and what
;;;;     the processor wrote.
;;;;   - The bit mask (register 8).  Where it has a 1 the pixel is written;
;;;;     where it has a 0 the pixel comes from the latches, that is, stays as
;;;;     it was when the address was last read.  So: mask, read, write.
;;;;   - Set/reset (register 0, enabled plane by plane by register 1).  The
;;;;     planes enabled do not get the processor's byte at all but all ones or
;;;;     all zeros, as the colour in register 0 says: any byte written sets
;;;;     eight pixels to that colour, in four planes, in one go.
;;;;   - The write mode (register 5, bits 0-1).  Mode 0 is the above.  In
;;;;     mode 1 the latches are written back as they are: a read and a write
;;;;     copy eight pixels.  In mode 2 the byte written is itself a colour,
;;;;     for the pixels the bit mask names.  In mode 3 the byte written is
;;;;     itself a mask, for the set/reset colour: one write draws a row of a
;;;;     character.
;;;;   - The logical operation (register 3, bits 3-4).  What is written can
;;;;     be ANDed, ORed or XORed with the latches.  XOR twice is nothing: a
;;;;     box drawn so is taken away by drawing it again.
;;;;
;;;; Five stages show them, and each says what it cost:
;;;;
;;;;   1. the screen filled a pixel at a time (bit mask, read, write), and
;;;;      a fan of lines;
;;;;   2. the screen filled again with set/reset, eight pixels a write, and
;;;;      rectangles whose edges are not at multiples of eight;
;;;;   3. write mode 2: sixteen bars and a pattern, with no colour register;
;;;;   4. the pattern copied with the latches (write mode 1), and, for
;;;;      comparison, a plane at a time;
;;;;   5. a box drawn in XOR moves about over the picture.  When a key stops
;;;;      it, video memory is read back and added up: the sum is what it was
;;;;      before the box.
;;;; The text is written in write mode 3, each row of a character twice, to
;;;; make letters of 8 by 16.
;;;;
;;;;   (doslib-examples:run "mode12")
;;;; Space goes on to the next stage; Esc leaves, and the numbers are printed.
;;;;
;;;; The times are of an emulator, if that is where this runs: there every
;;;; access to planar memory costs about the same, where on a real VGA of
;;;; 1990 the card was slow and the processor slower.  The counts of bytes
;;;; are the same everywhere.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.mode12
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.mode12)

(defconstant +screen-bytes+ (* 80 480)) ; addresses in a screen: 38,400, in each plane

;;; ---- the graphics controller
(defun controller (&key (mode 0) colour (mask #xFF) (operation 0))
  "Set the graphics controller: the write MODE; set/reset to COLOUR in all four planes, or
off if COLOUR is NIL; the bit MASK; and the logical OPERATION (0 replace, 1 AND, 2 OR,
3 XOR).  With no arguments, as the BIOS leaves it."
  (vga-graphics 5 mode)
  (vga-graphics 0 (or colour 0))
  (vga-graphics 1 (if colour #x0F 0))
  (vga-graphics 8 mask)
  (vga-graphics 3 (ash operation 3))
  (values))

(defvar *reads* 0) (defvar *writes* 0)  ; of video memory, counted by those who do them
(declaim (fixnum *reads* *writes*))
(declaim (notinline load-latches))
(defun load-latches (address)
  "Read ADDRESS of video memory: all four planes go into the latches.  The byte itself is
seldom wanted, and this is a function of its own so that the compiler cannot decide that a
read whose value nobody uses need not be done."
  (declare (fixnum address))
  (incf *reads*)
  (sap-ref-8 *vga* address))
(defmacro counting (&body body)
  "Run BODY; return the reads and writes of video memory it did, and the time in
milliseconds."
  `(let ((began (timer-count)))
     (setf *reads* 0 *writes* 0)
     ,@body
     (values *reads* *writes* (round (* 1000 (- (timer-count) began)) +timer-hz+))))

;;; ---- pixels and lines: bit mask, read, write
(defun pixel (x y)
  "Set the pixel at X, Y to the set/reset colour (CONTROLLER's :COLOUR): the bit mask names
it, a read fills the latches with its seven neighbours, and a write -- of any byte -- puts
those back with it."
  (declare (optimize speed) (type (integer 0 639) x) (type (integer 0 479) y))
  (let ((address (+ (* 80 y) (ash x -3))))
    (vga-graphics 8 (ash #x80 (- (logand x 7))))
    (load-latches address)
    (setf (sap-ref-8 *vga* address) #xFF)
    (incf *writes*)))
(defun line (x0 y0 x1 y1)
  "A line in the set/reset colour (Bresenham's algorithm)."
  (let* ((dx (abs (- x1 x0))) (dy (- (abs (- y1 y0))))
         (sx (if (< x0 x1) 1 -1)) (sy (if (< y0 y1) 1 -1)) (err (+ dx dy)))
    (loop (pixel x0 y0)
          (when (and (= x0 x1) (= y0 y1)) (return))
          (let ((e2 (* 2 err)))
            (when (>= e2 dy) (incf err dy) (incf x0 sx))
            (when (<= e2 dx) (incf err dx) (incf y0 sy))))))

;;; ---- rectangles: eight pixels a write
(defun rectangle (x0 y0 x1 y1 &key (byte #xFF) latched)
  "Write BYTE over the rectangle from X0, Y0 to X1, Y1 inclusive: what that does is up to
the graphics controller, which must be set first.  In write mode 0 with set/reset it fills
with the set/reset colour; in write mode 2 BYTE is the colour.  The bytes that lie wholly
inside are simply written.  The two at the ends of each row are not: the bit mask names the
pixels inside, and a read comes first, so that the ones outside stay.  If LATCHED, every
byte is read before it is written, as a logical operation needs."
  (declare (optimize speed) (fixnum x0 y0 x1 y1 byte))
  (let* ((vga *vga*)
         (first (ash x0 -3)) (last (ash x1 -3))
         (left (ash #xFF (- (logand x0 7))))    ; the pixels from x0 to the end of its byte
         (right (logand #xFF (ash #xFF (- 7 (logand x1 7))))))  ; and those up to x1
    (declare (type system-area-pointer vga) (fixnum first last left right))
    (flet ((column (at mask)            ; one byte of every row, with MASK
             (declare (fixnum at mask))
             (vga-graphics 8 mask)
             (loop for address fixnum from (+ at (* 80 y0)) by 80
                   repeat (- y1 y0 -1)
                   do (when (or latched (/= mask #xFF)) (load-latches address))
                      (setf (sap-ref-8 vga address) byte)
                      (incf *writes*))))
      (if (= first last)
          (column first (logand left right))
          (progn
            (column first left)
            (column last right)
            (vga-graphics 8 #xFF)
            (loop for y fixnum from y0 to y1
                  do (loop for address fixnum from (+ (* 80 y) first 1) below (+ (* 80 y) last)
                           do (when latched (load-latches address))
                              (setf (sap-ref-8 vga address) byte)
                              (incf *writes*))))))))

;;; ---- text: write mode 3
(defun text (column row string colour &optional (background 0))
  "Write STRING at character COLUMN (of 80), ROW (of 30), in COLOUR on BACKGROUND.  The
squares are filled first.  Then, in write mode 3, the byte written is ANDed with the bit
mask and says which pixels get the set/reset colour: so a row of a character from the
BIOS's font (the library's GLYPH-ROW) is written as it stands, after a read to keep the
other pixels.  Each row is written twice, for letters 16 high."
  (let ((string (subseq string 0 (max 0 (min (length string) (- 80 column))))))
    (controller :colour background)
    (rectangle (* 8 column) (* 16 row) (1- (* 8 (+ column (length string)))) (+ (* 16 row) 15))
    (controller :mode 3 :colour colour)
    (loop for character across string
          for address from (+ (* 80 16 row) column)
          do (dotimes (line 16)
               (let ((bits (glyph-row character (ash line -1))) (at (+ address (* 80 line))))
                 (unless (zerop bits)
                   (load-latches at)
                   (setf (sap-ref-8 *vga* at) bits)))))
    (controller)))

;;; ---- the five stages.  Each draws, and returns a line to say what it cost.
(defun pixel-fill (colour)
  "The whole screen in COLOUR, a pixel at a time."
  (declare (optimize speed))
  (controller :colour colour)
  (dotimes (y 480)
    (dotimes (x 640) (pixel x y)))
  (controller))
(defun fan ()
  "Lines from a point to all round it, in every colour but the background's."
  (loop for i from 0 below 60
        for angle = (* i (/ pi 30))
        do (controller :colour (let ((colour (mod i 16))) (if (= colour 1) 0 colour)))
           (line 160 140 (+ 160 (round (* 150 (cos angle)))) (+ 140 (round (* 90 (sin angle))))))
  (controller))

(defvar *pixel-time* 1 "How long stage 1 took to fill the screen, in milliseconds.")
(defun stage-pixels ()
  (multiple-value-bind (reads writes time) (counting (pixel-fill 1))
    (setf *pixel-time* time)
    (fan)
    (format nil "1 Pixels:    ~:D reads and ~:D writes filled the screen in ~:D ms"
            reads writes time)))

(defun boxes ()
  "Rectangles one upon another, with no edge at a multiple of eight."
  (loop for i from 0 below 9
        do (controller :colour (+ 2 i))
           (rectangle (+ 331 (* 13 i)) (+ 50 (* 9 i)) (- 627 (* 19 i)) (- 229 (* 9 i))))
  (controller))
(defun stage-set-reset ()
  (multiple-value-bind (reads writes time)
      (counting (controller :colour 1) (rectangle 0 0 639 479) (controller))
    (declare (ignore reads))
    (fan)
    (boxes)
    (format nil "2 Set/reset: ~:D writes filled it in ~:D ms, 8 pixels each: ~:D times as fast"
            writes time (round *pixel-time* (max 1 time)))))

(defun bars ()
  "Sixteen bars 19 pixels wide, a colour each, and under them a pattern made a pixel at a
time, with a white line round them: all in write mode 2, where the colour is the byte
written."
  (declare (optimize speed))
  (controller :mode 2)
  (rectangle 6 246 313 246 :byte 15) (rectangle 6 393 313 393 :byte 15)
  (rectangle 6 247 6 392 :byte 15) (rectangle 313 247 313 392 :byte 15)
  (dotimes (colour 16)
    (rectangle (+ 8 (* 19 colour)) 248 (+ 26 (* 19 colour)) 311 :byte colour))
  (let ((vga *vga*))
    (declare (type system-area-pointer vga))
    (loop for y fixnum from 320 to 391
          do (loop for x fixnum from 8 to 311
                   for address fixnum = (+ (* 80 y) (ash x -3))
                   do (vga-graphics 8 (ash #x80 (- (logand x 7))))
                      (load-latches address)
                      (setf (sap-ref-8 vga address) (logand (ash (+ x y) -2) 15))
                      (incf *writes*))))
  (controller))
(defun stage-mode-2 ()
  (multiple-value-bind (reads writes time) (counting (bars))
    (declare (ignore reads))
    (format nil "3 Mode 2:    bars and a pattern, the colour in the byte: ~:D writes, ~:D ms"
            writes time)))

;;; The copy: 40 addresses (320 pixels) of 152 rows, from the left of the screen to the right.
(defconstant +copy-from+ (* 80 244))
(defconstant +copy-to+ (+ (* 80 244) 40))
(defun copy-by-planes ()
  "Each plane in turn: register 4 says which plane a read gives, and the sequencer's map
mask (the library's VGA-PLANES) which a write reaches."
  (declare (optimize speed))
  (controller)
  (let ((vga *vga*))
    (declare (type system-area-pointer vga))
    (dotimes (plane 4)
      (vga-graphics 4 plane)
      (vga-planes (ash 1 plane))
      (dotimes (row 152)
        (dotimes (i 40)
          (setf (sap-ref-8 vga (+ +copy-to+ (* 80 row) i))
                (load-latches (+ +copy-from+ (* 80 row) i)))
          (incf *writes*)))))
  (vga-graphics 4 0)
  (vga-planes #x0F))
(defun copy-by-latches ()
  "Write mode 1: a read takes all four planes into the latches, and a write puts them down."
  (declare (optimize speed))
  (controller :mode 1)
  (let ((vga *vga*))
    (declare (type system-area-pointer vga))
    (dotimes (row 152)
      (dotimes (i 40)
        (load-latches (+ +copy-from+ (* 80 row) i))
        (setf (sap-ref-8 vga (+ +copy-to+ (* 80 row) i)) 0)
        (incf *writes*))))
  (controller))
(defun stage-latches ()
  (multiple-value-bind (reads writes time) (counting (copy-by-planes))
    (declare (ignore reads))
    (controller :colour 1)              ; rub it out, and do it again the other way
    (rectangle 320 244 639 395)
    (multiple-value-bind (latch-reads latch-writes latch-time) (counting (copy-by-latches))
      (declare (ignore latch-reads))
      (format nil "4 Latches:   ~:D pixels in ~:D reads/writes, ~:D ms; by planes ~:D, ~:D ms"
              (* 320 152) latch-writes latch-time writes time))))

(defun checksum ()
  "All four planes of the screen, read back and added up."
  (declare (optimize speed))
  (controller)
  (let ((vga *vga*) (sum 0))
    (declare (type system-area-pointer vga) (type (unsigned-byte 24) sum))
    (dotimes (plane 4)
      (vga-graphics 4 plane)            ; the plane that reads give
      (dotimes (address +screen-bytes+)
        (setf sum (logand #xFFFFFF (+ (* sum 31) (sap-ref-8 vga address))))))
    (vga-graphics 4 0)
    sum))
(defun xor-box (x y)
  "The outline of a box 160 by 100, XORed with what is there: colour 15 turns every pixel
it crosses into its opposite, and doing it again turns it back.  (The sides stop short of
the corners, which would otherwise be done twice and so not at all.)"
  (controller :colour 15 :operation 3)
  (rectangle x y (+ x 159) y :latched t)
  (rectangle x (+ y 99) (+ x 159) (+ y 99) :latched t)
  (rectangle x (1+ y) x (+ y 98) :latched t)
  (rectangle (+ x 159) (1+ y) (+ x 159) (+ y 98) :latched t)
  (controller))
(defun stage-xor ()
  "Move the box until a key is pressed.  The key is left to be read."
  (let ((before (checksum)) (x 40) (y 60) (dx 3) (dy 2) (moves 0))
    (xor-box x y)
    (loop until (key-pressed-p)
          do (wait-frame)
             (xor-box x y)              ; off
             (unless (<= 0 (+ x dx) 480) (setf dx (- dx)))
             (unless (<= 32 (+ y dy) 300) (setf dy (- dy)))
             (incf x dx) (incf y dy) (incf moves)
             (xor-box x y))             ; and on again, somewhere else
    (xor-box x y)
    (format nil "5 XOR:       the box moved ~:D times; memory's sum was ~6,'0X and is ~6,'0X"
            moves before (checksum))))

(defun mode12 ()
  (let ((results '()))
    (video-mode #x12)
    (unless (= (current-video-mode) #x12)
      (video-mode 3)
      (format t "~&MODE12 needs a VGA: the BIOS here has no mode 12h.~%")
      (return-from mode12))
    (unwind-protect
         (flet ((show (title)
                  (text 0 0 (format nil "~80A" title) 15 4)
                  (loop for result in (reverse results) for row from 25
                        do (text 0 row (format nil "~80A" result) 14 0)))
                (next-p ()
                  (loop (case (get-key) (#\Space (return t)) (#\Escape (return nil))))))
           (loop for (title stage)
                   in '(("Mode 12h, 640 by 480 in 16 colours.  1: a pixel at a time"
                         stage-pixels)
                        ("2: set/reset, eight pixels a write" stage-set-reset)
                        ("3: write mode 2, the colour in the byte written" stage-mode-2)
                        ("4: the latches, write mode 1" stage-latches)
                        ("5: a box in XOR, until a key is pressed" stage-xor))
                 for last = (eq stage 'stage-xor)
                 do (show (format nil "~A ..." title))
                    (push (funcall stage) results)
                    (show (format nil "~A.  ~:[Space goes on, Esc leaves.~;Space or Esc leaves.~]"
                                  title last))
                    ;; (the key that stopped the box is still to be read)
                    (when (or (and last (eql (get-key) #\Escape)) (not (next-p)) last)
                      (return))))
      (controller)
      (vga-planes #x0F)
      (video-mode 3))
    (dolist (result (reverse results)) (format t "~&~A~%" result))))

(defun main ()
  "Mode 12h: the VGA's four bit planes, written through the graphics controller."
  (mode12)
  (values))

(doslib-examples:define-example "mode12" 'main
  :category :graphics
  :description "Mode 12h: 640x480 in 16 colours through the graphics controller.")
