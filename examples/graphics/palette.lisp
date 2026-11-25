;;;; Palette tricks in mode 13h: a picture that moves, fades and changes from
;;;; day to night, and not one of its pixels is written again after it has
;;;; been drawn.
;;;;
;;;; In the 256-colour modes a byte of video memory is not a colour but the
;;;; number of one: the VGA's digital-to-analogue converter (the DAC) keeps a
;;;; table of 256 colours, each 6 bits of red, green and blue, and looks every
;;;; pixel up in it as the picture is sent to the monitor.  The table is
;;;; written through two ports: the number of the first colour to change goes
;;;; to 3C8h, and then red, green and blue to 3C9h, three bytes a colour, the
;;;; DAC going on to the next colour by itself.  (The library's SET-PALETTE
;;;; does that for one colour.)  So changing a colour changes every pixel
;;;; that has its number, at once, for three bytes; and that is the trick:
;;;;
;;;;   - colour cycling.  The waterfall, its pool and the lava are painted in
;;;;     bands of colours 32 to 63 and 64 to 95, and those two ranges of the
;;;;     table are turned round, a place at a time: every band takes the
;;;;     colour of its neighbour, and the water runs.  192 bytes a frame;
;;;;   - a fade.  Every colour is sent at a fraction of its brightness, from
;;;;     64/64 down to nothing and back up: 765 bytes a frame (colour 255,
;;;;     the white of the caption, is left alone, to show that the picture is
;;;;     still there in the dark);
;;;;   - a cross-fade.  There are two tables, day and night, and what is
;;;;     sent is a mixture of the two, a little more of one each frame.
;;;;
;;;; Redrawing the picture would be 64,000 bytes a frame.  The colours are
;;;; sent during the vertical retrace, when nothing is being displayed, so
;;;; that no frame is shown with half a table (on a real VGA writing the DAC
;;;; while the picture is being drawn also shows as "snow").
;;;;
;;;;   (doslib-examples:run "palette")
;;;; Space goes on to the next effect, and round again; Esc leaves, and says
;;;; how many frames there were, how many pixels were written in them (none)
;;;; and how many bytes went to the DAC.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.palette
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.palette)

;;; The colours: 0 black, 1 to 31 the sky from the top down, then
(defconstant +water+ 32)                ; 32 colours that are turned round: the water
(defconstant +lava+ 64)                 ; and 32 more: the lava
(defconstant +rock+ 96)                 ; 16 shades of the cliff
(defconstant +cinder+ 112)              ; 16 of the volcano
(defconstant +grass+ 128)               ; 8 of the ground
(defconstant +white+ 255)               ; the caption: never faded

;;; ---- the two tables: three bytes a colour, as the DAC wants them
(defvar *day* (make-array 768 :element-type '(unsigned-byte 8)))
(defvar *night* (make-array 768 :element-type '(unsigned-byte 8)))
(defun colour (index day night)
  "Make colour INDEX the list of red, green and blue DAY by day, NIGHT by night."
  (loop for table in (list *day* *night*) for parts in (list day night)
        do (loop for part in parts for i from 0
                 do (setf (aref table (+ (* 3 index) i)) (max 0 (min 63 (round part)))))))
(defun make-tables ()
  (fill *day* 0) (fill *night* 0)
  (loop for index from 1 to 31 for f = (/ (- index 1) 30.0)              ; sky
        do (colour index (list (+ 14 (* 36 f)) (+ 34 (* 24 f)) 63)
                   (list (* 12 f) (* 5 f) (+ 7 (* 16 f)))))
  (dotimes (j 32)                       ; water: blue, with a streak of foam and a ripple
    (let ((foam (max 0 (- 1 (/ (min j (- 32 j)) 4.0))))
          (ripple (/ (+ 1 (cos (/ (* 2 pi j) 16))) 2)))
      (colour (+ +water+ j)
              (list (+ 6 (* 52 foam) (* 6 ripple)) (+ 24 (* 36 foam) (* 12 ripple))
                    (+ 48 (* 15 foam) (* 6 ripple)))
              (list (+ 1 (* 34 foam)) (+ 4 (* 38 foam) (* 5 ripple))
                    (+ 16 (* 34 foam) (* 10 ripple))))))
  (dotimes (j 32)                       ; lava: dull red to yellow; at night it glows
    (let ((heat (/ (+ 1 (cos (/ (* 2 pi j) 32))) 2)))
      (colour (+ +lava+ j)
              (list (+ 30 (* 33 heat)) (+ 3 (* 45 heat heat)) 0)
              (list (+ 38 (* 25 heat)) (+ 6 (* 57 heat)) (* 40 (expt heat 4))))))
  (dotimes (k 16)
    (colour (+ +rock+ k) (list (+ 24 (* 2 k)) (+ 20 (* 1.8 k)) (+ 16 (* 1.5 k)))
            (list (+ 3 (* 0.6 k)) (+ 4 (* 0.6 k)) (+ 9 k)))
    (colour (+ +cinder+ k) (list (+ 15 k) (+ 11 (* 0.8 k)) (+ 9 (* 0.6 k)))
            (list (+ 5 (* 0.5 k)) (+ 2 (* 0.3 k)) (+ 3 (* 0.3 k)))))
  (dotimes (k 8)
    (colour (+ +grass+ k) (list (+ 8 (* 2 k)) (+ 30 (* 3 k)) (+ 6 k)) (list 1 (+ 7 k) (+ 6 k))))
  (colour +white+ '(63 63 63) '(63 63 63)))

;;; ---- sending colours to the DAC
(defvar *bytes* 0 "Bytes written to the DAC.")
(defun turned (index water lava)
  "The colour of the table that colour INDEX shows when the water's colours have been
turned WATER places and the lava's LAVA."
  (flet ((turn (first places) (+ first (mod (- index first places) 32))))
    (cond ((<= +water+ index (+ +water+ 31)) (turn +water+ water))
          ((<= +lava+ index (+ +lava+ 31)) (turn +lava+ lava))
          (t index))))
(defun send-colours (first last water lava &optional (level 64) (night 0))
  "Write colours FIRST to LAST of the DAC: the water turned WATER places and the lava
LAVA, NIGHT 64ths of the night table mixed with the rest of the day's, and all of it
at LEVEL 64ths of its brightness."
  (declare (type fixnum first last water lava level night))
  (let ((day-table *day*) (night-table *night*))
    (declare (type (simple-array (unsigned-byte 8) (*)) day-table night-table))
    (outb #x3C8 first)                  ; the first colour to write; the DAC counts on from it
    (loop for index fixnum from first to last
          for source fixnum = (* 3 (the fixnum (turned index water lava)))
          do (dotimes (i 3)             ; red, green, blue
               (outb #x3C9 (ash (* level (+ (* (aref day-table (+ source i)) (- 64 night))
                                            (* (aref night-table (+ source i)) night)))
                                -12))))
    (incf *bytes* (* 3 (- last first -1)))))

;;; ---- the picture, drawn once
(defvar *pixels* 0 "Pixels written to video memory.")
(defun speckle (x y n) (mod (logxor (* x 73) (* y 151) (ash (* x y) -2)) n))
(defun scene (x y)
  "The colour number of pixel X, Y: a cliff with a waterfall and a pool, and a volcano."
  (let* ((cliff-top (+ 36 (mod (* (floor x 7) 5) 4)))
         (down (- y 44))                ; rows below the crater
         (stream (+ 232 (floor down 3) (round (* 8 (sin (/ down 12.0)))))))
    (cond ((and (>= y 138) (< x (- 190 (floor (- y 138) 2))))      ; the pool: rings going outward
           (+ +water+ (mod (isqrt (+ (expt (- x 80) 2) (* 9 (expt (- y 136) 2)))) 32)))
          ((and (<= 58 x 101) (>= y cliff-top))                    ; the fall: bands going down,
           (+ +water+ (mod (+ y (* 13 (floor x 2))) 32)))          ; each column out of step
          ((and (>= y cliff-top) (< x (+ 132 (speckle 1 (floor y 6) 9))))
           (+ +rock+ (min 15 (+ (speckle x y 9) (floor (* 7 (abs (- x 80))) 80)))))
          ((< (+ (expt (- x 232) 2) (expt (- y 44) 2)) 40)         ; the crater
           (+ +lava+ (mod (* 4 (+ x y)) 32)))
          ((and (plusp down) (<= (abs (- x 232)) (+ 5 (floor (* down 2) 3))))  ; the volcano
           (if (<= (abs (- x stream)) (+ 2 (floor down 16)))
               (+ +lava+ (mod (+ y (* 2 (abs (- x stream)))) 32))  ; lava: chevrons going down
               (+ +cinder+ (min 15 (+ (speckle x y 8) (floor down 18))))))
          ((>= y 150) (+ +grass+ (speckle x y 8)))
          (t (+ 1 (min 30 (floor y 5)))))))
(defun draw-scene ()
  (dotimes (y 184)
    (dotimes (x 320)
      (setf (sap-ref-8 *vga* (+ x (* 320 y))) (scene x y))))
  (incf *pixels* (* 320 184)))
(defun caption (title bytes)
  "The two lines under the picture.  They are written when the effect changes, and
not again."
  (graphics-text 0 184 (format nil "~40A" title) +white+)
  (graphics-text 0 192 (format nil "~40A" (format nil "Pixels a frame: 0  palette bytes: ~D"
                                                  bytes))
                 +white+)
  (incf *pixels* (* 320 16)))

;;; ---- the effects
(defun swing (phase)
  "From 64 down to 0 and back, staying a while at each end: PHASE counts frames."
  (max 0 (min 64 (- (abs (- 112 (mod phase 224))) 24))))
(defparameter *effects*
  '(("Colour cycling: two ranges turn" 192)
    ("Fade to black and back" 765)
    ("Cross-fade: day to night and back" 765)))

(defun palette ()
  (let ((frames 0) (start (ticks)) (drawn 0) (elapsed 0))
    (make-tables)
    (setf *bytes* 0 *pixels* 0)
    (with-video-mode (#x13)
      (send-colours 0 255 0 0)
      (draw-scene)
      (let ((effect 0) (phase 0))
        (apply #'caption (first *effects*))
        (setf drawn *pixels* start (ticks))
        (loop
          (when (key-pressed-p)
            (case (get-key)
              (#\Escape (return))
              (#\Space
               (setf effect (mod (1+ effect) (length *effects*)) phase 0)
               (apply #'caption (nth effect *effects*))
               (send-colours 0 254 (floor frames 2) (floor frames 6)))))   ; all as by day
          (wait-frame t)                ; the retrace has begun: now
          (incf frames)
          (incf phase)
          ;; The water turns a place every second frame, the lava every sixth -- in
          ;; every effect: a fade sends the turned colours, dimmed.
          (let ((water (floor frames 2)) (lava (floor frames 6)))
            (ecase effect
              (0 (send-colours +water+ (+ +lava+ 31) water lava))
              (1 (send-colours 0 254 water lava (swing phase)))
              (2 (send-colours 0 254 water lava 64 (- 64 (swing phase)))))))
        (setf elapsed (- (ticks) start))))
    (format t "~&~D frames~@[, ~D a second~].  Pixels written in them: 0.~%~
               Bytes written to the DAC: ~D~@[, ~D a frame~].~%"
            frames (and (plusp elapsed) (round (* frames 18.2) elapsed))
            *bytes* (and (plusp frames) (round *bytes* frames)))
    (format t "The picture and its caption were drawn once, ~D pixels; the captions ~
               written~%when the effect changed were ~D more.  Redrawing the picture ~
               would be 64000~%bytes every frame.~%"
            drawn (- *pixels* drawn))))

(defun main ()
  "Palette tricks: colour cycling, a fade and a cross-fade, with no pixel redrawn."
  (palette)
  (values))

(doslib-examples:define-example "palette" 'main
  :category :graphics
  :description "Palette tricks: colour cycling, a fade and a cross-fade, with no pixel redrawn.")
