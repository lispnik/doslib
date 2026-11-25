;;;; A tile map that scrolls every way: a world 128 tiles by 40 (2,048 by 640
;;;; pixels: six screens across and nearly three down) seen through a window
;;;; of 320 by 224, in Mode X.  It is how the scrolling games of the early
;;;; 1990s moved a whole screen on a machine that could not redraw one in
;;;; time: the VGA moves the picture, and the program draws only the strip
;;;; of tiles that is about to come into view.
;;;;
;;;;   - The CRT controller's offset register (13h at port 3D4h) makes a row
;;;;     of video memory 88 addresses long (352 pixels, 22 tiles) though only
;;;;     80 are displayed: there is a "virtual screen" wider than the real
;;;;     one.  Pixel X, Y of the WORLD is kept at address 88 Y + X / 4 (plane
;;;;     X mod 4), after the status bar's 16 rows.
;;;;   - The start address (registers 0Ch and 0Dh) says where in memory the
;;;;     display begins: add 1 and the picture moves four pixels left, add 88
;;;;     and it moves one row up.  The attribute controller's pixel panning
;;;;     (register 13h at port 3C0h) does the one to three pixels between.
;;;;   - Going right, the hidden end of one row of memory is the beginning of
;;;;     the next: column 22 of the world is at the addresses of column 0, one
;;;;     row of pixels lower.  So the 22 columns of room go round and round:
;;;;     a new column of tiles is drawn over the one that left the other side
;;;;     of the screen 22 columns ago.  That is the wrap-around, and why the
;;;;     world may be any width.
;;;;   - Going down there is no such trick here: memory simply has a place for
;;;;     every row of the world, 56,320 addresses of the 65,536, most of them
;;;;     holding whatever was last drawn there -- which is why a row of tiles
;;;;     coming into view must be drawn too.  (A taller world would have to
;;;;     let its addresses run off the end of the 64K and come round again.)
;;;;
;;;; So each time the window crosses a tile boundary one column of at most 16
;;;; tiles or one row of at most 22 is drawn, and at every other frame
;;;; nothing: the bottom line shows the tiles drawn for each frame beside the
;;;; 315 (21 by 15) that redrawing what is in view would take.  The status bar
;;;; stays still by a third register, line compare (CRT controller 18h): at
;;;; that scan line the VGA starts again from address 0.
;;;;
;;;;   (doslib-examples:run "tiles")
;;;; It drifts about by itself until an arrow key is pressed.  An arrow key
;;;; scrolls for as long as it is held (faster with Shift held first); Space
;;;; sets it drifting again; P turns the pixel panning off and on; Esc
;;;; leaves, and prints the numbers.
;;;;
;;;; The BIOS says when a key is typed, not how long it is held.  The held
;;;; arrow is found the way simple games found it: port 60h goes on holding
;;;; the last scan code the keyboard sent, which is the key's number when it
;;;; went down (and each time it repeats) and that number with 80h added when
;;;; it came up.  That tells of one key at a time, which will do here.
;;;;
;;;; Without the panning the picture moves sideways four pixels at a time.
;;;; That is how it starts under QEMU (found by its configuration device at
;;;; port 510h), which moves a 256-colour picture four pixels for each one
;;;; asked of the panning register: press P there to see it jump about.  Nor
;;;; does QEMU have a real vertical retrace, so the frames are paced by the
;;;; timer chip (WAIT-FRAME).  On a VGA the panning starts on, the picture
;;;; moves a pixel at a time and the retrace sets the pace.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.tiles
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.tiles)

(eval-when (:compile-toplevel :load-toplevel :execute)   ; known when compiling, for #.
  (defconstant +row-bytes+ 88)            ; addresses from one row to the next: 352 pixels
  (defconstant +room+ 22)                 ; columns of tiles in a row of memory
  (defconstant +view-rows+ 224)           ; pixel rows of the window: the rest is the status bar
  (defconstant +kept-rows+ 16)            ; rows of tiles kept drawn: 15 can be in view
  (defconstant +map-width+ 128)           ; tiles across the world
  (defconstant +map-height+ 40)           ; and down
  (defconstant +base+ (* 16 +row-bytes+)) ; the world starts after the status bar's 16 rows
  (defconstant +limit-x+ (- (* 16 +map-width+) 320))             ; the furthest it can scroll
  (defconstant +limit-y+ (- (* 16 +map-height+) +view-rows+))
  (defconstant +redraw+ (* 21 15))        ; tiles wholly or partly in view, at most
  )

;;; ---- the mode
;;; (MODE-X, 320x240 in four planes, is the library's, with the writers of the VGA's
;;; registers used below: src/modex.lisp.)
(defun scrolling-mode ()
  (mode-x)
  (vga-crtc #x13 (floor +row-bytes+ 2)) ; the offset register counts in pairs of addresses
  ;; Split the screen after scan line 447 (pixel row 223: every row is two scan lines).
  ;; Line compare has ten bits: 0-7 in register 18h, 8 in 07h bit 4, 9 in 09h bit 6.
  (vga-crtc #x18 #xBF) (vga-crtc #x07 #x3E) (vga-crtc #x09 #x01)
  (vga-attribute #x10 #x61))            ; 256 colours, and no panning below the split
(defun qemu-p ()
  "Is this QEMU?  Its configuration device says so: item 0, read at port 511h."
  (outw #x510 0)
  (equal (loop repeat 4 collect (inb #x511)) '(81 69 77 85)))    ; "QEMU"
(defvar *panning* t "Use the pixel panning register?  If not, scroll four pixels at a time.")
(defun scroll-to (x y)
  "Display the world from its pixel X, Y."
  (let ((start (+ +base+ (* +row-bytes+ y) (ash x -2))) (x (if *panning* x 0)))
    (wait-frame)
    (vga-display-start start)                   ; the start address is taken at the retrace:
    (wait-retrace)                              ; set it before one,
    (vga-attribute #x13 (* 2 (logand x 3)))))   ; and the panning, used at once, during it

;;; ---- the tiles: 16 by 16, kept a plane at a time as video memory wants them
(eval-when (:compile-toplevel :load-toplevel :execute)   ; known when compiling, for #.
  (defconstant +sky+ 0) (defconstant +cloud-left+ 1) (defconstant +cloud+ 2)
  (defconstant +cloud-right+ 3) (defconstant +grass+ 4) (defconstant +earth+ 5)
  (defconstant +rock+ 6) (defconstant +bedrock+ 7) (defconstant +brick+ 8)
  (defconstant +cave+ 9) (defconstant +gem+ 10) (defconstant +flower+ 11)
  (defconstant +trunk+ 12) (defconstant +leaves+ 13) (defconstant +window+ 14)
  (defconstant +ladder+ 15) (defconstant +tiles+ 16)
  )

(defun set-colours ()
  (loop for (red green blue) in '((0 0 0) (24 40 62) (62 62 63) (48 52 60)      ; sky, cloud
                                  (20 50 14) (10 38 8) (38 24 11) (30 18 8)     ; grass, earth
                                  (42 42 44) (28 28 31)                         ; rock
                                  (46 20 12) (22 12 10) (56 30 20)              ; brick
                                  (8 6 10) (13 10 16) (63 63 63)                ; cave; white
                                  (10 50 60) (50 63 63)                         ; gem
                                  (26 15 6) (6 30 8) (14 42 12)                 ; trunk, leaves
                                  (60 20 36) (63 56 10) (4 4 12)                ; flower; window
                                  (44 34 16) (12 44 10))                        ; ladder; stem
        for index from 0
        do (set-palette index red green blue)))
(defun speckle (x y) (mod (+ (* x x 7) (* y 13) (* x y 5)) 11))
(defun tile-pixel (tile x y)
  "The colour of pixel X, Y of TILE."
  (flet ((cloud (x)                     ; x counted from the cloud's rounded end
           (cond ((> (+ (expt (- x 15) 2) (* 4 (expt (- y 8) 2))) 200) 1)
                 ((> y 10) 3) (t 2)))
         (earth () (if (< (speckle x y) 3) 7 6))
         (brick ()
           (cond ((= (mod y 8) 7) 11)                                   ; mortar under a course
                 ((= (mod (+ x (if (< y 8) 0 8)) 16) 15) 11)            ; and between bricks
                 ((= (mod y 8) 0) 12)
                 (t 10)))
         (cave () (if (< (speckle (+ x 5) y) 2) 14 13)))
    (ecase tile
      (#.+sky+ 1)
      (#.+cloud-left+ (cloud x))
      (#.+cloud+ (cond ((< y (+ 1 (abs (- (mod x 8) 4)))) 1) ((> y 14) 1) ((> y 10) 3) (t 2)))
      (#.+cloud-right+ (cloud (- 15 x)))
      (#.+grass+ (cond ((< y (+ 4 (mod (* x 5) 3))) (if (< (speckle x y) 4) 5 4)) (t (earth))))
      (#.+earth+ (earth))
      (#.+rock+ (cond ((> (+ (expt (- x 8) 2) (expt (- y 8) 2)) 30) (earth))
                      ((> (+ x y) 18) 9) (t 8)))
      (#.+bedrock+ (cond ((< (speckle (+ x 2) (+ y 7)) 4) 9) ((= (mod (+ x (* 3 y)) 9) 0) 7) (t 8)))
      (#.+brick+ (brick))
      (#.+cave+ (cave))
      (#.+gem+ (cond ((>= (+ (abs (- x 8)) (abs (- y 8))) 5) (earth))
                     ((and (< x 8) (< y 8)) 17) (t 16)))
      (#.+flower+ (let ((far (+ (expt (- x 8) 2) (expt (- y 5) 2))))
                    (cond ((<= far 1) 22) ((<= far 10) 21)
                          ((and (= x 8) (> y 5)) 25) (t 1))))
      (#.+trunk+ (cond ((<= 6 x 9) (if (= x 9) 7 18)) (t 1)))
      (#.+leaves+ (cond ((> (+ (expt (- x 8) 2) (expt (- y 8) 2)) 60) 1)
                        ((< (speckle x (+ y 2)) 4) 20) (t 19)))
      (#.+window+ (if (and (<= 5 x 10) (>= y 4) (> (+ y (min (- x 5) (- 10 x))) 5)) 23 (brick)))
      (#.+ladder+ (cond ((or (= x 3) (= x 12)) 24)
                        ((and (< 3 x 12) (= (mod y 4) 1)) 24)
                        (t (cave)))))))
(defvar *tiles* (make-array (* 256 +tiles+) :element-type '(unsigned-byte 8))
  "Each tile is 256 bytes: four planes of 16 rows of 4.")
(defun make-tiles ()
  (dotimes (tile +tiles+)
    (dotimes (y 16)
      (dotimes (x 16)
        (setf (aref *tiles* (+ (* 256 tile) (* 64 (mod x 4)) (* 4 y) (floor x 4)))
              (tile-pixel tile x y))))))

;;; ---- the map
(defvar *map* (make-array (* +map-width+ +map-height+) :element-type '(unsigned-byte 8)))
(defun tile-at (column row) (aref *map* (+ column (* +map-width+ row))))
(defun (setf tile-at) (tile column row)
  (when (and (< -1 column +map-width+) (< -1 row +map-height+))
    (setf (aref *map* (+ column (* +map-width+ row))) tile)))
(defun make-map ()
  "Sky with clouds and ledges of brick; uneven ground with trees, flowers and
towers; under it earth, rocks, gems, three levels of brick-lined tunnels and
bedrock.  The same every time."
  (let ((seed 7) (levels (make-array +map-width+)))
    (flet ((chance (n) (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
             (mod (ash seed -16) n)))
      (fill *map* +sky+)
      (let ((level 18) (run 0))                                                 ; the ground
        (dotimes (column +map-width+)
          (when (minusp (decf run))
            (setf run (+ 2 (chance 5))
                  level (max 15 (min 22 (+ level (chance 3) -1)))))
          (setf (aref levels column) level
                (tile-at column level) +grass+)
          (loop for row from (1+ level) below +map-height+
                do (setf (tile-at column row)
                         (cond ((>= row (- +map-height+ 2)) +bedrock+)
                               ((zerop (chance 9)) +rock+)
                               ((zerop (chance 30)) +gem+)
                               (t +earth+))))
          (case (chance 8)
            (0 (setf (tile-at column (- level 1)) +trunk+ (tile-at column (- level 2)) +leaves+))
            ((1 2) (setf (tile-at column (- level 1)) +flower+)))))
      (loop for column = (chance 6) then (+ column length 2 (chance 7))         ; clouds
            for length = (+ 2 (chance 4)) for row = (chance 12)
            while (< column +map-width+)
            do (setf (tile-at column row) +cloud-left+
                     (tile-at (+ column length -1) row) +cloud-right+)
               (loop for i from 1 below (1- length) do (setf (tile-at (+ column i) row) +cloud+)))
      (loop for column = (+ 4 (chance 8)) then (+ column length 5 (chance 10))  ; ledges
            for length = (+ 3 (chance 4)) for row = (+ 8 (chance 5))
            while (< (+ column length) +map-width+)
            do (dotimes (i length) (setf (tile-at (+ column i) row) +brick+)))
      (loop for column = (+ 9 (chance 6)) then (+ column 13 (chance 10))        ; towers
            while (< (+ column 3) +map-width+)
            do (let ((top (- (reduce #'min levels :start column :end (+ column 3)) 4 (chance 4))))
                 (dotimes (i 3)
                   (loop for row from top to (aref levels (+ column i))
                         do (setf (tile-at (+ column i) row)
                                  (if (and (= i 1) (oddp (- row top))) +window+ +brick+))))))
      (dolist (row '(25 30 34))                                                 ; tunnels
        (loop for column = (chance 10) then (+ column length 3 (chance 8))
              for length = (+ 8 (chance 14))
              while (< column +map-width+)
              do (dotimes (i length)
                   (let ((column (+ column i)) (ladder (= i (floor length 2))))
                     (setf (tile-at column (- row 1)) +brick+
                           (tile-at column row) (if ladder +ladder+ +cave+)
                           (tile-at column (+ row 1)) (if ladder +ladder+ +cave+)
                           (tile-at column (+ row 2)) +brick+))))))))

;;; ---- drawing
(defvar *drawn* 0 "Tiles written to video memory.")
(defun draw-tiles (column row count across)
  "COUNT tiles of the map from COLUMN, ROW, each at its place in video memory:
going across if ACROSS, or else down."
  (declare (type fixnum column row count) (optimize speed))
  (let ((vga *vga*) (tiles *tiles*) (map *map*))
    (declare (type system-area-pointer vga) (type (simple-array (unsigned-byte 8) (*)) tiles map))
    (dotimes (plane 4)
      (vga-planes (ash 1 plane))        ; map mask: this plane only
      (dotimes (i count)
        (let* ((column (if across (+ column i) column))
               (row (if across row (+ row i)))
               (source (+ (* 256 (aref map (+ column (* +map-width+ row)))) (* 64 plane)))
               (target (+ +base+ (* 4 column) (* 16 +row-bytes+ row))))
          (declare (type fixnum column row source target))
          (dotimes (line 16)
            (dotimes (i 4)
              (setf (sap-ref-8 vga (+ target i)) (aref tiles (+ source i))))
            (incf source 4)
            (incf target +row-bytes+))))))
  (incf *drawn* count))

;;; Text: the library's MODE-X-TEXT, on the status bar, which is at address 0 of rows 88
;;; addresses apart.
(defun draw-status (x y rate tiles-a-frame)
  (mode-x-text 0 0 (format nil "~40A" (format nil "X ~4D  Y ~3D  ~D frames/s  ~:[no ~;~]panning"
                                              x y rate *panning*))
               15 0 0 +row-bytes+)
  (mode-x-text 0 8 (format nil "~40A" (format nil "Tiles a frame: ~,1F  redrawing: ~D"
                                              tiles-a-frame +redraw+))
               22 0 0 +row-bytes+))

(defun arrow-held (step)
  "How far to scroll, as two values, for the arrow key that is held down: the
last scan code at port 60h, if it is an arrow's and not a release."
  (case (inb #x60)
    (#.+key-left+ (values (- step) 0))
    (#.+key-right+ (values step 0))
    (#.+key-up+ (values 0 (- step)))
    (#.+key-down+ (values 0 step))
    (t (values 0 0))))
(defun shift-held-p ()
  "Is a Shift key down?  The BIOS keeps that at 0040:0017, bits 0 and 1."
  (logtest 3 (peek #x417)))
(defun tiles ()
  (let ((frames 0) (clocks 0))
    (make-tiles)
    (make-map)
    (scrolling-mode)
    (unwind-protect
         (let* ((x 0) (y 96) (drift-x 2) (drift-y 1) (drifting t) (leave nil)
                ;; the columns and rows of tiles that are in video memory now
                (left 0) (right (1- +room+))
                (top (floor y 16)) (bottom (+ top +kept-rows+ -1))
                (rate 0) (tiles-a-frame 0))
           (setf *panning* (not (qemu-p)))
           (set-colours)
           (loop for row from top to bottom do (draw-tiles left row +room+ t))
           (setf *drawn* 0)             ; the first screenful is not counted
           (draw-status x y rate tiles-a-frame)
           (loop with start = (timer-count)
                 with mark = start with mark-frames = 0 with mark-drawn = 0
                 until leave
                 do (loop while (key-pressed-p)         ; an arrow held is typed over and over:
                          do (case (get-key)            ; take them all
                               (#\Escape (setf leave t))
                               (#\Space (setf drifting t))
                               ((#\p #\P) (setf *panning* (not *panning*)))
                               ((:left :right :up :down) (setf drifting nil))))
                    (multiple-value-bind (dx dy)
                        (if drifting
                            (progn      ; turning round at the edges of the world
                              (unless (<= 0 (+ x drift-x) +limit-x+) (setf drift-x (- drift-x)))
                              (unless (<= 0 (+ y drift-y) +limit-y+) (setf drift-y (- drift-y)))
                              (values drift-x drift-y))
                            (arrow-held (if (shift-held-p) 6 2)))
                      (setf x (max 0 (min +limit-x+ (+ x dx)))
                            y (max 0 (min +limit-y+ (+ y dy)))))
                    ;; Bring into memory the tiles about to be seen.  A new column is
                    ;; written over the one 22 columns away, out of sight by then; a new
                    ;; row has its own place, and the row 16 away is forgotten.
                    (loop while (< (floor x 16) left)
                          do (decf left)
                             (draw-tiles left top (- bottom top -1) nil)
                             (setf right (min right (+ left +room+ -1))))
                    (loop while (> (floor (+ x 319) 16) right)
                          do (incf right)
                             (draw-tiles right top (- bottom top -1) nil)
                             (setf left (max left (- right +room+ -1))))
                    (loop while (< (floor y 16) top)
                          do (decf top)
                             (draw-tiles left top (- right left -1) t)
                             (setf bottom (min bottom (+ top +kept-rows+ -1))))
                    (loop while (> (floor (+ y +view-rows+ -1) 16) bottom)
                          do (incf bottom)
                             (draw-tiles left bottom (- right left -1) t)
                             (setf top (max top (- bottom +kept-rows+ -1))))
                    (scroll-to x y)
                    (incf frames)
                    (let* ((now (timer-count)) (elapsed (- now mark)))
                      (setf clocks (- now start))
                      (when (>= elapsed +timer-hz+)
                        (setf rate (round (* (- frames mark-frames) +timer-hz+) elapsed)
                              tiles-a-frame (/ (- *drawn* mark-drawn)
                                               (float (- frames mark-frames)))
                              mark now mark-frames frames mark-drawn *drawn*)
                        (draw-status x y rate tiles-a-frame)))))
      (vga-attribute #x13 0)
      (video-mode 3))
    (when (and (plusp frames) (plusp clocks))
      (format t "~&~D frames, ~D a second; ~D tiles drawn: ~,1F a frame.~%"
              frames (round (* frames +timer-hz+) clocks) *drawn* (/ *drawn* (float frames)))
      (format t "Redrawing what is in view would be ~D tiles every frame.~%" +redraw+))))

(defun main ()
  "A tile map scrolled every way by the VGA, drawing only the tiles that come into view."
  (tiles)
  (values))

(doslib-examples:define-example "tiles" 'main
  :category :graphics
  :description
  "A tile map scrolled every way by the VGA, drawing only the tiles that come into view.")
