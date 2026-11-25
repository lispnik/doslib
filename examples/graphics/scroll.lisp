;;;; Hardware scrolling in Mode X: a landscape 240 tiles wide goes by, and
;;;; nothing on the screen is ever redrawn or moved.  The VGA does it:
;;;;
;;;;   - the CRT controller's start address (registers 0Ch and 0Dh at port
;;;;     3D4h) says where in video memory the display begins.  In the planar
;;;;     256-colour modes one address is four pixels, so adding 1 to it moves
;;;;     the whole picture four pixels left;
;;;;   - the attribute controller's horizontal pixel panning register (index
;;;;     13h at port 3C0h) shifts the picture left by the 0 to 3 pixels in
;;;;     between (in 256-colour modes its values are 0, 2, 4 and 6);
;;;;   - the CRT controller's offset register (13h) is the distance in memory
;;;;     from one row to the next, here 88 addresses (352 pixels) though only
;;;;     80 are displayed: 32 pixels of every row are out of sight.
;;;;
;;;; The program's work is to draw one column of 16 by 16 tiles in that
;;;; hidden part just before it scrolls into view: 3,584 bytes every sixteen
;;;; pixels, where redrawing the picture would be 71,680 bytes every frame.
;;;; The start address simply keeps going up through memory: what was the
;;;; hidden end of one row becomes the beginning of the next, so the same 22
;;;; columns of room are used over and over (and 64K is enough for a map
;;;; thousands of tiles wide).
;;;;
;;;; The two lines of text at the bottom stay put because of a third
;;;; register, line compare (CRT controller 18h, with bits in 07h and 09h):
;;;; at that scan line the VGA starts again from address 0, where the text
;;;; is, and bit 5 of attribute register 10h says not to pan that part.
;;;;
;;;;   (doslib-examples:run "scroll")
;;;; It scrolls by itself, turning round at each end of the map.  Right and
;;;; Left arrows change the speed, a pixel a frame at a time, through nought
;;;; to the other direction; Space stops and starts it; P turns the pixel
;;;; panning off and on; Esc leaves, and says how many bytes a frame were
;;;; written.
;;;;
;;;; Without the panning the picture moves four pixels at a time.  That is
;;;; how it starts under QEMU (found by its configuration device at port
;;;; 510h), which, as far as version 11, moves a 256-colour picture four
;;;; pixels for each one asked of the panning register: press P there to see
;;;; it jump about.  On a VGA, and wherever the register is done properly,
;;;; the panning starts on and the picture moves a pixel at a time.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.scroll
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.scroll)

(eval-when (:compile-toplevel :load-toplevel :execute)   ; known when compiling, for #.
  (defconstant +row-bytes+ 88)            ; addresses from one row to the next: 352 pixels
  (defconstant +rows+ 14)                 ; tiles down the scrolling part: 224 pixels
  (defconstant +map-width+ 240)           ; tiles across the map: 3,840 pixels
  (defconstant +base+ (* 16 +row-bytes+)) ; the map starts after the 16 rows of the status bar
  (defconstant +room+ 22)                 ; columns of tiles that fit in a row of memory
  (defconstant +redraw+ (* 320 16 +rows+)) ; bytes to draw what is displayed of the map
  (defconstant +limit+ (- (* 16 +map-width+) 320)) ; the furthest the picture can scroll
  )

;;; ---- the mode
;;; (MODE-X, 320x240 in four planes, is the library's, with the writers of the VGA's
;;; registers used below -- VGA-CRTC, VGA-ATTRIBUTE, VGA-PLANES, VGA-DISPLAY-START:
;;; src/modex.lisp.)
(defun scrolling-mode ()
  (mode-x)
  (vga-crtc #x13 (floor +row-bytes+ 2)) ; the offset register counts in pairs of addresses
  ;; Split the screen after scan line 447 (pixel row 223: every row is two scan lines).
  ;; Line compare has ten bits: 0-7 in register 18h, 8 in 07h bit 4, 9 in 09h bit 6.
  (vga-crtc #x18 #xBF) (vga-crtc #x07 #x3E) (vga-crtc #x09 #x01)
  (vga-attribute #x10 #x61))            ; 256 colours, and no panning below the split
;;; The vertical retrace is the clock: 60 frames a second in this mode.  But some
;;; emulators (QEMU is one) report a retrace whenever they are asked, so the frames are
;;; paced by the timer chip too, with the library's WAIT-FRAME: at most 74 a second, and
;;; a monitor's own rate where the retrace is real.
(defun qemu-p ()
  "Is this QEMU?  Its configuration device says so: item 0, read at port 511h."
  (outw #x510 0)
  (equal (loop repeat 4 collect (inb #x511)) '(81 69 77 85)))    ; "QEMU"
(defvar *panning* t "Use the pixel panning register?  If not, scroll four pixels at a time.")
(defun scroll-to (x)
  "Display the map from its pixel X."
  (let ((start (+ +base+ (ash x -2))) (x (if *panning* x 0)))
    (wait-frame)
    (vga-display-start start)                   ; the start address is taken at the retrace:
    (wait-retrace)                              ; set it before one,
    (vga-attribute #x13 (* 2 (logand x 3)))))   ; and the panning, used at once, during it

;;; ---- the tiles: 16 by 16, kept a plane at a time as video memory wants them
(eval-when (:compile-toplevel :load-toplevel :execute)   ; known when compiling, for #.
  (defconstant +sky+ 0) (defconstant +cloud-left+ 1) (defconstant +cloud+ 2)
  (defconstant +cloud-right+ 3) (defconstant +grass+ 4) (defconstant +earth+ 5)
  (defconstant +rock+ 6) (defconstant +hill+ 7) (defconstant +hill-up+ 8)
  (defconstant +hill-down+ 9) (defconstant +trunk+ 10) (defconstant +leaves+ 11)
  (defconstant +tiles+ 12)
  )

(defun set-colours ()
  (loop for (red green blue) in '((0 0 0) (22 38 60) (62 62 63) (48 52 60) (18 48 12) (10 38 8)
                                  (38 24 11) (30 18 8) (42 42 44) (28 28 31) (20 42 34) (16 36 30)
                                  (26 15 6) (6 30 8) (14 42 12) (63 63 63))
        for index from 0
        do (set-palette index red green blue)))
(defun speckle (x y) (mod (+ (* x x 7) (* y 13) (* x y 5)) 11))
(defun tile-pixel (tile x y)
  "The colour of pixel X, Y of TILE."
  (flet ((cloud (x)                     ; x counted from the cloud's rounded end
           (cond ((> (+ (expt (- x 15) 2) (* 4 (expt (- y 8) 2))) 200) 1)
                 ((> y 10) 3) (t 2)))
         (earth () (if (< (speckle x y) 3) 7 6))
         (hill () (if (< (speckle (+ x 3) y) 2) 11 10)))
    (ecase tile
      (#.+sky+ 1)
      (#.+cloud-left+ (cloud x))
      (#.+cloud+ (cond ((< y (+ 1 (abs (- (mod x 8) 4)))) 1) ((> y 14) 1) ((> y 10) 3) (t 2)))
      (#.+cloud-right+ (cloud (- 15 x)))
      (#.+grass+ (cond ((< y (+ 4 (mod (* x 5) 3))) (if (< (speckle x y) 4) 5 4)) (t (earth))))
      (#.+earth+ (earth))
      (#.+rock+ (cond ((> (+ (expt (- x 8) 2) (expt (- y 8) 2)) 30) (earth))
                      ((> (+ x y) 18) 9) (t 8)))
      (#.+hill+ (hill))
      (#.+hill-up+ (if (>= y (- 15 x)) (hill) 1))
      (#.+hill-down+ (if (>= y x) (hill) 1))
      (#.+trunk+ (cond ((<= 6 x 9) (if (= x 9) 7 12)) (t 1)))
      (#.+leaves+ (cond ((> (+ (expt (- x 8) 2) (expt (- y 8) 2)) 60) 1)
                        ((< (speckle x (+ y 2)) 4) 14) (t 13))))))
(defvar *tiles* (make-array (* 256 +tiles+) :element-type '(unsigned-byte 8))
  "Each tile is 256 bytes: four planes of 16 rows of 4.")
(defun make-tiles ()
  (dotimes (tile +tiles+)
    (dotimes (y 16)
      (dotimes (x 16)
        (setf (aref *tiles* (+ (* 256 tile) (* 64 (mod x 4)) (* 4 y) (floor x 4)))
              (tile-pixel tile x y))))))

;;; ---- the map
(defvar *map* (make-array (* +map-width+ +rows+) :element-type '(unsigned-byte 8)))
(defun tile-at (column row) (aref *map* (+ column (* +map-width+ row))))
(defun (setf tile-at) (tile column row)
  (when (and (< -1 column +map-width+) (< -1 row +rows+))
    (setf (aref *map* (+ column (* +map-width+ row))) tile)))
(defun make-map ()
  "Sky, clouds, hills in the distance, ground of uneven height, and trees: the
same every time."
  (let ((seed 1))
    (flet ((chance (n) (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
             (mod (ash seed -16) n)))
      (fill *map* +sky+)
      (loop for column = (chance 6) then (+ column length 3 (chance 9))        ; clouds
            for length = (+ 2 (chance 4)) for row = (chance 4)
            while (< column +map-width+)
            do (setf (tile-at column row) +cloud-left+
                     (tile-at (+ column length -1) row) +cloud-right+)
               (loop for i from 1 below (1- length) do (setf (tile-at (+ column i) row) +cloud+)))
      (loop for peak = 8 then (+ peak 5 (chance 14))                            ; hills
            for height = (+ 3 (chance 5))
            while (< peak +map-width+)
            do (loop for i from (- height) below height
                     for top = (- 12 (if (minusp i) (+ height i 1) (- height i)))
                     for column = (+ peak i)
                     do (when (and (< -1 column +map-width+) (/= (tile-at column top) +hill+))
                          (setf (tile-at column top) (if (minusp i) +hill-up+ +hill-down+)))
                        (loop for row from (1+ top) below +rows+
                              do (setf (tile-at column row) +hill+))))
      (let ((level 10) (run 0))                                                 ; the ground
        (dotimes (column +map-width+)
          (when (minusp (decf run))
            (setf run (+ 2 (chance 5))
                  level (max 9 (min 12 (+ level (chance 3) -1)))))
          (setf (tile-at column level) +grass+)
          (loop for row from (1+ level) below +rows+
                do (setf (tile-at column row) (if (zerop (chance 7)) +rock+ +earth+)))
          (when (and (zerop (chance 5))                                         ; a tree
                     (= (tile-at column (- level 1)) +sky+) (= (tile-at column (- level 2)) +sky+))
            (setf (tile-at column (- level 1)) +trunk+ (tile-at column (- level 2)) +leaves+)))))))

;;; ---- drawing
(defvar *written* 0 "Bytes of tiles written to video memory.")
(defun draw-column (column)
  "The tiles of map column COLUMN, at its place in video memory."
  (declare (type fixnum column) (optimize speed))
  (let ((vga *vga*) (tiles *tiles*) (map *map*))
    (declare (type system-area-pointer vga) (type (simple-array (unsigned-byte 8) (*)) tiles map))
    (dotimes (plane 4)
      (vga-planes (ash 1 plane))        ; map mask: this plane only
      (dotimes (row +rows+)
        (let ((source (+ (* 256 (aref map (+ column (* +map-width+ row)))) (* 64 plane)))
              (target (+ +base+ (* 4 column) (* 16 +row-bytes+ row))))
          (declare (type fixnum source target))
          (dotimes (line 16)
            (dotimes (i 4)
              (setf (sap-ref-8 vga (+ target i)) (aref tiles (+ source i))))
            (incf source 4)
            (incf target +row-bytes+))))))
  (incf *written* (* 256 +rows+)))

;;; Text: the library's MODE-X-TEXT, on the status bar, which is at address 0 of rows 88
;;; addresses apart.
(defun draw-status (x speed rate bytes)
  (mode-x-text 0 0 (format nil "~40A" (format nil "Tile ~3D of ~D  speed ~@D  ~D frames/s"
                                              (floor x 16) +map-width+ speed rate))
               15 0 0 +row-bytes+)
  (mode-x-text 0 8 (format nil "~40A" (format nil "Bytes a frame: ~D  redrawing: ~D~:[  P~;~]"
                                              bytes +redraw+ *panning*))
               14 0 0 +row-bytes+))

(defun scroll ()
  (let ((frames 0))
    (make-tiles)
    (make-map)
    (scrolling-mode)
    (unwind-protect
         (let ((x 0) (speed 2) (stopped nil)
               (low 0) (high 20)        ; the columns that are in video memory now
               (rate 0) (bytes 0))
           (setf *panning* (not (qemu-p)))
           (set-colours)
           (loop for column from low to high do (draw-column column))
           (setf *written* 0)           ; the first screenful is not counted
           (draw-status x speed rate bytes)
           (loop with mark = (ticks) with mark-frames = 0 with mark-written = 0
                 do (when (key-pressed-p)
                      (let ((key (get-key)))
                        (case key
                          (#\Escape (return))
                          (#\Space (setf stopped (not stopped)))
                          ((#\p #\P) (setf *panning* (not *panning*)))
                          (:right (setf speed (min 8 (1+ speed)) stopped nil))
                          (:left (setf speed (max -8 (1- speed)) stopped nil)))))
                    (unless stopped
                      (incf x speed)
                      (cond ((< x 0) (setf x 0 speed (- speed)))            ; turn round
                            ((> x +limit+) (setf x +limit+ speed (- speed)))))
                    ;; Bring into memory any column about to be seen.  Drawing one writes
                    ;; over the column 22 before or after it, which is out of sight by then.
                    (loop while (< (floor x 16) low)
                          do (draw-column (decf low)) (setf high (min high (+ low +room+ -1))))
                    (loop while (> (floor (+ x 319) 16) high)
                          do (when (< high (1- +map-width+)) (draw-column (incf high)))
                             (setf low (max low (- high +room+ -1)))
                          until (= high (1- +map-width+)))
                    (scroll-to x)
                    (incf frames)
                    (let ((elapsed (- (ticks) mark)))
                      (when (>= elapsed 18)
                        (setf rate (round (* (- frames mark-frames) 18.2) elapsed)
                              bytes (round (- *written* mark-written) (- frames mark-frames))
                              mark (ticks) mark-frames frames mark-written *written*)
                        (draw-status x speed rate bytes)))))
      (video-mode 3))
    (when (plusp frames)
      (format t "~&~D frames, ~D bytes of tiles written to video memory: ~D a frame.~%"
              frames *written* (round *written* frames))
      (format t "Redrawing what is displayed would be ~D bytes every frame.~%" +redraw+))))

(defun main ()
  "Hardware scrolling: a wide tile map moved by the VGA's start address and panning."
  (scroll)
  (values))

(doslib-examples:define-example "scroll" 'main
  :category :graphics
  :description "Hardware scrolling: a wide tile map moved by the VGA's start address and panning.")
