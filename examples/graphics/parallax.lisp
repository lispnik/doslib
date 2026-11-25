;;;; Parallax scrolling in Mode X: far mountains, nearer hills and the ground
;;;; at your feet go by at a quarter, a half and the whole of the speed, under
;;;; a sky that stays put, with two balloons and three birds in front.  Things
;;;; far away seem to move slowly: it is the cheapest way there is to make a
;;;; flat picture look deep, and the scrolling games of about 1990 all wanted
;;;; it.  A console or an Amiga had hardware for it (several "playfields", each
;;;; with its own scroll registers).  A VGA has one picture and one start
;;;; address, so its games used one of these:
;;;;
;;;;   - Draw every layer again, each frame, from the back to the front, on
;;;;     the page that is not being shown, and then flip the pages.  It is
;;;;     simple and anything may overlap anything, but it writes every pixel
;;;;     of every layer every frame, which is all a 386 could do.
;;;;   - Keep the layers in separate bands of the screen (sky, hills, road),
;;;;     so that each pixel belongs to one layer and is written once; or go
;;;;     further and move a band with no drawing at all, by changing the
;;;;     start address or the panning part of the way down the screen, timed
;;;;     against the beam (see the "copper" example for that kind of timing).
;;;;   - Move a layer through the palette: draw stripes or stars once in a run
;;;;     of colours and rotate what those colours are.
;;;;   - Put the far layer inside the near one's tiles: a few copies of each
;;;;     tile with the background behind it shifted, chosen as the map moves,
;;;;     while the VGA scrolls the lot (see "tiles").
;;;;
;;;; This is the first, done with some of the economy of the second.  Each
;;;; layer is a skyline: a strip of picture that repeats, kept by columns,
;;;; with the row at which each column begins.  Above that row the layer is
;;;; clear, and nothing is drawn: so there is no testing of every pixel for
;;;; "transparent", which is what makes overlaid layers slow.  And each strip
;;;; is only as tall as the part that can show above the layer in front.
;;;; Mode X suits it: the pixels of one plane in one column are 80 addresses
;;;; apart, straight down, and the sky is filled four pixels to the byte.
;;;; The sprites are drawn last, pixel by pixel, leaving out colour 0.
;;;;
;;;; The frame is drawn on the hidden page and shown by the start address
;;;; (registers 0Ch and 0Dh at port 3D4h), which the VGA takes at the vertical
;;;; retrace.  How far everything has moved comes from the timer chip
;;;; (TIMER-COUNT), not from counting frames, so the speed is the same on a
;;;; slow machine: it just shows fewer frames.
;;;;
;;;;   (doslib-examples:run "parallax")
;;;; Right and Left arrows change the speed, through nought to the other
;;;; direction; Space stops and starts the ground (the birds fly on); Esc
;;;; leaves, and prints the frames a second and the bytes written for a frame,
;;;; beside the 230,400 that three whole layers would be.
;;;;
;;;; QEMU reports a vertical retrace whenever it is asked, so the frames are
;;;; paced by the timer chip as well (WAIT-FRAME: 74 a second at most), and
;;;; it paints its window in its own time: there the flip is not tied to a
;;;; real beam as it is on a monitor.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.parallax
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.parallax)

(defconstant +page+ 19200)              ; addresses in a page: 80 a row, 240 rows
(defconstant +sky-top+ 16)              ; the first row of the picture: the text is above
(defconstant +sky-bottom+ 152)          ; the row after the lowest place sky can show
(defconstant +sky-colour+ 64)           ; the first of the sky's 17 colours, 8 rows each

;;; ---- the mode is the library's (src/modex.lisp): MODE-X sets it, VGA-DISPLAY-START
;;; says which page is shown, from the next retrace, and VGA-PLANES sets the map mask.

(defun set-colours ()
  (loop for (red green blue) in '((30 28 46) (40 38 54) (61 61 63) (50 52 60)   ; mountains, snow
                                  (14 36 16) (18 42 18) (28 50 22) (6 26 12) (12 34 14)  ; hills
                                  (24 54 12) (14 44 8) (38 24 11) (30 18 8)     ; grass, earth
                                  (40 40 42) (50 38 22) (30 20 10)              ; stone, posts
                                  (58 12 10) (63 54 10) (12 24 56) (62 62 62)   ; balloons
                                  (34 22 8) (20 16 12) (4 4 8))                 ; basket, rope, bird
        for index from 16
        do (set-palette index red green blue))
  (dotimes (i 17)                       ; the sky: deep blue down to a pale horizon
    (flet ((between (from to) (round (+ (* from (- 16 i)) (* to i)) 16)))
      (set-palette (+ +sky-colour+ i) (between 8 60) (between 20 52) (between 52 48)))))

;;; ---- the layers
(defstruct layer
  "A strip of picture that repeats every PERIOD pixels, HEIGHT rows tall, whose top row
is row TOP of the screen.  STRIP holds it by columns, HEIGHT bytes each; TOPS is the row
of the strip at which each column begins: above it the layer is clear.  It moves a pixel
for every SLOWNESS the ground moves."
  (strip nil :type (simple-array (unsigned-byte 8) (*)))
  (tops nil :type (simple-array (unsigned-byte 8) (*)))
  (period 0 :type fixnum) (height 0 :type fixnum) (top 0 :type fixnum) (slowness 1))
(defun build-layer (period height top slowness top-of colour-of)
  "TOP-OF is a function of the column, for the row its picture begins at; COLOUR-OF of
the column, the row and that first row."
  (let ((strip (make-array (* period height) :element-type '(unsigned-byte 8)))
        (tops (make-array period :element-type '(unsigned-byte 8))))
    (dotimes (x period)
      (let ((first (max 0 (min (1- height) (funcall top-of x)))))
        (setf (aref tops x) first)
        (loop for row from first below height
              do (setf (aref strip (+ (* x height) row)) (funcall colour-of x row first)))))
    (make-layer :strip strip :tops tops :period period :height height :top top
                :slowness slowness)))
(defun speckle (x y) (mod (+ (* x x 7) (* y 13) (* x y 5)) 11))

(defun make-mountains ()
  "Jagged: three triangle waves added up, with snow on what is high and the slopes that
face left in the light.  Rows 72 to 151 of the screen."
  (labels ((triangle (u) (- (* 4 (abs (- (mod u 1.0) 0.5))) 1))         ; from -1 to 1
           (top-of (x)
             (let ((u (/ x 384.0)))
               (round (+ 38 (* 22 (triangle (* 2 u))) (* 11 (triangle (+ 0.3 (* 5 u))))
                         (* 5 (triangle (+ 0.7 (* 13 u)))))))))
    (build-layer 384 80 72 4 #'top-of
                 (lambda (x row first)
                   (let ((lit (< (top-of (+ x 5)) (top-of (- x 5))))
                         (snow (< (- row first) (+ (floor (- 34 first) 2) (mod x 3)))))
                     (cond ((and snow lit) 18) (snow 19) (lit 17) (t 16)))))))
(defun make-hills ()
  "Rounded, with trees: two sine waves, and a disc standing on them here and there.
Rows 112 to 199."
  (labels ((ground (x)
             (let ((angle (* 2 pi (/ x 256))))
               (round (+ 24 (* 11 (sin (* 2 angle))) (* 5 (sin (+ 1 (* 5 angle))))))))
           (top-of (x)
             (let ((top (ground x)))
               (dolist (tree '(20 75 96 150 200 215) top)
                 (let ((away (abs (- x tree))))
                   (when (<= away 7)
                     (setf top (min top (- (ground tree) 5
                                           (round (sqrt (- 49 (* away away)))))))))))))
    (build-layer 256 88 112 2 #'top-of
                 (lambda (x row first)
                   (declare (ignore first))
                   (let ((ground (ground x)))
                     (cond ((< row ground) (if (< (speckle x row) 4) 24 23))    ; a tree
                           ((< row (+ ground 2)) 22)
                           ((< (speckle x row) 3) 21)
                           (t 20)))))))
(defun make-ground ()
  "Grass and earth with a stone now and then, and a post every 32 pixels to show how
fast it is going.  Rows 184 to 239."
  (flet ((post (x) (< (mod x 32) 3)))
    (build-layer 128 56 184 1
                 (lambda (x) (if (post x) 0 (+ 12 (mod (speckle x 1) 3))))
                 (lambda (x row first)
                   (declare (ignore first))
                   (cond ((and (post x) (< row 14)) (if (= (mod x 32) 2) 31 30))
                         ((< row 19) (if (< (speckle x row) 4) 26 25))
                         ((< (+ (expt (- (mod x 64) 20) 2) (expt (- row (if (< x 64) 34 44)) 2)) 30)
                          29)
                         ((< (speckle x row) 3) 28)
                         (t 27))))))

;;; ---- drawing: every function returns the number of bytes it wrote
(defun draw-sky (page)
  "The sky, as far down as any of it can be seen: a byte is four pixels."
  (declare (type fixnum page) (optimize speed))
  (vga-planes #x0F)
  (let ((vga *vga*))
    (declare (type system-area-pointer vga))
    (loop for row of-type fixnum from +sky-top+ below +sky-bottom+
          for word of-type (unsigned-byte 32)
            = (* #x01010101 (logand 255 (+ +sky-colour+ (ash (- row +sky-top+) -3))))
          do (loop for address of-type fixnum from (+ page (* 80 row)) by 4
                   repeat 20
                   do (setf (sap-ref-32 vga address) word))))
  (* 80 (- +sky-bottom+ +sky-top+)))
(defun draw-layer (page layer offset)
  "LAYER on the page at PAGE, its column OFFSET at the left of the screen: for each plane,
each column of it from the column's first row down."
  (declare (type fixnum page offset) (optimize speed))
  (let ((vga *vga*) (strip (layer-strip layer)) (tops (layer-tops layer))
        (period (layer-period layer)) (height (layer-height layer)) (top (layer-top layer))
        (count 0))
    (declare (type system-area-pointer vga) (type fixnum count))
    (dotimes (plane 4)
      (vga-planes (ash 1 plane))        ; map mask: this plane only
      (let ((column (mod (+ offset plane) period)))
        (declare (type fixnum column))
        (dotimes (x 80)                 ; pixel 4 X + PLANE of the screen
          (let* ((first (aref tops column))
                 (source (+ (* column height) first))
                 (address (+ page (* 80 (+ top first)) x)))
            (declare (type fixnum source address))
            (loop repeat (- height first)
                  do (setf (sap-ref-8 vga address) (aref strip source))
                     (incf source)
                     (incf address 80))
            (incf count (- height first)))
          (incf column 4)
          (when (>= column period) (decf column period)))))
    count))
(defun draw-sprite (page image width height x y)
  "IMAGE, WIDTH by HEIGHT by rows, with its corner at X, Y: colour 0 is left out, and so
is what is off either side of the screen."
  (declare (type fixnum page width height x y)
           (type (simple-array (unsigned-byte 8) (*)) image) (optimize speed))
  (let ((vga *vga*) (count 0))
    (declare (type system-area-pointer vga) (type fixnum count))
    (dotimes (plane 4)
      (vga-planes (ash 1 plane))
      (loop for i of-type fixnum from (mod (- plane x) 4) below width by 4
            do (let ((column (+ x i)))  ; the sprite's columns that are in this plane
                 (when (<= 0 column 319)
                   (let ((address (+ page (* 80 y) (ash column -2))))
                     (declare (type fixnum address))
                     (dotimes (row height)
                       (let ((colour (aref image (+ i (* width row)))))
                         (unless (zerop colour)
                           (setf (sap-ref-8 vga address) colour)
                           (incf count)))
                       (incf address 80)))))))
    count))

;;; ---- the sprites
(defstruct sprite
  "IMAGES are its pictures, shown in turn, each WIDTH by HEIGHT.  It starts at X, Y, flies
at SPEED pixels a second, bobs up and down by BOB, and is as far off as a layer that
moves DEPTH of the ground's speed."
  images width height x y speed bob depth)
(defun balloon-image (colour-1 colour-2)
  "24 by 36: a striped envelope, ropes and a basket."
  (let ((image (make-array (* 24 36) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (y 36 image)
      (dotimes (x 24)
        (setf (aref image (+ x (* 24 y)))
              (cond ((<= (+ (expt (/ (- x 11.5) 11.5) 2) (expt (/ (- y 12) 12.5) 2)) 1)
                     (if (evenp (floor x 4)) colour-1 colour-2))
                    ((and (<= 24 y 29) (or (= x (+ 6 (floor (- y 23) 2)))
                                           (= x (- 17 (floor (- y 23) 2)))))
                     37)
                    ((and (<= 30 y 35) (<= 9 x 14)) 36)
                    (t 0)))))))
(defun picture-image (colour rows)
  "An image from strings: # is COLOUR, anything else clear."
  (let* ((width (length (first rows)))
         (image (make-array (* width (length rows)) :element-type '(unsigned-byte 8))))
    (loop for row in rows for y from 0
          do (dotimes (x width)
               (setf (aref image (+ x (* width y))) (if (char= (char row x) #\#) colour 0))))
    image))
(defun make-sprites ()
  (let ((bird (list (picture-image 38 '("##........##"
                                        ".##......##."
                                        "..##.##.##.."
                                        "...######..."
                                        "....####...."
                                        "............"))
                    (picture-image 38 '("............"
                                        "....####...."
                                        "..########.."
                                        ".###.##.###."
                                        "##...##...##"
                                        "#..........#")))))
    (list (make-sprite :images (list (balloon-image 32 33)) :width 24 :height 36
                       :x 70 :y 34 :speed 6 :bob 5 :depth 0.35)
          (make-sprite :images bird :width 12 :height 6 :x 150 :y 104 :speed 40 :bob 3 :depth 0.6)
          (make-sprite :images bird :width 12 :height 6 :x 168 :y 96 :speed 40 :bob 4 :depth 0.6)
          (make-sprite :images bird :width 12 :height 6 :x 181 :y 110 :speed 40 :bob 3 :depth 0.6)
          (make-sprite :images (list (balloon-image 34 35)) :width 24 :height 36
                       :x 260 :y 76 :speed -5 :bob 8 :depth 0.8))))
(defun draw-sprites (page sprites ground seconds)
  "The sprites where they are when the ground has moved GROUND pixels, SECONDS after
the start.  They go round a track 400 pixels long, 40 of it off each side of the screen."
  (loop for sprite in sprites
        for number from 0
        for images = (sprite-images sprite)
        sum (draw-sprite page
                         (nth (mod (floor (* seconds 6)) (length images)) images)
                         (sprite-width sprite) (sprite-height sprite)
                         (- (mod (round (+ (sprite-x sprite) (* seconds (sprite-speed sprite))
                                           (- (* ground (sprite-depth sprite)))))
                                 400)
                            40)
                         (+ (sprite-y sprite)
                            (round (* (sprite-bob sprite) (sin (+ number (* 2 seconds)))))))))

(defun draw-status (page rate speed bytes)
  ;; two lines of the library's text for planes, at the top of the page
  (mode-x-text 0 0 (format nil "~40A" (format nil "Parallax: ~D frames/s  ground ~D px/s"
                                              rate speed))
               15 0 page)
  (mode-x-text 0 8 (format nil "~40A" (format nil "Bytes drawn a frame: ~D" bytes))
               33 0 page))

(defun parallax ()
  (let ((layers (list (make-mountains) (make-hills) (make-ground)))
        (sprites (make-sprites))
        (frames 0) (clocks 0) (sky-bytes 0) (layer-bytes 0) (sprite-bytes 0))
    (mode-x)
    (unwind-protect
         (let ((shown 0) (speed 120) (moving t)
               (distance 0)             ; how far the ground has moved, in 256ths of a pixel
               (rate 0) (bytes 0) (status 2))   ; pages that have yet to be told the numbers
           (set-colours)
           (loop with start = (timer-count)
                 with last = start
                 with mark = start with mark-frames = 0 with mark-bytes = 0
                 do (when (key-pressed-p)
                      (case (get-key)
                        (#\Escape (return))
                        (#\Space (setf moving (not moving) status 2))
                        (:right (setf speed (min 480 (+ speed 30)) moving t status 2))
                        (:left (setf speed (max -480 (- speed 30)) moving t status 2))))
                    (let ((now (timer-count)))
                      (when moving
                        (incf distance (round (* speed 256 (- now last)) +timer-hz+)))
                      (setf last now clocks (- now start)))
                    (let ((page (* +page+ (- 1 shown))) (ground (floor distance 256)))
                      (incf sky-bytes (draw-sky page))
                      (dolist (layer layers)    ; from the back to the front
                        (incf layer-bytes
                              (draw-layer page layer (floor ground (layer-slowness layer)))))
                      (incf sprite-bytes
                            (draw-sprites page sprites ground (/ clocks (float +timer-hz+))))
                      (when (plusp status)
                        (draw-status page rate (if moving speed 0) bytes)
                        (decf status))
                      (wait-frame)
                      (vga-display-start page)  ; the flip: two registers, at the retrace
                      (setf shown (- 1 shown))
                      (wait-retrace))
                    (incf frames)
                    (let ((elapsed (- (timer-count) mark))
                          (total (+ sky-bytes layer-bytes sprite-bytes)))
                      (when (>= elapsed +timer-hz+)
                        (setf rate (round (* (- frames mark-frames) +timer-hz+) elapsed)
                              bytes (round (- total mark-bytes) (- frames mark-frames))
                              mark (timer-count) mark-frames frames mark-bytes total
                              status 2)))))
      (video-mode 3))
    (when (and (plusp frames) (plusp clocks))
      (format t "~&~D frames, ~D a second, each drawn out of sight and flipped.~%"
              frames (round (* frames +timer-hz+) clocks))
      (format t "Bytes written for a frame: ~D of sky (four pixels each), ~D of layers,~%"
              (round sky-bytes frames) (round layer-bytes frames))
      (format t "~D of sprites: ~D in all.  Three whole layers would be 230,400.~%"
              (round sprite-bytes frames)
              (round (+ sky-bytes layer-bytes sprite-bytes) frames)))))

(defun main ()
  "Parallax: three layers at different speeds and sprites, redrawn and page-flipped."
  (parallax)
  (values))

(doslib-examples:define-example "parallax" 'main
  :category :graphics
  :description "Parallax: three layers at different speeds and sprites, redrawn and page-flipped.")
