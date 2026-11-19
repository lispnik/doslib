;;;; Page flipping in Mode X.  With the VGA's 256-colour memory unchained
;;;; into four planes, 320x240 takes only 19,200 addresses of the 65,536 at
;;;; A0000h, so there is room for more than one whole picture: a page.  The
;;;; CRT controller's start address (registers 0Ch and 0Dh at port 3D4h)
;;;; says where in that memory the display begins, and the VGA takes a new
;;;; value at the vertical retrace.  So each frame is drawn on the page that
;;;; is not being shown and then made visible by writing two registers: no
;;;; copying, and never a half-drawn picture on the screen.
;;;;
;;;;   (doslib-examples:run "flip")
;;;; Space changes to drawing straight on the page being shown, and back:
;;;; then the clearing and redrawing are seen as flicker and tearing.  (An
;;;; emulator that paints its window now and then shows this less evenly
;;;; than a monitor does, but it shows.)  Esc leaves.
;;;;
;;;; A plane is chosen with the sequencer's map mask (register 2 at 3C4h),
;;;; one bit a plane; pixel x is in plane x mod 4 at address 80y + x/4.  With
;;;; several bits set one byte written is several pixels, which is how the
;;;; page is cleared and the middles of the shapes are filled here.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.flip
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.flip)

(defconstant +page+ 19200)              ; addresses in a page: 80 a row, 240 rows

;;; ---- the mode is the library's (src/modex.lisp): MODE-X sets it, VGA-DISPLAY-START
;;; says which page is shown, from the next retrace, and VGA-PLANES sets the map mask.
;;;
;;; The vertical retrace is the clock: 60 frames a second in this mode.  But some
;;; emulators (QEMU is one) report a retrace whenever they are asked, so the frames are
;;; paced by the timer chip too, with the library's WAIT-FRAME: at most 74 a second, and
;;; a monitor's own rate where the retrace is real.

;;; ---- drawing in planes
(defun clear-page (start colour)
  (declare (type fixnum start) (type (unsigned-byte 8) colour) (optimize speed))
  (vga-planes #x0F)                     ; four pixels to the byte
  (let ((vga *vga*) (word (* colour #x01010101)))
    (declare (type system-area-pointer vga))
    (loop for i of-type fixnum from start below (+ start +page+) by 4
          do (setf (sap-ref-32 vga i) word))))
(defun span (start x1 x2 y colour)
  "Row Y of the page at START from X1 to X2, clipped: the ends a plane or
three at a time, the middle four pixels to the byte."
  (declare (type fixnum start x1 x2 y) (type (unsigned-byte 8) colour) (optimize speed))
  (let ((x1 (max x1 0)) (x2 (min x2 319)) (vga *vga*))
    (declare (type system-area-pointer vga))
    (when (and (<= x1 x2) (<= 0 y 239))
      (let* ((row (+ start (* 80 y))) (first (+ row (ash x1 -2))) (last (+ row (ash x2 -2)))
             (left (logand #x0F (ash #x0F (logand x1 3))))
             (right (ash #x0F (- (logand x2 3) 3))))
        (declare (type fixnum row first last left right))
        (cond ((= first last)
               (vga-planes (logand left right)) (setf (sap-ref-8 vga first) colour))
              (t (vga-planes left) (setf (sap-ref-8 vga first) colour)
                 (vga-planes right) (setf (sap-ref-8 vga last) colour)
                 (when (> last (1+ first))
                   (vga-planes #x0F)
                   (loop for i of-type fixnum from (1+ first) below last
                         do (setf (sap-ref-8 vga i) colour)))))))))
(defun disc (start x y radius colour)
  (loop for dy from (- radius) to radius
        for half = (isqrt (- (* radius radius) (* dy dy)))
        do (span start (- x half) (+ x half) (+ y dy) colour)))
(defvar *left* (make-array 240 :element-type 'fixnum))
(defvar *right* (make-array 240 :element-type 'fixnum))
(defun polygon (start points colour)
  "A filled convex polygon; POINTS is a list of (x . y)."
  (fill *left* 1000) (fill *right* -1000)
  (flet ((edge (x y)
           (when (<= 0 y 239)
             (setf (aref *left* y) (min x (aref *left* y))
                   (aref *right* y) (max x (aref *right* y))))))
    (loop for ((x1 . y1) (x2 . y2)) on (append points (list (first points)))
          while x2
          do (when (> y1 y2) (rotatef x1 x2) (rotatef y1 y2))
             (edge x1 y1) (edge x2 y2)
             (loop for y from (1+ y1) below y2
                   do (edge (+ x1 (round (* (- x2 x1) (- y y1)) (- y2 y1))) y))))
  (dotimes (y 240)
    (when (<= (aref *left* y) (aref *right* y))
      (span start (aref *left* y) (aref *right* y) y colour))))

;;; ---- the animation
(defstruct ball x y dx dy radius colour)
(defun make-balls ()
  (loop for (x y dx dy radius colour) in '((60 60 3 2 22 40) (200 100 -2 3 16 44)
                                           (120 180 4 -1 12 34) (260 200 -3 -2 18 48)
                                           (90 130 2 -4 9 52) (240 50 -1 2 14 37))
        collect (make-ball :x x :y y :dx dx :dy dy :radius radius :colour colour)))
(defun move-ball (ball)
  (let ((radius (ball-radius ball)))
    (unless (<= radius (+ (ball-x ball) (ball-dx ball)) (- 319 radius))
      (setf (ball-dx ball) (- (ball-dx ball))))
    (unless (<= (+ 17 radius) (+ (ball-y ball) (ball-dy ball)) (- 239 radius))
      (setf (ball-dy ball) (- (ball-dy ball))))
    (incf (ball-x ball) (ball-dx ball))
    (incf (ball-y ball) (ball-dy ball))))
(defun draw-frame (start frame balls)
  "The whole picture, from nothing, on the page at START."
  (clear-page start 1)
  (let ((x (let ((phase (mod (* 6 frame) 608))) (if (< phase 304) phase (- 608 phase)))))
    (dotimes (y 224) (span start x (+ x 15) (+ y 16) 14)))        ; a bar that sweeps across
  (let ((angle (* frame 0.04)))                                     ; a square that turns
    (polygon start
             (loop for corner below 4
                   for a = (+ angle (* corner (/ pi 2)))
                   collect (cons (round (+ 160 (* 60 (cos a)))) (round (+ 128 (* 60 (sin a))))))
             4))
  (dolist (ball balls)
    (disc start (ball-x ball) (ball-y ball) (ball-radius ball) (ball-colour ball))))
(defun draw-status (start page direct rate)
  ;; two lines of the library's text for planes, at the top of the page
  (mode-x-text 0 0 (format nil "~40A" (format nil "Page ~D is shown   ~D frames a second"
                                              page rate))
               15 0 start)
  (mode-x-text 0 8 (format nil "~40A" (if direct
                                          "Drawing on the page shown   (Space)"
                                          "Drawing on the hidden page  (Space)"))
               (if direct 12 10) 0 start))

(defun flip ()
  (let ((shown 0) (direct nil) (balls (make-balls)) (rate 0)
        (flipped 0) (flipped-ticks 0) (drawn 0) (drawn-ticks 0))
    (mode-x)
    (unwind-protect
         (loop with frames = 0 with mark = (ticks)
               for frame from 0
               do (when (key-pressed-p)
                    (let ((key (get-key)))
                      (cond ((eql key #\Escape) (return))
                            ((eql key #\Space) (setf direct (not direct))))))
                  (let ((page (if direct shown (- 1 shown))))
                    (draw-frame (* page +page+) frame balls)
                    (draw-status (* page +page+) (if direct shown page) direct rate)
                    (wait-frame)
                    (unless direct      ; the flip: two registers, at the retrace
                      (vga-display-start (* page +page+))
                      (setf shown page))
                    (wait-retrace))
                  (mapc #'move-ball balls)
                  (incf frames)
                  (let ((elapsed (- (ticks) mark)))
                    (when (>= elapsed 18)
                      (setf rate (round (* frames 18.2) elapsed))
                      (if direct
                          (setf drawn (+ drawn frames) drawn-ticks (+ drawn-ticks elapsed))
                          (setf flipped (+ flipped frames) flipped-ticks (+ flipped-ticks elapsed)))
                      (setf frames 0 mark (ticks)))))
      (video-mode 3))
    (when (plusp flipped-ticks)
      (format t "~&Flipping pages: ~D frames a second, each drawn out of sight.~%"
              (round (* flipped 18.2) flipped-ticks)))
    (when (plusp drawn-ticks)
      (format t "~&Drawing on the page shown: ~D frames a second, drawn in view.~%"
              (round (* drawn 18.2) drawn-ticks)))))

(defun main ()
  "Mode X page flipping: draw out of sight, then change the start address."
  (flip)
  (values))

(doslib-examples:define-example "flip" 'main
  :category :graphics
  :description "Mode X page flipping: draw out of sight, then change the start address.")
