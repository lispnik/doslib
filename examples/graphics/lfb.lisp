;;;; The VESA linear framebuffer: 640x480 in 256 colours with the whole
;;;; picture in one piece of memory, and no bank switching.
;;;;
;;;;   (doslib-examples:run "lfb")
;;;; Space changes between the two ways of putting the picture on the screen
;;;; (watch the frames a second), Esc leaves.  Where the VESA BIOS has no
;;;; linear framebuffer for mode 101h it says so and returns.
;;;;
;;;; A 640x480 picture is 307,200 bytes, and the VGA has 64K of address
;;;; space at A0000h.  The first answer (see the "vesa" example) was a window:
;;;; 64K of the picture shows there at a time, and the program asks the BIOS
;;;; to move the window -- five times to draw a whole screen.  Version 2 of
;;;; the VESA BIOS extension gave the better answer.  The card's memory is
;;;; also somewhere high in the machine's physical address space, all of it
;;;; in a row, and the BIOS says where; a 32-bit program writes there.
;;;;
;;;; Three steps:
;;;;   1. INT 10h, AX=4F01h, CX=mode, ES:DI -> 256 bytes.  The BIOS fills in
;;;;      what it knows about the mode: bit 7 of the first word says there
;;;;      is a linear framebuffer, the dword at 28h is its physical address,
;;;;      the word at 10h is the bytes in a line.  ES:DI is a real-mode
;;;;      pointer, so the block is in DOS memory and the call is REAL-INT.
;;;;   2. INT 10h, AX=4F02h, BX=mode + 4000h: set the mode, with bit 14 to
;;;;      say the linear framebuffer is wanted.
;;;;   3. A physical address is not one a protected-mode program can use:
;;;;      its addresses go through the page tables of the DPMI host.  DPMI
;;;;      function 0800h (INT 31h, BX:CX = physical address, SI:DI = size)
;;;;      makes the host map the device's memory and returns the address it
;;;;      is at -- which here, where the address space is flat and starts at
;;;;      0, is an address like any other.  Function 0801h takes it away
;;;;      again.
;;;;
;;;; The picture is worked out in a Lisp vector and copied to the screen
;;;; whole each frame.  Before the animation starts the program times that
;;;; copy both ways: through the linear framebuffer (one block move), and
;;;; through the window at A0000h (five moves and five calls of the BIOS).
;;;; The figures are on the screen, and printed at the end.  A mode is set
;;;; for one way or the other -- the BIOS will not move the window in a mode
;;;; that was set with bit 14 -- so the program sets the mode again to
;;;; change, with bit 15 as well, which asks for the picture to be kept.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.lfb
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:mode-info))
(in-package #:doslib-examples.lfb)

(defconstant +mode+ #x101)
(defconstant +width+ 640)
(defconstant +height+ 480)
(defconstant +bytes+ (* +width+ +height+))
(deftype picture () '(simple-array (unsigned-byte 8) (*)))

;;; ---- the three steps
(defun mode-info (mode)
  "What the VESA BIOS says of MODE: the physical address of its linear
framebuffer, the bytes in a line, and how many kilobytes one step of the
window at A0000h moves it.  NIL if there is no such mode or no framebuffer."
  (let ((info (vesa-mode-info mode)))                   ; step 1 is the library's
    (when (and info (getf info :framebuffer))
      (values (getf info :framebuffer) (getf info :line-bytes) (getf info :granularity)))))
;;; (Steps 2 and 3 are the library's too: VESA-SET-MODE with :LINEAR, src/video.lisp, and
;;; MAP-PHYSICAL and UNMAP-PHYSICAL, src/memory.lisp, which has the stub that calls DPMI.)

;;; ---- the picture: waves across, down and aslant, added up
(defvar *wave* (let ((wave (make-array 1024 :element-type '(unsigned-byte 8))))
                 (dotimes (i 1024 wave)
                   (setf (aref wave i) (round (* 63.5 (1+ (sin (* 2 pi (/ i 1024))))))))))
(defvar *across* (make-array +width+ :element-type '(unsigned-byte 16)))

(defun draw-waves (picture time)
  (declare (type picture picture) (type (unsigned-byte 16) time) (optimize speed (safety 0)))
  (let ((wave *wave*) (across *across*)
        (i 0))
    (declare (type (simple-array (unsigned-byte 8) (1024)) wave)
             (type (simple-array (unsigned-byte 16) (#.+width+)) across)
             (fixnum i))
    (dotimes (x +width+)
      (setf (aref across x) (+ (aref wave (logand (+ (* 2 x) (* 5 time)) 1023))
                               (aref wave (logand (- (* 3 x) (* 3 time)) 1023)))))
    (dotimes (y +height+)
      (let ((down (+ (aref wave (logand (+ (* 3 y) (* 4 time)) 1023))
                     (aref wave (logand (- y (* 7 time)) 1023))))
            (aslant (+ y (* 6 time))))
        (declare (type (unsigned-byte 24) down aslant))
        (dotimes (x +width+)
          (setf (aref picture i)
                (logand 255 (+ (aref across x) down (aref wave (logand (+ x aslant) 1023)))))
          (incf i))))))

(defun set-colours ()
  "Colours that go round: 0 is black, 128 white, blue and violet between."
  (dotimes (i 256)
    (let ((s (abs (sin (* pi (/ i 256))))))
      (set-palette i (round (* 63 s s)) (round (* 63 s s s s)) (round (* 63 (sqrt s)))))))

(defun draw-text (picture x y string)
  "STRING in the BIOS's 8 by 8 characters, white on a black bar, into PICTURE."
  (dotimes (row 10)
    (fill picture 0 :start (+ x -2 (* +width+ (+ y row -1)))
                    :end (+ x 2 (* 8 (length string)) (* +width+ (+ y row -1)))))
  (with-pinned-objects (picture)        ; the library's letters, with nothing behind them
    (draw-glyphs (vector-sap picture) (+ x (* +width+ y)) +width+ string 128 nil)))

;;; ---- to the screen, both ways
(defun show-linear (picture screen line-bytes)
  "Copy PICTURE to the linear framebuffer at SCREEN."
  (declare (type picture picture) (type system-area-pointer screen) (fixnum line-bytes))
  (with-pinned-objects (picture)
    (if (= line-bytes +width+)
        (sb-kernel:system-area-ub8-copy (vector-sap picture) 0 screen 0 +bytes+)
        (dotimes (y +height+)           ; a card may keep its lines further apart
          (sb-kernel:system-area-ub8-copy (vector-sap picture) (* y +width+)
                                          screen (* y line-bytes) +width+)))))
(defun show-banked (picture step)
  "Copy PICTURE to the screen through the window at A0000h, 64K at a time.
STEP is how many of the BIOS's units 64K is."
  (declare (type picture picture) (fixnum step))
  (with-pinned-objects (picture)
    (loop for bank fixnum from 0
          for start fixnum from 0 below +bytes+ by 65536
          do (vesa-set-bank (* bank step))
             (sb-kernel:system-area-ub8-copy (vector-sap picture) start *vga* 0
                                             (min 65536 (- +bytes+ start))))))

(defun milliseconds-each (function)
  "How long one call of FUNCTION takes, from calling it for half a second."
  (wait-tick)
  (let ((start (ticks)) (count 0))
    (loop while (< (- (ticks) start) 9)
          do (funcall function) (incf count))
    (/ (* 9 54.925) count)))

(defun wait-for-retrace ()
  "As WAIT-RETRACE, but not for ever: a VESA mode need not have the VGA's status port."
  (loop repeat 20000 while (logbitp 3 (inb #x3DA)))
  (loop repeat 20000 until (logbitp 3 (inb #x3DA))))

(defun main ()
  "640x480 in 256 colours through the VESA linear framebuffer."
  (multiple-value-bind (physical line-bytes granularity) (mode-info +mode+)
    (unless physical
      (format t "~&The VESA BIOS here has no linear framebuffer for mode ~Xh.~%" +mode+)
      (return-from main (values)))
    (let* ((size (* 4096 (ceiling (* line-bytes +height+) 4096)))
           (address (map-physical physical size))
           ;; the window can be used for the comparison if a line is 640
           ;; bytes and 64K is a whole number of the BIOS's steps
           (step (and (= line-bytes +width+) (plusp granularity) (zerop (mod 64 granularity))
                      (floor 64 granularity)))
           (picture (make-array +bytes+ :element-type '(unsigned-byte 8)))
           (linear-ms nil) (banked-ms nil) (rate 0) (frames 0))
      (unless address
        (format t "~&The DPMI host would not map the framebuffer at ~8,'0Xh.~%" physical)
        (return-from main (values)))
      (unwind-protect
           (let ((screen (int-sap address)) (linear t) (time 0.0)
                 (counted 0) (counted-from (ticks)))
             (flet ((set-mode (linear keep)
                      (unless (vesa-set-mode +mode+ :linear linear :keep keep)
                        (error "The VESA BIOS would not set mode ~Xh" +mode+))
                      (set-colours)))
               (draw-waves picture 0)
               (draw-text picture 8 8 "Timing a full screen each way...")
               (when step
                 (set-mode nil nil)
                 (if (= (word16 (vesa-set-bank 0)) #x004F)
                     (setf banked-ms (milliseconds-each (lambda () (show-banked picture step))))
                     (setf step nil)))  ; no window to move, then
               (set-mode t nil)
               (setf linear-ms (milliseconds-each
                                (lambda () (show-linear picture screen line-bytes))))
               (loop
                 (when (key-pressed-p)
                   (case (get-key)
                     ((#\Escape #\q #\Q) (return))
                     (#\Space (when step
                                (setf linear (not linear))
                                (set-mode linear t)))))
                 (draw-waves picture (mod (floor time) 4096))
                 (draw-text picture 8 8
                            (format nil "VESA mode ~Xh, ~Dx~D: framebuffer at ~8,'0Xh, ~
                                         mapped at ~8,'0Xh"
                                    +mode+ +width+ +height+ physical address))
                 (draw-text picture 8 20
                            (format nil "~:D bytes a screen: linear ~,2F ms~
                                         ~@[, through the window at A0000h ~,2F ms~]"
                                    +bytes+ linear-ms banked-ms))
                 (draw-text picture 8 (- +height+ 16)
                            (format nil "Now ~:[through the window~;linear~]: ~D frames a ~
                                         second.   ~:[~;Space: the other way   ~]Esc: leave"
                                    linear rate step))
                 (wait-for-retrace)
                 (if linear
                     (show-linear picture screen line-bytes)
                     (show-banked picture step))
                 ;; the waves move by the clock, not by the frame: as fast
                 ;; on a machine that draws 20 frames a second as on one
                 ;; that draws 200
                 (incf time (if (plusp rate) (/ 60.0 rate) 1.0))
                 (incf frames) (incf counted)
                 (when (>= (- (ticks) counted-from) 18)
                   (setf rate (round (* counted 18.2) (- (ticks) counted-from))
                         counted 0 counted-from (ticks))))))
        (video-mode 3)
        (unmap-physical address))
      (format t "~&VESA mode ~Xh: linear framebuffer at ~8,'0Xh, mapped at ~8,'0Xh.~%"
              +mode+ physical address)
      (format t "A full screen (~:D bytes): ~,2F ms linear (~,1F MB a second)"
              +bytes+ linear-ms (/ +bytes+ linear-ms 1000.0))
      (if banked-ms
          (format t ",~%~,2F ms through the 64K window with ~D bank switches: ~,1F times as long.~%"
                  banked-ms (ceiling +bytes+ 65536) (/ banked-ms linear-ms))
          (format t ".~%"))
      (format t "~D frames drawn.~%" frames)))
  (values))

(doslib-examples:define-example "lfb" 'main
  :category :graphics
  :description "The VESA linear framebuffer: 640x480 with no bank switching.")
