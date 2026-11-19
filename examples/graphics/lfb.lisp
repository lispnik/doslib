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
  (:export #:main #:mode-info #:map-physical #:unmap-physical))
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
  (with-dos-memory (segment 256)
    (let ((block (linear segment)))
      (dotimes (i 256) (setf (sap-ref-8 (int-sap block) i) 0))
      (let ((ax (real-int #x10 :ax #x4F01 :cx mode :es segment :di 0)))
        (when (and (= ax #x004F)                        ; the call is known, and worked
                   (logbitp 7 (peek16 block))           ; there is a linear framebuffer
                   (plusp (peek32 (+ block #x28))))
          (values (peek32 (+ block #x28))
                  (peek16 (+ block #x10))
                  (peek16 (+ block 4))))))))

(defun map-physical (physical size)
  "Make SIZE bytes of device memory at PHYSICAL addressable (DPMI 0800h).
Returns the address, or NIL if the host would not."
  ;; INT86 does not carry SI, which this call needs: so, a stub of its own.
  (let* ((stub (machine-code #x53 #x56 #x57              ; push ebx / esi / edi
                             #x8B #x4C #x24 #x10         ; mov ecx,[esp+16]   the address
                             #x89 #xCB #xC1 #xEB #x10    ; mov ebx,ecx / shr ebx,16
                             #x8B #x7C #x24 #x14         ; mov edi,[esp+20]   the size
                             #x89 #xFE #xC1 #xEE #x10    ; mov esi,edi / shr esi,16
                             #xB8 #x00 #x08 #x00 #x00    ; mov eax,0800h
                             #xCD #x31                   ; int 31h
                             #x72 10                     ; jc failed
                             #xC1 #xE3 #x10 #x66 #x89 #xCB   ; shl ebx,16 / mov bx,cx
                             #x89 #xD8 #xEB #x02         ; mov eax,ebx / jmp done
                             #x31 #xC0                   ; failed: xor eax,eax
                             #x5F #x5E #x5B #xC3))       ; done: pop edi / esi / ebx / ret
         (address (alien-funcall (sap-alien stub (function unsigned unsigned unsigned))
                                 physical size)))
    (unless (zerop address) address)))
(defun unmap-physical (address)
  "Take away a mapping that MAP-PHYSICAL made (DPMI 0801h)."
  (int86 #x31 :ax #x0801 :bx (ldb (byte 16 16) address) :cx (ldb (byte 16 0) address)))

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
  "STRING in the BIOS's 8 by 8 characters (they are in the ROM, at FFA6Eh),
white on black, into PICTURE."
  (let ((font (int-sap #xFFA6E)))
    (dotimes (row 10)
      (fill picture 0 :start (+ x -2 (* +width+ (+ y row -1)))
                      :end (+ x 2 (* 8 (length string)) (* +width+ (+ y row -1)))))
    (loop for c across string for cx from x by 8
          do (dotimes (row 8)
               (let ((bits (sap-ref-8 font (+ (* 8 (logand (char-code c) 127)) row))))
                 (dotimes (column 8)
                   (when (logbitp (- 7 column) bits)
                     (setf (aref picture (+ cx column (* +width+ (+ y row)))) 128))))))))

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
                      (unless (= (word16 (int86 #x10 :ax #x4F02
                                                     :bx (logior +mode+ (if linear #x4000 0)
                                                                 (if keep #x8000 0))))
                                 #x004F)
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
