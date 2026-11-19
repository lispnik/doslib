;;;; Copper bars: more colours on the screen than the picture has, by
;;;; changing one palette entry on every scanline as the screen is drawn.
;;;;
;;;;   (doslib-examples:run "copper")
;;;; F changes to the same picture faked with 200 palette entries (and
;;;; back); Esc leaves.
;;;;
;;;; The picture is mode 13h, 320x200, drawn in six colours.  Colour 0 is
;;;; the background -- and what colour 0 looks like is decided again for
;;;; each of the 400 scanlines the VGA draws (it draws every row of mode 13h
;;;; twice).  The monitor's beam goes across the screen line by line, 70
;;;; times a second; the VGA looks a pixel's colour up in its palette as the
;;;; beam reaches it; so if the palette is different by the time the beam
;;;; gets to the next line, that line is a different colour.  The name is
;;;; from the Amiga, whose "copper" chip did this by itself.  On a PC the
;;;; program must watch the beam.
;;;;
;;;; The VGA's input status register, port 3DAh, says where the beam is:
;;;; bit 3 is set during the vertical retrace (the beam going back to the
;;;; top), bit 0 whenever nothing is being displayed -- between lines, and
;;;; between frames.  So: wait for a vertical retrace to end, and then each
;;;; time bit 0 comes on a line has been drawn; while it is on, there are
;;;; six millionths of a second in which to write the next line's colour to
;;;; the palette (ports 3C8h and 3C9h, four bytes) unseen.
;;;;
;;;; That is done by seventy bytes of machine code, assembled below, which is
;;;; given a table of red, green and blue for the 400 lines of one frame and
;;;; returns when the frame has been drawn.  Interrupts are off meanwhile:
;;;; a timer tick in the middle would cost a few lines.  Lisp makes the
;;;; tables beforehand, for every frame of the bars' motion (140 frames of
;;;; 1,200 bytes), because between the last line and the next retrace there
;;;; is less than half a millisecond to do anything in.
;;;;
;;;; What to expect.  On a real VGA, and in emulators that draw the screen a
;;;; line at a time (DOSBox, 86Box, PCem): a dark blue sky that gets lighter
;;;; down the screen, with three shaded bars -- red, green and blue --
;;;; gliding up and down behind the lettering, passing in front of and
;;;; behind one another: some 170 colours from six palette entries.
;;;;
;;;; Under QEMU: none of that, for two reasons.  As QEMU is usually run, its
;;;; status port keeps no time at all: both bits simply change at every
;;;; read, so that a program waiting for a retrace does not wait for ever.
;;;; The program counts the lines the port shows between two retraces (a
;;;; real VGA shows 400; this QEMU shows none) and times ten frames, and
;;;; when they take no time at all it knows there is no beam to follow.
;;;; (QEMU can be started with -vga std,retrace=precise, and then the port
;;;; does keep time, 70 frames a second.)  And either way QEMU paints its whole
;;;; window at one moment, some 30 times a second, with whatever the palette
;;;; holds at that moment: the background is one flat colour, which
;;;; flickers through the colours of the table.
;;;;
;;;; That is why F is there, and why the program starts that way when the
;;;; port keeps no time.  It gives every row of the picture a palette entry
;;;; of its own (16 to 215) and moves the bars by changing those, between
;;;; frames, which any emulator shows.  It is the same picture at half the
;;;; detail -- 200 steps, not 400 -- and shows what the real thing looks
;;;; like; but it spends 200 palette entries to do what the real thing does
;;;; with one.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.copper
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.copper)

(defconstant +lines+ 400 "Scanlines in a frame of mode 13h.")
(defconstant +frames+ 140 "Frames in one round of the bars: two seconds.")
(defconstant +entry-bytes+ (* 3 +lines+))

;;; ---- the machine code
(defun copper-code ()
  "void copper(table, lines-1, colour): wait for the top of the frame, then
give COLOUR the next red, green and blue from TABLE before every scanline."
  (machine-code
   #x56 #x53                            ; push esi / push ebx
   #x8B #x74 #x24 #x0C                  ; mov esi,[esp+12]     the table
   #x8B #x4C #x24 #x10                  ; mov ecx,[esp+16]     lines after the first
   #x8B #x5C #x24 #x14                  ; mov ebx,[esp+20]     the palette entry
   #xBA #xDA #x03 #x00 #x00             ; mov edx,3DAh
   #xFA                                 ; cli
   #xEC #xA8 #x08 #x74 #xFB             ; in al,dx / test al,8 / jz: until the retrace starts
   #xEC #xA8 #x08 #x75 #xFB             ;    ... jnz: until it ends
   #xB2 #xC8 #x88 #xD8 #xEE             ; mov dl,C8h / mov al,bl / out dx,al   which entry
   #x42 #xAC #xEE #xAC #xEE #xAC #xEE   ; inc edx / 3 x (lodsb / out dx,al)    line 0's colour
   #xB2 #xDA                            ; mov dl,DAh
   #xEC #xA8 #x01 #x75 #xFB             ; in al,dx / test al,1 / jnz: until line 0 is being drawn
   ;; each line:
   #xEC #xA8 #x08 #x75 #x1A             ; in al,dx / test al,8 / jnz out: a retrace? give up
   #xA8 #x01 #x74 #xF7                  ; test al,1 / jz: until the line is done
   #xB2 #xC8 #x88 #xD8 #xEE             ; the entry, and the next line's colour, as above
   #x42 #xAC #xEE #xAC #xEE #xAC #xEE
   #xB2 #xDA
   #xEC #xA8 #x01 #x75 #xFB             ; in al,dx / test al,1 / jnz: until the next line starts
   #x49 #x75 #xE1                       ; dec ecx / jnz each line
   #xFB #x5B #x5E #xC3))                ; out: sti / pop ebx / pop esi / ret

(defun count-code ()
  "unsigned count(void): how many times bit 0 of the status port comes on
between one vertical retrace and the next."
  (machine-code
   #xBA #xDA #x03 #x00 #x00             ; mov edx,3DAh
   #x31 #xC9                            ; xor ecx,ecx
   #xFA                                 ; cli
   #xEC #xA8 #x08 #x75 #xFB             ; until out of a retrace
   #xEC #xA8 #x08 #x74 #xFB             ; until one starts
   #xEC #xA8 #x08 #x75 #xFB             ; until it ends
   #xEC #xA8 #x08 #x75 #x10             ; shown: in al,dx / test al,8 / jnz done
   #xA8 #x01 #x75 #xF7                  ;        test al,1 / jnz shown
   #xEC #xA8 #x08 #x75 #x07             ; blank: in al,dx / test al,8 / jnz done
   #xA8 #x01 #x74 #xF7                  ;        test al,1 / jz blank
   #x41 #xEB #xEB                       ; inc ecx / jmp shown
   #xFB #x89 #xC8 #xC3))                ; done: sti / mov eax,ecx / ret

(defun retrace-signals-p ()
  "Does the status port say anything?  (The machine code waits on it for ever.)"
  (and (loop repeat 300000 thereis (logbitp 3 (inb #x3DA)))
       (loop repeat 300000 thereis (not (logbitp 3 (inb #x3DA))))
       (loop repeat 300000 thereis (logbitp 0 (inb #x3DA)))
       (loop repeat 300000 thereis (not (logbitp 0 (inb #x3DA))))))

;;; ---- the colours of every line of every frame
(defparameter *bars* '((63 12 8) (10 63 14) (16 28 63)) "Red, green, blue: at their brightest.")
(defconstant +half+ 22 "Half the height of a bar, in scanlines.")

(defun make-tables (tables)
  "Fill TABLES (+frames+ times +lines+ times 3 bytes).  Returns the most
different colours that any one frame has."
  (let ((most 0) (seen (make-hash-table)) (count (length *bars*)))
    (dotimes (frame +frames+ most)
      (let ((table (sap+ tables (* frame +entry-bytes+))))
        ;; the sky
        (dotimes (line +lines+)
          (setf (sap-ref-8 table (* 3 line)) (floor line 100)
                (sap-ref-8 table (+ (* 3 line) 1)) (floor line 36)
                (sap-ref-8 table (+ (* 3 line) 2)) (+ 6 (floor line 11))))
        ;; the bars go round a circle seen edge-on: the furthest is drawn first
        (let ((bars (loop for colour in *bars* for k from 0
                          for angle = (* 2 pi (+ (/ frame +frames+) (/ k count)))
                          collect (list (cos angle) (+ 150 (round (* 110 (sin angle)))) colour))))
          (loop for (nil middle (r g b)) in (sort bars #'< :key #'first)
                do (loop for d from (- +half+) to +half+
                         for line = (+ middle d)
                         for shade = (cos (* pi (/ d (* 2 (1+ +half+)))))
                         for light = (if (minusp d) (* 14 shade) 0)     ; lit from above
                         do (when (< -1 line +lines+)
                              (flet ((mix (c) (min 63 (round (+ (* c shade) light)))))
                                (setf (sap-ref-8 table (* 3 line)) (mix r)
                                      (sap-ref-8 table (+ (* 3 line) 1)) (mix g)
                                      (sap-ref-8 table (+ (* 3 line) 2)) (mix b)))))))
        (clrhash seen)
        (dotimes (line +lines+)
          (setf (gethash (logand (sap-ref-32 table (* 3 line)) #xFFFFFF) seen) t))
        (setf most (max most (hash-table-count seen)))))))

;;; ---- the picture: six colours, kept in a vector
(defvar *picture* (make-array 64000 :element-type '(unsigned-byte 8)))
(defun box (x y width height colour)
  (dotimes (j height)
    (fill *picture* colour :start (+ x (* 320 (+ y j))) :end (+ x width (* 320 (+ y j))))))
(defun draw-text (x y string colour &optional (scale 1))
  "STRING in the BIOS's 8 by 8 characters (they are in the ROM, at FFA6Eh), SCALE times as big."
  (let ((font (int-sap #xFFA6E)))
    (loop for c across string for cx from x by (* 8 scale)
          do (dotimes (row 8)
               (let ((bits (sap-ref-8 font (+ (* 8 (logand (char-code c) 127)) row))))
                 (dotimes (column 8)
                   (when (logbitp (- 7 column) bits)
                     (box (+ cx (* scale column)) (+ y (* scale row)) scale scale colour))))))))
(defun draw-picture (colours faked timed)
  (fill *picture* 0)
  (dolist (x '(10 294))                 ; two pillars
    (box x 0 16 200 4) (box x 0 2 200 1) (box (+ x 14) 0 2 200 2))
  (draw-text 43 53 "DOSLIB" 2 5)        ; a shadow
  (draw-text 40 50 "DOSLIB" 1 5)
  (draw-text 72 104 "COPPER BARS" 3 2)
  (cond (faked
         (draw-text 32 166 "Faked with 200 palette entries" 5)
         (draw-text 32 178 (if timed "F: the real way" "(no beam to follow here)") 5)
         (draw-text 32 190 (if timed "Esc: leave" "F: the real way   Esc: leave") 5))
        (t
         (draw-text 32 166 (format nil "~D colours, 6 palette entries:" colours) 5)
         (draw-text 32 178 "one is set on every scanline" 5)
         (draw-text 32 190 "F: fake it   Esc: leave" 5))))
(defun show-picture (faked)
  "The picture to the screen; when FAKED, the background of row Y is colour 16+Y."
  (let ((picture *picture*))
    (declare (type (simple-array (unsigned-byte 8) (64000)) picture) (optimize speed))
    (dotimes (y 200)
      (dotimes (x 320)
        (let ((colour (aref picture (+ x (* 320 y)))))
          (setf (sap-ref-8 *vga* (+ x (* 320 y)))
                (if (and faked (zerop colour)) (+ 16 y) colour)))))))
(defun set-colours ()
  (set-palette 0 0 0 0)
  (set-palette 1 63 63 63) (set-palette 2 0 0 0) (set-palette 3 63 56 12)
  (set-palette 4 34 34 38) (set-palette 5 40 60 63))

(defun fake-frame (table)
  "Between frames, give palette entries 16 to 215 the colours of every other line of TABLE."
  (declare (type system-area-pointer table) (optimize speed))
  (wait-retrace)
  (outb #x3C8 16)
  (dotimes (row 200)
    (dotimes (i 3) (outb #x3C9 (sap-ref-8 table (+ (* 6 row) i))))))

(defun main ()
  "Copper bars in mode 13h: one palette entry, a different colour on every scanline."
  (unless (retrace-signals-p)
    (format t "~&The display's status port (3DAh) does not show the retrace: no VGA?~%")
    (return-from main (values)))
  (let* ((tables (make-alien (unsigned 8) (* +frames+ +entry-bytes+)))
         (colours (+ 5 (make-tables (alien-sap tables))))
         (copper (copper-code))
         (pulses 0) (timed nil) (frames 0) (faked-frames 0) (start 0) (end 0))
    (flet ((copper-frame (table)
             (alien-funcall (sap-alien copper (function void system-area-pointer int int))
                            table (1- +lines+) 0)))
      (unwind-protect
           (with-video-mode (#x13)
             (set-colours)
             ;; Is there a beam to follow?  Ten frames should take a
             ;; seventh of a second or more: two ticks at least.
             (wait-tick)
             (let ((from (ticks)) (count (count-code)))
               (dotimes (i 10)
                 (setf pulses (alien-funcall (sap-alien count (function unsigned)))))
               (setf timed (>= (- (ticks) from) 2)
                     start (ticks)))
             (let ((faked (not timed))
                   (place 0.0)                  ; which frame of the bars' round
                   (rate 70.0) (counted 0) (counted-from start))
               (unless timed
                 ;; Without the beam to wait for, a frame takes no time: see
                 ;; how many there are in a tick, so that the bars can go
                 ;; at the speed they should.
                 (wait-tick)
                 (let ((from (ticks)) (n 0))
                   (loop while (< (- (ticks) from) 2)
                         do (copper-frame (alien-sap tables)) (incf n))
                   (setf rate (* n 9.1) counted-from (ticks))))
               (draw-picture colours faked timed)
               (show-picture faked)
               (loop
                 ;; a key?  (The BIOS's own pointers into its keyboard
                 ;; buffer: there is no time between frames to call it.)
                 (when (/= (peek16 #x41A) (peek16 #x41C))
                   (case (get-key)
                     ((#\Escape #\q #\Q) (return))
                     ((#\f #\F)
                      (setf faked (not faked))
                      (set-palette 0 0 0 0)
                      (draw-picture colours faked timed)
                      (show-picture faked))))
                 (let ((table (sap+ (alien-sap tables) (* (floor place) +entry-bytes+))))
                   (cond (faked (fake-frame table) (incf faked-frames))
                         (t (copper-frame table) (incf frames))))
                 ;; a frame of the round for each seventieth of a second
                 (setf place (mod (+ place (/ 70.0 rate)) +frames+))
                 (incf counted)
                 (when (>= (- (ticks) counted-from) 9)
                   (setf rate (max 1.0 (/ (* counted 18.2) (- (ticks) counted-from)))
                         counted 0 counted-from (ticks)))))
             (setf end (ticks)))
        (free-alien tables)))
    (format t "~&~D frames with the palette rewritten on each of ~D scanlines~@[, ~D faked~]:~%~
               ~,1F frames a second.  ~D colours on the screen from 6 palette entries.~%"
            frames +lines+ (and (plusp faked-frames) faked-frames)
            (/ (* 18.2 (+ frames faked-frames)) (max 1 (- end start))) colours)
    (format t "The status port showed ~D lines between vertical retraces~A.~%" pulses
            (cond ((not timed) ": it keeps no time here (an emulator),
so there was no beam to follow and the bars were faked to begin with")
                  ((not (< 390 pulses 410)) " (an emulator's count: a real VGA shows 400)")
                  (t ""))))
  (values))

(doslib-examples:define-example "copper" 'main
  :category :graphics
  :description "Copper bars: a palette entry rewritten on every scanline.")
