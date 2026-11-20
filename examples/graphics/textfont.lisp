;;;; Text-mode fonts: the shapes of the characters are in video memory too,
;;;; and a program can change them.
;;;;
;;;; In text mode the screen at B8000h holds character codes, and the VGA's
;;;; character generator turns each code into dots as the picture is sent to
;;;; the monitor.  The dots are not in a ROM on the card, as they were on the
;;;; first PC's adapters: the BIOS copies its font into plane 2 of video
;;;; memory when it sets the mode, 32 bytes for each of 256 characters, a
;;;; byte for each row of a character's cell from the top (in mode 3 the
;;;; first 16 of the 32 are used: a cell is 16 scan lines), the leftmost dot
;;;; in bit 7.  Write other bytes there and every character with that code,
;;;; wherever it is on the screen, changes shape at once.
;;;;
;;;; Plane 2 is not where a program can reach it while text is displayed:
;;;; the VGA is set to show planes 0 and 1 (characters and colours) as
;;;; alternate bytes at B8000h.  There are two ways to the font:
;;;;
;;;;   - through the registers.  The sequencer's map mask (3C4h register 2)
;;;;     is set to write plane 2 only, and its memory mode (register 4) to
;;;;     stop sending odd and even addresses to different planes; the
;;;;     graphics controller (3CEh) is told to read plane 2 (register 4), to
;;;;     stop the odd and even business as well (register 5), and to put
;;;;     video memory at A0000h (register 6).  Then character C's rows are
;;;;     the bytes at A0000h + 32 C, to read or write, and afterwards the
;;;;     five registers are put back as they were.  WITH-FONT-MEMORY below;
;;;;   - through the BIOS, which does just that: INT 10h with AX=1100h loads
;;;;     CX characters from DX on, BH bytes each, from ES:BP.  A pointer in
;;;;     ES:BP means a real-mode call with the glyphs in DOS memory, and BP
;;;;     is a register the library's REAL-INT does not pass: so the DPMI
;;;;     host's real-mode call structure (*RM*) is filled in here by hand.
;;;;     AX=1112h loads the ROM's 8 by 8 font and sets the CRT controller for
;;;;     cells of 8 scan lines: 50 rows of text.  AX=1114h does the same for
;;;;     the 8 by 16 font and 25 rows; AX=1104h loads that font and leaves
;;;;     the CRT controller alone.
;;;;
;;;; A cell is 9 dots wide and a glyph only 8.  The ninth dot is blank,
;;;; except for codes C0h to DFh, the line-drawing characters, where it
;;;; repeats the eighth so that lines join up.  The redefined characters
;;;; here are from that range, so the bar has no gaps: nine glyphs at D0h to
;;;; D8h are a track with 0 to 8 of its columns filled, which lets a
;;;; progress bar 40 cells long move in 320 steps and not 40 (the eighth
;;;; step fills two dots); C6h and C7h together are a picture of a disk.
;;;;
;;;;   (doslib-examples:run "textfont")
;;;; Space goes on: the ROM's font; the eleven glyphs loaded (the bar
;;;; through the registers, the disk through the BIOS, and both read back
;;;; from plane 2 and compared); the letters slanted into italics; 80 by 50
;;;; with the 8 by 8 font; back to 25 rows; and round again.  Esc leaves.
;;;; However it ends, the ROM's font is loaded again (AX=1104h) and mode 3
;;;; set.  The large picture on the left of the screen is characters C6h and
;;;; C7h as they are in plane 2 at that moment, a block for each dot.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.textfont
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.textfont)

(defconstant +bar+ #xD0)                ; D0h: the empty track; D1h to D8h: 1 to 8 columns full
(defconstant +disk+ #xC6)               ; C6h and C7h: the two halves of the picture
(defconstant +bar-cells+ 40)

;;; ---- plane 2, through the registers
(defvar *written* 0 "Bytes of glyphs written to plane 2.")
(defvar *read* 0 "Bytes of glyphs read from plane 2.")
(defun call-with-font-memory (function)
  (let ((map-mask (vga-sequencer 2)) (memory-mode (vga-sequencer 4))
        (read-map (vga-graphics 4)) (mode (vga-graphics 5)) (miscellaneous (vga-graphics 6)))
    (unwind-protect
         (progn
           (vga-sequencer 0 1)          ; hold the sequencer while its memory mode changes
           (vga-sequencer 2 4)          ; writes go to plane 2 only
           (vga-sequencer 4 (logior memory-mode 4))    ; every address in the same plane
           (vga-sequencer 0 3)
           (vga-graphics 4 2)           ; reads come from plane 2
           (vga-graphics 5 (logand mode #xEF))         ; not odd and even addresses apart
           (vga-graphics 6 #x04)        ; memory at A0000h, 64K of it, and not odd/even
           (funcall function))
      (vga-sequencer 0 1)
      (vga-sequencer 2 map-mask)
      (vga-sequencer 4 memory-mode)
      (vga-sequencer 0 3)
      (vga-graphics 4 read-map)
      (vga-graphics 5 mode)
      (vga-graphics 6 miscellaneous))))
(defmacro with-font-memory (() &body body)
  "Run BODY with plane 2 of video memory, where the font is, at *VGA*: character C's
rows are the 16 bytes at 32 C.  The screen cannot be written meanwhile.  The VGA's
registers are put back for text however BODY ends."
  `(call-with-font-memory (lambda () ,@body)))
(defun read-glyphs (first count)
  "The glyphs of COUNT characters from code FIRST, 16 bytes each, as a vector."
  (let ((bytes (make-array (* 16 count) :element-type '(unsigned-byte 8))))
    (with-font-memory ()
      (dotimes (i count)
        (dotimes (row 16)
          (setf (aref bytes (+ (* 16 i) row)) (sap-ref-8 *vga* (+ (* 32 (+ first i)) row))))))
    (incf *read* (length bytes))
    bytes))
(defun write-glyphs (first bytes)
  "Make BYTES, 16 for each, the glyphs of the characters from code FIRST."
  (with-font-memory ()
    (dotimes (i (length bytes))
      (multiple-value-bind (character row) (floor i 16)
        (setf (sap-ref-8 *vga* (+ (* 32 (+ first character)) row)) (aref bytes i)))))
  (incf *written* (length bytes)))

;;; ---- and through the BIOS
(defun bios-load-glyphs (first bytes)
  "The same as WRITE-GLYPHS, by INT 10h AX=1100h: ES:BP points to the glyphs, CX is
how many, DX the first character's code, BH the bytes in each and BL the font (0)."
  (with-dos-memory (segment (length bytes))
    (dotimes (i (length bytes))
      (setf (sap-ref-8 (int-sap (linear segment)) i) (aref bytes i)))
    ;; The DPMI host's real-mode call structure, as REAL-INT fills it in -- and EBP,
    ;; which is at offset 8.
    (dotimes (i 64) (setf (sap-ref-8 *rm* i) 0))
    (setf (sap-ref-32 *rm* #x1C) #x1100                 ; EAX
          (sap-ref-32 *rm* #x10) #x1000                 ; EBX: BH = 16 rows, BL = font 0
          (sap-ref-32 *rm* #x18) (floor (length bytes) 16)   ; ECX
          (sap-ref-32 *rm* #x14) first                  ; EDX
          (sap-ref-32 *rm* #x08) 0                      ; EBP
          (sap-ref-16 *rm* #x22) segment)               ; ES
    (int86 #x31 :ax #x0300 :bx #x10 :di (sap-int *rm*)))     ; DPMI: do real-mode INT 10h
  (incf *written* (length bytes)))

;;; ---- the glyphs
(defun bar-glyphs ()
  "Nine glyphs: a track between two rails with none, then 1 to 8, of its columns full."
  (let ((bytes (make-array (* 16 9) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (step 9)
      (setf (aref bytes (+ (* 16 step) 2)) #xFF (aref bytes (+ (* 16 step) 13)) #xFF)
      (loop for row from 4 to 11
            do (setf (aref bytes (+ (* 16 step) row)) (logand #xFF (ash #xFF00 (- step))))))
    bytes))
(defparameter *disk*
  '("................"
    ".############..."
    ".##.....##.###.."
    ".##.....##.####."
    ".##.....##.####."
    ".##........####."
    ".##############."
    ".##############."
    ".##..........##."
    ".##.########.##."
    ".##..........##."
    ".##.########.##."
    ".##..........##."
    ".##..........##."
    ".##############."
    "................")
  "A disk, 16 dots by 16: two characters side by side.")
(defun disk-glyphs ()
  (let ((bytes (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for line in *disk* for row from 0
          do (dotimes (x 16)
               (when (char= (char line x) #\#)
                 (setf (ldb (byte 1 (- 7 (mod x 8))) (aref bytes (+ row (* 16 (floor x 8)))))
                       1))))
    bytes))
(defun italic-glyphs (glyphs)
  "GLYPHS slanted: the top five rows of each a dot to the right, the bottom five a dot
to the left."
  (let ((bytes (copy-seq glyphs)))
    (dotimes (i (length bytes))
      (let ((row (mod i 16)) (dots (aref glyphs i)))
        (setf (aref bytes i) (cond ((< row 5) (ash dots -1))
                                   ((> row 10) (logand #xFF (ash dots 1)))
                                   (t dots)))))
    bytes))

;;; ---- the screen.  (The library's TEXT-PUT stops at row 24, and here there are 50.)
(defun say (x y text &optional (attribute 7))
  (loop for character across text for i from x
        do (setf (sap-ref-16 *text-screen* (* 2 (+ i (* 80 y))))
                 (logior (logand (char-code character) 255) (ash attribute 8)))))
(defun cell (x y code &optional (attribute 7))
  (setf (sap-ref-16 *text-screen* (* 2 (+ x (* 80 y)))) (logior code (ash attribute 8))))
(defun draw-bar (progress)
  "The progress bar with PROGRESS of its 320 steps done: only the character codes are
written, 40 of them."
  (dotimes (i +bar-cells+)
    (setf (sap-ref-8 *text-screen* (* 2 (+ 12 i (* 80 6))))
          (+ +bar+ (max 0 (min 8 (- progress (* 8 i)))))))
  (say 54 6 (format nil "~3D%" (floor (* 100 progress) (* 8 +bar-cells+))) 15))
(defun draw-magnified (height)
  "Characters C6h and C7h as they are in plane 2 now, HEIGHT rows of each (16, or 8):
two cells across for each dot, and as many down as fill 16 rows of the screen."
  (let ((glyphs (read-glyphs +disk+ 2)) (tall (floor 16 height)))
    (dotimes (row height)
      (dotimes (x 16)
        (let ((dot (logbitp (- 7 (mod x 8)) (aref glyphs (+ row (* 16 (floor x 8)))))))
          (dotimes (i 2)
            (dotimes (j tall)
              (cell (+ 3 (* 2 x) i) (+ 8 (* tall row) j) (if dot #xDB #xFA) (if dot 14 8)))))))))
(defun draw-page (rows title first second)
  "The screen for one step: ROWS is 25 or 50."
  (dotimes (y rows) (say 0 y (make-string 80 :initial-element #\Space)))
  (say 0 0 (format nil "~80A" (format nil " Text-mode fonts: ~A" title)) #x1F)
  (say 1 2 first 15)
  (say 1 3 second 15)
  (say 1 5 "Codes D0h to D8h, C6h and C7h:")
  (dotimes (i 9) (cell (+ 33 (* 2 i)) 5 (+ +bar+ i) 11))
  (cell 53 5 +disk+ 14) (cell 54 5 (1+ +disk+) 14)
  (say 1 6 "Progress")
  (dotimes (i +bar-cells+) (cell (+ 12 i) 6 +bar+ 10))
  (say 1 7 "C6h, C7h as read from plane 2:")
  (say 44 7 "All 256 characters:")
  (dotimes (code 256)                   ; with 50 rows there is room to space them out
    (cell (+ 44 (* 2 (mod code 16))) (+ 8 (* (floor rows 25) (floor code 16))) code 7))
  (draw-magnified (floor 400 rows))
  (when (= rows 50)                     ; number the rows, to show that there are 50
    (dotimes (y 50) (say 77 y (format nil "~2D" (1+ y)) 3)))
  (say 1 (1- rows) "Space: the next step    Esc: leave" 14))

;;; ---- the steps
(defun bios-rows () (1+ (peek #x484)))  ; what the BIOS keeps at 0040:0084: the last row
(defun show-step (step rom)
  "Do step STEP to the font and draw its page.  ROM is the font the BIOS loaded."
  (ecase step
    (0 (draw-page 25 "the ROM's font"
                  "1. The font the BIOS copied from its ROM into plane 2 of video memory."
                  "   Codes D0h-D8h, C6h and C7h are line-drawing characters, as shown below."))
    (1 (let ((bar (bar-glyphs)) (disk (disk-glyphs)))
         (write-glyphs +bar+ bar)       ; through the registers
         (bios-load-glyphs +disk+ disk) ; through the BIOS
         (let ((same (+ (count t (map 'list #'= bar (read-glyphs +bar+ 9)))
                        (count t (map 'list #'= disk (read-glyphs +disk+ 2))))))
           (draw-page 25 "eleven characters redefined"
                      "2. 144 bytes written to plane 2 through the sequencer (the bar) and 32 by"
                      (format nil "   INT 10h AX=1100h (the disk).  Read back from plane 2: ~
                                   ~D of 176 agree." same)))))
    (2 (write-glyphs 32 (italic-glyphs (subseq rom (* 16 32) (* 16 128))))
       (draw-page 25 "italics"
                  "3. The 96 glyphs of codes 20h-7Fh slanted and written back: 1536 bytes."
                  "   Every letter on the screen changes shape, and its code is as it was."))
    (3 (int86 #x10 :ax #x1112 :bx 0)    ; the ROM's 8 by 8 font, and the CRTC set to suit
       (hide-text-cursor)
       (draw-page 50 "80 by 50"
                  "4. INT 10h AX=1112h: the ROM's 8 by 8 font, in cells of 8 scan lines."
                  (format nil "   The BIOS says ~D rows of ~D scan lines; ours were in the ~
                               font it replaced."
                          (bios-rows) (peek16 #x485))))
    (4 (int86 #x10 :ax #x1114 :bx 0)    ; the ROM's 8 by 16 font, and the CRTC set to suit
       (hide-text-cursor)
       (draw-page 25 "back to 80 by 25"
                  (format nil "5. INT 10h AX=1114h: the ROM's 8 by 16 font again: ~D rows ~
                               of ~D scan lines."
                          (bios-rows) (peek16 #x485))
                  "   Space starts again from the beginning."))))
(defun run-bar ()
  "Move the bar, a step a frame, until Space or Esc is pressed: returns which."
  (loop for progress = 0 then (mod (1+ progress) (+ 60 (* 8 +bar-cells+)))
        do (when (key-pressed-p)
             (let ((key (get-key)))
               (when (member key '(#\Space #\Escape)) (return key))))
           (wait-frame)
           (draw-bar (min progress (* 8 +bar-cells+)))))    ; it waits a while when full

(defun vga-p ()
  "Is there a VGA?  INT 10h AX=1A00h answers 1Ah in AL if the BIOS is a VGA's."
  (= (lo (int86 #x10 :ax #x1A00)) #x1A))

(defun textfont ()
  (unless (vga-p)
    (format t "~&This needs a VGA: no other adapter keeps its text font in memory like this.~%")
    (return-from textfont))
  (setf *written* 0 *read* 0)
  (video-mode 3)                        ; a known state: 25 rows and the ROM's font
  (unwind-protect
       (let ((rom (read-glyphs 0 256)))
         (hide-text-cursor)
         (loop for step = 0 then (mod (1+ step) 5)
               do (show-step step rom)
               until (eql (run-bar) #\Escape)))
    (int86 #x10 :ax #x1104 :bx 0)       ; the ROM's 8 by 16 font again,
    (video-mode 3))                     ; and the mode set afresh
  (format t "~&Bytes of glyphs written to plane 2: ~D; read from it: ~D.~%~
             The ROM's font has been loaded again.~%"
          *written* *read*))

(defun main ()
  "Text-mode fonts: glyphs redefined in plane 2, a smooth progress bar, 80 by 50."
  (textfont)
  (values))

(doslib-examples:define-example "textfont" 'main
  :category :graphics
  :description "Text-mode fonts: glyphs redefined in plane 2, a smooth progress bar, 80 by 50.")
