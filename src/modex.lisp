;;;; The VGA's registers, and "Mode X": 320 by 240 in 256 colours, in planes.
;;;;
;;;; The VGA is four sets of registers behind pairs of ports: a register's
;;;; number goes to the first port and its value to, or from, the second.
;;;; The sequencer (3C4h) says how memory is laid out and which planes a
;;;; write reaches; the CRT controller (3D4h) times the picture and says
;;;; where in memory it starts; the graphics controller (3CEh) stands between
;;;; the processor and video memory; the attribute controller (3C0h, one port
;;;; for both number and value) turns what is in memory into colours.
;;;;
;;;; Mode X is not a BIOS mode.  It is mode 13h with those registers set
;;;; otherwise: the four planes of video memory, which mode 13h chains
;;;; together to look like 64,000 bytes in a row, are unchained, and the
;;;; picture is timed for 480 scan lines, each row of pixels shown twice.
;;;; That gives 240 rows of square pixels, and 256K of video memory instead
;;;; of 64K: room for three pages, or for a picture wider than the screen
;;;; to scroll about in.  A pixel X, Y is in plane X mod 4 at address
;;;; 80 Y + X / 4: the price is that a byte is written to whichever planes
;;;; the sequencer's map mask names (VGA-PLANES), so that drawing goes a
;;;; plane at a time -- and the gain, that one write with all four planes
;;;; named sets four pixels.  The examples "modex", "flip", "scroll" and
;;;; "latch" show what it is good for.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defun vga-sequencer (index &optional value)
  "The VGA sequencer's register INDEX (ports 3C4h and 3C5h): set it to
VALUE, or, with no VALUE, return what it holds.  Register 2 is the map
mask (see VGA-PLANES), 4 the memory mode."
  (outb #x3C4 index)
  (if value (progn (outb #x3C5 value) value) (inb #x3C5)))
(defun vga-crtc (index &optional value)
  "The CRT controller's register INDEX (ports 3D4h and 3D5h): set it to
VALUE, or, with no VALUE, return what it holds.  Registers 0 to 7 cannot be
written while bit 7 of register 11h is set."
  (outb #x3D4 index)
  (if value (progn (outb #x3D5 value) value) (inb #x3D5)))
(defun vga-graphics (index &optional value)
  "The graphics controller's register INDEX (ports 3CEh and 3CFh): set it
to VALUE, or, with no VALUE, return what it holds.  Register 5 is the mode:
its low two bits are the write mode."
  (outb #x3CE index)
  (if value (progn (outb #x3CF value) value) (inb #x3CF)))
(defun vga-attribute (index &optional value)
  "The attribute controller's register INDEX (port 3C0h; read at 3C1h):
set it to VALUE, or, with no VALUE, return what it holds.  The display
stays on.  Register 13h is the horizontal pixel panning."
  (inb #x3DA)                           ; reading 3DAh makes the next write to 3C0h an index
  (outb #x3C0 (logior index #x20))      ; bit 5 keeps the display on
  (if value
      (progn (outb #x3C0 value) value)
      (prog1 (inb #x3C1) (inb #x3DA))))

(declaim (inline vga-planes))
(defun vga-planes (mask)
  "Say which of the four planes of video memory a write goes to: bit 0 of
MASK for plane 0, and so on to bit 3.  (The sequencer's map mask, register
2.)  In Mode X, 1, 2, 4 or 8 is one pixel of the four at an address, and 15
all four at once."
  (outw #x3C4 (logior 2 (ash mask 8)))
  (values))

(defun vga-display-start (address)
  "Have the display begin at ADDRESS of video memory (CRT controller
registers 0Ch and 0Dh), from the next vertical retrace: how pages are
flipped and pictures scrolled.  In Mode X one address is four pixels, and a
row is 80 addresses.  If a retrace is going on, its end is waited for, so
that the VGA does not take half of the old address and half of the new."
  (loop while (logbitp 3 (inb #x3DA)))
  (vga-crtc #x0C (ldb (byte 8 8) address))
  (vga-crtc #x0D (ldb (byte 8 0) address))
  (values))

(defun mode-x ()
  "Set Mode X: 320 by 240 in 256 colours, with square pixels and video
memory in four planes of 64K.  The pixel X, Y is in plane X mod 4, at
address 80 Y + X / 4 from *VGA*; VGA-PLANES says which planes are written.
All of video memory is cleared.  The BIOS still believes it is in mode 13h;
(video-mode 3) goes back to text."
  (video-mode #x13)
  (vga-sequencer 4 #x06)                ; unchain the four planes
  (vga-sequencer 0 #x01) (outb #x3C2 #xE3) (vga-sequencer 0 #x03)   ; 480-line timing
  (vga-crtc #x11 (logand (vga-crtc #x11) #x7F))                     ; unlock CRTC 0-7
  (loop for (index value) in '((#x06 #x0D) (#x07 #x3E) (#x09 #x41) (#x10 #xEA) (#x11 #xAC)
                               (#x12 #xDF) (#x14 #x00) (#x15 #xE7) (#x16 #x06) (#x17 #xE3))
        do (vga-crtc index value))
  (vga-planes #x0F)                     ; all planes: clear all of memory
  (dotimes (i 16384) (setf (sap-ref-32 *vga* (* 4 i)) 0))
  (values))

(defun mode-x-text (x y string colour &optional (background 0) (start 0) (row-bytes 80))
  "Write STRING in Mode X, in the BIOS's 8 by 8 characters, with the top
left corner of its first character at X (a multiple of 4), Y: the letters in
COLOUR on BACKGROUND, or over the picture as it is if BACKGROUND is NIL.
START is the address in video memory of the page to write on, and ROW-BYTES
the addresses from one row to the next, if it is not the usual 80.  Nothing
is clipped, and the map mask is left naming plane 3."
  (dotimes (plane 4)
    (vga-planes (ash 1 plane))
    (loop for character across string
          for address from (+ start (* row-bytes y) (ash x -2)) by 2
          do (dotimes (row 8)
               (let ((bits (glyph-row character row)))
                 (dotimes (i 2)         ; pixel 4i+plane of the character's eight
                   (let ((ink (logbitp (- 7 plane (* 4 i)) bits)))
                     (when (or ink background)
                       (setf (sap-ref-8 *vga* (+ address (* row-bytes row) i))
                             (if ink colour background)))))))))
  (values))
