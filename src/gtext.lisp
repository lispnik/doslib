;;;; Text in the graphics modes: the BIOS's 8 by 8 characters, as pixels.
;;;;
;;;; In a graphics mode there are no character cells: a letter is sixty-four
;;;; pixels that the program sets.  The patterns are in every PC's ROM, at
;;;; F000:FA6E (FFA6Eh), where the BIOS keeps the first 128 characters of
;;;; its 8 by 8 font for its own use in the graphics modes: eight bytes a
;;;; character, a byte a row from the top, the leftmost pixel in bit 7.
;;;;
;;;; GRAPHICS-TEXT writes a string in mode 13h.  DRAW-GLYPHS writes one into
;;;; any memory that has a byte for each pixel -- a back buffer, a VESA
;;;; linear framebuffer, a Lisp vector that is pinned -- and GLYPH-ROW gives
;;;; the patterns themselves, for memory laid out some other way (MODE-X-TEXT
;;;; in modex.lisp uses it for planes) or letters of another size.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(declaim (inline glyph-row))
(defun glyph-row (character row)
  "Row ROW (0 at the top to 7) of CHARACTER, a character or its code, in the
BIOS's 8 by 8 font: a byte, the leftmost pixel in bit 7.  The ROM has the
first 128 characters; one above that is given the pattern 128 below it."
  (sap-ref-8 (int-sap #xFFA6E)
             (+ (* 8 (logand (if (characterp character) (char-code character) character) 127))
                row)))
(defun glyph-rows (character)
  "The eight rows of CHARACTER in the BIOS's 8 by 8 font, top first, as a
vector of bytes: see GLYPH-ROW."
  (let ((rows (make-array 8 :element-type '(unsigned-byte 8))))
    (dotimes (row 8 rows) (setf (aref rows row) (glyph-row character row)))))

(defun draw-glyphs (memory offset row-bytes string colour &optional (background 0))
  "Write STRING in the BIOS's 8 by 8 characters into memory that has a byte
for each pixel.  MEMORY is a system area pointer, OFFSET where in it the
top left pixel of the first character goes, and ROW-BYTES the distance from
one row of pixels to the next: 320 in mode 13h.  The pixels of the letters
are set to COLOUR and the rest of each character's square to BACKGROUND;
if that is NIL they are left as they were.  Nothing is clipped."
  (declare (type system-area-pointer memory) (fixnum offset row-bytes)
           (type (unsigned-byte 8) colour) (type (or null (unsigned-byte 8)) background)
           (optimize speed))
  (let ((font (int-sap #xFFA6E))
        (string (if (simple-string-p string) string (coerce string 'simple-string))))
    (declare (simple-string string))
    (dotimes (i (length string))
      (let ((glyph (* 8 (logand (char-code (char string i)) 127)))
            (at (+ offset (* 8 i))))
        (declare (fixnum glyph at))
        (dotimes (row 8)
          (let ((bits (sap-ref-8 font (+ glyph row))))
            (if background
                (dotimes (column 8)
                  (setf (sap-ref-8 memory (+ at column))
                        (if (logbitp (- 7 column) bits) colour background)))
                (dotimes (column 8)
                  (when (logbitp (- 7 column) bits)
                    (setf (sap-ref-8 memory (+ at column)) colour)))))
          (incf at row-bytes)))))
  (values))

(defun graphics-text (x y string colour &optional (background 0))
  "Write STRING in mode 13h with the top left corner of its first character
at X, Y: each character is 8 by 8 pixels, the letters in COLOUR on
BACKGROUND, or over the picture as it is if BACKGROUND is NIL.  Characters
that would not be wholly on the screen are left out."
  (when (<= 0 y 192)
    (let ((first (max 0 (ceiling (- x) 8)))
          (end (min (length string) (floor (- 320 x) 8))))
      (when (< first end)
        (draw-glyphs *vga* (+ x (* 8 first) (* 320 y)) 320
                     (if (and (zerop first) (= end (length string)))
                         string
                         (subseq string first end))
                     colour background))))
  (values))
