;;;; VGA: video modes, the palette, pixels and lines in mode 13h; VESA modes.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; Video
(defvar *vga* (int-sap #xA0000) "Video memory in the graphics modes: a byte a pixel in mode 13h.")
(defun video-mode (mode)
  "Set BIOS video mode MODE: 3 is 80x25 text, #x13 is 320x200 in 256 colours."
  (int86 #x10 :ax mode))
(defun current-video-mode ()
  "The BIOS's video mode: 3 is 80x25 text, #x13 is 320x200 in 256 colours."
  (peek #x449))
;;; Which display card.  A VGA's BIOS says so itself (INT 10h, AX=1A00h); an
;;; EGA's answers a call a VGA also answers (AH=12h, BL=10h); before those
;;; there were the colour card (CGA) and the monochrome one (MDA, and the
;;; Hercules card that added graphics to it), told apart by the "equipment"
;;; switches the BIOS keeps at 0410h.  It matters to a text program in two
;;; ways: a monochrome card's text memory is at B0000h, not B8000h, and
;;; its attribute bytes are not colours; and only an EGA or a VGA can show
;;; more than 25 lines, or says how many it is showing (at 0484h).
(defvar *display-adapter* nil "What DISPLAY-ADAPTER found, kept for this process.")
(defun display-adapter ()
  "The display card: :VGA, :MCGA, :EGA, :CGA, :HERCULES or :MDA."
  (or *display-adapter*
      (setf *display-adapter*
            (multiple-value-bind (ax bx) (int86 #x10 :ax #x1A00)
              (cond ((= (logand ax #xFF) #x1A)
                     (case (logand bx #xFF)
                       (1 :mda) (2 :cga) ((4 5) :ega) ((7 8) :vga) ((10 11 12) :mcga)
                       (t :vga)))
                    ((/= (logand (nth-value 1 (int86 #x10 :ax #x1200 :bx #xFF10)) #xFF) #x10) :ega)
                    ((/= (logand (peek #x410) #x30) #x30) :cga)
                    ;; a Hercules card's status port has the vertical retrace in bit 7: it changes
                    ((cdr (remove-duplicates (loop repeat 30000 collect (logand #x80 (inb #x3BA)))))
                     :hercules)
                    (t :mda))))))
(defun monochrome-display-p ()
  "Is the text screen a monochrome card's (at B0000h, with attributes that are not colours)?"
  (or (member (display-adapter) '(:mda :hercules))
      (= (logand (peek #x410) #x30) #x30)))
(defun text-memory-address ()
  "Where the text screen's cells are: B8000h, or B0000h on a monochrome card."
  (if (monochrome-display-p) #xB0000 #xB8000))
(defun text-columns () "How many columns the text screen has, by the BIOS." (peek16 #x44A))
(defun text-lines ()
  "How many lines the text screen has: 25, or what an EGA's or VGA's BIOS says."
  (if (member (display-adapter) '(:ega :vga :mcga)) (1+ (peek #x484)) 25))
(defun text-mode ()
  "Go back to the 80 by 25 text screen: mode 3, or 7 on a monochrome card."
  (video-mode (if (monochrome-display-p) 7 3)))
(defun monochrome-attribute (attribute)
  "The attribute byte that shows on a monochrome card what the colour
ATTRIBUTE means on a colour one: reversed for a coloured background, bright
for a bright colour.  (Unchanged, blue would be underlined.)"
  (logior (logand attribute #x80)
          (cond ((logtest attribute #x70) #x70)
                ((zerop (logand attribute #x0F)) #x00)
                ((= (logand attribute #x0F) 8) #x07)        ; dark grey is dim, not bright
                ((logtest attribute #x08) #x0F)
                (t #x07))))
(defmacro with-video-mode ((mode) &body body)
  "Run BODY in video mode MODE, and go back to 80x25 text however it ends."
  `(progn (video-mode ,mode)
          (unwind-protect (progn ,@body) (video-mode 3))))
(defun wait-retrace ()
  "Wait for the start of the next vertical retrace: the moment to change
the palette or the picture without it showing."
  (loop while (logbitp 3 (inb #x3DA)))
  (loop until (logbitp 3 (inb #x3DA))))
(defun vesa-set-mode (mode &key linear keep)
  "Set a VESA BIOS mode (#x101 is 640x480 in 256 colours).  True if the BIOS
did.  With LINEAR the mode is set for its linear framebuffer (see
VESA-MODE-INFO) and not for the 64K window at A0000h that VESA-SET-BANK
moves; with KEEP video memory is not cleared, and the picture stays."
  (= (word16 (int86 #x10 :ax #x4F02
                         :bx (logior mode (if linear #x4000 0) (if keep #x8000 0))))
     #x004F))
(defun vesa-mode-info (mode)
  "What the VESA BIOS says of MODE (INT 10h, AX=4F01h), as a property list,
or NIL if it knows no such mode.  :WIDTH and :HEIGHT are in pixels, :BITS the
bits in a pixel, :LINE-BYTES the bytes from one row to the next;
:FRAMEBUFFER is the physical address of the mode's linear framebuffer, for
MAP-PHYSICAL, or NIL if it has none; :GRANULARITY is how many kilobytes one
step of VESA-SET-BANK moves the window; :ATTRIBUTES is the mode's attribute
word (bit 7: a linear framebuffer, bit 4: graphics)."
  ;; ES:DI is a real-mode pointer to 256 bytes for the BIOS to fill in: so
  ;; they are in DOS memory, and the call is REAL-INT.
  (with-dos-memory (segment 256)
    (let ((block (linear segment)))
      (dotimes (i 256) (setf (sap-ref-8 (int-sap block) i) 0))
      (when (= (word16 (real-int #x10 :ax #x4F01 :cx mode :es segment :di 0)) #x004F)
        (let ((attributes (peek16 block)) (framebuffer (peek32 (+ block #x28))))
          (list :width (peek16 (+ block #x12)) :height (peek16 (+ block #x14))
                :bits (peek (+ block #x19)) :line-bytes (peek16 (+ block #x10))
                :framebuffer (and (logbitp 7 attributes) (plusp framebuffer) framebuffer)
                :granularity (peek16 (+ block 4))
                :attributes attributes))))))
(defun vesa-set-bank (bank &optional (window 0))
  "Move the 64K window at A0000h to BANK of a VESA mode's video memory."
  (int86 #x10 :ax #x4F05 :bx window :dx bank))
(defun set-palette (index r g b)
  "Make colour INDEX the mixture R, G, B: each from 0 to 63."
  (outb #x3C8 index) (outb #x3C9 r) (outb #x3C9 g) (outb #x3C9 b))
(defun rainbow-palette (&optional (first 1) (last 255))
  "Make colours FIRST to LAST a rainbow, and colour 0 black."
  (set-palette 0 0 0 0)
  (loop for i from first to last
        for a = (* 2 pi (/ (- i first) (- last first -1)))
        do (flet ((v (phase) (round (* 31.5 (1+ (sin (+ a phase)))))))
             (set-palette i (v 0) (v 2.094) (v 4.189)))))
;;; More palettes.  The VGA's 256 colours are whatever its palette says,
;;; and most pictures drawn by arithmetic want a run of colours that shade
;;; into one another: GRADIENT-PALETTE makes one from a few colours to pass
;;; through, and the palettes with names are gradients someone has chosen.
;;; Like RAINBOW-PALETTE they fill colours FIRST to LAST (1 to 255 unless
;;; told otherwise) and make colour 0 black.
(defun get-palette (index)
  "What colour INDEX is: its R, G and B, each from 0 to 63."
  (outb #x3C7 index)
  (values (inb #x3C9) (inb #x3C9) (inb #x3C9)))
(defun gradient-palette (colours &optional (first 1) (last 255))
  "Make colours FIRST to LAST shade evenly through COLOURS, a list of
\(r g b) lists with each part from 0 to 63, and colour 0 black:
\(gradient-palette '((0 0 0) (63 0 0) (63 63 0))) goes from black through
red to yellow."
  (set-palette 0 0 0 0)
  (let ((steps (max 1 (- last first))) (spans (max 1 (1- (length colours)))))
    (loop for i from first to last
          do (multiple-value-bind (span part) (floor (* (- i first) spans) steps)
               (let ((from (nth (min span spans) colours))
                     (to (nth (min (1+ span) spans) colours)))
                 (flet ((mix (a b) (round (+ (* a (- steps part)) (* b part)) steps)))
                   (set-palette i (mix (first from) (first to)) (mix (second from) (second to))
                                (mix (third from) (third to)))))))))
(defvar *palettes*
  '((:grey ((0 0 0) (63 63 63)))
    (:fire ((0 0 0) (32 0 0) (63 8 0) (63 40 0) (63 63 16) (63 63 63)))
    (:ice ((0 0 8) (0 8 32) (0 32 56) (24 56 63) (63 63 63)))
    (:ocean ((0 0 6) (0 6 24) (0 20 40) (4 40 48) (28 56 56) (56 63 60)))
    (:forest ((2 6 0) (4 20 4) (12 36 6) (32 48 10) (52 58 24)))
    (:sunset ((4 0 16) (24 0 28) (52 8 20) (63 28 4) (63 52 12) (63 63 40)))
    (:heat ((0 0 20) (0 20 63) (0 56 56) (0 56 0) (63 63 0) (63 28 0) (63 0 0)))
    (:copper ((0 0 0) (20 8 2) (44 22 8) (60 40 20) (63 58 44)))
    (:sepia ((4 2 0) (24 16 8) (44 34 22) (60 54 42)))
    (:amber ((0 0 0) (24 12 0) (48 28 0) (63 44 0) (63 56 20)))
    (:phosphor ((0 2 0) (0 24 4) (4 48 8) (24 63 24) (52 63 52)))
    (:neon ((63 0 40) (40 0 63) (0 24 63) (0 63 56) (20 63 0) (63 60 0) (63 0 40)))
    (:pastel ((63 44 44) (63 56 40) (52 63 44) (40 60 60) (48 44 63) (63 44 56) (63 44 44)))
    (:earth ((4 8 24) (10 24 40) (48 44 28) (12 36 8) (28 24 12) (44 40 36) (63 63 63))))
  "The palettes with names: each a name and the colours GRADIENT-PALETTE
passes through.  Push one of your own on, and USE-PALETTE knows it.")
(defun palette-names ()
  "The names USE-PALETTE takes."
  (list* :rainbow :cube (mapcar #'first *palettes*)))
(defun cube-colour (r g b)
  "The colour of CUBE-PALETTE nearest to R, G, B, each from 0 to 5."
  (+ 16 (* 36 r) (* 6 g) b))
(defun cube-palette ()
  "Make the palette a cube of colours, so that a colour can be asked for by
its parts: colours 16 to 231 are every mixture of six levels of red, green
and blue (see CUBE-COLOUR), 232 to 255 are greys from dark to light, and 0
to 15 are left as the sixteen colours of the text screen."
  (dotimes (r 6)
    (dotimes (g 6)
      (dotimes (b 6)
        (set-palette (cube-colour r g b) (round (* r 63) 5) (round (* g 63) 5) (round (* b 63) 5)))))
  (dotimes (i 24)
    (let ((level (round (* (1+ i) 63) 25)))
      (set-palette (+ 232 i) level level level))))
(defun use-palette (name &optional (first 1) (last 255))
  "Make colours FIRST to LAST the palette called NAME: :RAINBOW, :CUBE (which
takes the whole palette), or one of *PALETTES* -- :GREY, :FIRE, :ICE,
:OCEAN, :FOREST, :SUNSET, :HEAT, :COPPER, :SEPIA, :AMBER, :PHOSPHOR, :NEON,
:PASTEL, :EARTH.  (PALETTE-NAMES lists them.)"
  (case name
    (:rainbow (rainbow-palette first last))
    (:cube (cube-palette))
    (t (let ((colours (second (assoc name *palettes*))))
         (unless colours (error "No palette is called ~S: there are ~{~S~^, ~}" name (palette-names)))
         (gradient-palette colours first last))))
  name)
(defun save-palette ()
  "The whole palette as it is now: a vector of 768 numbers, for RESTORE-PALETTE and FADE-PALETTE."
  (let ((saved (make-array 768 :element-type '(unsigned-byte 8))))
    (outb #x3C7 0)
    (dotimes (i 768 saved) (setf (aref saved i) (logand 63 (inb #x3C9))))))
(defun fade-palette (saved level)
  "Show the palette SAVED (from SAVE-PALETTE) at LEVEL sixty-fourths of its
brightness: 0 is black, 64 is the palette itself.  Called once a frame
with a rising level, a picture fades in."
  (outb #x3C8 0)
  (dotimes (i 768) (outb #x3C9 (ash (* (aref saved i) (max 0 (min 64 level))) -6))))
(defun restore-palette (saved)
  "Make the palette what SAVE-PALETTE found."
  (fade-palette saved 64))
(defun rotate-palette (&optional (first 1) (last 255) (step 1))
  "Move colours FIRST to LAST along by STEP places, the ones pushed off one
end coming back at the other: called once a frame, the colours flow
through a picture that itself stays as it is."
  (let* ((n (- last first -1))
         (colours (loop for i from first to last
                        collect (multiple-value-list (get-palette i)))))
    (loop for i below n
          for (r g b) = (nth (mod (- i step) n) colours)
          do (set-palette (+ first i) r g b))))
(declaim (inline plot))
(defun plot (x y colour)
  "Set the pixel at X, Y in mode 13h; off the screen, nothing happens."
  (declare (fixnum x y colour))
  (when (and (< -1 x 320) (< -1 y 200))
    (setf (sap-ref-8 *vga* (+ x (* 320 y))) colour)))
(defun draw-line (x0 y0 x1 y1 colour)
  "Draw a line in mode 13h (Bresenham's algorithm)."
  (let* ((dx (abs (- x1 x0))) (dy (- (abs (- y1 y0))))
         (sx (if (< x0 x1) 1 -1)) (sy (if (< y0 y1) 1 -1)) (err (+ dx dy)))
    (loop (plot x0 y0 colour)
          (when (and (= x0 x1) (= y0 y1)) (return))
          (let ((e2 (* 2 err)))
            (when (>= e2 dy) (incf err dy) (incf x0 sx))
            (when (<= e2 dx) (incf err dx) (incf y0 sy))))))

;;; More to draw with, all in mode 13h and all clipped to the screen: a
;;; shape that is partly, or wholly, off it draws the part that is on.
;;; Filled shapes are made of horizontal runs, which is the quick way to
;;; fill anything in a mode where a row of pixels is a row of bytes.
(defun get-pixel (x y)
  "The colour of the pixel at X, Y in mode 13h, or NIL off the screen."
  (when (and (< -1 x 320) (< -1 y 200))
    (sap-ref-8 *vga* (+ x (* 320 y)))))
(defun clear-screen (&optional (colour 0))
  "Make every pixel of mode 13h COLOUR."
  (let ((word (* colour #x01010101)))
    (dotimes (i 16000) (setf (sap-ref-32 *vga* (* 4 i)) word))))
(defun horizontal-line (x0 x1 y colour)
  "Draw the pixels from X0 to X1 of row Y."
  (declare (fixnum x0 x1 y colour))
  (when (< -1 y 200)
    (let ((row (* 320 y)))
      (loop for x fixnum from (max 0 (min x0 x1)) to (min 319 (max x0 x1))
            do (setf (sap-ref-8 *vga* (+ row x)) colour)))))
(defun draw-rectangle (x y width height colour)
  "Draw the outline of the rectangle WIDTH by HEIGHT whose top left corner is X, Y."
  (when (and (plusp width) (plusp height))
    (let ((right (+ x width -1)) (bottom (+ y height -1)))
      (horizontal-line x right y colour)
      (horizontal-line x right bottom colour)
      (loop for row from y to bottom
            do (plot x row colour) (plot right row colour)))))
(defun fill-rectangle (x y width height colour)
  "Fill the rectangle WIDTH by HEIGHT whose top left corner is X, Y."
  (when (plusp width)
    (loop for row from (max 0 y) below (min 200 (+ y height))
          do (horizontal-line x (+ x width -1) row colour))))
(defun ellipse-half-width (rx ry dy)
  ;; how far the ellipse reaches to each side, DY rows from its centre
  (cond ((> (abs dy) ry) -1)
        ((zerop ry) rx)
        (t (values (round (sqrt (/ (* rx rx (- (* ry ry) (* dy dy))) (* ry ry))))))))
(defun draw-ellipse (cx cy rx ry colour)
  "Draw the outline of the ellipse centred on CX, CY that reaches RX to each
side and RY up and down."
  (loop for dy from (max (- ry) (- -1 cy)) to (min ry (- 200 cy))
        for w = (ellipse-half-width rx ry dy)
        ;; (as far in as the next row towards the nearer end, so that the outline has no gaps)
        for inner = (min w (1+ (ellipse-half-width rx ry (if (minusp dy) (1- dy) (1+ dy)))))
        do (horizontal-line (- cx w) (- cx inner) (+ cy dy) colour)
           (horizontal-line (+ cx inner) (+ cx w) (+ cy dy) colour)))
(defun fill-ellipse (cx cy rx ry colour)
  "Fill the ellipse centred on CX, CY that reaches RX to each side and RY up and down."
  (loop for dy from (max (- ry) (- -1 cy)) to (min ry (- 200 cy))
        for w = (ellipse-half-width rx ry dy)
        do (horizontal-line (- cx w) (+ cx w) (+ cy dy) colour)))
(defun draw-circle (cx cy radius colour)
  "Draw a circle centred on CX, CY.  (In mode 13h a pixel is taller than it
is wide, so on the screen it is an upright oval: DRAW-ELLIPSE with RY five
sixths of RX looks round.)"
  (draw-ellipse cx cy radius radius colour))
(defun fill-circle (cx cy radius colour)
  "Fill a circle centred on CX, CY."
  (fill-ellipse cx cy radius radius colour))
(defun draw-polygon (points colour)
  "Draw the outline of the polygon whose corners are POINTS, a list of (x y)
lists: the last is joined to the first."
  (loop for (from to) on (append points (list (first points)))
        while to
        do (draw-line (first from) (second from) (first to) (second to) colour)))
(defun fill-polygon (points colour)
  "Fill the polygon whose corners are POINTS, a list of (x y) lists.  It need
not be convex; where it crosses itself, what is inside an odd number of
times is filled."
  (let ((top (reduce #'min points :key #'second))
        (bottom (reduce #'max points :key #'second))
        (edges (loop for (from to) on (append points (list (first points)))
                     while to
                     unless (= (second from) (second to))
                       collect (if (< (second from) (second to)) (list from to) (list to from)))))
    (loop for y from (max 0 top) to (min 199 bottom)
          do (let ((crossings
                     (sort (loop for ((x0 y0) (x1 y1)) in edges
                                 ;; (an edge owns its top end and not its bottom: a corner counts once)
                                 when (and (<= y0 y) (< y y1))
                                   collect (+ x0 (round (* (- y y0) (- x1 x0)) (- y1 y0))))
                           #'<)))
               (loop for (left right) on crossings by #'cddr
                     while right
                     do (horizontal-line left right y colour))))
    ;; the outline, so that the bottom corners and flat edges are there too
    (draw-polygon points colour)))
(defun fill-area (x y colour)
  "Fill, with COLOUR, the patch of one colour that the pixel at X, Y is in:
it spreads until it meets other colours or the edge of the screen."
  (let ((old (get-pixel x y)))
    (when (and old (/= old colour))
      (let ((todo (list (cons x y))))
        (loop while todo
              do (destructuring-bind (px . py) (pop todo)
                   (when (eql (get-pixel px py) old)
                     (let ((left px) (right px))
                       (loop while (eql (get-pixel (1- left) py) old) do (decf left))
                       (loop while (eql (get-pixel (1+ right) py) old) do (incf right))
                       (horizontal-line left right py colour)
                       ;; one seed for each run of the old colour in the rows above and below
                       (dolist (row (list (1- py) (1+ py)))
                         (let ((inside nil))
                           (loop for sx from left to right
                                 do (if (eql (get-pixel sx row) old)
                                        (unless inside (push (cons sx row) todo) (setf inside t))
                                        (setf inside nil)))))))))))))
