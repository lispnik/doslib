;;;; Live coding on two displays: the editor on one, and on the VGA screen
;;;; the picture that the code in the editor draws -- redrawn eighteen times
;;;; a second, and redefined as you type.
;;;;
;;;;   M-x live        start: the editor moves to the second display, the VGA
;;;;                   goes to 320x200 graphics, and a sketch opens to edit
;;;;   M-x live-auto   whether forms are evaluated by themselves, half a
;;;;                   second after you stop typing (on, to begin with);
;;;;                   C-c C-c evaluates the form at point at any time
;;;;   M-x live-stop   back to one screen
;;;;
;;;; The sketch is ordinary Lisp in the editor's user package.  Its function
;;;; DRAW is called with the frame number, and draws with CLEAR, DOT, LINE,
;;;; DISC and BOX (colours are 0 to 255; the palette is DOSLIB's rainbow).
;;;; When the text of a top-level form changes and reads as Lisp, that form
;;;; alone is evaluated again; a form that is half typed is left alone, and
;;;; after an error in DRAW the picture stands still, DRAW being tried again
;;;; once a second and whenever something is evaluated.
;;;;
;;;; The second display is one of
;;;;   - the "secondary-vga" card of QEMU and UTM: a PCI framebuffer with no
;;;;     text mode and no BIOS, on which the editor's character cells are
;;;;     drawn as pixels, in the VGA's own font;
;;;;   - a monochrome (MDA or Hercules) card next to the VGA, the classic
;;;;     two-monitor arrangement of DOS debuggers.  This path is written from
;;;;     the documentation and has not been run: QEMU has no such card.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-live
  (:use #:cl #:doslib #+sbcl #:sb-sys #:doslib-compat)
  (:export #:clear #:dot #:line #:disc #:box))
(in-package #:doslib-live)

;;; ---- what a sketch draws with.  It draws into memory; the finished frame
;;; is copied to the screen in one go, so that nothing flickers.
(deftype canvas () '(simple-array (unsigned-byte 8) (64000)))
(defvar *canvas* (make-array 64000 :element-type '(unsigned-byte 8)))
(defun clear (&optional (colour 0))
  "Fill the picture with COLOUR."
  (fill (the canvas *canvas*) (logand colour 255))
  (values))
(declaim (inline %dot))
(defun %dot (x y colour)
  (declare (fixnum x y colour))
  (when (and (< -1 x 320) (< -1 y 200))
    (setf (aref (the canvas *canvas*) (+ x (* 320 y))) (logand colour 255))))
(defun dot (x y colour)
  "One pixel.  Coordinates may be any real numbers; they are rounded."
  (%dot (round x) (round y) colour)
  (values))
(defun line (x0 y0 x1 y1 colour)
  "A line from X0, Y0 to X1, Y1."
  (let* ((x0 (round x0)) (y0 (round y0)) (x1 (round x1)) (y1 (round y1))
         (dx (abs (- x1 x0))) (dy (- (abs (- y1 y0))))
         (sx (if (< x0 x1) 1 -1)) (sy (if (< y0 y1) 1 -1)) (err (+ dx dy)))
    (when (< (+ dx (- dy)) 4000)        ; a wild number must not hang the picture
      (loop (%dot x0 y0 colour)
            (when (and (= x0 x1) (= y0 y1)) (return))
            (let ((e2 (* 2 err)))
              (when (>= e2 dy) (incf err dy) (incf x0 sx))
              (when (<= e2 dx) (incf err dx) (incf y0 sy))))))
  (values))
(defun box (x y width height colour)
  "A filled rectangle with its corner at X, Y."
  (let ((x (round x)) (y (round y)))
    (loop for py from (max 0 y) below (min 200 (+ y (round height)))
          do (loop for px from (max 0 x) below (min 320 (+ x (round width)))
                   do (%dot px py colour))))
  (values))
(defun disc (x y radius colour)
  "A filled circle."
  (let ((x (round x)) (y (round y)) (r (min 400 (round radius))))
    (loop for dy from (- r) to r
          do (let ((half (isqrt (- (* r r) (* dy dy)))))
               (loop for px from (max 0 (- x half)) to (min 319 (+ x half))
                     do (%dot px (+ y dy) colour)))))
  (values))
(defun show ()                          ; the finished frame, onto the VGA
  (let ((canvas *canvas*) (vga *vga*))
    (declare (type canvas canvas) (optimize speed))
    (copy-vector-to-memory canvas 0 vga 0 64000)))

(in-package #:doslib-emacs)

;;; ---- the second display.  Either kind is fed from the same 80x25 cells:
;;; the editor draws into *LIVE-CELLS*, and a flush function puts on the
;;; display the cells that differ from what it last put there.
(defvar *live-cells* nil)               ; 4000 bytes, in place of the VGA text memory
(defvar *live-shown* (make-array 2000 :element-type '(unsigned-byte 32)))
(defvar *live-cursor* 0)                ; the cell the cursor is in
(defvar *live-cursor-shown* 0)
(defconstant +live-never+ #x10000)      ; no cell has this value: "draw it"

;;; -- QEMU's secondary-vga: a framebuffer (BAR 0) and, in BAR 2, the VGA
;;; registers and the Bochs "dispi" registers that set its resolution.
(defvar *live-fb* nil)
(defvar *live-mmio* nil)
(defvar *live-font* (make-array 4096 :element-type '(unsigned-byte 8)))
(defparameter *live-colours*            ; the 16 colours of text mode, as 00RRGGBB
  (coerce '(#x000000 #x0000AA #x00AA00 #x00AAAA #xAA0000 #xAA00AA #xAA5500 #xAAAAAA
            #x555555 #x5555FF #x55FF55 #x55FFFF #xFF5555 #xFF55FF #xFFFF55 #xFFFFFF)
          '(simple-array (unsigned-byte 32) (16))))
(defun live-find-card ()
  "The addresses of the framebuffer and the registers of a secondary-vga card, or NIL."
  (dotimes (bus 4)
    (dotimes (device 32)
      (flet ((register (r) (doslib-examples.pciscan::pci-read bus device 0 r)))
        (when (and (= (register 0) #x11111234)            ; QEMU's "standard VGA"...
                   (= (ash (register 8) -16) #x0380))     ; ...as a display that is not the VGA
          (doslib-examples.pciscan::pci-write bus device 0 4
                                              (logior (register 4) 2))   ; answer to memory
          (return-from live-find-card
            (values (logandc2 (register #x10) 15) (logandc2 (register #x18) 15))))))))
(defun live-map (physical size)
  "Make SIZE bytes of device memory at PHYSICAL addressable (the library's MAP-PHYSICAL:
DPMI 0800h); returns a SAP."
  (let ((linear (map-physical physical size)))
    (unless linear (editor-error "The DPMI host would not map the display's memory"))
    (int-sap linear)))
(defun live-read-font ()
  "Copy the VGA's 8x16 font out of the card.  The VGA must be in text mode."
  (doslib-examples.font::call-with-font-memory
   (lambda ()
     (dotimes (code 256)
       (dotimes (row 16)
         (setf (aref *live-font* (+ (* 16 code) row)) (sap-ref-8 *vga* (+ (* 32 code) row))))))))
(defun live-flush-pixels ()
  (let ((cells *live-cells*) (shown *live-shown*) (fb *live-fb*) (font *live-font*)
        (colours *live-colours*) (cursor *live-cursor*))
    (declare (type (simple-array (unsigned-byte 32) (2000)) shown)
             (type (simple-array (unsigned-byte 8) (4096)) font)
             (type (simple-array (unsigned-byte 32) (16)) colours)
             (type system-area-pointer cells fb) (type (integer 0 1999) cursor)
             (optimize speed))
    (unless (= cursor (the fixnum *live-cursor-shown*))  ; draw both again: where it was, where it is
      (setf (aref shown (the (integer 0 1999) *live-cursor-shown*)) +live-never+
            (aref shown cursor) +live-never+
            *live-cursor-shown* cursor))
    (dotimes (i 2000)
      (let ((cell (sap-ref-16 cells (* 2 i))))
        (unless (= cell (aref shown i))
          (setf (aref shown i) cell)
          (let ((ink (aref colours (ldb (byte 4 8) cell)))
                (paper (aref colours (ldb (byte 4 12) cell)))
                (glyph (* 16 (ldb (byte 8 0) cell))))
            (multiple-value-bind (row column) (floor i 80)
              (dotimes (y 16)
                (let ((bits (if (and (= i cursor) (>= y 13)) 255 (aref font (+ glyph y))))
                      (offset (* 4 (+ (* 640 (+ y (* 16 row))) (* 8 column)))))
                  (declare (type (unsigned-byte 8) bits) (fixnum offset))
                  (dotimes (x 8)
                    (setf (sap-ref-32 fb (+ offset (* 4 x)))
                          (if (logbitp (- 7 x) bits) ink paper))))))))))))
(defun live-open-pixels (framebuffer registers)
  (live-read-font)
  (setf *live-fb* (live-map framebuffer (* 640 400 4))
        *live-mmio* (live-map registers 4096))
  (flet ((dispi (index value) (setf (sap-ref-16 *live-mmio* (+ #x500 (* 2 index))) value)))
    (dispi 4 0)                                 ; off, while it is set up
    (dispi 1 640) (dispi 2 400) (dispi 3 32)    ; 640x400, 32 bits a pixel: 80x25 cells of 8x16
    (dispi 4 #x41))                             ; on, with the linear framebuffer
  (setf (sap-ref-8 *live-mmio* #x400) #x20)     ; the VGA's "screen on" bit (port 3C0h)
  (setf *flush-screen* 'live-flush-pixels))
(defun live-close-pixels ()
  (when *live-mmio*
    (dotimes (i (* 640 400)) (setf (sap-ref-32 *live-fb* (* 4 i)) 0))
    (setf *live-mmio* nil *live-fb* nil)))

;;; -- a monochrome card: text cells at B0000h, its 6845 at ports 3B4h/3B5h.
;;; (Not run: see the top of the file.)
(defun live-mono-present-p ()
  (outb #x3B4 #x0F)
  (let ((old (inb #x3B5)))
    (outb #x3B5 #x5A)
    (prog1 (progn (outb #x3B4 #x0F) (= (inb #x3B5) #x5A))
      (outb #x3B5 old))))
(defun live-flush-mono ()
  (let ((cells *live-cells*) (shown *live-shown*) (card (int-sap #xB0000)))
    (dotimes (i 2000)
      (let ((cell (sap-ref-16 cells (* 2 i))))
        (unless (= cell (aref shown i))
          (setf (aref shown i) cell)
          ;; colours mean nothing here: a coloured background becomes
          ;; reverse video, a bright colour bright, and all else plain
          (let ((attribute (ash cell -8)))
            (setf (sap-ref-16 card (* 2 i))
                  (logior (logand cell 255)
                          (ash (cond ((logtest attribute #x70) #x70)
                                     ((logtest attribute 8) #x0F)
                                     (t 7))
                               8)))))))
    (outb #x3B4 #x0E) (outb #x3B5 (ash *live-cursor* -8))
    (outb #x3B4 #x0F) (outb #x3B5 (logand *live-cursor* 255))))
(defun live-open-mono ()
  ;; A card that is not the BIOS's display has never been set up: give its
  ;; 6845 the standard 80x25 timings and turn the picture on.
  (outb #x3B8 1)
  (loop for value in '(#x61 #x50 #x52 #x0F #x19 #x06 #x19 #x19 #x02 #x0D #x0B #x0C 0 0 0 0)
        for index from 0
        do (outb #x3B4 index) (outb #x3B5 value))
  (outb #x3B8 #x29)
  (setf *flush-screen* 'live-flush-mono))

;;; ---- the sketch
(defvar *live-buffer* nil)
(defvar *live-package* nil)
(defvar *live-forms* '() "The text of each top-level form, as last evaluated.")
(defvar *live-auto* t)
(defvar *live-checksum* 0)
(defvar *live-changed-at* 0)
(defvar *live-dirty* nil)
(defvar *live-tick* 0)
(defvar *live-frame* 0)
(defvar *live-stopped* nil
  "After an error in DRAW: the tick at which to try it again (a second later,
or as soon as something is evaluated).")
(defparameter *live-sketch* ";;; Live coding.  The VGA screen shows what DRAW draws, eighteen times a
;;; second.  Change a number and stop typing for a moment: the form is
;;; evaluated again by itself.  (C-c C-c does it at once; M-x live-stop ends.)

(defparameter *count* 40)

(defun draw (frame)
  (clear)
  (dotimes (i *count*)
    (let* ((a (+ (* i 0.3) (* frame 0.04)))
           (x (+ 160 (* 120 (cos a))))
           (y (+ 100 (* 70 (sin (* 2 a))))))
      (disc x y (+ 3 (mod i 5)) (+ 32 (* 5 i))))))
")

(defun live-checksum (b)
  (let ((sum 0))
    (dotimes (i (nlines b) sum)
      (setf sum (logand #xFFFFFF (+ (* 31 sum) (sxhash (line-text i b))))))))
(defun live-form-name (form)            ; DRAW, for (defun draw ...): something to say
  (if (and (consp form) (consp (cdr form)) (symbolp (second form)))
      (string-downcase (second form))
      "a form"))
(defun live-evaluate (b)
  "Evaluate the top-level forms of B whose text has changed.  True if there is something to show."
  (let ((text (buffer-string b)) (*package* *live-package*) (position 0) (eof '#:eof)
        (forms '()) (kept '()) (done '())
        (*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
    (handler-case
        (progn
          (loop (multiple-value-bind (form end) (read-from-string text nil eof :start position)
                  (when (eq form eof) (return))
                  (push (cons form (string-trim '(#\Space #\Newline #\Tab) (subseq text position end)))
                        forms)
                  (setf position end)))
          (dolist (entry (nreverse forms))
            (unless (member (cdr entry) *live-forms* :test #'string=)
              (handler-bind ((warning #'muffle-warning))
                (eval (car entry)))
              (push (live-form-name (car entry)) done))
            (push (cdr entry) kept))
          (setf *live-forms* kept)
          (when done
            (setf *live-stopped* nil)
            (message "Live: ~{~A~^, ~} evaluated" (reverse done))
            t))
      ((or end-of-file reader-error) () nil)    ; half typed: wait for the rest
      (error (e)
        (setf *live-forms* kept)                ; what did work need not be done again
        (message "Live: ~A" e)
        t))))
(defun live-draw ()
  "One frame.  True if there is something to show in the editor."
  (unless (= (peek #x449) #x13)         ; an example, or the calculator, changed the mode
    (video-mode #x13)
    (rainbow-palette))
  (let ((draw (find-symbol "DRAW" *live-package*)))
    (when (and draw (fboundp draw)
               (or (null *live-stopped*) (>= (abs (- *live-tick* *live-stopped*)) 18)))
      (setf *live-stopped* nil)
      (handler-case
          (let ((*standard-output* (make-broadcast-stream)))
            (funcall draw *live-frame*)
            (incf *live-frame*)
            (doslib-live::show)
            nil)
        (error (e)
          (setf *live-stopped* *live-tick*)
          (message "Live: DRAW stopped: ~A" e)
          t)))))
(defun live-idle ()
  (cond ((not (member *live-buffer* *buffers*)) (live-stop) t)
        (t (let ((now (peek32 #x46C)) (changed nil))
             (unless (= now *live-tick*)        ; once a tick: 18.2 times a second
               (setf *live-tick* now)
               (let ((checksum (live-checksum *live-buffer*)))
                 (unless (= checksum *live-checksum*)
                   (setf *live-checksum* checksum *live-changed-at* now *live-dirty* t)))
               (when (and *live-dirty* *live-auto*
                          (>= (abs (- now *live-changed-at*)) 9))       ; half a second of quiet
                 (setf *live-dirty* nil)
                 (when (live-evaluate *live-buffer*) (setf changed t)))
               (when (live-draw) (setf changed t)))
             changed))))

(define-command live ()
  "Live coding: the editor on a second display, the picture its code draws on the VGA."
  (when *flush-screen* (editor-error "Live coding is already going (M-x live-stop ends it)"))
  (multiple-value-bind (framebuffer registers) (live-find-card)
    (let ((mono (and (not framebuffer) (live-mono-present-p))))
      (unless (or framebuffer mono)
        (editor-error "No second display: it needs QEMU's secondary-vga, or a monochrome card"))
      (setf *live-cells* (alien-sap (make-alien (unsigned 8) 4000)))
      (dotimes (i 2000) (setf (sap-ref-16 *live-cells* (* 2 i)) #x0720))
      (fill *live-shown* +live-never+)
      (setf *live-cursor* 0 *live-cursor-shown* 0)
      (if framebuffer (live-open-pixels framebuffer registers) (live-open-mono))
      (display-cells 80 25)             ; the second display is 80 by 25, whatever the VGA was
      (setf *screen* *live-cells*
            *place-cursor* (lambda (x y) (setf *live-cursor* (max 0 (min 1999 (+ x (* 80 y))))))
            *leave-display* 'live-stop)))
  (video-mode #x13)
  (rainbow-palette)
  ;; the drawing functions, by their plain names, in the package the sketch is read in
  (setf *live-package* *repl-package*)
  (do-external-symbols (symbol '#:doslib-live) (shadowing-import symbol *live-package*))
  (let ((b (or (find-buf "live.lisp") (make-buf "live.lisp"))))
    (when (and (= (nlines b) 1) (string= (line-text 0 b) ""))
      (setf (buf-mode b) 'lisp-mode)
      (insert-at 0 0 *live-sketch* :b b)
      (setf (buf-line b) 4 (buf-col b) 22 (buf-undo b) nil (buf-modified b) nil))
    (select-buffer b)
    (setf *live-buffer* b *live-forms* '() *live-checksum* -1 *live-stopped* nil
          *live-frame* 0 *live-dirty* nil))
  (add-idle-hook 'live-idle)
  (message "Live: edit, and watch the VGA screen.  M-x live-stop ends it."))
(define-command live-stop ()
  "End live coding: the editor comes back to the VGA screen."
  (remove-idle-hook 'live-idle)
  (when *flush-screen*
    (live-close-pixels)
    (reset-display)
    (video-mode 3)
    (message "Live coding ended")))
(define-command live-auto ()
  "Switch between evaluating changed forms by themselves and only on C-c C-c."
  (setf *live-auto* (not *live-auto*))
  (message "Live: changed forms are ~:[evaluated only on C-c C-c~;evaluated by themselves~]" *live-auto*))
