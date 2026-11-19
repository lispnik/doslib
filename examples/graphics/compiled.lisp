;;;; Compiled sprites: a sprite turned into the machine code that draws it,
;;;; and raced against a blitter written in Lisp.
;;;;
;;;;   (doslib-examples:run "compiled")
;;;; Esc leaves.
;;;;
;;;; A blitter is a loop: for each of the sprite's 256 pixels, fetch it, see
;;;; whether it is the transparent colour, and if it is not, store it.  But
;;;; a sprite's picture does not change, so all the fetching and testing
;;;; comes out the same every time.  A compiled sprite does that work once,
;;;; ahead of time: the sprite becomes a routine with no loop and no tests
;;;; in it, only the stores, with the colours written into the instructions
;;;; themselves:
;;;;
;;;;       push edi
;;;;       mov  edi, [esp+8]                    ; where the corner goes
;;;;       mov  dword [edi+5], 04040404h        ; four pixels of the top row
;;;;       mov  word  [edi+9], 0404h            ; and two more
;;;;       mov  dword [edi+323], 0C0C0404h      ; the next row is 320 further on
;;;;       ...
;;;;       mov  byte  [edi+4810], 04h
;;;;       pop  edi
;;;;       ret
;;;;
;;;; Transparent pixels cost nothing at all: there is simply no instruction
;;;; for them.  The price is memory (the routine is about twice the size of
;;;; the 256 bytes it draws), a routine for every picture, and no way to
;;;; clip at the edge of the screen.  Games of the early 1990s did it for
;;;; the sprites that mattered.
;;;;
;;;; Here the code is assembled when the program runs, by COMPILE-SPRITE,
;;;; put in memory with the library's MACHINE-CODE and called with
;;;; ALIEN-FUNCALL.  The screen shows how big it came out, and then the two
;;;; ways of drawing take turns, each scattering sprites over its own half
;;;; of the screen -- the Lisp blitter (compiled Lisp, with its types
;;;; declared) a thousand at a time, the compiled sprite four thousand --
;;;; timed by the timer chip, with sprites a second for each.  Before the
;;;; race both draw one sprite into ordinary memory and the results are
;;;; compared, so the bottom line can say that they draw the same pixels.
;;;;
;;;; If the sprite editor (the example "sprite") has saved a sprite in
;;;; SPRITE.DAT in the current directory, that is the sprite compiled;
;;;; otherwise it is a ball.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.compiled
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.compiled)

(defconstant +width+ 320)
(defconstant +size+ 16)                 ; a sprite is this many pixels square
(defconstant +panel-y+ 40)              ; each way has a panel 152 wide and 144 high
(defconstant +lisp-x+ 0) (defconstant +compiled-x+ 168)
(defconstant +lisp-turn+ 1000) (defconstant +compiled-turn+ 4000)    ; sprites in a turn

(deftype bytes () '(simple-array (unsigned-byte 8) (*)))
(defun make-bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

;;; ---- time, finer than the BIOS's 18.2 ticks a second
(defconstant +clocks-a-second+ 1193182)
(defvar *clock* 0 "The last time CLOCK gave.")
(defun clock ()
  "The time in the timer chip's clocks, 1,193,182 to the second: the BIOS's
count of ticks, and how far counter 0 has got through the 65,536 clocks of
this one."
  (loop
    (let ((tick (ticks)))
      (outb #x43 #xC2)                  ; read back counter 0: its status, then its count
      (let* ((status (inb #x40)) (low (inb #x40))
             (gone (logand (- (+ low (* 256 (inb #x40)))) #xFFFF))
             (now (+ (* tick 65536)
                     (if (= (logand status 6) 6)
                         ;; mode 3, a square wave: it counts down twice a tick, by twos,
                         ;; and its output (bit 7) is high the first time
                         (+ (if (logbitp 7 status) 0 32768) (floor gone 2))
                         gone))))       ; mode 2: once a tick, by ones
        ;; The counter can start again a moment before the BIOS has counted the
        ;; tick, and then the time seems to go back by one tick: read again.
        (unless (or (/= tick (ticks)) (< -65537 (- now *clock*) 0))
          (return (setf *clock* now)))))))
;;; ---- the sprite
(defun ball ()
  "The built-in sprite: a red ball with a highlight, colour 0 around it."
  (let ((sprite (make-bytes (* +size+ +size+))))
    (dotimes (row +size+ sprite)
      (dotimes (column +size+)
        (let ((d (+ (expt (- column 7.5) 2) (expt (- row 7.5) 2))))
          (setf (aref sprite (+ column (* +size+ row)))
                (cond ((> d 56) 0) ((> d 42) 4)
                      ((< (+ (expt (- column 5) 2) (expt (- row 5) 2)) 5) 15)
                      (t 12))))))))
(defun load-sprite (&optional (file "SPRITE.DAT"))
  "The sprite saved by the sprite editor, or the ball if there is none (or
it is empty)."
  (let ((sprite (make-bytes (* +size+ +size+))))
    (if (and (ignore-errors
              (with-open-file (in file :element-type '(unsigned-byte 8) :if-does-not-exist nil)
                (and in (= (read-sequence sprite in) (length sprite)))))
             (find 0 sprite :test #'/=))
        sprite
        (ball))))

;;; ---- the blitter in Lisp, and text.  DEST is the address of the picture.
(defun blit (dest sprite x y)
  "Draw SPRITE with its corner at X, Y, leaving out colour 0."
  (declare (type system-area-pointer dest) (type bytes sprite) (type (unsigned-byte 16) x y)
           (optimize speed (safety 0)))
  (let ((at (+ x (* +width+ y))) (i 0))
    (declare (fixnum at i))
    (dotimes (row +size+)
      (dotimes (column +size+)
        (let ((colour (aref sprite i)))
          (unless (zerop colour) (setf (sap-ref-8 dest (+ at column)) colour)))
        (incf i))
      (incf at +width+))))

(defun draw-text (dest x y string colour)
  "STRING in the BIOS's 8 by 8 characters (they are in the ROM, at FFA6Eh),
on a black bar."
  (declare (type system-area-pointer dest) (type (unsigned-byte 16) x y) (fixnum colour)
           (simple-string string)
           (optimize speed (safety 0)))
  (let ((font (int-sap #xFFA6E)))
    (dotimes (i (min (length string) 40))
      (let ((code (logand (char-code (char string i)) 127)))
        (dotimes (row 8)
          (let ((bits (sap-ref-8 font (+ (* 8 code) row)))
                (at (+ x (* 8 i) (* +width+ (+ y row)))))
            (declare (fixnum at))
            (dotimes (column 8)
              (setf (sap-ref-8 dest (+ at column))
                    (if (logbitp (- 7 column) bits) colour 0)))))))))


;;; ---- the compiler
(defun compile-sprite (sprite)
  "The machine code that draws SPRITE: void draw (unsigned char *corner),
for a picture 320 bytes wide.  Returns the bytes as a list, and how many
instructions store pixels."
  (let ((code '()) (stores 0))
    (labels ((emit (&rest bytes) (dolist (byte bytes) (push byte code)))
             (emit-number (value bytes)           ; low byte first
               (dotimes (i bytes) (emit (ldb (byte 8 (* 8 i)) value))))
             (pixel (column row)
               (if (< column +size+) (aref sprite (+ column (* +size+ row))) 0))
             (opaque (column row)       ; how many pixels from here on are to be drawn
               (loop for c from column while (/= 0 (pixel c row)) count t))
             (store (column row pixels)
               "mov byte, word or dword [edi+offset], the colours of 1, 2 or 4 PIXELS"
               (let ((offset (+ column (* +width+ row)))
                     (colours (loop for i below pixels
                                    sum (ash (pixel (+ column i) row) (* 8 i)))))
                 (when (= pixels 2) (emit #x66))                ; operand size: 16 bits
                 (emit (if (= pixels 1) #xC6 #xC7))             ; mov r/m, immediate
                 (cond ((< offset 128) (emit #x47 offset))      ; [edi+disp8]
                       (t (emit #x87) (emit-number offset 4)))  ; [edi+disp32]
                 (emit-number colours pixels)
                 (incf stores))))
      (emit #x57)                       ; push edi
      (emit #x8B #x7C #x24 #x08)        ; mov edi,[esp+8]
      (dotimes (row +size+)
        (let ((column 0))
          (loop while (< column +size+)
                do (let* ((run (opaque column row))
                          (pixels (cond ((>= run 4) 4) ((>= run 2) 2) (t run))))
                     (cond ((zerop run) (incf column))          ; transparent: nothing
                           (t (store column row pixels) (incf column pixels)))))))
      (emit #x5F #xC3))                 ; pop edi / ret
    (values (nreverse code) stores)))

;;; ---- the race.  Each way draws COUNT sprites at scattered places in its
;;; panel; the places come from SEED, and the new seed is returned.  The
;;; two loops are the same but for the line that draws.
(defmacro scattering ((at seed count panel-x) &body body)
  `(let ((seed ,seed))
     (declare (type (unsigned-byte 16) seed))
     (dotimes (i ,count seed)
       (setf seed (logand (+ (* seed 4005) 1) #xFFFF))
       (let ((,at (+ ,panel-x (ash (* (ash seed -8) 137) -8)                    ; x: 0 to 136
                     (* +width+ (+ +panel-y+ (ash (* (logand seed 255) 129) -8))))))  ; y: to 128
         (declare (type (unsigned-byte 16) ,at))
         ,@body))))
(defun lisp-turn (dest sprite seed count)
  (declare (type system-area-pointer dest) (type bytes sprite) (fixnum count)
           (optimize speed (safety 0)))
  (scattering (at seed count +lisp-x+)
    (blit dest sprite at 0)))
(defun compiled-turn (dest code seed count)
  (declare (type (unsigned-byte 32) dest) (type system-area-pointer code) (fixnum count)
           (optimize speed (safety 0)))
  (scattering (at seed count +compiled-x+)
    (alien-funcall (sap-alien code (function void unsigned-long)) (+ dest at))))

(defun same-pixels-p (sprite code)
  "Do the blitter and the compiled sprite draw the same thing?  Each draws
into 16 lines of ordinary memory, over a pattern, and the two are compared."
  (let ((one (make-bytes (* +width+ +size+))) (other (make-bytes (* +width+ +size+))))
    (dotimes (i (length one)) (setf (aref one i) (logand i 255) (aref other i) (logand i 255)))
    (with-pinned-objects (one other)
      (blit (vector-sap one) sprite 100 0)
      (alien-funcall (sap-alien code (function void system-area-pointer))
                     (sap+ (vector-sap other) 100)))
    (equalp one other)))

(defun text (control &rest arguments)
  (coerce (apply #'format nil control arguments) 'simple-string))

(defun compiled-demo ()
  (let ((sprite (load-sprite)) (lisp-rate 0) (compiled-rate 0) (bytes 0) (same nil))
    (multiple-value-bind (code-bytes stores) (compile-sprite sprite)
      (setf bytes (length code-bytes))
      (let ((code (apply #'machine-code code-bytes)) (screen *vga*))
        (unwind-protect
             (with-video-mode (#x13)
               (setf same (same-pixels-p sprite code))
               (draw-text screen 0 0 (text "A sprite of ~D pixels, compiled:"
                                           (count 0 sprite :test #'/=))
                          15)
               (draw-text screen 0 8 (text "~D instructions, ~:D bytes of code" (+ stores 4) bytes)
                          15)
               (draw-text screen +lisp-x+ 20 "Lisp blitter" 14)
               (draw-text screen +compiled-x+ 20 "Compiled sprite" 14)
               (draw-text screen 296 192 "Esc" 7)
               (let ((lisp-seed 1) (compiled-seed 1)
                     (lisp-clocks 0) (compiled-clocks 0) (lisp-sprites 0) (compiled-sprites 0))
                 (loop
                   (when (and (key-pressed-p) (eql (get-key) #\Escape)) (return))
                   (let ((start (clock)) middle)
                     (setf lisp-seed (lisp-turn screen sprite lisp-seed +lisp-turn+)
                           middle (clock)
                           compiled-seed (compiled-turn (sap-int screen) code compiled-seed
                                                        +compiled-turn+))
                     (let ((end (clock)))
                       (when (<= start middle end)              ; (not over midnight)
                         (incf lisp-clocks (- middle start)) (incf lisp-sprites +lisp-turn+)
                         (incf compiled-clocks (- end middle))
                         (incf compiled-sprites +compiled-turn+))))
                   ;; about twice a second, say how it is going
                   (when (and (> (+ lisp-clocks compiled-clocks) (floor +clocks-a-second+ 2))
                              (> lisp-clocks 0) (> compiled-clocks 0))
                     (setf lisp-rate (round (* lisp-sprites +clocks-a-second+) lisp-clocks)
                           compiled-rate (round (* compiled-sprites +clocks-a-second+)
                                                compiled-clocks))
                     (draw-text screen +lisp-x+ 28 (text "~9:D a second" lisp-rate) 15)
                     (draw-text screen +compiled-x+ 28 (text "~9:D a second" compiled-rate) 15)
                     (draw-text screen 0 192
                                (text "~:[THE PIXELS DIFFER!~;Same pixels~], ~,1F times as fast "
                                      same (/ compiled-rate (max 1.0 (float lisp-rate))))
                                (if same 10 12))
                     (setf lisp-clocks 0 compiled-clocks 0 lisp-sprites 0 compiled-sprites 0)))))
          (free-alien (sap-alien code (* (unsigned 8)))))))
    (format t "~&compiled: the sprite is ~:D bytes of machine code~
               ~:[, WHICH DRAWS IT WRONGLY~;~].~%~
               ~:D sprites a second with the compiled sprite, ~:D with the Lisp blitter.~%"
            bytes same compiled-rate lisp-rate)))

(defun main ()
  "A sprite compiled to machine code, raced against a blitter in Lisp."
  (compiled-demo)
  (values))

(doslib-examples:define-example "compiled" 'main
  :category :graphics
  :description "Compiled sprites: a sprite as machine code, raced against a Lisp blitter.")
