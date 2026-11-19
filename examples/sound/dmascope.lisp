;;;; A DMA scope: watch the DMA controller hand a buffer to the Sound Blaster.
;;;;
;;;;   (doslib-examples:run "dmascope")
;;;; Space pauses the card (and so the DMA controller) and lets it go again;
;;;; Esc ends.  Without a card it says so and returns.
;;;;
;;;; The "blaster" example plays sound from a buffer of two blocks that the
;;;; DMA controller goes round for ever, the card interrupting at the end of
;;;; each block and Lisp then writing new samples into the block just played.
;;;; This plays the same tune the same way and draws what is going on:
;;;;
;;;;   - the whole buffer as a waveform, block 0 on the left and block 1 on
;;;;     the right, read back from the very memory the card is given;
;;;;   - a white marker where the card is reading at this moment.  Nothing
;;;;     in the program moves it: it is the DMA controller's own "current
;;;;     address" register for the channel (port 2*channel), read after the
;;;;     byte flip-flop is cleared at port 0Ch, low byte and then high;
;;;;   - the "current count" register (port 2*channel+1): the bytes still to
;;;;     go, less one, before the controller starts again at the beginning
;;;;     (it reloads both registers by itself: that is auto-initialization);
;;;;   - the number of interrupts, and which block Lisp refilled last: it is
;;;;     drawn brighter, and it is always the one the marker has just left.
;;;;
;;;; The registers are sixteen bits wide and are read as two bytes while the
;;;; transfer goes on, so a reading may straddle a carry: they are read
;;;; until two readings agree in the high byte.  Under QEMU the registers
;;;; move in steps, a few hundred samples at a time, because the emulated
;;;; card takes its samples in batches; on a real machine they move a sample
;;;; at a time.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.dmascope
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.dmascope)

(defvar *base* #x220 "The card's first port.")
(defvar *irq* 5)
(defvar *dma* 1 "The 8-bit DMA channel.")

(defun read-settings ()
  "Take the port, interrupt and DMA channel from BLASTER=A220 I5 D1, if it is set."
  (setf *base* #x220 *irq* 5 *dma* 1)
  (let ((text (posix-getenv "BLASTER")))
    (when text
      (loop with start = 0
            for end = (or (position #\Space text :start start) (length text))
            do (when (> end (1+ start))
                 (let ((number (subseq text (1+ start) end)))
                   (case (char-upcase (char text start))
                     (#\A (setf *base* (or (parse-integer number :radix 16 :junk-allowed t)
                                           *base*)))
                     (#\I (setf *irq* (or (parse-integer number :junk-allowed t) *irq*)))
                     (#\D (setf *dma* (or (parse-integer number :junk-allowed t) *dma*))))))
               (when (>= end (length text)) (return))
               (setf start (1+ end))))))

;;; ---- the card's processor (the "DSP"), as in the blaster example
(defun dsp-write (byte)
  (loop repeat 100000 while (logbitp 7 (inb (+ *base* #xC))))
  (outb (+ *base* #xC) byte))
(defun dsp-read ()
  (loop repeat 100000 until (logbitp 7 (inb (+ *base* #xE))))
  (inb (+ *base* #xA)))
(defun dsp-reset ()
  "Reset the card.  True if one answered."
  (outb (+ *base* 6) 1)
  (dotimes (i 200) (inb (+ *base* 6)))
  (outb (+ *base* 6) 0)
  (loop repeat 2000
        thereis (and (logbitp 7 (inb (+ *base* #xE)))
                     (= (inb (+ *base* #xA)) #xAA))))
(defun dsp-version ()
  (dsp-write #xE1)
  (let* ((major (dsp-read)) (minor (dsp-read)))
    (values major minor)))

;;; (The DMA controller is the library's: DMA-START, DMA-STOP, and DMA-ADDRESS
;;; and DMA-COUNT, which read its registers while it counts.  src/dma.lisp.)

;;; ---- the interrupt: acknowledge, count, return
(defvar *counter* nil "Four bytes the handler counts interrupts in.")
(defun handler-code ()
  (let ((ds (data-selector)) (acknowledge (+ *base* #xE)))
    (machine-code #x50 #x52 #x1E                        ; push eax / push edx / push ds
                  #x66 #xB8 (lo ds) (hi ds)             ; mov ax, our data selector
                  #x8E #xD8                             ; mov ds, ax
                  #xBA (lo acknowledge) (hi acknowledge) 0 0    ; mov edx, base+0Eh
                  #xEC                                  ; in al, dx      the card: seen
                  #xFF #x05 (lo (sap-int *counter*)) (hi (sap-int *counter*))
                  (ldb (byte 8 16) (sap-int *counter*)) (ldb (byte 8 24) (sap-int *counter*))
                                                        ; inc dword [counter]
                  #xB0 #x20 #xE6 #x20                   ; mov al,20h / out 20h,al
                  #x1F #x5A #x58 #xCF)))                ; pop ds / pop edx / pop eax / iretd
(defun interrupts () (sap-ref-32 *counter* 0))

;;; ---- drawing: mode 13h, and letters from the BIOS's 8x8 font in ROM
(defconstant +font+ #xFFA6E "The first 128 characters, eight bytes each, in every PC's ROM.")

(defun draw-text (x y string colour &optional (background 0))
  "Write STRING with its top left corner at X, Y, over whatever was there."
  (loop for c across string
        for left from x by 8
        do (let ((glyph (+ +font+ (* 8 (logand (char-code c) 127)))))
             (dotimes (row 8)
               (let ((bits (peek (+ glyph row))))
                 (dotimes (column 8)
                   (plot (+ left column) (+ y row)
                         (if (logbitp (- 7 column) bits) colour background))))))))

(defconstant +block+ 4096 "Samples between interrupts.")
(defconstant +top+ 20 "The first line of the scope.")
(defconstant +height+ 96)

;;; colours: 1 dim background, 2 bright background, 3 wave, 4 bright wave,
;;; 5 the marker, 6 text, 7 numbers, 8 the centre line
(defun set-colours ()
  (set-palette 0 0 0 0)
  (set-palette 1 0 6 12) (set-palette 2 4 18 30)
  (set-palette 3 10 40 20) (set-palette 4 30 63 30)
  (set-palette 5 63 63 63) (set-palette 6 40 40 40) (set-palette 7 63 56 20)
  (set-palette 8 12 16 24))

(defun draw-scope (buffer marker bright)
  "The two blocks at BUFFER as 320 columns; column MARKER is where the card
is reading, and block BRIGHT (or neither, if NIL) is the one just refilled."
  (declare (system-area-pointer buffer) (fixnum marker) (optimize speed))
  (let ((screen (int-sap #xA0000)) (per-column (floor (* 2 +block+) 320)))
    (declare (system-area-pointer screen) (fixnum per-column))
    (dotimes (x 320)
      (let* ((first (floor (* x 2 +block+) 320))
             (low 255) (high 0)
             (lit (eql bright (floor x 160)))
             (ground (if lit 2 1)) (wave (if lit 4 3)))
        (declare (fixnum first low high))
        (dotimes (i per-column)
          (let ((sample (sap-ref-8 buffer (+ first i))))
            (when (< sample low) (setf low sample))
            (when (> sample high) (setf high sample))))
        ;; samples are 0 to 255, 128 silence: three levels to a line
        (let ((y-top (floor (* (- 255 high) +height+) 256))
              (y-bottom (floor (* (- 255 low) +height+) 256)))
          (declare (fixnum y-top y-bottom))
          (dotimes (y +height+)
            (setf (sap-ref-8 screen (+ x (* 320 (+ +top+ y))))
                  (cond ((= x marker) 5)
                        ((<= y-top y y-bottom) wave)
                        ((= y (floor +height+ 2)) 8)
                        (t ground)))))))))

;;; ---- something to play: the blaster example's tune of plucked strings,
;;; made here with whole numbers so that it is ready at once
(defparameter *tune*
  ;; (semitones above A 220, eighths)
  '((7 2) (7 2) (8 2) (10 2) (10 2) (8 2) (7 2) (5 2) (3 2) (3 2) (5 2) (7 2) (7 3) (5 1) (5 4)
    (7 2) (7 2) (8 2) (10 2) (10 2) (8 2) (7 2) (5 2) (3 2) (3 2) (5 2) (7 2) (5 3) (3 1) (3 4)))

(defun synthesize (rate)
  "The tune as a vector of bytes, 128 silence: each note one wave of three
harmonics, read at the note's speed and dying away."
  (let* ((eighth (floor (* rate 30) 132))
         (length (* eighth (reduce #'+ *tune* :key #'second)))
         (wave (make-array 256 :element-type 'fixnum))
         (mix (make-array length :element-type 'fixnum :initial-element 0))
         (samples (make-array length :element-type '(unsigned-byte 8)))
         (start 0) (peak 1))
    (declare (fixnum start peak))
    (dotimes (i 256)
      (let ((phase (/ (* 2 (float pi 1.0) i) 256)))
        (setf (aref wave i) (round (* 255 (+ (* 0.6 (sin phase)) (* 0.25 (sin (* 2 phase)))
                                             (* 0.15 (sin (* 3 phase)))))))))
    (loop for (note eighths) in *tune*
          do (let ((step (round (* 220.0 (expt 2.0 (/ note 12.0)) 65536 256) rate))
                   (phase 0) (loudness 4096))
               (declare (fixnum step phase loudness))
               ;; a note rings on over the next ones, round the end to the beginning
               (dotimes (i (* 3 eighth (max 2 eighths)))
                 (let ((at (mod (+ start i) length)))
                   (incf (aref mix at) (ash (* loudness (aref wave (ldb (byte 8 16) phase))) -8))
                   (setf phase (logand (+ phase step) #xFFFFFF))
                   (when (zerop (logand i 63)) (decf loudness (ash loudness -6))))))
             (incf start (* eighth eighths)))
    (dotimes (i length) (setf peak (max peak (abs (aref mix i)))))
    (dotimes (i length samples)
      (setf (aref samples i) (+ 128 (truncate (* 120 (aref mix i)) peak))))))

(defun main ()
  "Draw the Sound Blaster's DMA buffer and the DMA controller's registers as it plays."
  (read-settings)
  (unless (and (<= 0 *dma* 3) (/= *dma* 2) (<= 2 *irq* 7) (dsp-reset))
    (format t "~&No Sound Blaster answered at port ~Xh.  (QEMU: -device sb16; ~
               SET BLASTER=A220 I5 D1 says where a card is.)~%" *base*)
    (return-from main (values)))
  (multiple-value-bind (major minor) (dsp-version)
    (when (< major 2)
      (format t "~&The card's DSP is version ~D.~2,'0D: auto-initialized DMA came with version 2.~%"
              major minor)
      (return-from main (values)))
    (unless *counter* (setf *counter* (alien-sap (make-alien (unsigned 32) 1))))
    (setf (sap-ref-32 *counter* 0) 0)
    (let* ((rate 11025)
           (samples (synthesize rate))
           (total (length samples))
           (position 0) (handled 0) (refilled nil) (paused nil)
           (frames 0) (start-ticks (ticks)) (steps 0) (last-address -1)
           (vector (+ 8 *irq*))
           (old-mask (inb #x21)))
      (with-dos-memory (segment (* 4 +block+))
        ;; A DMA transfer cannot cross a 64K boundary of memory: there is room
        ;; here for two blocks on one side or the other of any boundary.
        (let* ((start (dma-fit (linear segment) (* 2 +block+)))
               (buffer (int-sap start)))
          (flet ((fill-half (half)      ; the tune goes round and round
                   (dotimes (i +block+)
                     (setf (sap-ref-8 buffer (+ (* half +block+) i)) (aref samples position)
                           position (mod (1+ position) total)))))
            (multiple-value-bind (old-selector old-offset) (get-vector vector)
              (unwind-protect
                   (with-video-mode (#x13)
                     (set-colours)
                     (draw-text 0 0 (format nil "DMA scope: ~Xh IRQ ~D DMA ~D, DSP ~D.~2,'0D"
                                            *base* *irq* *dma* major minor)
                                6)
                     (draw-text 0 (+ +top+ +height+ 2) "block 0" 6)
                     (draw-text 160 (+ +top+ +height+ 2) "block 1" 6)
                     (draw-text 0 192 "Space pauses the card   Esc ends" 6)
                     (fill-half 0) (fill-half 1)
                     (set-vector vector (code-selector) (sap-int (handler-code)))
                     (outb #x21 (logandc2 old-mask (ash 1 *irq*)))  ; let the interrupt through
                     (dma-start *dma* start (* 2 +block+))
                     (dsp-write #xD1)                               ; speaker on
                     (dsp-write #x40) (dsp-write (- 256 (round 1000000 rate)))
                     (dsp-write #x48) (dsp-write (lo (1- +block+))) (dsp-write (hi (1- +block+)))
                     (dsp-write #x1C)                               ; go: 8 bits, auto-initialize
                     (loop
                       ;; Interrupt N means block N (0, 1, 0, 1 ...) has been
                       ;; played and is free for new samples.
                       (loop while (/= handled (interrupts))
                             do (setf refilled (mod handled 2))
                                (fill-half refilled)
                                (incf handled))
                       (let* ((address (dma-address *dma*))
                              (count (dma-count *dma*))
                              (offset (logand (- address start) #xFFFF))
                              (reading (if (< offset +block+) 0 1)))
                         (unless (= address last-address)
                           (incf steps) (setf last-address address))
                         (wait-retrace)
                         (draw-scope buffer (min 319 (floor (* offset 320) (* 2 +block+))) refilled)
                         (draw-text 0 132 (format nil "address ~4,'0Xh = start ~4,'0Xh + ~4D"
                                                  address (logand start #xFFFF) offset)
                                    7)
                         (draw-text 0 142 (format nil "count   ~4,'0Xh = ~4D bytes to go, less 1"
                                                  count count)
                                    7)
                         (draw-text 0 152 (format nil "page    ~2,'0Xh   the card reads block ~D"
                                                  (ldb (byte 8 16) start) reading)
                                    7)
                         (draw-text 0 166 (format nil "interrupts ~4D   Lisp refilled block ~A"
                                                  handled (or refilled "-"))
                                    7)
                         (draw-text 0 176 (if paused
                                              "paused: DSP command D0h; D4h goes on    "
                                              (format nil "~6,1F s of tune sent to the buffer   "
                                                      (/ (* handled +block+) (float rate))))
                                    6))
                       (incf frames)
                       (when (key-pressed-p)
                         (let ((key (get-key)))
                           (cond ((eql key #\Space)
                                  (setf paused (not paused))
                                  (dsp-write (if paused #xD0 #xD4)))
                                 (t (return)))))))
                (dsp-write #xD0) (dsp-write #xDA) (dsp-write #xD3)  ; pause, end, speaker off
                (dma-stop *dma*)
                (outb #x21 (logior (logand (inb #x21) (lognot (ash 1 *irq*)))
                                   (logand old-mask (ash 1 *irq*))))    ; the mask bit as it was
                (set-vector vector old-selector old-offset)
                (dsp-reset))))))
      (let ((seconds (/ (- (ticks) start-ticks) 18.2)))
        (format t "~&~D interrupts in ~,1F s (one every ~D samples at ~D a second: ~,2F s);~%~
                   ~D frames drawn, the DMA address seen at ~D different places.~%"
                handled seconds +block+ rate (/ +block+ (float rate)) frames steps))))
  (values))

(doslib-examples:define-example "dmascope" 'main
  :category :sound
  :description "A scope on the Sound Blaster's DMA buffer: the DMA controller's registers, live.")
