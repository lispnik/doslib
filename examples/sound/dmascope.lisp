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
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.dmascope)

;;; The card is the library's (src/blaster.lisp): where it is, commands to its processor
;;; (BLASTER-WRITE), and its interrupt, counted (WITH-BLASTER-INTERRUPT, BLASTER-INTERRUPTS).
;;; So is the DMA controller (src/dma.lisp): DMA-START, DMA-STOP, and DMA-ADDRESS and
;;; DMA-COUNT, which read its registers while it counts.

;;; ---- drawing: mode 13h; the letters are the library's (GRAPHICS-TEXT)
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
  (unless (blaster-present-p)
    (format t "~&No Sound Blaster answered at port ~Xh.  (QEMU: -device sb16; ~
               SET BLASTER=A220 I5 D1 says where a card is.)~%" *blaster-port*)
    (return-from main (values)))
  (multiple-value-bind (major minor) (blaster-version)
    (when (< major 2)
      (format t "~&The card's DSP is version ~D.~2,'0D: auto-initialized DMA came with version 2.~%"
              major minor)
      (return-from main (values)))
    (let* ((rate 11025)
           (samples (synthesize rate))
           (total (length samples))
           (position 0) (handled 0) (refilled nil) (paused nil)
           (frames 0) (start-ticks (ticks)) (steps 0) (last-address -1))
      (with-dos-memory (segment (* 4 +block+))
        ;; A DMA transfer cannot cross a 64K boundary of memory: there is room
        ;; here for two blocks on one side or the other of any boundary.
        (let* ((start (dma-fit (linear segment) (* 2 +block+)))
               (buffer (int-sap start)))
          (flet ((fill-half (half)      ; the tune goes round and round
                   (dotimes (i +block+)
                     (setf (sap-ref-8 buffer (+ (* half +block+) i)) (aref samples position)
                           position (mod (1+ position) total)))))
            (with-blaster-interrupt ()
              (unwind-protect
                   (with-video-mode (#x13)
                     (set-colours)
                     (graphics-text 0 0 (format nil "DMA scope: ~Xh IRQ ~D DMA ~D, DSP ~D.~2,'0D"
                                                *blaster-port* *blaster-irq* *blaster-dma*
                                                major minor)
                                    6)
                     (graphics-text 0 (+ +top+ +height+ 2) "block 0" 6)
                     (graphics-text 160 (+ +top+ +height+ 2) "block 1" 6)
                     (graphics-text 0 192 "Space pauses the card   Esc ends" 6)
                     (fill-half 0) (fill-half 1)
                     (dma-start *blaster-dma* start (* 2 +block+))
                     (blaster-write #xD1)                           ; speaker on
                     (blaster-write #x40) (blaster-write (- 256 (round 1000000 rate)))
                     (blaster-write #x48)
                     (blaster-write (lo (1- +block+))) (blaster-write (hi (1- +block+)))
                     (blaster-write #x1C)                           ; go: 8 bits, auto-initialize
                     (loop
                       ;; Interrupt N means block N (0, 1, 0, 1 ...) has been
                       ;; played and is free for new samples.
                       (loop while (/= handled (blaster-interrupts))
                             do (setf refilled (mod handled 2))
                                (fill-half refilled)
                                (incf handled))
                       (let* ((address (dma-address *blaster-dma*))
                              (count (dma-count *blaster-dma*))
                              (offset (logand (- address start) #xFFFF))
                              (reading (if (< offset +block+) 0 1)))
                         (unless (= address last-address)
                           (incf steps) (setf last-address address))
                         (wait-retrace)
                         (draw-scope buffer (min 319 (floor (* offset 320) (* 2 +block+))) refilled)
                         (graphics-text 0 132
                                        (format nil "address ~4,'0Xh = start ~4,'0Xh + ~4D"
                                                address (logand start #xFFFF) offset)
                                        7)
                         (graphics-text 0 142
                                        (format nil "count   ~4,'0Xh = ~4D bytes to go, less 1"
                                                count count)
                                        7)
                         (graphics-text 0 152
                                        (format nil "page    ~2,'0Xh   the card reads block ~D"
                                                (ldb (byte 8 16) start) reading)
                                        7)
                         (graphics-text 0 166
                                        (format nil "interrupts ~4D   Lisp refilled block ~A"
                                                handled (or refilled "-"))
                                        7)
                         (graphics-text 0 176
                                        (if paused
                                            "paused: DSP command D0h; D4h goes on    "
                                            (format nil "~6,1F s of tune sent to the buffer   "
                                                    (/ (* handled +block+) (float rate))))
                                        6))
                       (incf frames)
                       (when (key-pressed-p)
                         (let ((key (get-key)))
                           (cond ((eql key #\Space)
                                  (setf paused (not paused))
                                  (blaster-write (if paused #xD0 #xD4)))
                                 (t (return)))))))
                ;; pause, end, speaker off
                (blaster-write #xD0) (blaster-write #xDA) (blaster-write #xD3)
                (dma-stop *blaster-dma*))))))
      (let ((seconds (/ (- (ticks) start-ticks) 18.2)))
        (format t "~&~D interrupts in ~,1F s (one every ~D samples at ~D a second: ~,2F s);~%~
                   ~D frames drawn, the DMA address seen at ~D different places.~%"
                handled seconds +block+ rate (/ +block+ (float rate)) frames steps))))
  (values))

(doslib-examples:define-example "dmascope" 'main
  :category :sound
  :description "A scope on the Sound Blaster's DMA buffer: the DMA controller's registers, live.")
