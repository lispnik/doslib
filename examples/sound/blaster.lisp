;;;; Digital sound on a Sound Blaster: samples played by DMA, with an
;;;; interrupt handler to say when the card wants more.
;;;;
;;;;   (doslib-examples:run "blaster")              a tune, synthesized in Lisp
;;;;   (doslib-examples:run "blaster" "16")         the tune in 16 bits and stereo
;;;;   (doslib-examples:run "blaster" "ding.wav")   a WAV file (PCM, 8 or 16 bits)
;;;; Any key stops it.  Without a card it says so and returns.
;;;;
;;;; How the card plays: the samples are in memory below 1MB, and the DMA
;;;; controller -- not the processor -- hands them to the card one at a time,
;;;; at the rate the card asks for them.  The buffer here is two blocks, and
;;;; the DMA controller goes round it for ever ("auto-initialize"); the card
;;;; raises an interrupt each time a block has been played.  While it plays
;;;; one block the program writes the next samples into the other.
;;;;
;;;; The interrupt handler is the first in this project for a hardware
;;;; interrupt.  It cannot be Lisp: an interrupt may arrive in the middle of
;;;; anything, the garbage collector included.  So it is some thirty bytes of
;;;; machine code, assembled below, that tells the card and the interrupt
;;;; controller the interrupt has been seen and adds one to a counter.  Lisp
;;;; watches the counter.
;;;;
;;;; Eight-bit sound, one channel, works on any Sound Blaster from version
;;;; 2.  A Sound Blaster 16 also plays 16-bit samples, in stereo, through a
;;;; second DMA channel that moves words: "16" plays the tune that way, and
;;;; a 16-bit WAV file is played as it is on such a card.
;;;;
;;;; The card is where the BLASTER environment variable says (A220 I5 D1 H5
;;;; is the usual: port 220h, interrupt 5, DMA channels 1 and 5), or there
;;;; if it is not set.  QEMU's sb16, 86Box's cards and real ones all do.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.blaster
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:play-samples #:synthesize #:synthesize-stereo #:read-wav #:read-wav-16))
(in-package #:doslib-examples.blaster)

(defvar *base* #x220 "The card's first port.")
(defvar *irq* 5)
(defvar *dma* 1 "The 8-bit DMA channel.")
(defvar *dma16* 5 "The 16-bit DMA channel (a Sound Blaster 16 has both).")

(defun read-settings ()
  "Take the port, interrupt and DMA channels from BLASTER=A220 I5 D1 H5, if it is set."
  (setf *base* #x220 *irq* 5 *dma* 1 *dma16* 5)
  (let ((text (posix-getenv "BLASTER")))
    (when text
      (loop with start = 0
            for end = (or (position #\Space text :start start) (length text))
            do (when (> end (1+ start))
                 (let ((number (subseq text (1+ start) end)))
                   (case (char-upcase (char text start))
                     (#\A (setf *base* (or (parse-integer number :radix 16 :junk-allowed t) *base*)))
                     (#\I (setf *irq* (or (parse-integer number :junk-allowed t) *irq*)))
                     (#\D (setf *dma* (or (parse-integer number :junk-allowed t) *dma*)))
                     (#\H (setf *dma16* (or (parse-integer number :junk-allowed t) *dma16*))))))
               (when (>= end (length text)) (return))
               (setf start (1+ end))))))

;;; ---- the card's processor (the "DSP"): commands go to port 2xCh when
;;; bit 7 there is clear, answers come from 2xAh when bit 7 of 2xEh is set
(defun dsp-write (byte)
  (loop repeat 100000 while (logbitp 7 (inb (+ *base* #xC))))
  (outb (+ *base* #xC) byte))
(defun dsp-read ()
  (loop repeat 100000 until (logbitp 7 (inb (+ *base* #xE))))
  (inb (+ *base* #xA)))
(defun dsp-reset ()
  "Reset the card.  True if one answered."
  (outb (+ *base* 6) 1)
  (dotimes (i 200) (inb (+ *base* 6)))          ; a few microseconds
  (outb (+ *base* 6) 0)
  (loop repeat 2000
        thereis (and (logbitp 7 (inb (+ *base* #xE)))
                     (= (inb (+ *base* #xA)) #xAA))))
(defun dsp-version ()
  (dsp-write #xE1)
  (let* ((major (dsp-read)) (minor (dsp-read)))
    (values major minor)))

;;; (The DMA controller is programmed by the library: DMA-START and DMA-STOP,
;;; src/dma.lisp.)

;;; ---- the interrupt
(defvar *counter* nil "Four bytes the handler counts interrupts in.")
(defun handler-code ()
  "The interrupt handler: acknowledge, count, return."
  (let ((ds (data-selector)) (acknowledge (+ *base* #xE)))
    (machine-code #x50 #x52 #x1E                        ; push eax / push edx / push ds
                  #x66 #xB8 (lo ds) (hi ds)             ; mov ax, our data selector
                  #x8E #xD8                             ; mov ds, ax
                  #xBA (lo acknowledge) (hi acknowledge) 0 0    ; mov edx, base+0Eh
                  #xEC                                  ; in al, dx      the card: seen
                  #x42 #xEC                             ; inc edx / in al, dx   (16-bit sound: 2xFh)
                  #xFF #x05 (lo (sap-int *counter*)) (hi (sap-int *counter*))
                  (ldb (byte 8 16) (sap-int *counter*)) (ldb (byte 8 24) (sap-int *counter*))
                                                        ; inc dword [counter]
                  #xB0 #x20 #xE6 #x20                   ; mov al,20h / out 20h,al   the controller: done
                  #x1F #x5A #x58 #xCF)))                ; pop ds / pop edx / pop eax / iretd
(defun interrupts () (sap-ref-32 *counter* 0))

;;; ---- playing
(defconstant +block+ 2048 "Sample frames between interrupts.")

(defun play-samples (samples rate &key (show t) (channels 1))
  "Play SAMPLES at RATE frames a second.  A vector of bytes is 8-bit sound
(128 is silence), one channel; a vector of (SIGNED-BYTE 16) is 16-bit sound,
which needs a Sound Blaster 16, and with CHANNELS 2 it is stereo: left and
right samples in turn.  Returns the number of interrupts the card raised, or
NIL if there is no card that can play it."
  (read-settings)
  (unless (and (<= 0 *dma* 3) (/= *dma* 2) (<= 5 *dma16* 7) (<= 2 *irq* 7) (dsp-reset))
    (return-from play-samples nil))
  (unless *counter* (setf *counter* (alien-sap (make-alien (unsigned 32) 1))))
  (setf (sap-ref-32 *counter* 0) 0)
  (let* ((wide (typep samples '(array (signed-byte 16) (*))))
         (width (* (if wide 2 1) channels))             ; bytes in a frame
         (half (* +block+ width))                       ; bytes in a block
         (channel (if wide *dma16* *dma*)))
    (multiple-value-bind (major minor) (dsp-version)
      (when show
        (format t "~&Sound Blaster at ~Xh, interrupt ~D, DMA channel ~D: DSP version ~D.~2,'0D.~%"
                *base* *irq* channel major minor))
      (when (< major 2)
        (format t "~&It is too old for this: auto-initialized DMA came with version 2.~%")
        (return-from play-samples nil))
      (when (and (or wide (> channels 1)) (< major 4))
        (format t "~&16-bit and stereo sound here need a Sound Blaster 16 (DSP version 4).~%")
        (return-from play-samples nil)))
    (with-dos-memory (segment (* 4 half))
      ;; A DMA transfer cannot cross a 64K boundary of memory: there is room
      ;; here for two blocks on one side or the other of any boundary.
      (let* ((start (dma-fit (linear segment) (* 2 half)))
             (buffer (int-sap start))
             (vector (+ 8 *irq*))
             (old-mask (inb #x21))
             (position 0) (handled 0) (silent-blocks 0)
             (total (length samples))
             (per-block (* +block+ channels)))          ; samples, not frames
        (flet ((fill-half (which)       ; the next block of samples, or silence
                 (let ((peak 0) (base (* which half)))
                   (dotimes (i per-block)
                     (if wide
                         (let ((sample (if (< position total) (aref samples position) 0)))
                           (setf (signed-sap-ref-16 buffer (+ base (* 2 i))) sample
                                 peak (max peak (ash (abs sample) -8))))
                         (let ((sample (if (< position total) (aref samples position) 128)))
                           (setf (sap-ref-8 buffer (+ base i)) sample
                                 peak (max peak (abs (- sample 128))))))
                     (incf position))
                   (when (> position (+ total per-block)) (incf silent-blocks))
                   peak)))
          (multiple-value-bind (old-selector old-offset) (get-vector vector)
            (unwind-protect
                 (progn
                   (fill-half 0) (fill-half 1)
                   (set-vector vector (code-selector) (sap-int (handler-code)))
                   (outb #x21 (logandc2 old-mask (ash 1 *irq*)))    ; let the interrupt through
                   (dma-start channel start (* 2 half))
                   (dsp-write #xD1)                                 ; speaker on
                   (cond (wide
                          ;; the Sound Blaster 16's own commands: the rate as a
                          ;; number, then "16 bits out, auto-initialize", the
                          ;; format (signed; stereo or not) and the block's length
                          (dsp-write #x41) (dsp-write (hi rate)) (dsp-write (lo rate))
                          (dsp-write #xB6) (dsp-write (if (= channels 2) #x30 #x10))
                          (dsp-write (lo (1- per-block))) (dsp-write (hi (1- per-block))))
                         (t
                          (dsp-write #x40) (dsp-write (- 256 (round 1000000 rate)))  ; the rate
                          (dsp-write #x48) (dsp-write (lo (1- +block+))) (dsp-write (hi (1- +block+)))
                          (dsp-write #x1C)))                        ; go: 8 bits, auto-initialize
                   ;; Interrupt N means block N has been played (they are
                   ;; played 0, 1, 0, 1 ...) and is free for new samples.
                   (loop until (or (>= silent-blocks 2) (key-pressed-p))
                         do (when (/= handled (interrupts))
                              (let ((peak (fill-half (mod handled 2))))
                                (incf handled)
                                (when show
                                  (format t "~C~5,1F s  ~4D interrupts  ~A~A"
                                          #\Return (/ (min position total) (* rate channels)) handled
                                          (make-string (floor peak 4) :initial-element #\#)
                                          (make-string (- 32 (floor peak 4)) :initial-element #\Space))
                                  (finish-output)))))
                   (when (key-pressed-p) (wait-key)))
              (if wide
                  (progn (dsp-write #xD5) (dsp-write #xD9))         ; pause, end: 16 bits
                  (progn (dsp-write #xD0) (dsp-write #xDA)))        ; and 8
              (dsp-write #xD3)                                      ; speaker off
              (dma-stop channel)
              (outb #x21 (logior (logand (inb #x21) (lognot (ash 1 *irq*)))
                                 (logand old-mask (ash 1 *irq*))))  ; the mask bit as it was
              (set-vector vector old-selector old-offset)
              (dsp-reset))))
        (when show (terpri))
        handled))))

;;; ---- something to play: a tune of plucked strings, computed
(defparameter *tune*
  ;; (semitones above A 220, eighths); NIL is a rest
  '((7 2) (7 2) (8 2) (10 2) (10 2) (8 2) (7 2) (5 2) (3 2) (3 2) (5 2) (7 2) (7 3) (5 1) (5 4)
    (7 2) (7 2) (8 2) (10 2) (10 2) (8 2) (7 2) (5 2) (3 2) (3 2) (5 2) (7 2) (5 3) (3 1) (3 4)))

(defun synthesize (&key (rate 11025) (tempo 132))
  "The tune as a vector of samples: each note a string plucked, two octaves at once."
  (let* ((eighth (floor (* rate 30) tempo))
         (length (+ rate (* eighth (reduce #'+ *tune* :key #'second))))
         (mix (make-array length :element-type 'single-float :initial-element 0.0))
         (start 0))
    (loop for (note eighths) in *tune*
          do (when note
               (let* ((frequency (* 220.0 (expt 2.0 (/ note 12.0))))
                      (step (/ (* 2 (float pi 1.0) frequency) rate))
                      (samples (min (- length start) (* 3 eighth (max 2 eighths)))))
                 (dotimes (i samples)
                   (let ((envelope (exp (/ (* -4.0 i) rate))) (phase (* step i)))
                     (incf (aref mix (+ start i))
                           (* envelope (+ (* 0.6 (sin phase)) (* 0.25 (sin (* 2 phase)))
                                          (* 0.15 (sin (* 3 phase))))))))))
             (incf start (* eighth eighths)))
    (let ((peak (max 0.001 (reduce #'max mix :key #'abs)))
          (samples (make-array length :element-type '(unsigned-byte 8))))
      (dotimes (i length samples)
        (setf (aref samples i) (+ 128 (round (* 120 (/ (aref mix i) peak)))))))))

;;; ---- and WAV files
(defun read-wav (file)
  "The samples of a PCM WAV file as bytes, one channel, and its rate."
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      (flet ((u16 (i) (+ (aref bytes i) (ash (aref bytes (1+ i)) 8)))
             (u32 (i) (+ (aref bytes i) (ash (aref bytes (+ i 1)) 8)
                         (ash (aref bytes (+ i 2)) 16) (ash (aref bytes (+ i 3)) 24)))
             (tag (i) (map 'string #'code-char (subseq bytes i (+ i 4)))))
        (unless (and (> (length bytes) 44) (string= (tag 0) "RIFF") (string= (tag 8) "WAVE"))
          (error "~A is not a WAV file" file))
        (let ((channels 1) (rate 11025) (bits 8) (i 12))
          (loop while (< (+ i 8) (length bytes))
                do (let ((name (tag i)) (size (u32 (+ i 4))))
                     (cond ((string= name "fmt ")
                            (unless (= (u16 (+ i 8)) 1) (error "~A is not plain PCM" file))
                            (setf channels (u16 (+ i 10)) rate (u32 (+ i 12)) bits (u16 (+ i 22))))
                           ((string= name "data")
                            (let* ((frame (* channels (floor bits 8)))
                                   (count (floor (min size (- (length bytes) i 8)) frame))
                                   (samples (make-array count :element-type '(unsigned-byte 8))))
                              (dotimes (n count)
                                (let ((sum 0) (at (+ i 8 (* n frame))))
                                  (dotimes (c channels)
                                    (incf sum (if (= bits 8)
                                                  (aref bytes (+ at c))
                                                  ;; 16 bits, signed: the high byte, made unsigned
                                                  (logxor #x80 (aref bytes (+ at (* 2 c) 1))))))
                                  (setf (aref samples n) (floor sum channels))))
                              (return-from read-wav (values samples rate)))))
                     (incf i (+ 8 size (logand size 1)))))
          (error "~A has no samples in it" file))))))

(defun synthesize-stereo (&key (rate 22050) (tempo 132))
  "The tune in 16 bits and two channels: the notes go from side to side."
  (let* ((eighth (floor (* rate 30) tempo))
         (length (+ rate (* eighth (reduce #'+ *tune* :key #'second))))
         (left (make-array length :element-type 'single-float :initial-element 0.0))
         (right (make-array length :element-type 'single-float :initial-element 0.0))
         (start 0) (count 0))
    (loop for (note eighths) in *tune*
          do (when note
               (let* ((frequency (* 220.0 (expt 2.0 (/ note 12.0))))
                      (step (/ (* 2 (float pi 1.0) frequency) rate))
                      (samples (min (- length start) (* 3 eighth (max 2 eighths))))
                      (pan (/ (mod count 5) 4.0)))              ; 0 is left, 1 is right
                 (dotimes (i samples)
                   (let* ((envelope (exp (/ (* -4.0 i) rate))) (phase (* step i))
                          (value (* envelope (+ (* 0.6 (sin phase)) (* 0.25 (sin (* 2 phase)))
                                                (* 0.15 (sin (* 3 phase)))))))
                     (incf (aref left (+ start i)) (* (- 1.0 pan) value))
                     (incf (aref right (+ start i)) (* pan value))))
                 (incf count)))
             (incf start (* eighth eighths)))
    (let ((peak (max 0.001 (reduce #'max left :key #'abs) (reduce #'max right :key #'abs)))
          (samples (make-array (* 2 length) :element-type '(signed-byte 16))))
      (dotimes (i length samples)
        (setf (aref samples (* 2 i)) (round (* 30000 (/ (aref left i) peak)))
              (aref samples (1+ (* 2 i))) (round (* 30000 (/ (aref right i) peak))))))))

(defun read-wav-16 (file)
  "The samples of a 16-bit PCM WAV file as they are: the samples, the rate
and the number of channels.  NIL if the file is not 16-bit, one or two channels."
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      (flet ((u16 (i) (+ (aref bytes i) (ash (aref bytes (1+ i)) 8)))
             (u32 (i) (+ (aref bytes i) (ash (aref bytes (+ i 1)) 8)
                         (ash (aref bytes (+ i 2)) 16) (ash (aref bytes (+ i 3)) 24)))
             (tag (i) (map 'string #'code-char (subseq bytes i (+ i 4)))))
        (when (and (> (length bytes) 44) (string= (tag 0) "RIFF") (string= (tag 8) "WAVE"))
          (let ((channels 1) (rate 11025) (bits 8) (i 12))
            (loop while (< (+ i 8) (length bytes))
                  do (let ((name (tag i)) (size (u32 (+ i 4))))
                       (cond ((string= name "fmt ")
                              (setf channels (u16 (+ i 10)) rate (u32 (+ i 12)) bits (u16 (+ i 22)))
                              (unless (and (= (u16 (+ i 8)) 1) (= bits 16) (<= 1 channels 2))
                                (return nil)))
                             ((string= name "data")
                              (let* ((count (floor (min size (- (length bytes) i 8)) 2))
                                     (samples (make-array count :element-type '(signed-byte 16))))
                                (dotimes (n count)
                                  (let ((word (u16 (+ i 8 (* 2 n)))))
                                    (setf (aref samples n) (if (logbitp 15 word) (- word 65536) word))))
                                (return (values samples rate channels)))))
                       (incf i (+ 8 size (logand size 1)))))))))))

(defun card-is-16-bit-p ()
  (read-settings)
  (and (dsp-reset) (>= (dsp-version) 4)))

(defun main (&optional what)
  "Digital sound on a Sound Blaster: a tune, the tune in 16-bit stereo, or a WAV file."
  (let ((count nil) (channels 1))
    (multiple-value-bind (samples rate)
        (cond ((null what) (values (synthesize) 11025))
              ((member what '("16" "stereo") :test #'equalp)
               (setf channels 2)
               (values (synthesize-stereo) 22050))
              (t (multiple-value-bind (samples rate file-channels)
                     (and (card-is-16-bit-p) (read-wav-16 what))
                   (if samples
                       (progn (setf channels file-channels) (values samples rate))
                       (read-wav what)))))          ; 8 bits, or made so
      (format t "~&~:D ~:[8~;16~]-bit samples at ~D a second, ~[~;one channel~;two channels~]~
                 ~@[ from ~A~].  Any key stops.~%"
              (length samples) (typep samples '(array (signed-byte 16) (*))) rate channels
              (and what (not (member what '("16" "stereo") :test #'equalp)) what))
      (setf count (play-samples samples (max 4000 (min rate 44100)) :channels channels)))
    (if count
        (format t "~&Played, in blocks of ~D sample frames: ~D interrupts handled.~%" +block+ count)
        (format t "~&No Sound Blaster that can play this answered at port ~Xh.  (QEMU: -device ~
                   sb16; SET BLASTER=A220 I5 D1 H5 says where a card is.)~%" *base*)))
  (values))

(doslib-examples:define-example "blaster" 'main
  :category :sound
  :description "Digital sound on a Sound Blaster: DMA and an interrupt handler.")
