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
;;;; machine code, assembled in the library, that tells the card and the interrupt
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

;;; The card is the library's (src/blaster.lisp): where it is (*BLASTER-PORT* and the
;;; others, from the BLASTER variable), commands to its processor (BLASTER-WRITE), and its
;;; interrupt, counted by a handler in machine code (WITH-BLASTER-INTERRUPT,
;;; BLASTER-INTERRUPTS).  So is the DMA controller: DMA-START and DMA-STOP, src/dma.lisp.

;;; ---- playing
(defconstant +block+ 2048 "Sample frames between interrupts.")

(defun play-samples (samples rate &key (show t) (channels 1))
  "Play SAMPLES at RATE frames a second.  A vector of bytes is 8-bit sound
(128 is silence), one channel; a vector of (SIGNED-BYTE 16) is 16-bit sound,
which needs a Sound Blaster 16, and with CHANNELS 2 it is stereo: left and
right samples in turn.  Returns the number of interrupts the card raised, or
NIL if there is no card that can play it."
  (unless (and (blaster-present-p) (<= 5 *blaster-dma16* 7))
    (return-from play-samples nil))
  (let* ((wide (typep samples '(array (signed-byte 16) (*))))
         (width (* (if wide 2 1) channels))             ; bytes in a frame
         (half (* +block+ width))                       ; bytes in a block
         (channel (if wide *blaster-dma16* *blaster-dma*)))
    (multiple-value-bind (major minor) (blaster-version)
      (when show
        (format t "~&Sound Blaster at ~Xh, interrupt ~D, DMA channel ~D: DSP version ~D.~2,'0D.~%"
                *blaster-port* *blaster-irq* channel major minor))
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
          (with-blaster-interrupt ()
            (unwind-protect
                 (progn
                   (fill-half 0) (fill-half 1)
                   (dma-start channel start (* 2 half))
                   (blaster-write #xD1)                             ; speaker on
                   (cond (wide
                          ;; the Sound Blaster 16's own commands: the rate as a
                          ;; number, then "16 bits out, auto-initialize", the
                          ;; format (signed; stereo or not) and the block's length
                          (blaster-write #x41) (blaster-write (hi rate)) (blaster-write (lo rate))
                          (blaster-write #xB6) (blaster-write (if (= channels 2) #x30 #x10))
                          (blaster-write (lo (1- per-block))) (blaster-write (hi (1- per-block))))
                         (t
                          (blaster-write #x40)                      ; the rate
                          (blaster-write (- 256 (round 1000000 rate)))
                          (blaster-write #x48)                      ; the block's length
                          (blaster-write (lo (1- +block+))) (blaster-write (hi (1- +block+)))
                          (blaster-write #x1C)))                    ; go: 8 bits, auto-initialize
                   ;; Interrupt N means block N has been played (they are
                   ;; played 0, 1, 0, 1 ...) and is free for new samples.
                   (loop until (or (>= silent-blocks 2) (key-pressed-p))
                         do (when (/= handled (blaster-interrupts))
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
                  (progn (blaster-write #xD5) (blaster-write #xD9)) ; pause, end: 16 bits
                  (progn (blaster-write #xD0) (blaster-write #xDA))) ; and 8
              (blaster-write #xD3)                                  ; speaker off
              (dma-stop channel))))
        (when show (terpri))
        handled))))

;;; ---- something to play: a tune of plucked strings, computed
(defparameter *tune*
  ;; (semitones above A 220, eighths); NIL is a rest
  '((7 2) (7 2) (8 2) (10 2) (10 2) (8 2) (7 2) (5 2) (3 2) (3 2) (5 2) (7 2) (7 3) (5 1) (5 4)
    (7 2) (7 2) (8 2) (10 2) (10 2) (8 2) (7 2) (5 2) (3 2) (3 2) (5 2) (7 2) (5 3) (3 1) (3 4)))

(defun add-note (mix start samples frequency rate gain)
  "Add a string plucked at FREQUENCY to MIX, SAMPLES long from START: three
harmonics, dying away.  (With the types said, so that the arithmetic is the
processor's own: a tune is some millions of sines.)"
  (declare (type (simple-array single-float (*)) mix) (type fixnum start samples rate)
           (type single-float frequency gain))
  (let ((step (/ (* 2 (float pi 1.0) frequency) rate))
        (decay (exp (/ -4.0 rate)))                     ; the envelope is e^(-4t)
        (envelope gain))
    (declare (type single-float step decay envelope))
    (dotimes (i samples mix)
      (let ((phase (* step i)))
        (declare (type (single-float 0.0 1.0e9) phase))
        (incf (aref mix (+ start i))
              (* envelope (+ (* 0.6 (sin phase)) (* 0.25 (sin (* 2 phase)))
                             (* 0.15 (sin (* 3 phase)))))))
      (setf envelope (* envelope decay)))))

(defun peak-of (mix)
  (declare (type (simple-array single-float (*)) mix))
  (let ((peak 0.001))
    (declare (type single-float peak))
    (dotimes (i (length mix) peak) (setf peak (max peak (abs (aref mix i)))))))

(defun synthesize (&key (rate 11025) (tempo 132))
  "The tune as a vector of samples: each note a string plucked, two octaves at once."
  (let* ((eighth (floor (* rate 30) tempo))
         (length (+ rate (* eighth (reduce #'+ *tune* :key #'second))))
         (mix (make-array length :element-type 'single-float :initial-element 0.0))
         (start 0))
    (loop for (note eighths) in *tune*
          do (when note
               (add-note mix start (min (- length start) (* 3 eighth (max 2 eighths)))
                         (* 220.0 (expt 2.0 (/ note 12.0))) rate 1.0))
             (incf start (* eighth eighths)))
    (let ((scale (/ 120.0 (peak-of mix)))
          (samples (make-array length :element-type '(unsigned-byte 8))))
      (declare (type single-float scale))
      (dotimes (i length samples)
        (setf (aref samples i) (+ 128 (the (integer -120 120) (round (* scale (aref mix i))))))))))

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
               (let ((frequency (* 220.0 (expt 2.0 (/ note 12.0))))
                     (samples (min (- length start) (* 3 eighth (max 2 eighths))))
                     (pan (/ (mod count 5) 4.0)))               ; 0 is left, 1 is right
                 (add-note left start samples frequency rate (- 1.0 pan))
                 (add-note right start samples frequency rate pan)
                 (incf count)))
             (incf start (* eighth eighths)))
    (let ((scale (/ 30000.0 (max (peak-of left) (peak-of right))))
          (samples (make-array (* 2 length) :element-type '(signed-byte 16))))
      (declare (type single-float scale) (type (simple-array single-float (*)) left right))
      (dotimes (i length samples)
        (setf (aref samples (* 2 i)) (the (integer -30000 30000) (round (* scale (aref left i))))
              (aref samples (1+ (* 2 i))) (the (integer -30000 30000) (round (* scale (aref right i)))))))))

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
  (blaster-settings)
  (and (blaster-reset) (>= (blaster-version) 4)))

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
                   sb16; SET BLASTER=A220 I5 D1 H5 says where a card is.)~%" *blaster-port*)))
  (values))

(doslib-examples:define-example "blaster" 'main
  :category :sound
  :description "Digital sound on a Sound Blaster: DMA and an interrupt handler.")
