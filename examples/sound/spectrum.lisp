;;;; A spectrum analyser: a fast Fourier transform, in Lisp, of the sound the
;;;; Sound Blaster is playing, drawn as bars under the waveform.
;;;;
;;;;   (doslib-examples:run "spectrum")            through the Sound Blaster
;;;;   (doslib-examples:run "spectrum" "silent")   without the card
;;;;   (doslib-examples.spectrum:self-test)        only the check of the transform
;;;; Space goes on to the next signal; Esc ends.  Without a Sound Blaster it
;;;; says so and runs silently on the same signal, kept in step by the clock.
;;;;
;;;; A sound is a wave, and any wave is a sum of sines; the transform says
;;;; how much of each sine there is.  Given 256 samples it gives 128 "bins":
;;;; bin k is the strength of the sine that goes round k times in those 256
;;;; samples, which at 11,025 samples a second is k * 43.07 Hz.  So a 440 Hz
;;;; tone shows as a bar at bin 10 (440 / 43.07 = 10.2), and a chord of three
;;;; tones as three bars.  Done as its definition says the transform is
;;;; 256 * 256 multiplications; the FAST transform (Cooley and Tukey's) gets
;;;; the same numbers in 8 passes of 128 "butterflies", each of which
;;;; combines two numbers with one complex multiplication: 1,024 of them.
;;;;
;;;; Two things are done to the samples first.  They are put in bit-reversed
;;;; order (sample 1 goes to place 128, 2 to 64 ...), which is the order the
;;;; passes need to work in place.  And they are multiplied by a "window", a
;;;; curve that is nothing at both ends and one in the middle (Hann's, a
;;;; raised cosine): the transform treats its 256 samples as if they
;;;; repeated for ever, a tone that does not fit a whole number of times
;;;; into them makes a jump where the end meets the beginning, and the jump
;;;; would smear the tone across every bin.  With the window a tone is a
;;;; peak three or four bins wide and nothing elsewhere.
;;;;
;;;; The arithmetic is FIXED POINT, in whole numbers, not floating point.
;;;; The samples are bytes to begin with; scaled up by 128 and halved in
;;;; every pass (which also divides the result by 256, as wanted) they stay
;;;; within 16 bits, the sines are a table of 256 numbers scaled by 16,384,
;;;; and so every product fits a 32-bit machine word: SBCL compiles the
;;;; butterfly to plain integer instructions that allocate nothing, where
;;;; single floats would be boxed whenever they left a register and every
;;;; operation would go to the 387 -- which the machines this card was made
;;;; for mostly did not have (a 386 or a 486SX), and which an emulator has
;;;; to imitate slowly in software.  The price is precision: about 12 bits
;;;; for a full-scale tone, and rounding noise in the last two, far below
;;;; what an 8-bit sample has anyway.  SELF-TEST compares the result with
;;;; the definition worked out in double floats.  The bars are logarithmic,
;;;; 3 dB for each doubling of power, read off the position of the top bit:
;;;; no logarithm either.
;;;;
;;;; The signal is made here, so that what the picture should show is known:
;;;; four chords of three sines (the white marks under the bars are the bins
;;;; the tones belong in), then one sine sweeping from 150 Hz to 5,000 (the
;;;; bar walks to the right under its mark), then noise (all bars about the
;;;; same height).  It is played as "dmascope" plays its tune: a DMA buffer
;;;; of two blocks that the card goes round, an interrupt at the end of each,
;;;; and the block just played refilled.  The 256 samples transformed are
;;;; read back from that buffer starting where the DMA controller's address
;;;; register says the card is now, so the picture is of what is being
;;;; played, not of what is being made (which is up to two blocks ahead).
;;;; The program counts the frames in which the strongest peaks were in the
;;;; bins expected, and says so at the end.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.spectrum
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main #:self-test #:fft))
(in-package #:doslib-examples.spectrum)

(defconstant +rate+ 11025 "Samples a second.")
(defconstant +n+ 256 "Samples transformed: half as many bins.")
(defconstant +block+ 1024 "Samples between interrupts.")

;;; ---- the transform
(deftype fft-vector () '(simple-array (signed-byte 32) (*)))
(deftype bytes () '(simple-array (unsigned-byte 8) (*)))
(deftype powers () '(simple-array (unsigned-byte 32) (*)))

(declaim (type fft-vector *sines* *window*) (type bytes *reversed*))
(defvar *sines*
  ;; sin(2 pi i / 256) * 16384; the cosine is the sine a quarter turn on
  (let ((table (make-array +n+ :element-type '(signed-byte 32))))
    (dotimes (i +n+ table)
      (setf (aref table i) (round (* 16384 (sin (/ (* 2 pi i) +n+))))))))
(defvar *window*
  ;; Hann's window, * 32767
  (let ((table (make-array +n+ :element-type '(signed-byte 32))))
    (dotimes (i +n+ table)
      (setf (aref table i) (round (* 32767 0.5d0 (- 1 (cos (/ (* 2 pi i) +n+)))))))))
(defvar *reversed*
  ;; i with its eight bits in the opposite order
  (let ((table (make-array +n+ :element-type '(unsigned-byte 8))))
    (dotimes (i +n+ table)
      (let ((r 0))
        (dotimes (bit 8) (when (logbitp bit i) (setf r (logior r (ash 1 (- 7 bit))))))
        (setf (aref table i) r)))))

(defun fft (re im)
  "The discrete Fourier transform of the 256 complex numbers in RE and IM
(real and imaginary parts, each within 16 bits), in place, divided by 256."
  (declare (type fft-vector re im) (optimize speed (safety 0)))
  (let ((sines *sines*) (reversed *reversed*) (half 1) (stride (ash +n+ -1)))
    (declare (type (integer 0 256) half stride))
    (dotimes (i +n+)
      (let ((j (aref reversed i)))
        (when (< i j)
          (rotatef (aref re i) (aref re j))
          (rotatef (aref im i) (aref im j)))))
    ;; Eight passes: pairs 1 apart, then 2, 4 ... 128.  Each pair (a, b)
    ;; becomes (a + w b) / 2 and (a - w b) / 2, where w is a point on the
    ;; unit circle: cos - i sin of k / (2 * half) of a turn.
    (loop while (< half +n+)
          do (loop for start of-type fixnum from 0 below +n+ by (* 2 half)
                   do (dotimes (k half)
                        (let* ((angle (* k stride))
                               (wr (aref sines (logand (+ angle (ash +n+ -2)) (1- +n+))))
                               (wi (- (aref sines angle)))
                               (a (+ start k)) (b (+ a half))
                               (ar (aref re a)) (ai (aref im a))
                               (br (aref re b)) (bi (aref im b)))
                          (declare (type (integer -16384 16384) wr wi)
                                   (type (signed-byte 16) ar ai br bi))
                          (let ((tr (ash (- (* wr br) (* wi bi)) -14))
                                (ti (ash (+ (* wr bi) (* wi br)) -14)))
                            (declare (type (signed-byte 17) tr ti))
                            (setf (aref re a) (ash (+ ar tr) -1)
                                  (aref im a) (ash (+ ai ti) -1)
                                  (aref re b) (ash (- ar tr) -1)
                                  (aref im b) (ash (- ai ti) -1))))))
             (setf half (* 2 half) stride (ash stride -1))))
  nil)

(defun analyse (samples re im power)
  "Transform the 256 bytes in SAMPLES (128 is silence): RE and IM get the
bins, POWER the squares of the first 128 bins' sizes."
  (declare (type bytes samples) (type fft-vector re im) (type powers power)
           (optimize speed (safety 0)))
  (let ((window *window*))
    (dotimes (i +n+)
      (setf (aref re i) (ash (* (- (aref samples i) 128)
                                (the (integer 0 32767) (aref window i)))
                             -8)
            (aref im i) 0)))
  (fft re im)
  (dotimes (k (ash +n+ -1))
    (let ((r (aref re k)) (i (aref im k)))
      (declare (type (signed-byte 16) r i))
      (setf (aref power k) (+ (* r r) (* i i)))))
  nil)

(declaim (inline eighth-bits))
(defun eighth-bits (power)
  "Eight times the logarithm to base 2 of POWER, near enough: the place of
its top bit and the three bits after.  Each eighth is 3/8 of a decibel."
  (declare (type (unsigned-byte 32) power) (optimize speed))
  (if (zerop power)
      0
      (let ((top (1- (integer-length power))))
        (+ (* 8 top)
           (if (>= top 3)
               (ldb (byte 3 (- top 3)) power)
               (logand 7 (ash (the (integer 0 7) power) (- 3 top))))))))

(defun find-peaks (power peaks)
  "Put in PEAKS the bins of the three strongest peaks of POWER -- bins
stronger than both neighbours -- in order of bin; -1 where there are fewer."
  (declare (type powers power) (type simple-vector peaks) (optimize speed))
  (fill peaks -1)
  (dotimes (n 3)
    (let ((best -1) (most 4096))                ; (what is weaker than this is not a peak)
      (declare (fixnum best) (type (unsigned-byte 32) most))
      (loop for k of-type fixnum from 1 below (- (ash +n+ -1) 1)
            do (let ((p (aref power k)))
                 (when (and (> p most) (> p (aref power (1- k))) (>= p (aref power (1+ k)))
                            (not (find k peaks)))
                   (setf best k most p))))
      (setf (svref peaks n) best)))
  (sort peaks (lambda (a b)
                (declare (fixnum a b))
                (and (>= a 0) (or (< b 0) (< a b))))))

(defun strongest (power)
  "The strongest bin of POWER but the lowest."
  (declare (type powers power) (optimize speed))
  (let ((best 1))
    (declare (fixnum best))
    (loop for k of-type fixnum from 2 below (ash +n+ -1)
          do (when (> (aref power k) (aref power best)) (setf best k)))
    best))

(defun bin-of (hz)
  "The bin a tone of HZ belongs in."
  (round (* hz +n+) +rate+))

;;; ---- the signal: chords, a sweep, noise
(defstruct section
  kind                                  ; :chord, :sweep or :noise
  (label "" :type simple-string)
  (blocks 0 :type fixnum)               ; how long it lasts
  (tones nil)                           ; hertz: a chord's three, or the sweep's two ends
  (bins (vector -1 -1 -1) :type simple-vector))

(defun chord (name &rest tones)
  (make-section :kind :chord :blocks 16 :tones tones
                :label (format nil "chord ~A: ~{~D ~}Hz" name (mapcar #'round tones))
                :bins (coerce (sort (mapcar #'bin-of tones) #'<) 'simple-vector)))

(defparameter *sections*
  (vector (chord "A" 220.0 554.4 880.0)
          (chord "D" 293.7 740.0 1174.7)
          (chord "E" 329.6 830.6 1318.5)
          (chord "F#m" 370.0 880.0 1108.7)
          (make-section :kind :sweep :blocks 48 :tones '(150.0 5000.0) :label "sweep")
          (make-section :kind :noise :blocks 24 :label "noise: every bin, about equally")))

(declaim (type (simple-array (signed-byte 16) (*)) *wave*))
(defvar *wave*
  ;; one turn of a sine in 1024 steps, * 32767
  (let ((table (make-array 1024 :element-type '(signed-byte 16))))
    (dotimes (i 1024 table)
      (setf (aref table i) (round (* 32767 (sin (/ (* 2 pi i) 1024))))))))

(defun step-of (hz)
  "How far a tone of HZ moves round the wave table for each sample, 16 bits
below the point: a whole turn is 2^26.  (Bin k is a step of k * 2^18.)"
  (round (* hz (ash 1 26)) +rate+))

(defstruct (generator (:conc-name sg-))
  (index -1 :type fixnum)               ; which section
  (left 0 :type fixnum)                 ; blocks of it still to make
  (phases (make-array 3 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (3)))
  (steps (make-array 3 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (3)))
  (delta 0 :type fixnum)                ; the sweep: what its step grows by each sample
  (seed 1 :type (unsigned-byte 16))
  ;; what was put in each half of the buffer: its section, and the sweep's step at its start
  (half-section (vector nil nil) :type simple-vector)
  (half-step (make-array 2 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (2))))

(defun next-section (source)
  (let* ((index (mod (1+ (sg-index source)) (length *sections*)))
         (section (svref *sections* index))
         (steps (sg-steps source)))
    (setf (sg-index source) index
          (sg-left source) (section-blocks section))
    (fill (sg-phases source) 0)
    (ecase (section-kind section)
      (:chord (loop for hz in (section-tones section) for i from 0
                    do (setf (aref steps i) (step-of hz))))
      (:sweep (destructuring-bind (from to) (section-tones section)
                (setf (aref steps 0) (step-of from)
                      (sg-delta source) (round (- (step-of to) (step-of from))
                                               (* (section-blocks section) +block+)))))
      (:noise))
    section))

(defun fill-half (source buffer half)
  "Make the next block of SIGNAL in half HALF of the buffer at BUFFER."
  (declare (type generator source) (system-area-pointer buffer) (type (integer 0 1) half)
           (optimize speed))
  (when (<= (sg-left source) 0) (next-section source))
  (decf (sg-left source))
  (let* ((section (svref *sections* (sg-index source)))
         (phases (sg-phases source)) (steps (sg-steps source))
         (wave *wave*) (base (* half +block+)))
    (setf (svref (sg-half-section source) half) section
          (aref (sg-half-step source) half) (aref steps 0))
    (ecase (section-kind section)
      (:chord
       (let ((p0 (aref phases 0)) (p1 (aref phases 1)) (p2 (aref phases 2))
             (s0 (aref steps 0)) (s1 (aref steps 1)) (s2 (aref steps 2)))
         (declare (type (unsigned-byte 26) p0 p1 p2 s0 s1 s2))
         (dotimes (i +block+)
           (setf (sap-ref-8 buffer (+ base i))
                 ;; (rounded, not cut off, or the sound would lean half a
                 ;; step below silence and the spectrum show it at bin 0)
                 (+ 128 (ash (+ 16384 (* 42 (+ (aref wave (ash p0 -16)) (aref wave (ash p1 -16))
                                               (aref wave (ash p2 -16)))))
                             -15))
                 p0 (logand (+ p0 s0) #x3FFFFFF)
                 p1 (logand (+ p1 s1) #x3FFFFFF)
                 p2 (logand (+ p2 s2) #x3FFFFFF)))
         (setf (aref phases 0) p0 (aref phases 1) p1 (aref phases 2) p2)))
      (:sweep
       (let ((p (aref phases 0)) (s (aref steps 0)) (delta (sg-delta source)))
         (declare (type (unsigned-byte 26) p s) (type (integer 0 65535) delta))
         (dotimes (i +block+)
           (setf (sap-ref-8 buffer (+ base i))
                 (+ 128 (ash (+ 16384 (* 110 (aref wave (ash p -16)))) -15))
                 p (logand (+ p s) #x3FFFFFF)
                 s (logand (+ s delta) #x3FFFFFF)))
         (setf (aref phases 0) p (aref steps 0) s)))
      (:noise
       (let ((seed (sg-seed source)))
         (declare (type (unsigned-byte 16) seed))
         (dotimes (i +block+)
           (setf seed (logand (+ (* seed 25173) 13849) #xFFFF))
           (setf (sap-ref-8 buffer (+ base i)) (+ 128 (ash (* 100 (- (ash seed -8) 128)) -7))))
         (setf (sg-seed source) seed))))
    nil))

;;; ---- the check: the transform against its definition, and tones against their bins
(defun self-test (&optional (report t))
  "Transform each chord, made with floating-point sines, and see that the
three peaks are in the tones' bins; and compare all 128 bins of the first
with the transform's definition in double floats.  Returns the number of
chords right, the number of chords, and the largest difference found in a
bin's real or imaginary part (in units of which a full-scale tone is 4096)."
  (let ((samples (make-array +n+ :element-type '(unsigned-byte 8)))
        (re (make-array +n+ :element-type '(signed-byte 32)))
        (im (make-array +n+ :element-type '(signed-byte 32)))
        (power (make-array (ash +n+ -1) :element-type '(unsigned-byte 32)))
        (peaks (vector -1 -1 -1))
        (right 0) (chords 0) (worst 0))
    (loop for section across *sections*
          when (eq (section-kind section) :chord)
            do (dotimes (i +n+)
                 (setf (aref samples i)
                       (+ 128 (round (* 42 (loop for hz in (section-tones section)
                                                 sum (sin (/ (* 2 pi hz i) +rate+))))))))
               (analyse samples re im power)
               (find-peaks power peaks)
               (incf chords)
               (when (equalp peaks (section-bins section)) (incf right))
               (when (= chords 1)
                 (dotimes (k (ash +n+ -1))
                   (let ((sum-r 0d0) (sum-i 0d0))
                     (dotimes (i +n+)
                       (let ((x (ash (* (- (aref samples i) 128) (aref *window* i)) -8))
                             (angle (/ (* 2 pi k i) +n+)))
                         (incf sum-r (* x (cos angle)))
                         (decf sum-i (* x (sin angle)))))
                     (setf worst (max worst
                                      (abs (- (aref re k) (round sum-r +n+)))
                                      (abs (- (aref im k) (round sum-i +n+)))))))))
    (when report
      (format t "~&Spectrum self-test: ~D of ~D chords have their three peaks in the tones' bins;~%~
                 the fixed-point transform is within ~D of the definition in floating point~%~
                 (a full-scale tone is 4096).~%"
              right chords worst))
    (values right chords worst)))

;;; ---- drawing: mode 13h; the letters are the library's (GRAPHICS-TEXT)
(defconstant +left+ 32 "The first column of the waveform and the bars.")
(defconstant +wave-top+ 10)
(defconstant +wave-height+ 64)
(defconstant +bars-top+ 88)
(defconstant +bars-height+ 80)

;;; colours: 1 the ground, 2 the centre line, 3 the wave, 4 text, 5 numbers,
;;; 6 the marks, 16 to 95 the bars from the bottom up
(defun set-colours ()
  (set-palette 0 0 0 0)
  (set-palette 1 0 5 12) (set-palette 2 10 16 26) (set-palette 3 20 63 30)
  (set-palette 4 40 40 40) (set-palette 5 63 56 20) (set-palette 6 63 63 63)
  (dotimes (y +bars-height+)
    (set-palette (+ 16 y)
                 (min 63 (floor (* y 63) 40))
                 (if (< y 40) 63 (max 0 (- 63 (floor (* (- y 40) 63) 40))))
                 8)))

(defun draw-wave (samples)
  "The 256 samples as a line, four levels to a row."
  (declare (type bytes samples) (optimize speed))
  (let ((screen (int-sap #xA0000)) (last (- 63 (ash (aref samples 0) -2))))
    (declare (system-area-pointer screen) (type (integer 0 63) last))
    (dotimes (x +n+)
      (let* ((y (- 63 (ash (aref samples x) -2)))
             (low (min y last)) (high (max y last)))
        (dotimes (row +wave-height+)
          (setf (sap-ref-8 screen (+ +left+ x (* 320 (+ +wave-top+ row))))
                (cond ((<= low row high) 3)
                      ((= row 31) 2)
                      (t 1))))
        (setf last y)))))

(declaim (inline bar-height))
(defun bar-height (power)
  "The height of the bar for POWER: 5/9 of a row for 3/8 dB, 54 dB in all."
  (declare (type (unsigned-byte 32) power))
  (max 0 (min +bars-height+ (floor (* 5 (- (eighth-bits power) 56)) 9))))

(defun draw-bars (power)
  "The 128 bins as bars two columns wide."
  (declare (type powers power) (optimize speed))
  (let ((screen (int-sap #xA0000)))
    (declare (system-area-pointer screen))
    (dotimes (k (ash +n+ -1))
      (let ((height (bar-height (aref power k))))
        (declare (type (integer 0 80) height))
        (dotimes (row +bars-height+)
          (let ((up (- +bars-height+ 1 row)))           ; rows above the bottom
            (setf (sap-ref-16 screen (+ +left+ (* 2 k) (* 320 (+ +bars-top+ row))))
                  (if (< up height) (* #x0101 (+ 16 up)) #x0101))))))))

(defun draw-marks (bins)
  "White marks under the bars of BINS (-1: none)."
  (declare (type simple-vector bins))
  (let ((screen (int-sap #xA0000)))
    (dotimes (row 4)
      (let ((line (+ +left+ (* 320 (+ +bars-top+ +bars-height+ 1 row)))))
        (dotimes (x 256) (setf (sap-ref-8 screen (+ line x)) 0))
        (dotimes (i 3)
          (let ((bin (svref bins i)))
            (when (<= 0 bin 127)
              (setf (sap-ref-16 screen (+ line (* 2 bin))) #x0606))))))))

(defun draw-frame (card)
  (set-colours)
  (graphics-text 0 0 (if card
                         (format nil "Spectrum of the Sound Blaster's buffer")
                         (format nil "No Sound Blaster: the signal, silently"))
                 6)
  (loop for khz from 0 to 5
        do (graphics-text (+ +left+ (* 2 (bin-of (* 1000 khz))) (if (zerop khz) 0 -8))
                          (+ +bars-top+ +bars-height+ 5)
                          (if (zerop khz) "0" (format nil "~Dk" khz))
                          4)))

;;; ---- the program
(defun play (card major)
  "Show the signal until Esc: through the card if CARD, by the clock if not.
Returns what there is to say afterwards, as a list."
  (let* ((source (make-generator))
         (samples (make-array +n+ :element-type '(unsigned-byte 8)))
         (re (make-array +n+ :element-type '(signed-byte 32)))
         (im (make-array +n+ :element-type '(signed-byte 32)))
         (power (make-array (ash +n+ -1) :element-type '(unsigned-byte 32)))
         (peaks (vector -1 -1 -1)) (expected (vector -1 -1 -1))
         (handled 0) (late 0) (frames 0) (clocks 0) (began-ticks (ticks))
         (chord-frames 0) (chord-right 0) (sweep-frames 0) (sweep-right 0)
         (began (timer-count)))
    (declare (fixnum handled late frames clocks chord-frames chord-right sweep-frames sweep-right))
    (with-dma-memory (start (* 2 +block+))
      (let ((buffer (int-sap start)))
        (labels ((elapsed ()            ; samples the clock says have gone by
                   (floor (* (- (timer-count) began) +rate+) +timer-hz+))
                 (interrupts ()         ; blocks played
                   (if card (blaster-interrupts) (floor (elapsed) +block+)))
                 (place ()              ; the sample being played, 0 to 2 * +block+ - 1
                   (if card
                       (min (1- (* 2 +block+))
                            (logand (- (dma-address *blaster-dma*) start) #xFFFF))
                       (mod (elapsed) (* 2 +block+))))
                 (text (y string colour)
                   (graphics-text 0 y (format nil "~40A" string) colour)))
          (fill-half source buffer 0) (fill-half source buffer 1)
          (draw-frame card)
          (when card
            (dma-start *blaster-dma* start (* 2 +block+))
            (blaster-write #xD1)                            ; speaker on
            (cond ((>= major 4)                             ; the rate itself
                   (blaster-write #x41) (blaster-write (hi +rate+)) (blaster-write (lo +rate+)))
                  (t                                        ; or as near as 256 steps go
                   (blaster-write #x40) (blaster-write (- 256 (round 1000000 +rate+)))))
            (blaster-write #x48)
            (blaster-write (lo (1- +block+))) (blaster-write (hi (1- +block+)))
            (blaster-write #x1C))                           ; go: 8 bits, auto-initialised
          (setf began (timer-count) began-ticks (ticks))
          (loop
            (wait-frame)
            ;; A block has been played: refill it, which is the half the card is not in.
            (let ((raised (interrupts)))
              (declare (fixnum raised))
              (when (/= raised handled)
                (when (> (- raised handled) 1) (incf late))
                (setf handled raised)
                (fill-half source buffer (- 1 (floor (place) +block+)))))
            (let* ((place (place))
                   (half (floor place +block+))
                   (offset (- place (* half +block+)))
                   (section (svref (sg-half-section source) half))
                   (kind (section-kind section))
                   (sweep-step (+ (aref (sg-half-step source) half)
                                  (* (sg-delta source) (+ offset (ash +n+ -1)))))
                   ;; (not judged when the 256 samples run on into the other half)
                   (whole (<= offset (- +block+ +n+))))
              (declare (fixnum place half offset sweep-step))
              (dotimes (i +n+)
                (setf (aref samples i)
                      (sap-ref-8 buffer (logand (+ place i) (1- (* 2 +block+))))))
              (let ((before (timer-count)))
                (analyse samples re im power)
                (incf clocks (max 0 (min 1000000 (- (timer-count) before)))))
              (find-peaks power peaks)
              (ecase kind
                (:chord (replace expected (section-bins section))
                 (when whole
                   (incf chord-frames)
                   (when (equalp peaks expected) (incf chord-right))))
                (:sweep (fill expected -1)
                 (setf (svref expected 0) (ash (+ sweep-step (ash 1 17)) -18))
                 (when whole
                   (incf sweep-frames)
                   (when (<= (abs (- (strongest power) (svref expected 0))) 1)
                     (incf sweep-right))))
                (:noise (fill expected -1)))
              (draw-wave samples)
              (draw-bars power)
              (draw-marks expected)
              (when (zerop (mod frames 4))          ; the words, a few times a second
                (text 76 (if (eq kind :sweep)
                             (format nil "sweep: ~D Hz"
                                     (ash (* (ash sweep-step -10) +rate+) -16))
                             (section-label section))
                      5)
                (text 182 (format nil "peaks~{ ~3D~}  marks~{ ~3D~}"
                                  (remove -1 (coerce peaks 'list))
                                  (remove -1 (coerce expected 'list)))
                      4)
                (text 191 (format nil "FFT ~D us  ~D ~:[blocks~;interrupts~]  Space Esc"
                                  (round (* (floor clocks (1+ frames)) 1000000) +timer-hz+)
                                  handled card)
                      4)))
            (incf frames)
            (when (key-pressed-p)
              (let ((key (get-key)))
                (cond ((eql key #\Space) (setf (sg-left source) 0))
                      ((eql key #\Escape) (return))))))
          (list :frames frames :seconds (/ (- (ticks) began-ticks) 18.2065)
                :blocks handled :late late
                :fft-us (round (* (floor clocks (max 1 frames)) 1000000) +timer-hz+)
                :chord-frames chord-frames :chord-right chord-right
                :sweep-frames sweep-frames :sweep-right sweep-right))))))

(defun main (&optional how)
  "Show the spectrum of a signal as the Sound Blaster plays it; HOW \"silent\": without the card."
  (let* ((card (and (not (equalp how "silent")) (blaster-present-p)))
         (major (if card (blaster-version) 0))
         result)
    (when (and card (< major 2))        ; too old to go round a buffer by itself
      (setf card nil))
    (multiple-value-bind (right chords worst) (self-test nil)
      (with-video-mode (#x13)
        (if card
            (with-blaster-interrupt ()
              (unwind-protect (setf result (play t major))
                ;; pause, end, speaker off
                (blaster-write #xD0) (blaster-write #xDA) (blaster-write #xD3)
                (dma-stop *blaster-dma*)))
            (setf result (play nil 0))))
      (if card
          (format t "~&Played through the Sound Blaster at ~Xh: ~D interrupts in ~,1F s,~%~
                     ~,2F a second (~D samples a second / ~D is ~,2F); ~D came late.~%"
                  *blaster-port* (getf result :blocks) (getf result :seconds)
                  (/ (getf result :blocks) (max 0.1 (getf result :seconds)))
                  +rate+ +block+ (/ +rate+ (float +block+)) (getf result :late))
          (format t "~&No Sound Blaster~:[ answered at port ~Xh~;~* was asked for~]: the signal ~
                     ran silently, by the clock,~%for ~,1F s (~D blocks).~%"
                  (equalp how "silent") *blaster-port*
                  (getf result :seconds) (getf result :blocks)))
      (format t "~D frames; a ~D-point transform took ~:D us on average.~%~
                 Chords: the three strongest peaks were in the tones' bins in ~D frames~%~
                 of ~D.  ~
                 Sweep: the strongest bin was within one of the tone's in~%~D of ~D.~%~
                 Self-test: ~D of ~D chords' peaks right; fixed point within ~D of floating ~
                 point~%(a full-scale tone is 4096).~%"
              (getf result :frames) +n+ (getf result :fft-us)
              (getf result :chord-right) (getf result :chord-frames)
              (getf result :sweep-right) (getf result :sweep-frames)
              right chords worst)))
  (values))

(doslib-examples:define-example "spectrum" 'main
  :category :sound
  :description "A spectrum analyser: a fixed-point FFT in Lisp of what the Sound Blaster plays.")
