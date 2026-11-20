;;;; A software mixer: eight voices of samples, each with its own pitch,
;;;; volume and place between left and right, mixed in Lisp into the Sound
;;;; Blaster's DMA buffer as it plays.
;;;;
;;;;   (doslib-examples:run "mixer")           stereo on a Sound Blaster 16
;;;;   (doslib-examples:run "mixer" "mono")    one channel, as on an older card
;;;;   (doslib-examples:run "mixer" 1024)      blocks of 1024 samples, not 512
;;;;   (doslib-examples.mixer:self-test)       only the check of the mixed bytes
;;;; 1 to 4 play a bass drum, a snare drum, a hi-hat and a sweep; 5, 6 and 7
;;;; pluck a string at three pitches; 8 holds an organ note until it is
;;;; pressed again; Space stops and starts the tune that plays by itself;
;;;; M changes between volume tables and multiplying; D makes the program
;;;; miss its time on purpose (a tenth of a second asleep: underruns);
;;;; Esc ends.  Without a card it says so and returns.
;;;;
;;;; A Sound Blaster plays one stream of samples.  A game wants a shot, an
;;;; engine, footsteps and music at once, so every game's sound engine was a
;;;; mixer: a few "voices", each playing one recording, added together in
;;;; software and the sum given to the card.  This is one.
;;;;
;;;; A voice has a sample (signed bytes), a position in it, and three
;;;; settings.  Pitch: for every output sample the position moves on by a
;;;; STEP that need not be 1 -- a step of 2 plays the sample an octave up,
;;;; 1/2 an octave down.  The position is kept in fixed point, a whole part
;;;; and 16 bits of fraction, and the step likewise (10000h is 1), so that
;;;; moving on is one addition and a carry.  Volume, 0 to 64.  Pan, 0 (left)
;;;; to 64 (right): the volume is shared between the two sides in that
;;;; proportion, which gives each voice a left and a right volume.
;;;;
;;;; The inner loop then adds sample * left volume to the left sum and
;;;; sample * right volume to the right sum, for every voice, for every
;;;; output sample: two multiplications, 22,050 times a second, times eight
;;;; voices.  A 386 took 9 to 38 clock cycles to multiply and a 486 13 or
;;;; more, against 2 to 4 for reading memory, so the engines of the day did
;;;; not multiply.  They kept a VOLUME TABLE: for each of the 65 volumes, the
;;;; 256 possible sample bytes already multiplied (65 * 256 16-bit entries,
;;;; 33K), and the inner loop looks the product up with the sample byte as
;;;; the index.  (The other trick, when memory was short, was to allow only
;;;; volumes that are powers of two and shift.)  The sums are 32 bits wide
;;;; while they are being made; at the end each is shifted right 6 bits --
;;;; the volume's 64 -- and turned into a byte through a second table, the
;;;; CLIP TABLE, which holds 0 for everything too negative and 255 for
;;;; everything too positive: no comparisons either.  Both ways are here,
;;;; MIX-VOICE-TABLE and MIX-VOICE-MULTIPLY, and they give the same bytes;
;;;; M changes between them while it plays, and the last lines printed say
;;;; what each costs on this machine.  (On a Pentium, and under QEMU, a
;;;; multiplication costs little: do not expect the 386's difference.)
;;;;
;;;; The card goes round a buffer of two blocks for ever (auto-initialised
;;;; DMA) and interrupts at the end of each; Lisp then mixes the next block
;;;; into the half that has just been played, while the card plays the
;;;; other.  Which half that is, the DMA controller's count register says.
;;;; On a Sound Blaster 16 the sound is stereo, left and right bytes in
;;;; turn (DSP command C6h with mode 20h); on an older card, or with "mono",
;;;; the two sums are added and the sound has one channel (commands 48h and
;;;; 1Ch).  The screen shows the voices, and what the mixer costs: the
;;;; microseconds it took to mix the last block (by TIMER-COUNT) against the
;;;; microseconds the block lasts, which is the share of the processor the
;;;; mixer needs.  An "underrun" is a block that was not ready in time: two
;;;; interrupts had come when Lisp looked, so the card has played a block
;;;; twice.
;;;;
;;;; The tune is played by the mixer's "tick": a function the mixer calls
;;;; every so many output samples, in the middle of a block if need be, so
;;;; that notes start on the beat and not on a block's edge.
;;;;
;;;; The mixing itself is in functions that do not know about the screen or
;;;; the tune: MAKE-MIXER, PLAY-SAMPLE, STOP-VOICE, MIX-BLOCK, and for the
;;;; card WITH-MIXER (MIXER-START, MIXER-STOP) and MIXER-SERVICE.  Since
;;;; sound cannot always be heard, SELF-TEST mixes known samples and
;;;; compares every byte with the same sum worked out another way: with no
;;;; tables, no fixed point carried from sample to sample, and exact
;;;; arithmetic.
;;;;
;;;; Under QEMU the card takes its samples in batches and the interrupts
;;;; come in bunches, so a short block can be "late" there with a processor
;;;; that is nearly idle; the count of interrupts a second is still right.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.mixer
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:self-test
           #:make-sound #:make-mixer #:play-sample #:stop-voice #:mix-block
           #:with-mixer #:mixer-start #:mixer-stop #:mixer-service))
(in-package #:doslib-examples.mixer)

;;; ---- the mixing core: nothing here knows about the card or the screen
(deftype sample-data () '(simple-array (signed-byte 8) (*)))
(deftype mix-buffer () '(simple-array (signed-byte 32) (*)))

(defstruct sound
  "A recording: DATA played at RATE samples a second sounds as it was made.
With a LOOP-LENGTH it goes round its last LOOP-LENGTH samples from LOOP-START for ever."
  (name "" :type simple-string)
  (data (make-array 0 :element-type '(signed-byte 8)) :type sample-data)
  (rate 22050 :type fixnum)
  (loop-start 0 :type fixnum)
  (loop-length 0 :type fixnum))

(defstruct (voice (:conc-name v-))
  (active nil)
  (sound nil)
  (data (make-array 0 :element-type '(signed-byte 8)) :type sample-data)
  (position 0 :type (integer 0 #xFFFFF))        ; the whole part
  (fraction 0 :type (integer 0 #xFFFF))         ; and 16 bits below the point
  (step 0 :type (integer 0 #xFFFFFF))           ; 10000h: one sample for each one made
  (end 0 :type (integer 0 #xFFFFF))
  (loop-length 0 :type (integer 0 #xFFFFF))
  (left 0 :type (integer 0 64))
  (right 0 :type (integer 0 64))
  (volume 0 :type (integer 0 64))
  (pan 32 :type (integer 0 64))
  (started 0 :type fixnum))                     ; the oldest voice is the one taken

(defstruct (mixer (:conc-name mx-) (:constructor %make-mixer))
  (rate 22050 :type fixnum)                     ; output samples a second
  (frames 512 :type fixnum)                     ; output samples (pairs, in stereo) in a block
  (stereo t)
  (tables t)                                    ; volume tables, or multiply
  (voices (vector) :type simple-vector)
  (mix (make-array 0 :element-type '(signed-byte 32)) :type mix-buffer)
  (serial 0 :type fixnum)
  (tick-function nil)                           ; called every TICK-FRAMES output samples
  (tick-frames 0 :type fixnum)
  (tick-left 0 :type fixnum)
  ;; the card
  (segment nil) (address 0 :type fixnum) (half-bytes 0 :type fixnum)
  (card-rate 22050 :type fixnum)                ; what the card was told, which may differ a little
  (handled 0 :type fixnum)
  ;; what it has cost
  (blocks 0 :type fixnum) (underruns 0 :type fixnum)
  (last-clocks 0 :type fixnum) (total-clocks 0 :type fixnum) (longest-clocks 0 :type fixnum))

(defconstant +most-voices+ 16 "The clip table has room for the sum of this many.")

(declaim (type (simple-array (signed-byte 16) (*)) *volume-table*)
         (type (simple-array (unsigned-byte 8) (*)) *clip-table*))
(defvar *volume-table*
  ;; entry 256 * volume + byte: the byte, read as a signed sample, times the volume
  (let ((table (make-array (* 65 256) :element-type '(signed-byte 16))))
    (dotimes (volume 65 table)
      (dotimes (byte 256)
        (setf (aref table (+ (* 256 volume) byte))
              (* volume (if (< byte 128) byte (- byte 256))))))))
(defvar *clip-table*
  ;; entry 2048 + sum: the byte the card is given, 128 for silence
  (let ((table (make-array 4096 :element-type '(unsigned-byte 8))))
    (dotimes (i 4096 table)
      (setf (aref table i) (max 0 (min 255 (+ 128 (- i 2048))))))))

(defun make-mixer (&key (rate 22050) (voices 8) (frames 512) (stereo t) tick (tick-frames 0))
  "A mixer of VOICES voices making RATE samples a second in blocks of FRAMES.
TICK, if given, is called with the mixer every TICK-FRAMES samples."
  (assert (<= 1 voices +most-voices+))
  (let ((all (make-array voices)))
    (dotimes (i voices) (setf (svref all i) (make-voice)))
    (%make-mixer :rate rate :frames frames :stereo stereo :voices all
                 :mix (make-array (* 2 frames) :element-type '(signed-byte 32)
                                               :initial-element 0)
                 :tick-function tick :tick-frames tick-frames)))

(defun play-sample (mixer sound &key (rate (sound-rate sound)) (volume 64) (pan 32) voice)
  "Start SOUND on a voice of MIXER: at RATE of its samples a second (twice
the sound's own rate is an octave up), VOLUME 0 to 64, PAN 0 (left) to 64
(right).  VOICE says which voice; otherwise one that is silent is taken, or
the one that has played longest.  Returns the voice's number."
  (declare (type mixer mixer) (type sound sound) (fixnum rate volume pan))
  (let* ((voices (mx-voices mixer))
         (index (or voice
                    (position-if-not #'v-active voices)
                    (let ((oldest 0))
                      (dotimes (i (length voices) oldest)
                        (when (< (v-started (svref voices i)) (v-started (svref voices oldest)))
                          (setf oldest i))))))
         (v (svref voices index))
         (volume (max 0 (min 64 volume)))
         (pan (max 0 (min 64 pan)))
         (right (ash (+ (* volume pan) 32) -6)))
    (setf (v-active v) nil)                     ; (not half set up, should the mixer look)
    (setf (v-sound v) sound
          (v-data v) (sound-data sound)
          (v-position v) 0
          (v-fraction v) 0
          (v-step v) (max 1 (min #xFFFFFF (round (* rate 65536) (mx-rate mixer))))
          (v-loop-length v) (sound-loop-length sound)
          (v-end v) (if (plusp (sound-loop-length sound))
                        (+ (sound-loop-start sound) (sound-loop-length sound))
                        (length (sound-data sound)))
          (v-volume v) volume
          (v-pan v) pan
          (v-right v) right
          (v-left v) (- volume right)
          (v-started v) (incf (mx-serial mixer))
          (v-active v) (plusp (v-end v)))
    index))

(defun stop-voice (mixer index)
  "Silence voice INDEX of MIXER."
  (setf (v-active (svref (mx-voices mixer) index)) nil))

;;; The inner loop, twice: the two differ only in how a sample becomes its
;;; two products.  MIX holds a left and a right sum for each output sample.
(defmacro define-voice-mixer (name documentation bindings sample left right)
  `(defun ,name (voice mix start count)
     ,documentation
     (declare (type voice voice) (type mix-buffer mix)
              (type (integer 0 8192) start count)
              (optimize speed (safety 0)))
     (when (v-active voice)
       (let* ((data (v-data voice))
              (position (v-position voice))
              (fraction (v-fraction voice))
              (step (v-step voice))
              (end (v-end voice))
              (loop-length (v-loop-length voice))
              ,@bindings)
         (declare (type (integer 0 #xFFFFF) position end loop-length)
                  (type (integer 0 #x1FFFFFF) fraction)
                  (type (integer 0 #xFFFFFF) step))
         (loop for i of-type fixnum from (* 2 start) below (* 2 (+ start count)) by 2
               do (let ((sample ,sample))
                    (incf (aref mix i) ,left)
                    (incf (aref mix (1+ i)) ,right))
                  (setf fraction (+ fraction step)
                        position (+ position (ash fraction -16))
                        fraction (logand fraction #xFFFF))
                  (when (>= position end)
                    (cond ((plusp loop-length)
                           (loop do (setf position (- position loop-length))
                                 while (>= position end)))
                          (t (setf (v-active voice) nil position 0)
                             (return)))))
         (setf (v-position voice) position
               (v-fraction voice) fraction)))
     nil))

(define-voice-mixer mix-voice-table
  "Add COUNT output samples of VOICE to MIX from START on, and move VOICE
along: the products come from the volume table."
  ((table *volume-table*)
   (left (ash (v-left voice) 8))
   (right (ash (v-right voice) 8)))
  (logand (aref data position) 255)
  (aref table (+ left sample))
  (aref table (+ right sample)))

(define-voice-mixer mix-voice-multiply
  "Add COUNT output samples of VOICE to MIX from START on, and move VOICE
along: the products are multiplied out."
  ((left (v-left voice))
   (right (v-right voice)))
  (aref data position)
  (* sample left)
  (* sample right))

(defun mix-block (mixer buffer frames)
  "Make the next FRAMES output samples of MIXER and store them at BUFFER as
the card wants them: bytes, 128 silence; in stereo a left byte and then a
right one, 2 * FRAMES in all."
  (declare (type mixer mixer) (system-area-pointer buffer) (type (integer 0 4096) frames)
           (optimize speed))
  (let ((mix (mx-mix mixer)) (voices (mx-voices mixer)) (tables (mx-tables mixer))
        (tick (mx-tick-function mixer)) (clip *clip-table*) (done 0))
    (declare (type (integer 0 4096) done))
    (loop while (< done frames)
          do (let ((now (- frames done)))
               (declare (type (integer 0 4096) now))
               (when tick                       ; up to the next tick and no further
                 (when (<= (mx-tick-left mixer) 0)
                   (funcall (the function tick) mixer)
                   (setf (mx-tick-left mixer) (max 1 (mx-tick-frames mixer))))
                 (setf now (min now (mx-tick-left mixer)))
                 (decf (mx-tick-left mixer) now))
               (dotimes (i (length voices))
                 (if tables
                     (mix-voice-table (svref voices i) mix done now)
                     (mix-voice-multiply (svref voices i) mix done now)))
               (incf done now)))
    ;; The sums are of products with volumes up to 64: divide by 64, and the
    ;; clip table makes a byte of what is left.  Four voices at full volume
    ;; in the middle are just not clipped.
    (if (mx-stereo mixer)
        (dotimes (i (* 2 frames))
          (setf (sap-ref-8 buffer i) (aref clip (+ 2048 (ash (aref mix i) -6)))
                (aref mix i) 0))
        (dotimes (i frames)
          (let* ((at (* 2 i))
                 (sum (+ (ldb (byte 24 0) (aref mix at)) (ldb (byte 24 0) (aref mix (1+ at))))))
            ;; (24 bits of each sum are plenty, and adding them so needs no
            ;; more than a machine word; the sign is put back afterwards)
            (setf (sap-ref-8 buffer i)
                  (aref clip (+ 2048 (ash (- (ldb (byte 24 0) (+ sum #x800000)) #x800000) -6)))
                  (aref mix at) 0
                  (aref mix (1+ at)) 0))))
    nil))

;;; ---- the card: the library's Sound Blaster and DMA functions (src/blaster.lisp, src/dma.lisp)
(defun mixer-start (mixer)
  "Give MIXER a DMA buffer of two blocks, mix the first two, and start the
card playing them round and round.  Inside WITH-BLASTER-INTERRUPT."
  (let* ((frames (mx-frames mixer))
         (rate (mx-rate mixer))
         (half (* frames (if (mx-stereo mixer) 2 1)))
         ;; A DMA transfer cannot cross a 64K boundary of memory: twice the
         ;; room has the buffer on one side or the other of any boundary.
         ;; And the buffer is given whole 4K pages to itself.  That is for
         ;; emulators: QEMU watches every page that holds code it has
         ;; translated, and a small block of DOS memory is apt to share a
         ;; page with a driver; each byte stored there then costs over a
         ;; microsecond, which made a block 1.5 ms dearer to mix.
         (room (* 4096 (ceiling (* 2 half) 4096)))
         (segment (dos-alloc (+ 4096 (* 2 room))))
         (address (dma-fit (logandc2 (+ (linear segment) 4095) 4095) room)))
    (setf (mx-segment mixer) segment
          (mx-address mixer) address
          (mx-half-bytes mixer) half
          (mx-handled mixer) (blaster-interrupts))
    (mix-block mixer (int-sap address) frames)
    (mix-block mixer (int-sap (+ address half)) frames)
    (dma-start *blaster-dma* address (* 2 half))
    (blaster-write #xD1)                                        ; speaker on
    (cond ((mx-stereo mixer)
           ;; a Sound Blaster 16: the rate itself; then 8 bits, auto-initialised,
           ;; stereo, unsigned, and the bytes between interrupts less one
           (setf (mx-card-rate mixer) rate)
           (blaster-write #x41) (blaster-write (hi rate)) (blaster-write (lo rate))
           (blaster-write #xC6) (blaster-write #x20)
           (blaster-write (lo (1- half))) (blaster-write (hi (1- half))))
          (t
           ;; any card from DSP version 2: the rate as a "time constant", as
           ;; near as its 256 steps go
           (let ((constant (- 256 (round 1000000 rate))))
             (setf (mx-card-rate mixer) (round 1000000 (- 256 constant)))
             (blaster-write #x40) (blaster-write constant)
             (blaster-write #x48)
             (blaster-write (lo (1- half))) (blaster-write (hi (1- half)))
             (blaster-write #x1C))))
    mixer))

(defun mixer-stop (mixer)
  "Stop the card, mask the DMA channel and give MIXER's buffer back."
  (when (mx-segment mixer)
    (blaster-write #xD0) (blaster-write #xDA) (blaster-write #xD3)  ; pause, end, speaker off
    (dma-stop *blaster-dma*)
    (dos-free (mx-segment mixer))
    (setf (mx-segment mixer) nil))
  mixer)

(defmacro with-mixer ((mixer) &body body)
  "Run BODY with MIXER playing through the Sound Blaster; BODY must call
MIXER-SERVICE often.  However BODY ends the card is stopped and reset, its
interrupt and DMA channel put back, and the buffer freed."
  (let ((m (gensym "MIXER")))
    `(let ((,m ,mixer))
       (with-blaster-interrupt ()
         (unwind-protect (progn (mixer-start ,m) ,@body)
           (mixer-stop ,m))))))

(defun mixer-service (mixer)
  "If the card has finished a block since last time, mix the next one into
the half of the buffer it has just played, and return true.  The time that
took is in the mixer, in TIMER-COUNT's clocks."
  (declare (type mixer mixer))
  (let ((raised (blaster-interrupts)))
    (declare (fixnum raised))
    (unless (= raised (mx-handled mixer))
      (incf (mx-underruns mixer) (- raised (mx-handled mixer) 1))
      (setf (mx-handled mixer) raised)
      ;; The count register goes down from two halves, less one, to nothing:
      ;; while it is in its upper half the card is playing the first half,
      ;; and the second is the one to fill.
      (let* ((half (mx-half-bytes mixer))
             (offset (if (>= (dma-count *blaster-dma*) half) half 0))
             (before (timer-count)))
        (mix-block mixer (int-sap (+ (mx-address mixer) offset)) (mx-frames mixer))
        (let ((clocks (max 0 (min 10000000 (- (timer-count) before)))))
          (declare (fixnum clocks))
          (setf (mx-last-clocks mixer) clocks)
          (incf (mx-total-clocks mixer) clocks)
          (when (> clocks (mx-longest-clocks mixer)) (setf (mx-longest-clocks mixer) clocks))
          (incf (mx-blocks mixer))))
      t)))

(defun microseconds (clocks)
  "TIMER-COUNT's clocks (1,193,182 a second) as microseconds."
  (declare (fixnum clocks))
  (if (< clocks 60000) (floor (* clocks 8381) 10000) (round (* clocks 1000000) +timer-hz+)))

;;; ---- the samples: made here, 22,050 to the second
(defvar *seed* 12345)
(defun noise ()
  "The next of a fixed run of random numbers from -1 to 1."
  (setf *seed* (logand (+ (* *seed* 1103515245) 12345) #x7FFFFFFF))
  (- (/ (ldb (byte 16 8) *seed*) 32768.0) 1.0))

(defun sound-from (name seconds function &key (rate 22050) (loop-length 0))
  "A sound of SECONDS seconds: FUNCTION is called with the sample's number
and its time in seconds, in order, and returns from -1 to 1."
  (let* ((length (round (* seconds rate)))
         (data (make-array length :element-type '(signed-byte 8))))
    (dotimes (i length)
      (setf (aref data i)
            (max -127 (min 127 (round (* 127 (funcall function i (/ i (float rate)))))))))
    (make-sound :name name :data data :rate rate :loop-length loop-length)))

(defconstant +string-hz+ 220.5 "The plucked string's own pitch: 100 samples to a wave.")

(defun make-sounds ()
  "The six sounds, as a list: bass drum, snare drum, hi-hat, sweep, plucked string, organ."
  (setf *seed* 12345)
  (let ((two-pi (* 2 (float pi 1.0))) (phase 0.0) (last 0.0)
        (line (make-array 100 :element-type 'single-float)) (at 0))
    (list
     ;; a bass drum: a sine that falls from 150 Hz to 40 as it dies away
     (sound-from "kick  " 0.30
                 (lambda (i time)
                   (declare (ignore i))
                   (incf phase (/ (* two-pi (+ 40 (* 110 (exp (* -18 time))))) 22050))
                   (* (exp (* -9 time)) (sin phase))))
     ;; a snare drum: noise dying away fast over a short low tone
     (sound-from "snare " 0.20
                 (lambda (i time)
                   (declare (ignore i))
                   (+ (* 0.7 (noise) (exp (* -20 time)))
                      (* 0.5 (sin (* two-pi 190 time)) (exp (* -25 time))))))
     ;; a hi-hat: only what changes from one sample of noise to the next
     (sound-from "hi-hat" 0.08
                 (lambda (i time)
                   (declare (ignore i))
                   (let ((new (noise)))
                     (prog1 (* 0.5 (- new last) (exp (* -60 time)))
                       (setf last new)))))
     ;; a sweep: a sine rising from 300 Hz to 2400, swelling and fading
     (progn
       (setf phase 0.0)
       (sound-from "sweep " 0.60
                   (lambda (i time)
                     (declare (ignore i))
                     (incf phase (/ (* two-pi (+ 300 (* 3500 time))) 22050))
                     (* (sin (* (float pi 1.0) (/ time 0.6))) (sin phase)))))
     ;; a plucked string (Karplus and Strong's): a hundred samples of noise
     ;; going round and round, each averaged with its neighbour every time
     (progn
       (dotimes (i 100) (setf (aref line i) (noise)))
       (sound-from "pluck " 0.90
                   (lambda (i time)
                     (declare (ignore i time))
                     (let* ((next (mod (1+ at) 100))
                            (value (aref line at)))
                       (setf (aref line at) (* 0.996 0.5 (+ value (aref line next)))
                             at next)
                       value))))
     ;; an organ: one wave of three harmonics, a hundred samples, round for ever
     (sound-from "organ " (/ 100 22050.0)
                 (lambda (i time)
                   (declare (ignore time))
                   (let ((angle (/ (* two-pi i) 100)))
                     (+ (* 0.55 (sin angle)) (* 0.3 (sin (* 2 angle))) (* 0.15 (sin (* 3 angle))))))
                 :loop-length 100))))

(defun string-rate (hz)
  "The rate to play the plucked string or the organ at for a note of HZ."
  (round (* 22050 hz) +string-hz+))

;;; ---- the check: the same sums another way
(defun reference-byte (settings frame side rate)
  "What output sample FRAME should be on SIDE (0 left, 1 right, NIL both
together) when the voices in SETTINGS -- lists of sound, rate, volume, pan
-- all started at sample 0: worked out from the definitions, exactly."
  (let ((sum 0))
    (loop for (sound voice-rate volume pan) in settings
          do (let* ((data (sound-data sound))
                    (step (round (* voice-rate 65536) rate))
                    (at (floor (* frame step) 65536))
                    (looped (plusp (sound-loop-length sound)))
                    (end (if looped
                             (+ (sound-loop-start sound) (sound-loop-length sound))
                             (length data)))
                    (right (floor (+ (/ (* volume pan) 64) 1/2)))
                    (share (case side ((0) (- volume right)) ((1) right) (t volume))))
               (when (and looped (>= at end))
                 (setf at (+ (sound-loop-start sound)
                             (mod (- at (sound-loop-start sound)) (sound-loop-length sound)))))
               (when (< at end)
                 (incf sum (* (aref data at) share)))))
    (max 0 (min 255 (+ 128 (floor sum 64))))))

(defun mixed-bytes (settings rate stereo tables frames blocks)
  "The bytes a new mixer makes of SETTINGS in BLOCKS blocks of FRAMES."
  (let* ((mixer (make-mixer :rate rate :voices (max 1 (length settings))
                            :frames frames :stereo stereo))
         (per-block (* frames (if stereo 2 1)))
         (bytes (make-array (* blocks per-block) :element-type '(unsigned-byte 8))))
    (setf (mx-tables mixer) tables)
    (loop for (sound voice-rate volume pan) in settings
          do (play-sample mixer sound :rate voice-rate :volume volume :pan pan))
    (with-pinned-objects (bytes)
      (dotimes (b blocks)
        (mix-block mixer (sap+ (vector-sap bytes) (* b per-block)) frames)))
    bytes))

(defun constant-sound (value length)
  (make-sound :name "const " :rate 22050
              :data (make-array length :element-type '(signed-byte 8) :initial-element value)))

(defun self-test (&optional (sounds (make-sounds)) (report t))
  "Mix known samples and compare every byte with what it should be.  Returns
the bytes compared and the number that differ; prints them if REPORT."
  (let ((checked 0) (wrong 0))
    (flet ((expect (bytes expected)
             (dotimes (i (length bytes))
               (incf checked)
               (unless (= (aref bytes i) (funcall expected i)) (incf wrong)))))
      ;; Three sums done by hand.  100 at volume 64 in the middle is 100 * 32
      ;; on each side; -128 at volume 32 on the left takes 128 * 32 from the
      ;; left: (3200 - 4096) / 64 = -14 there and 3200 / 64 = 50 on the right.
      (let ((two (list (list (constant-sound 100 64) 22050 64 32)
                       (list (constant-sound -128 64) 22050 32 0)))
            ;; four voices of 127 at full volume on the right: 508, clipped
            (loud (loop repeat 4 collect (list (constant-sound 127 64) 22050 64 64))))
        (dolist (tables '(t nil))
          (expect (mixed-bytes two 22050 t tables 16 1)
                  (lambda (i) (if (evenp i) 114 178)))
          (expect (mixed-bytes two 22050 nil tables 16 1)       ; (3200 + 3200 - 4096) / 64 = 36
                  (constantly 164))
          (expect (mixed-bytes loud 22050 t tables 16 1)
                  (lambda (i) (if (evenp i) 128 255)))))
      ;; Eight voices of the real sounds: pitches above and below the
      ;; sounds' own, one that ends on the way, two that go round.
      (destructuring-bind (kick snare hat sweep pluck organ) sounds
        (let ((settings (list (list pluck 22050 64 32) (list pluck 27783 40 10)
                              (list kick 11025 64 32) (list snare 22050 50 50)
                              (list hat 44100 64 0) (list organ 33075 30 64)
                              (list sweep 15000 64 20) (list organ 7000 64 32))))
          (dolist (tables '(t nil))
            (expect (mixed-bytes settings 22050 t tables 256 4)
                    (lambda (i) (reference-byte settings (floor i 2) (mod i 2) 22050)))
            (expect (mixed-bytes settings 22050 nil tables 256 4)
                    (lambda (i) (reference-byte settings i nil 22050)))))))
    (when report
      (format t "~&Mixer self-test: ~:D bytes mixed with tables and by multiplying, ~
                 in stereo and mono;~%~:[~:D differ from the sums worked out exactly.~;~
                 all equal to the sums worked out exactly.~]~%"
              checked (zerop wrong) wrong))
    (values checked wrong)))

(defun measure (sounds tables)
  "Microseconds to mix a block of 512 with eight voices playing: 16 blocks are timed."
  (let ((mixer (make-mixer :voices 8 :frames 512))
        (organ (sixth sounds))
        (bytes (make-array 1024 :element-type '(unsigned-byte 8))))
    (setf (mx-tables mixer) tables)
    (dotimes (i 8)
      (play-sample mixer organ :rate (* 3000 (+ 3 i)) :volume 40 :pan (* 9 i)))
    (with-pinned-objects (bytes)
      (let ((before (timer-count)))
        (dotimes (i 16) (mix-block mixer (vector-sap bytes) 512))
        (microseconds (floor (- (timer-count) before) 16))))))

;;; ---- the tune that plays by itself: sixteenths at 120 beats a minute, two bars
(defparameter *pattern*
  ;; (step sound-number hertz-or-nil volume pan)
  '((0 0 nil 64 32) (8 0 nil 64 32) (16 0 nil 64 32) (22 0 nil 40 32) (24 0 nil 64 32)
    (4 1 nil 48 40) (12 1 nil 48 40) (20 1 nil 48 40) (28 1 nil 48 40)
    (2 2 nil 40 12) (6 2 nil 40 12) (10 2 nil 40 12) (14 2 nil 40 12)
    (18 2 nil 40 12) (22 2 nil 40 12) (26 2 nil 40 12) (30 2 nil 40 12)
    ;; the bass string
    (0 4 110.0 56 28) (6 4 110.0 56 28) (10 4 164.8 56 28)
    (16 4 146.8 56 28) (22 4 146.8 56 28) (26 4 123.5 56 28)
    ;; and the tune, wandering from side to side
    (0 4 440.0 44 10) (3 4 554.4 44 22) (6 4 659.3 44 36) (8 4 880.0 44 52)
    (11 4 659.3 44 40) (14 4 554.4 44 26) (16 4 587.3 44 12) (19 4 740.0 44 24)
    (22 4 880.0 44 38) (24 4 659.3 44 52) (27 4 493.9 44 44) (30 4 415.3 44 30)))

(defun make-tune (sounds)
  "A tick function for a mixer, and a function that turns the tune on and off."
  (let ((step 0) (playing t)
        (pattern (loop for (at number hz volume pan) in *pattern*
                       for sound = (nth number sounds)
                       collect (list at sound (if hz (string-rate hz) (sound-rate sound))
                                     volume pan))))
    (values
     (lambda (mixer)
       (when playing
         (let ((now (mod step 32)))
           (dolist (note pattern)
             (when (= (the fixnum (first note)) now)
               (play-sample mixer (second note) :rate (third note)
                                                :volume (fourth note) :pan (fifth note))))
           (when (= (mod step 128) 124)         ; a sweep to end every fourth time round
             (play-sample mixer (fourth sounds) :volume 36 :pan 54)))
         (incf step)))
     (lambda () (setf playing (not playing))))))

;;; ---- the screen: straight into text-mode memory, so that nothing is allocated
(defun put-char (x y code attribute)
  (declare (fixnum x y code attribute))
  (when (and (<= 0 x 79) (<= 0 y 24))
    (setf (sap-ref-16 (int-sap #xB8000) (* 2 (+ x (* 80 y)))) (logior code (ash attribute 8)))))
(defun put-string (x y string attribute)
  (declare (simple-string string) (fixnum x))
  (dotimes (i (length string))
    (put-char (+ x i) y (logand (char-code (char string i)) 255) attribute)))
(defun put-number (x y number width attribute)
  "NUMBER in WIDTH columns from X, padded on the left."
  (declare (fixnum x number width))
  (loop for column of-type fixnum from (+ x width -1) downto x
        for first = t then nil
        do (if (or first (plusp number))
               (multiple-value-bind (rest digit) (truncate number 10)
                 (put-char column y (+ 48 digit) attribute)
                 (setf number rest))
               (put-char column y 32 attribute))))
(defun put-tenths (x y tenths width attribute)
  "TENTHS as a number with one decimal place, in WIDTH columns from X."
  (declare (fixnum x tenths width))
  (put-number x y (floor tenths 10) (- width 2) attribute)
  (put-char (+ x width -2) y 46 attribute)
  (put-char (+ x width -1) y (+ 48 (mod tenths 10)) attribute))

(defconstant +bar+ 34 "The width of a voice's bar.")

(defun voice-level (voice)
  "How loud VOICE is about now, 0 to 127: its largest sample of the next 48, times its volume.
(The bars show half of that range: few voices are louder.)"
  (declare (type voice voice) (optimize speed))
  (let* ((data (v-data voice)) (position (v-position voice)) (peak 0))
    (declare (fixnum peak))
    (loop for i of-type fixnum from position below (min (length data) (+ position 48))
          do (let ((size (abs (aref data i))))
               (when (> size peak) (setf peak size))))
    (ash (* peak (v-volume voice)) -6)))

(defun draw-frame (mixer major minor)
  (text-fill 0 0 80 25 7)
  (put-string 0 0 (format nil " Software mixer: ~D voices, ~:D samples a second, ~
                               ~:[mono~;stereo~], blocks of ~D~80T"
                          (length (mx-voices mixer)) (mx-rate mixer) (mx-stereo mixer)
                          (mx-frames mixer))
              #x1F)
  (put-string 1 1 (format nil "Sound Blaster at ~Xh, interrupt ~D, DMA ~D, DSP ~D.~2,'0D, ~
                               playing ~:D a second"
                          *blaster-port* *blaster-irq* *blaster-dma* major minor
                          (mx-card-rate mixer))
              8)
  (put-string 1 3 "voice sample  rate Hz  vol  pan L       R  level" 11)
  (put-string 1 13 "a block lasts" 7) (put-string 23 13 "us" 7)
  (put-string 1 14 "mixing it took" 7)
  (put-string 23 14 "us" 7) (put-string 34 14 "us on average" 7) (put-string 56 14 "us at most" 7)
  (put-string 1 15 "the mixer uses" 7) (put-string 22 15 "% of the processor, with" 7)
  (put-string 1 17 "interrupts" 7)
  (put-string 26 17 "a second (the card's rate / block:" 7) (put-string 66 17 ")" 7)
  (put-string 1 18 "underruns" 7) (put-string 23 18 "blocks not mixed in time" 7)
  (put-string 1 19 "DMA at" 7) (put-string 23 19 "of        filling half" 7)
  (put-string 1 22 "1 kick  2 snare  3 hi-hat  4 sweep  5 6 7 pluck  8 organ on and off" 14)
  (put-string 1 23 "Space the tune   M tables or multiply   D sleep 0.1 s   Esc ends" 14))

(defun draw-state (mixer block-us elapsed-ticks tune-on filling)
  (declare (type mixer mixer) (fixnum block-us elapsed-ticks))
  (let ((voices (mx-voices mixer)))
    (dotimes (i (length voices))
      (let* ((voice (svref voices i)) (y (+ 4 i))
             (active (v-active voice)))
        (put-number 2 y (1+ i) 2 (if active 15 8))
        (cond (active
               (put-string 7 y (sound-name (v-sound voice)) 15)
               (put-number 14 y (ash (* (ash (v-step voice) -4) (mx-rate mixer)) -12) 7 7)
               (put-number 23 y (v-volume voice) 3 7)
               (dotimes (k 9)
                 (if (= k (ash (v-pan voice) -3))
                     (put-char (+ 32 k) y 254 14)
                     (put-char (+ 32 k) y 196 8)))
               (let ((length (min +bar+ (ash (* (voice-level voice) +bar+) -6))))
                 (dotimes (k +bar+)
                   (if (< k length)
                       (put-char (+ 44 k) y 219 (if (< k 24) 10 (if (< k 30) 14 12)))
                       (put-char (+ 44 k) y 250 8)))))
              (t
               (put-string 7 y "-     " 8)
               (put-string 14 y "                           " 7)
               (dotimes (k +bar+) (put-char (+ 44 k) y 250 8))))))
    (let* ((blocks (max 1 (mx-blocks mixer)))
           (average (microseconds (floor (mx-total-clocks mixer) blocks))))
      (declare (fixnum blocks average))
      (put-number 16 13 block-us 6 15)
      (put-number 16 14 (microseconds (mx-last-clocks mixer)) 6 15)
      (put-number 27 14 average 6 15)
      (put-number 49 14 (microseconds (mx-longest-clocks mixer)) 6 15)
      (put-tenths 16 15 (floor (* 1000 average) block-us) 5 15)
      (put-string 47 15 (if (mx-tables mixer) "volume tables  " "multiplications") 15)
      (put-number 12 17 (mx-handled mixer) 6 15)
      (when (plusp elapsed-ticks)               ; 18.2065 ticks to the second
        (put-tenths 20 17 (floor (* (mx-handled mixer) 182) elapsed-ticks) 5 15))
      (put-tenths 61 17 (floor (* 10 (mx-card-rate mixer)) (mx-frames mixer)) 5 15)
      (put-number 12 18 (mx-underruns mixer) 6 (if (zerop (mx-underruns mixer)) 15 12))
      (put-number 12 19 (logand (- (dma-address *blaster-dma*) (mx-address mixer)) #xFFFF) 6 15)
      (put-number 26 19 (* 2 (mx-half-bytes mixer)) 5 7)
      (put-number 46 19 filling 1 15)
      (put-string 1 21 (if tune-on "the tune is playing" "the tune is off    ") 7))))

;;; ---- the program
(defun main (&rest options)
  "Play eight voices mixed in Lisp through the Sound Blaster.  OPTIONS:
\"mono\" for one channel; a number is the samples in a block instead of 512."
  (unless (blaster-present-p)
    (format t "~&No Sound Blaster answered at port ~Xh.  (QEMU: -device sb16; ~
               SET BLASTER=A220 I5 D1 says where a card is.)~%" *blaster-port*)
    (return-from main (values)))
  (multiple-value-bind (major minor) (blaster-version)
    (when (< major 2)
      (format t "~&The card's DSP is version ~D.~2,'0D: auto-initialised DMA came with version 2.~%"
              major minor)
      (return-from main (values)))
    (let* ((sounds (make-sounds))
           (stereo (and (>= major 4) (not (member "mono" options :test #'equalp))))
           (frames (max 64 (min 4096 (or (find-if #'integerp options) 512))))
           (tune-on t)
           (organ-voice nil)
           (began 0) (seconds 0.0) (places 0) (last-place -1) (allocated 0)
           (table-us 0) (multiply-us 0) checked wrong mixer)
      (format t "~&Making the sounds and checking the mixer ...~%")
      (multiple-value-setq (checked wrong) (self-test sounds nil))
      (setf table-us (measure sounds t) multiply-us (measure sounds nil))
      (multiple-value-bind (tick toggle) (make-tune sounds)
        (setf mixer (make-mixer :rate 22050 :voices 8 :frames frames :stereo stereo
                                :tick tick :tick-frames (floor 22050 8)))
        (with-mixer (mixer)
          (unwind-protect
               (let ((block-us (round (* 1000000 (mx-frames mixer)) (mx-card-rate mixer)))
                     (last-tick 0))
                 (declare (fixnum last-tick block-us))
                 (hide-text-cursor)
                 (draw-frame mixer major minor)
                 (put-string 1 20 (format nil "self-test: ~:D bytes checked, ~D wrong"
                                          checked wrong)
                             (if (zerop wrong) 10 12))
                 (setf began (ticks) last-tick began
                       allocated (get-bytes-consed))
                 (loop
                   (when (mixer-service mixer)
                     (let ((place (dma-address *blaster-dma*)))
                       (unless (= place last-place) (incf places) (setf last-place place)))
                     (draw-state mixer block-us (- (ticks) began) tune-on
                                 (if (>= (dma-count *blaster-dma*) (mx-half-bytes mixer)) 1 0)))
                   (let ((now (ticks)))                 ; look for a key 18 times a second
                     (declare (fixnum now))
                     (when (/= now last-tick)
                       (setf last-tick now)
                       (when (key-pressed-p)
                         (let ((key (get-key)))
                           (case key
                             (#\1 (play-sample mixer (first sounds)))
                             (#\2 (play-sample mixer (second sounds) :pan 44))
                             (#\3 (play-sample mixer (third sounds) :pan 12))
                             (#\4 (play-sample mixer (fourth sounds) :pan 54))
                             (#\5 (play-sample mixer (fifth sounds) :rate (string-rate 440.0)
                                                                     :pan 8))
                             (#\6 (play-sample mixer (fifth sounds) :rate (string-rate 554.4)))
                             (#\7 (play-sample mixer (fifth sounds) :rate (string-rate 659.3)
                                                                     :pan 56))
                             (#\8 (if organ-voice
                                      ;; (the voice may have been taken for another sound since)
                                      (let ((voice (svref (mx-voices mixer) organ-voice)))
                                        (when (eq (v-sound voice) (sixth sounds))
                                          (stop-voice mixer organ-voice))
                                        (setf organ-voice nil))
                                      (setf organ-voice
                                            (play-sample mixer (sixth sounds)
                                                         :rate (string-rate 329.6) :volume 28))))
                             (#\Space (funcall toggle) (setf tune-on (not tune-on)))
                             ((#\m #\M) (setf (mx-tables mixer) (not (mx-tables mixer))))
                             ((#\d #\D) (wait-tick 2))
                             (#\Escape (return))))))))
                 (setf seconds (/ (- (ticks) began) 18.2065)
                       allocated (- (get-bytes-consed) allocated)))
            (text-fill 0 0 80 25 7) (text-cursor 0 0) (show-text-cursor))))
      (let* ((blocks (max 1 (mx-blocks mixer)))
             (average (microseconds (floor (mx-total-clocks mixer) blocks)))
             (block-us (round (* 1000000 (mx-frames mixer)) (mx-card-rate mixer))))
        (format t "~&~:D interrupts in ~,1F s: ~,1F a second (the card's ~:D samples a second~%/ ~
                   ~D: ~,1F);~%~:D blocks mixed, ~D underrun~:P; the DMA address was seen at ~
                   ~:D places;~%~:D bytes allocated meanwhile.~%"
                (mx-handled mixer) seconds (/ (mx-handled mixer) (max seconds 0.1))
                (mx-card-rate mixer) (mx-frames mixer)
                (/ (mx-card-rate mixer) (float (mx-frames mixer)))
                (mx-blocks mixer) (mx-underruns mixer) places allocated)
        (format t "A block of ~D (~:D us of ~:[mono~;stereo~] sound) took ~:D us to mix on ~
                   average,~%~:D at most: ~,1F% of the processor.~%Eight voices all playing, ~
                   a block of 512: ~:D us with volume tables,~%~:D multiplying.~%"
                (mx-frames mixer) block-us (mx-stereo mixer) average
                (microseconds (mx-longest-clocks mixer)) (/ (* 100.0 average) block-us)
                table-us multiply-us)
        (format t "Self-test: ~:D bytes mixed four ways, ~:[~D differ from~;~*all equal to~] ~
                   the sums worked out~%exactly.~%"
                checked (zerop wrong) wrong))))
  (values))

(doslib-examples:define-example "mixer" 'main
  :category :sound
  :description "A software mixer: 8 voices with pitch, volume and pan, and what mixing them costs.")
