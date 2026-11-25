;;;; A tune in four voices on an AdLib card -- melody, bass, a drum and a
;;;; cymbal, all of them made by the one FM chip -- kept in time by the
;;;; timer's interrupt.
;;;;
;;;;   (doslib-examples:run "tune")         the "Ode to Joy"
;;;;   (doslib-examples:run "tune" 12)      faster: 12 interrupts to a semiquaver
;;;; From DOS: DEMO TUNE, DEMO TUNE 12.  The tune goes round until Esc (or Q)
;;;; is pressed.  Without a card it says so and returns.
;;;;
;;;; The "adlib" example makes sounds one song at a time with a notation
;;;; that is compiled into a list of register writes.  This one is the other
;;;; half of a game's music: a sequencer, which keeps several voices going
;;;; against a steady clock while the program does something else (here: it
;;;; draws the screen).
;;;;
;;;; THE CHIP.  The Yamaha YM3812 ("OPL2") has two ports: write a register's
;;;; number to 388h, wait a moment, write its value to 389h.  Nothing can be
;;;; read back but a status byte at 388h.  It has nine channels, and each
;;;; channel two "operators": sine oscillators with an envelope.  The first,
;;;; the modulator, is not heard itself: its output bends the phase of the
;;;; second, the carrier, and that is what makes the overtones (frequency
;;;; modulation).  An instrument -- a "patch" -- is the settings of the two
;;;; operators, five registers each, and one for the channel:
;;;;
;;;;   20h + operator   bit 7 tremolo, bit 6 vibrato, bit 5 hold the sustain
;;;;                    level while the key is down (clear: die away at once,
;;;;                    like a struck or plucked thing), bit 4 shorter
;;;;                    envelopes for high notes, bits 0-3 the frequency as a
;;;;                    multiple of the note's (0 means a half)
;;;;   40h + operator   bits 6-7 softer high notes, bits 0-5 attenuation in
;;;;                    steps of 0.75 dB: 0 is loudest, 3Fh nearly silent.
;;;;                    For the carrier this is the volume; for the modulator
;;;;                    it is how bright the sound is.
;;;;   60h + operator   attack rate (bits 4-7) and decay rate (0-3); F is fastest
;;;;   80h + operator   sustain level (bits 4-7: 0 loudest, each step 3 dB down)
;;;;                    and release rate (bits 0-3)
;;;;   E0h + operator   the wave: 0 sine, 1 half a sine, 2 rectified, 3 quarter pulses
;;;;   C0h + channel    bits 1-3 feedback (the modulator bending its own phase:
;;;;                    7 turns it into noise), bit 0 set: no modulation, both
;;;;                    operators heard
;;;;
;;;; "+ operator" is not the channel's number: the operators of channels 0 to
;;;; 8 are at 0 1 2 8 9 10 16 17 18 (modulator) and three more (carrier).
;;;; A note is two more registers: A0h + channel has the low eight bits of
;;;; the "F-number", B0h + channel its top two, the octave ("block", bits
;;;; 2-4) and the key (bit 5: set starts the attack, clear starts the
;;;; release).  The pitch is  F-number * 49716 / 2^(20 - block)  Hz.
;;;;
;;;; The chip also has a rhythm mode (register BDh) which gives up three
;;;; channels for five fixed drum sounds.  It is not used here: the drum is
;;;; an ordinary patch that dies away quickly, played low, and the cymbal one
;;;; with full feedback and operators at awkward multiples, which is noise.
;;;;
;;;; THE CLOCK.  Music needs a finer clock than the BIOS's 18.2 ticks a
;;;; second, and one that goes on counting while Lisp is busy.  Two were on
;;;; offer: reading the timer chip's counter in a loop (TIMER-COUNT; "adlib"
;;;; does that) or having the timer interrupt more often and counting the
;;;; interrupts.  This uses the interrupt, because that is what a program
;;;; with other things to do must use -- the count is kept by a handler in
;;;; machine code and goes up whether or not Lisp is looking, and looking is
;;;; one read of memory, where the counter costs four port accesses and gives
;;;; numbers too big to stay fixnums for long.  The library builds the
;;;; handler (WITH-IRQ-HANDLER, src/irq.lisp).  The timer's divisor is
;;;; changed from 65536 to 8192: 145.65 interrupts a second instead of 18.2.
;;;; With :CHAIN 8192 the handler adds 8192 to a 16-bit sum at every
;;;; interrupt and passes the interrupt on to the handler that was there
;;;; before only when the sum overflows, which is every eighth time: so the
;;;; BIOS's clock (and DOS's time of day) is told exactly as often as it
;;;; always was.
;;;;
;;;; A semiquaver is 16 interrupts unless the argument says otherwise (136
;;;; crotchets a minute).  Every note has the number of the interrupt it
;;;; starts at, counted from the beginning of the tune; Lisp looks at the
;;;; count, starts and stops what is due, and draws.  If Lisp is late -- a
;;;; garbage collection -- the notes that were due come late, but the ones
;;;; after are on time again: the lateness does not add up.
;;;;
;;;; THE TUNE is the theme of the last movement of Beethoven's ninth
;;;; symphony (1824; long out of copyright), sixteen bars in C; the bass, the
;;;; drum and the cymbal under it are made up here.  The voices are written
;;;; below as lists: a note is a name like E4 or F#3 (C4 is middle C), R is a
;;;; rest, a number sets the length in semiquavers of what follows, and
;;;; (REP n ...) repeats.
;;;;
;;;; On the screen: for each voice the note sounding, the two bytes last
;;;; written for it, and where the note is on a keyboard of six octaves; the
;;;; bar; and the count of interrupts beside the BIOS's ticks and the
;;;; battery-backed clock chip's seconds, which is how the clock can be
;;;; checked without ears.  When it ends it prints those numbers.  Whether
;;;; the patches sound like what they are called it cannot check, and under
;;;; an emulator the interrupts may come in bursts when the host is busy.
;;;;
;;;; On the way out, however that happens: every channel's key is released
;;;; and its operators silenced, the timer goes back to 65536, and the
;;;; interrupt vector to what it was.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.tune
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main #:parse-voice #:note-word))
(in-package #:doslib-examples.tune)

;;; ---- the chip
(defvar *writes* 0 "Registers written since the tune began.")

(defun opl-write (register value)
  "Give a register of the chip a value.  It needs 3.3 microseconds after
the number and 23 after the value; reading its status port, which every
machine takes about a microsecond over, is the usual way to wait."
  (outb #x388 register)
  (dotimes (i 6) (inb #x388))
  (outb #x389 value)
  (dotimes (i 35) (inb #x388))
  (incf *writes*))

(defun opl-present-p ()
  "Is there an OPL2 at 388h?  The chip has two timers of its own, and its
status says when one has run out: stop them and clear the flags, and the
top three bits of the status are 0; start timer 1 from FFh and, 80
microseconds later, they are C0h.  Nothing else at that port does that."
  (opl-write 4 #x60)                    ; stop both timers
  (opl-write 4 #x80)                    ; clear their flags
  (let ((before (logand (inb #x388) #xE0)))
    (opl-write 2 #xFF)                  ; timer 1 counts from FFh
    (opl-write 4 #x21)                  ; and starts
    (wait-tick 2)
    (let ((after (logand (inb #x388) #xE0)))
      (opl-write 4 #x60)
      (opl-write 4 #x80)
      (and (= before 0) (= after #xC0)))))

(defun modulator (channel) (svref #(0 1 2 8 9 10 16 17 18) channel))
(defun carrier (channel) (+ 3 (modulator channel)))

(defun opl-silence ()
  "Every channel: key released, both operators at full attenuation and the
fastest release."
  (dotimes (channel 9)
    (opl-write (+ #xB0 channel) 0)
    (dolist (operator (list (modulator channel) (carrier channel)))
      (opl-write (+ #x40 operator) #x3F)
      (opl-write (+ #x80 operator) #xFF))))

;;; ---- the patches: modulator's 20h 40h 60h 80h E0h, carrier's, and C0h
(defparameter *patches*
  '(;; A reed: both operators at the note's own frequency and held while the
    ;; key is down; the modulator fairly loud (attenuation 18h) with some
    ;; feedback, for a bright buzz.  Vibrato on the carrier.
    (reed  #x21 #x18 #xF3 #x75 #x00   #x61 #x04 #xF2 #x46 #x00   #x06)
    ;; A bass: the same shape, a duller modulator that falls back further
    ;; (sustain 5), more feedback, the carrier at full volume.
    (bass  #x21 #x1C #xF4 #x53 #x00   #x21 #x00 #xF3 #x55 #x00   #x0A)
    ;; A drum: not held (bit 5 of 20h clear), so the envelope runs straight
    ;; through attack, decay and release; both operators at half the note's
    ;; frequency, the fastest attack and a quick decay: a thump.
    (drum  #x00 #x0B #xF8 #xF6 #x00   #x00 #x00 #xF6 #xF7 #x00   #x08)
    ;; A cymbal: full feedback and no attenuation on a modulator at 12 times
    ;; the note makes noise of it; the carrier at 15 times, soft, gone at once.
    (cymbal #x0C #x00 #xF8 #xF7 #x00  #x0F #x10 #xF7 #xF8 #x00   #x0E))
  "Each patch: a name and eleven register values.")

(defun set-patch (channel name)
  "Give CHANNEL (0 to 8) the patch NAME."
  (let ((bytes (rest (or (assoc name *patches*) (error "There is no patch ~A." name)))))
    (loop for operator in (list (modulator channel) (carrier channel))
          for five on bytes by (lambda (list) (nthcdr 5 list))
          do (loop for register in '(#x20 #x40 #x60 #x80 #xE0)
                   for value in five
                   do (opl-write (+ register operator) value)))
    (opl-write (+ #xC0 channel) (nth 10 bytes))))

;;; ---- notes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun note-pitch (name)
    "The number of the note NAME (a symbol like E4, F#3 or BB2) counting
semitones with middle C, C4, as 60; NIL for the rest, R."
    (let* ((text (string-upcase name))
           (degree (position (char text 0) "C D EF G A B"))
           (octave (digit-char-p (char text (1- (length text))))))
      (cond ((string= text "R") nil)
            ((and degree octave (char/= (char text 0) #\Space)
                  (every (lambda (c) (find c "#B")) (subseq text 1 (1- (length text)))))
             (+ degree (* 12 (1+ octave))
                (loop for c across (subseq text 1 (1- (length text)))
                      sum (if (char= c #\#) 1 -1))))
            (t (error "~S is not a note: write a letter, # or B for sharp or flat, and an ~
                       octave, like E4, F#3 or BB2." name)))))

  (defun parse-voice (notation)
    "The notes of NOTATION as a list of (START LENGTH PITCH NAME), START and
LENGTH in semiquavers, rests left out; and, second, the length of it all."
    (let ((length 4) (at 0) (notes '()))
      (labels ((walk (items)
                 (dolist (item items)
                   (cond ((integerp item) (setf length item))
                         ((and (consp item) (symbolp (first item))
                               (string-equal (first item) "REP"))
                          (dotimes (i (second item)) (walk (cddr item))))
                         ((symbolp item)
                          (let ((pitch (note-pitch item)))
                            (when pitch
                              (push (list at length pitch (string-upcase item)) notes))
                            (incf at length)))
                         (t (error "~S is not a note, a length or a (REP n ...)." item))))))
        (walk notation))
      (values (nreverse notes) at))))

(defun note-word (pitch)
  "What registers A0h and B0h want for the note PITCH, as one number: the
F-number in bits 0-9, the block in bits 10-12.  The smallest block that
the F-number fits in gives the finest tuning."
  (let ((hertz (* 440d0 (expt 2d0 (/ (- pitch 69) 12d0)))))
    (loop for block from 0 to 7
          for f-number = (round (* hertz (expt 2 (- 20 block))) 49716)
          when (< f-number 1024) return (logior (ash block 10) f-number)
          finally (error "Note ~D is too high for the chip." pitch))))

;;; ---- the tune
(defmacro define-voice (name patch &body notation)
  "A voice of the tune: (NAME PATCH LENGTH . NOTES), worked out when this file is compiled."
  (multiple-value-bind (notes length) (parse-voice notation)
    `(defparameter ,name '(,(string-downcase (string-trim "*" name)) ,patch ,length ,@notes))))

(define-voice *melody* reed
  4 e4 e4 f4 g4   g4 f4 e4 d4   c4 c4 d4 e4   6 e4 2 d4 8 d4
  4 e4 e4 f4 g4   g4 f4 e4 d4   c4 c4 d4 e4   6 d4 2 c4 8 c4
  4 d4 d4 e4 c4   d4 2 e4 f4 4 e4 c4   d4 2 e4 f4 4 e4 d4   c4 d4 8 g3
  4 e4 e4 f4 g4   g4 f4 e4 d4   c4 c4 d4 e4   6 d4 2 c4 8 c4)

(define-voice *bass* bass
  4 c3 g2 c3 g2   g2 d3 g2 b2   c3 g2 c3 e3   g2 d3 8 g2
  4 c3 g2 c3 g2   g2 d3 g2 b2   c3 g2 c3 e3   g2 b2 8 c3
  4 g2 d3 c3 g2   g2 d3 c3 g2   g2 d3 e3 b2   a2 d3 8 g2
  4 c3 g2 c3 g2   g2 d3 g2 b2   c3 g2 c3 e3   g2 b2 8 c3)

(define-voice *drum* drum               ; low on the beat, higher off it; a fill to end with
  (rep 15 4 c2 g2 c2 g2)
  2 c2 c2 g2 g2 c2 g2 4 c2)

(define-voice *cymbal* cymbal           ; quavers, with a rest on each bar's first beat
  (rep 16 4 r 2 c5 c5 c5 c5 c5 c5))

(defparameter *voices* (list *melody* *bass* *drum* *cymbal*)
  "The voices; the first is on the chip's channel 0, the next on 1, and so on.")

(defun tune-length ()
  "The tune's length in semiquavers.  The voices must agree about it."
  (let ((lengths (mapcar #'third *voices*)))
    (unless (apply #'= lengths)
      (error "The voices are not the same length: ~{~D~^, ~} semiquavers." lengths))
    (first lengths)))

;;; ---- the timer
(defconstant +divisor+ 8192
  "The timer interrupts every so many of its clocks: 145.65 times a second, 8 to a BIOS tick.")

(defun set-timer-divisor (divisor)
  "Give channel 0 of the timer chip a new divisor (0 means 65536, what the BIOS sets)."
  (outb #x43 #x36)                      ; channel 0, low byte then high, square wave
  (outb #x40 (lo divisor))
  (outb #x40 (hi divisor)))

;;; ---- the screen
(defconstant +low-pitch+ 24 "The note in the first column of the keyboards: C1.")
(defconstant +keys+ 72 "Six octaves.")

(defun line (x y attribute control &rest arguments)
  (text-put x y (apply #'format nil control arguments) attribute))

(defun voice-row (index) (+ 5 (* 3 index)))

(defun draw-keyboard (index &optional pitch sounding)
  "Voice INDEX's keyboard: a column for every semitone, each C marked, and the note PITCH."
  (let ((keys (make-string +keys+)))
    (dotimes (i +keys+)
      (setf (char keys i) (code-char (if (zerop (mod i 12)) 179 250))))
    (text-put 4 (1+ (voice-row index)) keys 8)
    (when (and pitch (< -1 (- pitch +low-pitch+) +keys+))
      (text-put (+ 4 (- pitch +low-pitch+)) (1+ (voice-row index))
                (string (code-char 219)) (if sounding 14 6)))))

(defun draw-frame (step-ticks)
  (text-fill 0 0 80 25 7)
  (text-fill 0 0 80 1 #x1F)
  (text-put 1 0 "AdLib tune" #x1E)
  (text-put 13 0 "Ode to Joy (Beethoven, 1824): four voices on the OPL2 at 388h" #x1F)
  (text-put 4 3 "Voice    Patch    Channel  Note   Written to the chip          Notes begun" 3)
  (loop for (name patch) in *voices*
        for index from 0
        do (line 4 (voice-row index) 15 "~8A ~(~8A~)    ~D" name patch index)
           (draw-keyboard index))
  (loop for octave from 1 to 6
        do (line (+ 4 (* 12 (1- octave))) (voice-row (length *voices*)) 8 "C~D" octave))
  (line 4 20 7 "Timer divisor ~D: ~,2F interrupts a second; a semiquaver is ~D (~D crotchets ~
                a minute)"
        +divisor+ (/ +timer-hz+ (float +divisor+)) step-ticks
        (round (* 60 +timer-hz+) (* 4 step-ticks +divisor+)))
  (text-put 4 24 "Esc: leave" #x70))

(defun draw-voice (index name word key-on begun)
  "Voice INDEX's line: the note, the two bytes it was started or stopped with, the count."
  (line 31 (voice-row index) (if key-on 14 7) "~4A   A~Xh=~2,'0Xh B~Xh=~2,'0Xh ~A      ~5D"
        name index (logand word #xFF)
        index (logior (if key-on #x20 0) (ash word -8))
        (if key-on "key on " "key off") begun))

;;; ---- playing
(defun play (hook step-ticks)
  "Play the tune round and round until Esc.  Returns the interrupts counted,
the BIOS's ticks meanwhile, the clock chip's seconds between its first and last
change and the interrupts between those, the notes begun and the times through."
  (let* ((count (length *voices*))
         (steps (tune-length))
         (notes (map 'vector #'cdddr *voices*))         ; what each voice has yet to play
         (word (make-array count :initial-element 0))   ; the note each last played
         (off (make-array count :initial-element nil))  ; when to release it, or NIL
         (begun (make-array count :initial-element 0))
         (origin 0) (times 0) (last -1)
         (tick (ticks)) (first-tick tick)
         (second (cmos 0)) (seconds -1) (count0 0) (count1 0))
    (setf (irq-count hook) 0)
    (loop
      (let* ((now (irq-count hook)) (time (- now origin)))
        (when (/= now last)
          (setf last now)
          ;; the clock chip's seconds, for a check on our clock by another
          (let ((s (cmos 0)))
            (unless (= s second)
              (setf second s)
              (if (minusp seconds) (setf count0 now) (setf count1 now))
              (incf seconds)))
          (when (>= time (* steps step-ticks))          ; the end: go round again
            (incf origin (* steps step-ticks))
            (setf time (- now origin))
            (incf times)
            (dotimes (v count)
              (setf (svref notes v) (cdddr (nth v *voices*)))
              (when (svref off v) (decf (svref off v) (* steps step-ticks)))))
          (dotimes (v count)
            ;; release the note that has had its time, then start what is due
            (when (and (svref off v) (>= time (svref off v)))
              (opl-write (+ #xB0 v) (ash (svref word v) -8))
              (setf (svref off v) nil)
              (draw-voice v "" (svref word v) nil (svref begun v))
              (draw-keyboard v))
            (loop while (and (svref notes v)
                             (>= time (* step-ticks (first (first (svref notes v))))))
                  do (destructuring-bind (start length pitch name) (pop (svref notes v))
                       (let ((w (note-word pitch)))
                         (when (svref off v)            ; still sounding: release it first
                           (opl-write (+ #xB0 v) (ash (svref word v) -8)))
                         (opl-write (+ #xA0 v) (logand w #xFF))
                         (opl-write (+ #xB0 v) (logior #x20 (ash w -8)))
                         ;; released after 7/8 of its length, so that the same
                         ;; note twice is heard as two notes
                         (setf (svref word v) w
                               (svref off v) (+ (* step-ticks start)
                                                (floor (* 7 step-ticks length) 8)))
                         (incf (svref begun v))
                         (draw-voice v name w t (svref begun v))
                         (draw-keyboard v pitch t)))))))
      ;; eighteen times a second: the numbers, and the keyboard
      (let ((now (ticks)))
        (when (/= now tick)
          (setf tick now)
          (let* ((interrupts (irq-count hook))
                 (step (floor (- interrupts origin) step-ticks)))
            (line 4 18 15 "Bar ~2D of ~D, beat ~D     time through: ~D     "
                  (1+ (floor step 16)) (floor steps 16) (1+ (mod (floor step 4) 4)) (1+ times))
            (line 4 21 7 "Interrupts ~7D   BIOS ticks ~6D: ~,2F to a tick   Registers written ~6D"
                  interrupts (- now first-tick)
                  (/ interrupts (float (max 1 (- now first-tick)))) *writes*)
            (when (> seconds 0)
              (line 4 22 7 "By the clock chip: ~D interrupts in ~D second~:P, ~,1F a second   "
                    (- count1 count0) seconds (/ (- count1 count0) (float seconds)))))
          (when (key-pressed-p)
            (when (member (get-key) '(#\Escape #\q #\Q))
              (return))))))
    (values (irq-count hook) (- (ticks) first-tick) (max seconds 0) (- count1 count0)
            (reduce #'+ begun) times)))

(defun main (&optional (step-ticks 16))
  "A tune in four voices on the AdLib card, in time with the timer's
interrupt.  STEP-TICKS is how many interrupts a semiquaver lasts."
  (let ((step-ticks (if (integerp step-ticks) (max 4 (min 64 step-ticks)) 16)))
    (tune-length)                       ; complains if the voices disagree
    (unless (opl-present-p)
      (format t "~&No AdLib card: nothing answers like an OPL2 at port 388h.  ~
                 (QEMU: -device adlib.)~%")
      (return-from main (values)))
    (setf *writes* 0)
    (multiple-value-bind (interrupts bios seconds in-seconds begun times)
        (unwind-protect
             (progn
               (loop for register from 1 to #xF5 do (opl-write register 0))
               (opl-write 1 #x20)       ; let the E0h registers choose the wave
               (loop for (nil patch) in *voices*
                     for channel from 0
                     do (set-patch channel patch))
               (hide-text-cursor)
               (draw-frame step-ticks)
               (with-irq-handler (hook 0 :chain +divisor+)
                 (unwind-protect
                      (progn (set-timer-divisor +divisor+)
                             (play hook step-ticks))
                   (set-timer-divisor 0))))
          (opl-silence)
          (loop while (key-pressed-p) do (wait-key))
          (text-fill 0 0 80 25 7) (text-cursor 0 0) (show-text-cursor))
      (format t "~&Ode to Joy: ~D notes begun, ~D time~:P through, ~D registers written.~%~
                 ~D timer interrupts and ~D BIOS ticks: ~,2F to a tick (8 expected).~%"
              begun times *writes* interrupts bios (/ interrupts (float (max 1 bios))))
      (when (plusp seconds)
        (format t "Clock chip: ~D interrupts in ~D second~:P, ~,1F a second ~
                   (~,1F expected).~%"
                in-seconds seconds (/ in-seconds (float seconds))
                (/ +timer-hz+ (float +divisor+))))
      (format t "Every voice is silent and the timer is back at 18.2 interrupts a second.~%")))
  (values))

(doslib-examples:define-example "tune" 'main
  :category :sound
  :description "An AdLib tune in four voices, sequenced from the timer interrupt.")
