;;;; FM synthesis on an AdLib or Sound Blaster card: the Yamaha OPL2 chip at
;;;; ports 388h (register number) and 389h (value).  Nine channels, each a
;;;; pair of sine oscillators ("operators"), one modulating the phase of the
;;;; other; an instrument is eleven register values.
;;;;
;;;;   (doslib-examples:run "adlib")             play "Frere Jacques" as a round
;;;;   (doslib-examples:run "adlib" "london")    another song; "?" lists them
;;;;   (doslib-examples:run "adlib" "scale" "bell")   a song on one instrument
;;;; Any key stops the music.  Without a card it says so and returns.
;;;;
;;;; Three things here are more Lisp than DOS:
;;;;  - music is written in a small notation, (c4 e4 (g4 2) r (repeat 2 ...)),
;;;;    which PARSE-SCORE turns into notes while the file is being compiled;
;;;;  - DEFINE-INSTRUMENT turns a description of an instrument in words into
;;;;    the eleven bytes the chip wants, also at compile time, and refuses a
;;;;    value that does not fit its register field;
;;;;  - DEFINE-SONG merges several voices into one list of timed register
;;;;    writes, so that playing is a loop that waits and writes.
;;;;
;;;; The notation and the clock are used by the speaker example as well
;;;; (speaker.lisp), which is why they are exported from this package: this
;;;; file is loaded first.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.adlib
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main
           ;; the notation and the clock, shared with speaker.lisp
           #:parse-score #:note-frequency #:+pit-hz+ #:beat-counts
           #:start-clock #:pit-now #:wait-until #:clock-step #:swallow-keys
           ;; the synthesizer
           #:define-instrument #:define-song #:detect #:reset #:silence
           #:write-register #:set-instrument #:frequency-word #:note-on #:note-off))
(in-package #:doslib-examples.adlib)

;;;; ------------------------------------------------------------------
;;;; The notation
;;;;
;;;; A score is a list of
;;;;   c4  f#3  bb5  eb4     a note: letter, # (or s) for sharp, b for flat, octave
;;;;                         (c4 is middle C, a4 is 440 Hz); one beat long
;;;;   r                     a rest
;;;;   (c4 2)  (r 1/2)       the same with a length in beats
;;;;   (len 1/2 ...)         the notes inside are 1/2 beat long unless they say otherwise
;;;;   (repeat 3 ...)        the notes inside, three times
;;;;   (transpose -12 ...)   the notes inside, so many semitones up (or down)
;;;; Symbols are compared by name, so a score can be written in any package.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant +pit-hz+ 1193182
  "The frequency of the PC's timer chip: the clock everything here is timed by.")

(defun parse-pitch (symbol)
  "The MIDI number of the note SYMBOL names (c4 = 60), or NIL for a rest."
  (let* ((name (string-upcase symbol))
         (degree (position (char name 0) "C-D-EF-G-A-B"))
         (digits (position-if #'digit-char-p name)))
    (cond ((string= name "R") nil)
          ((and degree digits (char/= (char name 0) #\-)
                (every #'digit-char-p (subseq name digits))
                (every (lambda (c) (find c "#SB")) (subseq name 1 digits)))
           (+ degree (* 12 (1+ (parse-integer name :start digits)))
              (loop for c across (subseq name 1 digits) sum (if (char= c #\B) -1 1))))
          (t (error "~S is not a note: write a letter, sharps or flats and an octave, ~
                     like C4, F#3 or BB5" symbol)))))

(defun pitch-name (pitch)
  (if pitch
      (format nil "~A~D" (aref #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B") (mod pitch 12))
              (1- (floor pitch 12)))
      "-"))

(defun note-frequency (pitch)
  "The frequency in Hz of MIDI note PITCH, equal temperament, A4 = 440."
  (* 440d0 (expt 2d0 (/ (- pitch 69) 12d0))))

(defun parse-score (score &key (transpose 0) (length 1))
  "The notes of SCORE in order, each a list (PITCH BEATS NAME): PITCH is a MIDI
number or NIL for a rest, BEATS a rational, NAME a string like \"C#4\"."
  (flet ((keyword-p (form word)
           (and (consp form) (symbolp (first form)) (string-equal (first form) word)))
         (note (symbol beats)
           (let ((pitch (parse-pitch symbol)))
             (when pitch (incf pitch transpose))
             (unless (and (rationalp beats) (plusp beats))
               (error "The length of ~S is ~S, which is not a positive number of beats" symbol beats))
             (list pitch beats (pitch-name pitch)))))
    (loop for form in score
          append (cond ((symbolp form) (list (note form length)))
                       ((keyword-p form "REPEAT")
                        (loop repeat (second form)
                              append (parse-score (cddr form) :transpose transpose :length length)))
                       ((keyword-p form "TRANSPOSE")
                        (parse-score (cddr form) :transpose (+ transpose (second form)) :length length))
                       ((keyword-p form "LEN")
                        (parse-score (cddr form) :transpose transpose :length (second form)))
                       ((and (consp form) (symbolp (first form)) (= (length form) 2))
                        (list (note (first form) (second form))))
                       (t (error "~S is not a note, a rest, or a REPEAT, TRANSPOSE or LEN form" form))))))

(defun beat-counts (beats tempo)
  "BEATS at TEMPO beats a minute, in periods of the timer chip's clock."
  (values (round (* beats 60 +pit-hz+) tempo)))

) ; eval-when

;;;; ------------------------------------------------------------------
;;;; The clock
;;;;
;;;; The BIOS counts timer interrupts at 0040:006C, 18.2 a second: too coarse
;;;; for music (a quaver at 120 beats a minute is 4.55 ticks).  But the timer
;;;; chip's channel 0, which makes those interrupts, can be read on the way:
;;;; it counts 65536 clock periods per tick.  Tick count and counter together
;;;; give the time in units of 1/1193182 s; what is achieved in practice is
;;;; how often the pair can be read (see CLOCK-STEP).
;;;;
;;;; Such times are big numbers, and on a 32-bit machine a number over 2^29
;;;; is a bignum, which takes memory: a loop reading the clock forty thousand
;;;; times a second would bring on a garbage collection every few seconds,
;;;; and each one is a hiccup in the music.  So the clock counts from the
;;;; moment START-CLOCK was last called, and the numbers stay small for the
;;;; first seven and a half minutes after that (and correct ever after).

(defvar *epoch* 0)                      ; the BIOS tick the clock counts from
(defvar *last-tick* 0)
(defvar *days* 0)                       ; the BIOS starts counting again at midnight
(defconstant +ticks-a-day+ #x1800B0)

(defun start-clock ()
  "Make PIT-NOW count from now."
  (setf *epoch* (peek32 #x46C) *last-tick* *epoch* *days* 0)
  (values))

(defun pit-now ()
  "The time since START-CLOCK in periods of the 1193182 Hz clock (65536 to a
BIOS tick)."
  (loop
    (let ((tick (peek32 #x46C)))
      ;; Read-back command: latch the status and the count of channel 0.
      (outb #x43 #xC2)
      (let* ((status (logand (inb #x40) #xFF))
             (count (logior (logand (inb #x40) #xFF) (ash (logand (inb #x40) #xFF) 8)))
             (down (if (zerop count) 0 (- 65536 count)))
             ;; In mode 3 (square wave: what the BIOS sets up) the counter goes
             ;; down by two and round twice per tick, and the output pin (status
             ;; bit 7) is high for the first round.  In mode 2 it goes round once.
             (elapsed (if (= (ldb (byte 2 1) status) 3)
                          (+ (ash down -1) (if (logbitp 7 status) 0 32768))
                          down)))
        ;; A tick in the middle of all that: the two halves do not belong
        ;; together, read again.
        (when (= tick (peek32 #x46C))
          (when (< tick *last-tick*) (incf *days*))
          (setf *last-tick* tick)
          (return (+ (* 65536 (- (+ tick (* *days* +ticks-a-day+)) *epoch*)) elapsed)))))))

(defun wait-until (time)
  "Wait until PIT-NOW reaches TIME.  True if it did, false if a key was pressed
first (the key is left in the keyboard buffer)."
  (let ((tick (peek32 #x46C)))
    (loop
      (when (>= (pit-now) time) (return t))
      ;; Asking the BIOS about the keyboard is slow: once a tick is enough.
      (let ((now (peek32 #x46C)))
        (when (/= now tick)
          (setf tick now)
          (when (key-pressed-p) (return nil)))))))

(defun clock-step ()
  "How fine the clock is in practice: the mean time between two readings of
it, in clock periods, measured over two BIOS ticks."
  (start-clock)
  (let* ((start (pit-now)) (end (+ start (* 2 65536))) (readings 0))
    (loop while (< (pit-now) end) do (incf readings))
    (values (round (* 2 65536) (max readings 1)))))

(defun swallow-keys ()
  (loop while (key-pressed-p) do (wait-key)))

;;;; ------------------------------------------------------------------
;;;; The chip

(defun write-register (register value)
  ;; The chip needs 3.3 microseconds after the register number and 23 after
  ;; the value before it can be spoken to again.  The classic way to wait is
  ;; to read its status port: 6 and 35 times, each read being at least a bus
  ;; cycle long on any machine.
  (outb #x388 register)
  (dotimes (i 6) (inb #x388))
  (outb #x389 value)
  (dotimes (i 35) (inb #x388)))

(defun status () (logand (inb #x388) #xE0))

(defun detect ()
  "True if there is an OPL2 (or something that behaves like one) at port 388h."
  ;; The standard test: the chip has two timers of its own, and a status
  ;; register that says when one has run out.  Reset them, look; start timer
  ;; 1 from FFh so that it overflows after 80 microseconds, look again.
  (write-register 4 #x60)               ; mask both timers
  (write-register 4 #x80)               ; clear the overflow flags
  (let ((before (status)))
    (write-register 2 #xFF)
    (write-register 4 #x21)             ; start timer 1
    (wait-tick 2)
    (let ((after (status)))
      (write-register 4 #x60)
      (write-register 4 #x80)
      (and (= before 0) (= after #xC0)))))

;;; The registers of the two operators of a channel are not next to each
;;; other: operator slots are numbered like this.
(defparameter *modulator* #(0 1 2 8 9 10 16 17 18))
(defun modulator (channel) (aref *modulator* channel))
(defun carrier (channel) (+ 3 (aref *modulator* channel)))

(defun silence ()
  "Stop every channel at once: full attenuation, fastest release, key off."
  (dotimes (channel 9)
    (dolist (slot (list (modulator channel) (carrier channel)))
      (write-register (+ #x40 slot) #x3F)
      (write-register (+ #x80 slot) #xFF))
    (write-register (+ #xB0 channel) 0)))

(defun reset ()
  (silence)
  (loop for register from 1 to #xF5
        unless (or (<= #x40 register #x55) (<= #x80 register #x95)) ; SILENCE set those
          do (write-register register 0))
  (write-register 1 #x20))              ; allow other waveforms than the sine

;;;; ------------------------------------------------------------------
;;;; Instruments
;;;;
;;;;   (define-instrument name (:feedback 0-7 :additive boolean)
;;;;     (:modulator key value ...)
;;;;     (:carrier key value ...))
;;;; The carrier is what is heard; the modulator bends the carrier's phase,
;;;; which is where the overtones come from (:additive t: both are heard, as
;;;; in an organ).  For each operator:
;;;;   :multiple 0-15   frequency as a multiple of the note's (0 means 1/2)
;;;;   :level 0-63      loudness, 63 loudest; for a modulator, how much it bends
;;;;   :attack :decay :release 0-15   rates, 15 fastest
;;;;   :sustain 0-15    the level the decay stops at, 15 loudest
;;;;   :hold t          stay at the sustain level while the key is down
;;;;                    (otherwise go on into the release: a struck sound)
;;;;   :wave 0-3        sine, half sine, rectified sine, quarter sine pulses
;;;;   :tremolo :vibrato :fast-high t   the chip's own wobbles; shorter high notes
;;;;   :scale 0-3       softer high notes
;;;; The result is eleven bytes: registers 20h 40h 60h 80h E0h of the
;;;; modulator, the same of the carrier, and register C0h of the channel.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun field (instrument name value limit)
  (unless (and (integerp value) (<= 0 value limit))
    (error "Instrument ~A: ~S is ~S, but must be between 0 and ~D" instrument name value limit))
  value)

(defun encode-operator (instrument which &key (multiple 1) (level 63) (attack 15) (decay 0)
                                           (sustain 15) (release 8) (wave 0) (scale 0)
                                           hold tremolo vibrato fast-high)
  (flet ((f (name value limit) (field instrument (list which name) value limit)))
    (list (logior (if tremolo #x80 0) (if vibrato #x40 0) (if hold #x20 0) (if fast-high #x10 0)
                  (f :multiple multiple 15))
          (logior (ash (f :scale scale 3) 6) (- 63 (f :level level 63)))
          (logior (ash (f :attack attack 15) 4) (f :decay decay 15))
          (logior (ash (- 15 (f :sustain sustain 15)) 4) (f :release release 15))
          (f :wave wave 3))))

(defun encode-instrument (name options operators)
  (destructuring-bind (&key (feedback 0) additive) options
    (flet ((operator (which)
             (apply #'encode-operator name which
                    (cdr (or (assoc which operators)
                             (error "Instrument ~A has no ~S" name which))))))
      (coerce (append (operator :modulator) (operator :carrier)
                      (list (logior (ash (field name :feedback feedback 7) 1) (if additive 1 0))))
              '(simple-array (unsigned-byte 8) (*))))))

) ; eval-when

(defvar *instruments* (make-hash-table :test 'equal) "Name (a string) -> eleven bytes.")

(defmacro define-instrument (name (&rest options) &body operators)
  "Define the instrument NAME; see the comment above for the notation."
  `(setf (gethash ,(string-downcase name) *instruments*)
         ,(encode-instrument name options operators)))

(defun instrument-names ()
  (sort (loop for name being the hash-keys of *instruments* collect name) #'string<))

(defun find-instrument (name)
  (or (gethash (string-downcase name) *instruments*)
      (error "There is no instrument named ~A; there are ~{~A~^, ~}" name (instrument-names))))

(define-instrument piano (:feedback 2)
  (:modulator :multiple 1 :level 36 :attack 15 :decay 5 :sustain 9 :release 4 :fast-high t)
  (:carrier :multiple 1 :level 63 :attack 15 :decay 3 :sustain 5 :release 5 :fast-high t))

(define-instrument organ (:feedback 3 :additive t)
  (:modulator :multiple 2 :level 48 :attack 13 :decay 0 :sustain 15 :release 9 :hold t)
  (:carrier :multiple 1 :level 60 :attack 13 :decay 0 :sustain 15 :release 9 :hold t :vibrato t))

(define-instrument bell (:feedback 1)
  (:modulator :multiple 7 :level 30 :attack 15 :decay 5 :sustain 3 :release 3)
  (:carrier :multiple 1 :level 60 :attack 15 :decay 3 :sustain 0 :release 3))

(define-instrument bass (:feedback 5)
  (:modulator :multiple 1 :level 42 :attack 15 :decay 3 :sustain 10 :release 6 :hold t)
  (:carrier :multiple 1 :level 63 :attack 14 :decay 2 :sustain 11 :release 7 :hold t))

(defun set-instrument (channel instrument)
  "Give CHANNEL (0-8) the sound INSTRUMENT: eleven bytes, or a name."
  (let ((bytes (if (typep instrument '(or string symbol)) (find-instrument instrument) instrument)))
    (loop for slot in (list (modulator channel) (carrier channel))
          for start in '(0 5)
          do (loop for register in '(#x20 #x40 #x60 #x80 #xE0)
                   for i from start
                   do (write-register (+ register slot) (aref bytes i))))
    (write-register (+ #xC0 channel) (aref bytes 10))))

;;;; ------------------------------------------------------------------
;;;; Notes
;;;;
;;;; The chip makes  f = fnumber * 49716 / 2^(20 - block)  Hz, with a 10-bit
;;;; fnumber and a 3-bit block (an octave).  The smallest block that fits
;;;; gives the finest steps.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun frequency-word (frequency)
  "FREQUENCY in Hz as the chip wants it: block in bits 10-12, fnumber in bits 0-9."
  (loop for octave from 0 to 7
        for fnumber = (round (* frequency (expt 2 (- 20 octave))) 49716)
        when (< fnumber 1024) return (logior (ash octave 10) fnumber)
        finally (error "~D Hz is too high for the OPL2" frequency))))

(defun note-on (channel word)
  "Start a note on CHANNEL; WORD comes from FREQUENCY-WORD."
  (write-register (+ #xA0 channel) (logand word #xFF))
  (write-register (+ #xB0 channel) (logior #x20 (ash word -8))))  ; 20h: key on

(defun note-off (channel word)
  ;; The frequency stays, so that the release sounds at the right pitch.
  (write-register (+ #xB0 channel) (ash word -8)))

;;;; ------------------------------------------------------------------
;;;; Songs
;;;;
;;;;   (define-song name (:tempo beats-a-minute)
;;;;     (instrument note ...)          one voice, on channel 0
;;;;     (instrument note ...))         another, on channel 1; up to nine
;;;; Each voice is a score as described at the top.  The song is compiled
;;;; into one list of events in order of time, (TIME CHANNEL WORD ON NAME):
;;;; TIME in clock periods from the start, ON true to start the note.

(defstruct song name tempo instruments events)
(defvar *songs* '() "The songs, in the order they were defined.")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compile-voices (voices tempo)
  (when (> (length voices) 9)
    (error "The OPL2 has nine channels; this song has ~D voices" (length voices)))
  (stable-sort
   (loop for (nil . score) in voices
         for channel from 0
         append (loop with beat = 0
                      for (pitch beats name) in (parse-score score)
                      when pitch
                        append (let ((word (frequency-word (note-frequency pitch))))
                                 ;; released after 7/8 of its length, so that the
                                 ;; same note twice is heard as two notes
                                 (list (list (beat-counts beat tempo) channel word t name)
                                       (list (beat-counts (+ beat (* 7/8 beats)) tempo)
                                             channel word nil name)))
                      do (incf beat beats)))
   #'< :key #'first)))

(defmacro define-song (name (&key (tempo 120)) &body voices)
  "Define the song NAME; see the comment above for the notation."
  (let ((name (string-downcase name)))
    `(progn
       (setf *songs* (remove ,name *songs* :key #'song-name :test #'string=))
       (setf *songs*
             (append *songs*
                     (list (make-song :name ,name :tempo ,tempo
                                      :instruments ',(mapcar (lambda (voice) (string-downcase (first voice)))
                                                             voices)
                                      :events ',(compile-voices voices tempo)))))
       ,name)))

;;; "Frere Jacques", traditional, as the round it is: the second voice comes
;;; in two bars late with the same tune an octave up, over a two-note bass.
(define-song jacques (:tempo 132)
  (piano (repeat 2 c4 d4 e4 c4)
         (repeat 2 e4 f4 (g4 2))
         (repeat 2 (len 1/2 g4 a4 g4 f4) e4 c4)
         (repeat 2 c4 g3 (c4 2))
         (repeat 2 c4 d4 e4 c4)
         (repeat 2 e4 f4 (g4 2)))
  (bell (r 8)
        (transpose 12
          (repeat 2 c4 d4 e4 c4)
          (repeat 2 e4 f4 (g4 2))
          (repeat 2 (len 1/2 g4 a4 g4 f4) e4 c4)
          (repeat 2 c4 g3 (c4 2))))
  (bass (repeat 12 (c3 2) (g2 2))))

;;; "London Bridge is Falling Down", traditional.
(define-song london (:tempo 126)
  (organ (repeat 2
           (g4 3/2) (a4 1/2) g4 f4  e4 f4 (g4 2)
           d4 e4 (f4 2)  e4 f4 (g4 2)
           (g4 3/2) (a4 1/2) g4 f4  e4 f4 (g4 2)
           (d4 2) (g4 2)  e4 (c4 3)))
  (bass (repeat 2
          (len 2 c3 c3 c3 c3 g2 g2 c3 c3
                 c3 c3 c3 c3 g2 g2 c3 c3))))

;;; One octave of C major, for checking the tuning with a frequency meter.
(define-song scale (:tempo 120)
  (piano c4 d4 e4 f4 g4 a4 b4 c5))

;;;; ------------------------------------------------------------------
;;;; Playing

(defun play (song &optional instrument)
  "Play SONG until it ends or a key is pressed.  With INSTRUMENT, every voice
uses that one."
  (reset)
  (loop for name in (song-instruments song)
        for channel from 0
        do (set-instrument channel (or instrument name)))
  (start-clock)
  (let ((start (+ (pit-now) 20000))     ; a moment for the first note to be on time
        (column 0))
    (loop for (time channel word on name) in (song-events song)
          do (unless (wait-until (+ start time)) (return))
             (cond ((not on) (note-off channel word))
                   (t (note-on channel word)
                      ;; the first voice's notes go on the screen as they sound
                      (when (zerop channel)
                        (when (> (incf column (1+ (length name))) 78)
                          (terpri)
                          (setf column (1+ (length name))))
                        (format t "~A " name)
                        (finish-output))))
          finally (wait-until (+ (pit-now) (* 9 65536))))))  ; half a second for the last release

(defun main (&optional (song "jacques") instrument)
  "FM synthesis on an AdLib or Sound Blaster card: play SONG (a name; \"?\"
lists them), on INSTRUMENT if one is named."
  (let ((found (find (string-downcase song) *songs* :key #'song-name :test #'string=)))
    (cond ((not found)
           (format t "~&Songs: ~{~A~^, ~}.  Instruments: ~{~A~^, ~}.~%"
                   (mapcar #'song-name *songs*) (instrument-names)))
          ((and instrument (not (gethash (string-downcase instrument) *instruments*)))
           (format t "~&There is no instrument named ~A.  Instruments: ~{~A~^, ~}.~%"
                   instrument (instrument-names)))
          ((not (detect))
           (format t "~&No AdLib card: nothing answers like an OPL2 at port 388h.~%"))
          (t
           (format t "~&AdLib card found.  Any key stops.~%Playing ~A (~{~A~^, ~}) at ~D beats a minute.~%"
                   (song-name found)
                   (if instrument (list instrument) (song-instruments found))
                   (song-tempo found))
           (unwind-protect (play found instrument)
             (silence)
             (swallow-keys))
           (terpri))))
  (values))

(doslib-examples:define-example "adlib" 'main
  :category :sound
  :description "FM synthesis on an AdLib or Sound Blaster card.")
