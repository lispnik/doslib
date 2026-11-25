;;;; Music on the PC speaker.  The speaker is driven by channel 2 of the timer
;;;; chip, a square wave of 1193182 Hz divided by a 16-bit number (ports 43h
;;;; and 42h), let through by bits 0 and 1 of port 61h.  One voice, one
;;;; loudness, and every PC since 1981 has it.
;;;;
;;;;   (doslib-examples:run "speaker")            play the "Ode to Joy" theme
;;;;   (doslib-examples:run "speaker" "scale")    another tune; "?" lists them
;;;;   (doslib-examples.speaker:beep 880 9)       880 Hz for 9 ticks (half a second)
;;;; The names of the notes appear as they sound.  Any key stops the music.
;;;;
;;;; A tune is written with DEFINE-TUNE in the notation of adlib.lisp (see
;;;; "The notation" there):
;;;;   (define-tune name (:tempo 120) c4 e4 (g4 2) r (repeat 2 ...) (transpose 12 ...))
;;;; and the macro does all the arithmetic while this file is compiled: what
;;;; is left in the program is a vector of (DIVISOR . LENGTH) pairs, DIVISOR
;;;; for the timer chip (0: a rest) and LENGTH in periods of the same
;;;; 1193182 Hz clock (65536 of them are one BIOS tick).
;;;;
;;;; Timing: notes start at times computed from the beginning of the tune, so
;;;; errors do not add up; the clock is the BIOS tick count refined by the
;;;; timer chip's own counter (PIT-NOW in adlib.lisp), read in a loop.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.speaker
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:shadow #:beep)                      ; SB-SYS has one of its own
  (:import-from #:doslib-examples.adlib
                #:parse-score #:note-frequency #:+pit-hz+ #:beat-counts
                #:start-clock #:pit-now #:wait-until #:clock-step #:swallow-keys)
  (:export #:main #:beep #:define-tune #:sound #:quiet))
(in-package #:doslib-examples.speaker)

;;;; The speaker

(defun sound (divisor)
  "Start a tone of 1193182/DIVISOR Hz."
  (speaker-divisor divisor))

(defun quiet ()
  (speaker-off))

(defun beep (&optional (frequency 880) (ticks 4))
  "Sound FREQUENCY Hz (19 to 20000 or so) for TICKS BIOS ticks of 1/18.2 s
each; fractions of a tick are allowed.  A key cuts it short."
  (unwind-protect
       (progn (start-clock)
              (sound (max 1 (min 65535 (round +pit-hz+ frequency))))
              (wait-until (+ (pit-now) (round (* ticks 65536)))))
    (quiet))
  (values))

;;;; Tunes

(defstruct tune name tempo notes names)
(defvar *tunes* '() "The tunes, in the order they were defined.")

(defmacro define-tune (name (&key (tempo 120)) &body score)
  "Define the tune NAME: SCORE in the notation of adlib.lisp, at TEMPO beats a minute."
  (let* ((name (string-downcase name))
         (notes (parse-score score))
         ;; Lengths are differences of rounded start times, so that rounding
         ;; errors do not add up over a long tune.
         (times (let ((beat 0))
                  (cons 0 (loop for (nil beats) in notes
                                collect (beat-counts (incf beat beats) tempo)))))
         (pairs (loop for (pitch) in notes
                      for (start end) on times
                      collect (cons (if pitch (round +pit-hz+ (note-frequency pitch)) 0)
                                    (- end start)))))
    (dolist (pair pairs)
      (unless (<= 0 (car pair) 65535)
        (error "Tune ~A has a note too low for the timer chip (below 19 Hz)" name)))
    `(progn
       (setf *tunes* (remove ,name *tunes* :key #'tune-name :test #'string=))
       (setf *tunes* (append *tunes*
                             (list (make-tune :name ,name :tempo ,tempo
                                              :notes ',(coerce pairs 'vector)
                                              :names ',(map 'vector #'third notes)))))
       ,name)))

;;; One octave of C major and back, then the same run a fifth up and twice as fast.
(define-tune scale (:tempo 120)
  c4 d4 e4 f4 g4 a4 b4 c5
  b4 a4 g4 f4 e4 d4 (c4 2)
  (transpose 7 (len 1/2 c4 d4 e4 f4 g4 a4 b4 c5 b4 a4 g4 f4 e4 d4 (c4 1))))

;;; The theme of the last movement of Beethoven's ninth symphony (1824).
(define-tune ode (:tempo 132)
  (repeat 2
    e4 e4 f4 g4  g4 f4 e4 d4  c4 c4 d4 e4  (e4 3/2) (d4 1/2) (d4 2)
    e4 e4 f4 g4  g4 f4 e4 d4  c4 c4 d4 e4  (d4 3/2) (c4 1/2) (c4 2))
  d4 d4 e4 c4  d4 (len 1/2 e4 f4) e4 c4  d4 (len 1/2 e4 f4) e4 d4  c4 d4 (g3 2)
  e4 e4 f4 g4  g4 f4 e4 d4  c4 c4 d4 e4  (d4 3/2) (c4 1/2) (c4 2))

;;; "Ah! vous dirai-je, maman", French, 18th century: "Twinkle, twinkle, little star".
(define-tune twinkle (:tempo 120)
  c4 c4 g4 g4 a4 a4 (g4 2)  f4 f4 e4 e4 d4 d4 (c4 2)
  (repeat 2 g4 g4 f4 f4 e4 e4 (d4 2))
  c4 c4 g4 g4 a4 a4 (g4 2)  f4 f4 e4 e4 d4 d4 (c4 2))

;;;; Playing

(defun play (tune)
  "Play TUNE until it ends or a key is pressed, showing the notes."
  (start-clock)
  (let ((time (+ (pit-now) 20000))      ; a moment for the first note to be on time
        (column 0))
    (loop for (divisor . counts) across (tune-notes tune)
          for name across (tune-names tune)
          do (unless (wait-until time) (return))
             (when (> (incf column (1+ (length name))) 78)
               (terpri)
               (setf column (1+ (length name))))
             (format t "~A " name)
             (finish-output)
             (incf time counts)
             (when (plusp divisor)
               (sound divisor)
               ;; quiet for the last eighth, so that the same note twice is two notes
               (unless (wait-until (- time (floor counts 8))) (return))
               (quiet))
          finally (wait-until time))))

(defun main (&optional (tune "ode"))
  "Music on the PC speaker: play TUNE (a name; \"?\" lists them)."
  (let ((found (find (string-downcase tune) *tunes* :key #'tune-name :test #'string=)))
    (cond ((not found)
           (format t "~&Tunes: ~{~A~^, ~}.~%" (mapcar #'tune-name *tunes*)))
          (t
           (format t "~&Playing ~A: ~D notes at ~D beats a minute.  Any key stops.~%~
                      (The clock can be read every ~D microseconds.)~%"
                   (tune-name found) (length (tune-notes found)) (tune-tempo found)
                   (round (* 1000000 (clock-step)) +pit-hz+))
           ;; whatever happens, do not leave the speaker whistling
           (unwind-protect (play found)
             (quiet)
             (swallow-keys))
           (terpri))))
  (values))

(doslib-examples:define-example "speaker" 'main
  :category :sound
  :description "Music on the PC speaker.")
