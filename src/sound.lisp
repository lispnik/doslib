;;;; The PC speaker.
;;;;
;;;; The speaker is driven by channel 2 of the timer chip: a square wave of
;;;; 1193182 Hz divided by a 16-bit number (ports 43h and 42h), let through
;;;; to the speaker by bits 0 and 1 of port 61h.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defconstant +timer-hz+ 1193182 "The frequency of the clock that drives the timer chip.")

(defun speaker-divisor (divisor)
  "Start a tone of 1193182/DIVISOR Hz (DIVISOR from 1 to 65535)."
  (outb #x43 #xB6)                      ; channel 2, low byte then high, square wave
  (outb #x42 (logand divisor #xFF))
  (outb #x42 (ash divisor -8))
  (outb #x61 (logior (inb #x61) 3))     ; gate the timer and connect the speaker
  divisor)
(defun speaker-on (frequency)
  "Start a tone of FREQUENCY Hz (19 to 20000 or so).  It sounds until SPEAKER-OFF."
  (speaker-divisor (max 1 (min 65535 (round +timer-hz+ frequency)))))
(defun speaker-off ()
  "Silence the speaker."
  (outb #x61 (logand (inb #x61) #xFC))
  nil)
(defun speaker-beep (&optional (frequency 880) (ticks 4))
  "Sound FREQUENCY Hz for TICKS timer ticks (18.2 to the second)."
  (unwind-protect (progn (speaker-on frequency) (wait-tick ticks))
    (speaker-off)))
