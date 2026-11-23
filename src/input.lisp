;;;; The BIOS timer tick and keyboard.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; These three are where a program waits, so they are where hot keys
;;; (see hotkey.lisp) get their turn.
(defun ticks ()
  "The BIOS's count of timer ticks: 18.2 a second, zero at midnight."
  (peek32 #x46C))
(defun wait-tick (&optional (n 1))
  "Wait for N ticks of the BIOS timer: 18.2 to the second."
  (dotimes (i n)
    (let ((tick (peek32 #x46C)))
      (loop while (= tick (peek32 #x46C)) do (poll-hotkeys)))))

;;; Time finer than the tick.  Counter 0 of the timer chip is what makes the
;;; ticks: it counts the chip's clock (+TIMER-HZ+, 1,193,182 a second) down
;;; from 65,536, and at the end interrupts -- that is a tick -- and starts
;;; again.  So the tick count and the counter together are a clock that
;;; goes up 1,193,182 times a second.  In the counter's mode 3, a square
;;; wave, which many a BIOS sets, it counts down twice a tick, by twos, with
;;; its output high the first time and low the second; a "read back" command
;;; (C2h to port 43h) gives the output and the mode in a status byte, and
;;; then the count.
(defvar *timer-count* 0 "The last time TIMER-COUNT gave.")
(defun timer-place ()
  "The BIOS's count of ticks, and how many of the timer chip's clocks (of
65,536) the timer has got through this one."
  (loop
    (let ((tick (ticks)))
      (outb #x43 #xC2)                  ; read back counter 0: its status, then its count
      (let* ((status (inb #x40)) (low (inb #x40))
             (gone (logand (- (+ low (* 256 (inb #x40)))) #xFFFF)))
        (when (= tick (ticks))
          (return (values tick
                          (if (= (logand status 6) 6)
                              ;; mode 3: twice a tick, by twos, the output (bit 7)
                              ;; high the first time
                              (+ (if (logbitp 7 status) 0 32768) (floor gone 2))
                              gone))))))))      ; mode 2: once a tick, by ones
(defun timer-count ()
  "The time in the timer chip's clocks, +TIMER-HZ+ (1,193,182) to the
second: the BIOS's count of ticks, 65,536 clocks each, and how far the
timer has got through this one.  For timing what is over in less than a
tick, and for pacing: see WAIT-FRAME."
  (loop
    (multiple-value-bind (tick place) (timer-place)
      (let ((now (+ (* tick 65536) place)))
        ;; The counter can start again a moment before the BIOS has counted the
        ;; tick, and then the time seems to go back by one tick: read again.
        (unless (< -65537 (- now *timer-count*) 0)
          (return (setf *timer-count* now)))))))

;;; TIMER-COUNT's numbers are too big for a machine word, so each one is
;;; made in the heap -- and a loop that waits by asking the time over and
;;; over made so many that the garbage collector ran about once a second,
;;; for a twentieth of a second: a dropped frame.  The wait keeps its time
;;; in 28 bits instead (the low 12 of the tick count, then the place in the
;;; tick), which wrap every 225 seconds and need no memory.
(defun frame-clock ()
  (multiple-value-bind (tick place) (timer-place)
    (logior (ash (logand tick #xFFF) 16) place)))
(defvar *last-frame* 0 "When WAIT-FRAME last returned, by FRAME-CLOCK.")
(defun wait-frame (&optional retrace)
  "Wait until a frame's time has passed since WAIT-FRAME last returned:
16,000 of TIMER-COUNT's clocks, a little less than a frame at 70 a second.
Then, if RETRACE, wait for the vertical retrace as well.  This paces an
animation where WAIT-RETRACE alone does not: some emulators (QEMU is one)
report a retrace whenever they are asked."
  ;; (a tick boundary can make the clock seem to step back for a moment:
  ;; that reads as a very long time, not a negative one, so it is asked again)
  (loop for passed = (logand (- (frame-clock) *last-frame*) #xFFFFFFF)
        while (or (< passed 16000) (> passed #xFFF0000)))
  (when retrace (wait-retrace))
  (setf *last-frame* (frame-clock))
  (values))

;;; The BIOS keeps the keys that have been typed in a ring in its data
;;; area, with the offsets of its head and its tail at 0040:001Ah and
;;; 0040:001Ch: equal when there is nothing in it.  Asking the BIOS itself
;;; means going to real mode and back -- 70 us under QEMU, against 0.1 us
;;; to look -- and this is asked in every loop that a key can stop.  So the
;;; BIOS is asked only when there is something there: it knows which keys
;;; the plain "read a key" call will hand over.
;;;
;;; Ctrl-Break puts a key of its own in the ring, a word of zero, as well as
;;; interrupting the program -- which happens at the next tick of the timer,
;;; up to an eighteenth of a second later.  A loop that ends at any key
;;; would be ended by that zero first, with the interrupt still to come and
;;; nothing left to catch it: so that key is thrown away here.
(defun key-pressed-p ()
  "Is a key waiting to be read?"
  (poll-hotkeys)
  (let ((head (peek16 #x41A)))
    (cond ((= head (peek16 #x41C)) nil)
          ((and (zerop (peek16 (+ #x400 head))) (interrupts-supported-p))
           (let ((next (+ head 2)))     ; the ring is from the offset at 0480h to that at 0482h
             (setf (sap-ref-16 (int-sap #x41A) 0)
                   (if (>= next (peek16 #x482)) (peek16 #x480) next)))
           nil)
          (t (zerop (logand (nth-value 5 (int86 #x16 :ax #x0100)) #x40))))))
(defun wait-key ()
  "Wait for a key.  Returns the BIOS's word for it: the scan code in the
high byte, the character in the low.  GET-KEY gives it a name."
  (loop
    (when (hotkeys-installed-p)         ; the BIOS would wait without polling
      (loop until (key-pressed-p)))
    (let ((key (int86 #x16 :ax 0)))
      ;; Ctrl-Break's own key (see KEY-PRESSED-P) is not one to hand over.
      ;; The interrupt it stands for cannot arrive while the BIOS is waiting,
      ;; only in Lisp: so wait here, in Lisp, for the tick that brings it.
      (unless (and (zerop (word16 key)) (interrupts-supported-p))
        (return key))
      (wait-tick 2))))
(defun finish ()
  "Wait for a key, then go back to 80x25 text: how a picture ends."
  (wait-key) (video-mode 3))
