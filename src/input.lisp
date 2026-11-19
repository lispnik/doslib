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
(defun timer-count ()
  "The time in the timer chip's clocks, +TIMER-HZ+ (1,193,182) to the
second: the BIOS's count of ticks, 65,536 clocks each, and how far the
timer has got through this one.  For timing what is over in less than a
tick, and for pacing: see WAIT-FRAME."
  (loop
    (let ((tick (ticks)))
      (outb #x43 #xC2)                  ; read back counter 0: its status, then its count
      (let* ((status (inb #x40)) (low (inb #x40))
             (gone (logand (- (+ low (* 256 (inb #x40)))) #xFFFF))
             (now (+ (* tick 65536)
                     (if (= (logand status 6) 6)
                         ;; mode 3: twice a tick, by twos, the output (bit 7)
                         ;; high the first time
                         (+ (if (logbitp 7 status) 0 32768) (floor gone 2))
                         gone))))       ; mode 2: once a tick, by ones
        ;; The counter can start again a moment before the BIOS has counted the
        ;; tick, and then the time seems to go back by one tick: read again.
        (unless (or (/= tick (ticks)) (< -65537 (- now *timer-count*) 0))
          (return (setf *timer-count* now)))))))
(defvar *last-frame* 0 "When WAIT-FRAME last returned, by TIMER-COUNT.")
(defun wait-frame (&optional retrace)
  "Wait until a frame's time has passed since WAIT-FRAME last returned:
16,000 of TIMER-COUNT's clocks, a little less than a frame at 70 a second.
Then, if RETRACE, wait for the vertical retrace as well.  This paces an
animation where WAIT-RETRACE alone does not: some emulators (QEMU is one)
report a retrace whenever they are asked."
  (loop while (< -1 (- (timer-count) *last-frame*) 16000))
  (when retrace (wait-retrace))
  (setf *last-frame* (timer-count))
  (values))

(defun key-pressed-p ()
  "Is a key waiting to be read?"
  (poll-hotkeys)
  (zerop (logand (nth-value 5 (int86 #x16 :ax #x0100)) #x40)))
(defun wait-key ()
  "Wait for a key.  Returns the BIOS's word for it: the scan code in the
high byte, the character in the low.  GET-KEY gives it a name."
  (when (hotkeys-installed-p)           ; the BIOS would wait without polling
    (loop until (key-pressed-p)))
  (int86 #x16 :ax 0))
(defun finish ()
  "Wait for a key, then go back to 80x25 text: how a picture ends."
  (wait-key) (video-mode 3))
