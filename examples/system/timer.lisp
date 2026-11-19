;;;; Reprogramming the timer: a thousand interrupts a second instead of 18.2,
;;;; counted by an interrupt handler of our own, with the BIOS clock left
;;;; running at its proper rate.
;;;;
;;;;   (doslib-examples:run "timer")
;;;; Space stops and starts the stopwatch, R sets it to zero, S draws the
;;;; screen only seven times a second (and back), Esc leaves.
;;;;
;;;; The PC's timer chip (an 8253 or 8254) divides 1,193,182 Hz by a 16-bit
;;;; number and raises interrupt request 0 each time it has counted that
;;;; far.  The BIOS leaves the divisor at 65536, the most there is: 18.2
;;;; interrupts a second, each of which adds one to the tick count at
;;;; 0040:006Ch that DOS tells the time by.  That is all the clock a PC
;;;; program is given, and 55 milliseconds is a long time: a game that
;;;; moves things once a tick moves them in jerks.
;;;;
;;;; So programs that wanted better changed the divisor.  With 1193 the
;;;; interrupt comes 1000.15 times a second.  But then the BIOS would count
;;;; 55 times too fast, and the time of day would race ahead; so the program
;;;; takes the interrupt itself, and passes on to the old handler only as
;;;; many as it would have had.  The classic way, used here: add the divisor
;;;; to a 16-bit sum at each interrupt, and pass the interrupt on when the
;;;; sum overflows -- which it does once for every 65536 counts of the chip,
;;;; exactly as often as before.  The other times, the handler tells the
;;;; interrupt controller it is done (20h to port 20h) and returns.
;;;;
;;;; The handler cannot be Lisp (an interrupt arrives in the middle of
;;;; anything), so it is some forty bytes of machine code, assembled below.
;;;; It adds one to a counter in memory: milliseconds, near enough.  Lisp
;;;; reads the counter.  The old handler it passes on to is the Lisp
;;;; runtime's own (which watches for Ctrl-Break) and, after that, the DPMI
;;;; host, which gives the interrupt to the BIOS in real mode.
;;;;
;;;; What is on the screen: a stopwatch that counts milliseconds; two
;;;; markers that cross the screen at the same speed, one placed by our
;;;; counter and one by the BIOS's ticks (watch the second one jerk); and a
;;;; check against a third clock, the battery-backed clock chip, of how
;;;; fast each count is really going.  The top marker's place is worked out
;;;; from the time, not moved a step a frame, so it goes at the same speed
;;;; however often the screen is drawn: press S.
;;;;
;;;; An emulator may not manage it.  QEMU on a busy host delivered 750 to
;;;; 800 of the thousand interrupts a second when this was written (at 100
;;;; a second it lost none): then both counts fall short by the same
;;;; fraction, the BIOS clock runs slow while the program runs, and the
;;;; last figure -- ours for each BIOS tick -- stays within a few per cent
;;;; of 54.9.  A real machine has no trouble: an interrupt a millisecond
;;;; is not many.
;;;;
;;;; On the way out, however that happens, the divisor goes back to 65536
;;;; and the interrupt vector to what it was.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.timer
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:call-with-fast-timer #:milliseconds))
(in-package #:doslib-examples.timer)

(defconstant +chip-hz+ 1193182 "What the timer chip counts.")
(defconstant +divisor+ 1193 "Interrupt every so many counts: 1000.15 times a second.")

;;; Twelve bytes outside the Lisp heap, where the garbage collector will
;;; not move them: the count of interrupts (4 bytes), the 16-bit sum (at 4),
;;; and the old handler's address (offset at 6, selector at 10) in the form
;;; a far jump through memory wants.
(defvar *memory* nil)
(defun milliseconds ()
  "How many of our interrupts there have been: about one a millisecond."
  (sap-ref-32 *memory* 0))

(defun handler-code ()
  "The handler for interrupt 8: count, and now and then pass it on."
  (let ((ds (data-selector)) (at (sap-int *memory*)))
    (flet ((address (offset) (loop for i below 4 collect (ldb (byte 8 (* 8 i)) (+ at offset)))))
      (apply #'machine-code
             `(#x50 #x1E                            ; push eax / push ds
               #x66 #xB8 ,(lo ds) ,(hi ds)          ; mov ax, our data selector
               #x8E #xD8                            ; mov ds, ax
               #xFF #x05 ,@(address 0)              ; inc dword [count]
               #x66 #x81 #x05 ,@(address 4)         ; add word [sum], divisor
               ,(lo +divisor+) ,(hi +divisor+)
               #x72 #x07                            ; jc pass-on       it overflowed
               #xB0 #x20 #xE6 #x20                  ; mov al,20h / out 20h,al   controller: done
               #x1F #x58 #xCF                       ; pop ds / pop eax / iretd
               #x1F #x58                            ; pass-on: pop ds / pop eax
               #x2E #xFF #x2D ,@(address 6))))))    ; jmp far cs:[old handler]

(defun set-divisor (divisor)
  "Give channel 0 of the timer chip a new divisor (0 means 65536)."
  (outb #x43 #x36)                      ; channel 0, low byte then high, square wave
  (outb #x40 (lo divisor))
  (outb #x40 (hi divisor)))

(defun call-with-fast-timer (function)
  "Call FUNCTION with the timer interrupting 1000 times a second and
MILLISECONDS counting them; put the timer back afterwards."
  (unless *memory* (setf *memory* (alien-sap (make-alien (unsigned 8) 12))))
  (multiple-value-bind (old-selector old-offset) (get-vector 8)
    (setf (sap-ref-32 *memory* 0) 0
          (sap-ref-16 *memory* 4) 0
          (sap-ref-32 *memory* 6) old-offset
          (sap-ref-16 *memory* 10) old-selector)
    (unwind-protect
         (progn (set-vector 8 (code-selector) (sap-int (handler-code)))
                (set-divisor +divisor+)
                (funcall function))
      (set-divisor 0)
      (set-vector 8 old-selector old-offset))))

;;; ---- the screen
(defconstant +track-x+ 4)
(defconstant +track-width+ 72)          ; characters; a marker has twice as many places
(defconstant +crossing+ 3000 "Milliseconds for a marker to cross the screen.")

(defun marker-place (time)
  "Where a marker is at TIME (milliseconds): there and back, for ever."
  (let* ((places (1- (* 2 +track-width+)))
         (p (mod (floor (* time places) +crossing+) (* 2 places))))
    (if (< p places) p (- (* 2 places) p))))
(defun draw-marker (row place attribute)
  "A marker at PLACE on ROW: half a character wide, so twice as fine as the columns."
  (text-fill +track-x+ row +track-width+ 1 #x08 (code-char 196))
  (text-put (+ +track-x+ (floor place 2)) row
            (string (code-char (if (evenp place) 221 222))) attribute))

(defun clock-seconds ()
  "The seconds of the battery-backed clock chip (as it keeps them)."
  (cmos 0))

(defun draw-labels ()
  (text-fill 0 0 80 25 #x07)
  (text-fill 0 0 80 1 #x1F)
  (text-put 2 0 "The timer chip, reprogrammed: 1000 interrupts a second instead of 18.2" #x1F)
  (text-put 4 3 "Stopwatch" #x0B)
  (text-put 4 7 "Placed by our count, a thousandth of a second at a time" #x0B)
  (text-put 4 10 "Placed by the BIOS's ticks, 18.2 a second" #x0B)
  (text-put 4 14 "Against the clock chip" #x0B)
  (text-put 4 24 "Space: stop/start   R: zero   S: draw the screen less often   Esc: leave" #x70))

(defun show ()
  "The demonstration proper; the fast timer is running.  Returns the figures."
  (let* ((watch 0) (watch-from (milliseconds)) (running t)     ; the stopwatch
         (slow nil) (last-draw -1000)
         (frames 0) (frames-from (milliseconds)) (rate 0)
         (second (clock-seconds)) (seconds -1)                 ; the check
         (ms0 0) (ticks0 0) (ms1 0) (ticks1 0)
         (first-tick (ticks)) (last-place -1) (last-tick-place -1))
    (draw-labels)
    (hide-text-cursor)
    (loop
      (let ((now (milliseconds)))
        ;; the clock chip's seconds: note both counts each time they change
        (let ((s (clock-seconds)))
          (unless (= s second)
            (setf second s)
            (incf seconds)
            (if (zerop seconds)
                (setf ms0 now ticks0 (ticks))
                (setf ms1 now ticks1 (ticks)))))
        (when (key-pressed-p)
          (let ((key (get-key)))
            (case key
              ((#\Escape #\q #\Q) (return))
              (#\Space (if running
                           (setf watch (+ watch (- now watch-from)) running nil)
                           (setf watch-from now running t)))
              ((#\r #\R) (setf watch 0 watch-from now))
              ((#\s #\S) (setf slow (not slow))))))
        (when (>= (- now last-draw) (if slow 140 0))
          (setf last-draw now)
          (incf frames)
          (when (>= (- now frames-from) 1000)
            (setf rate (round (* frames 1000) (- now frames-from))
                  frames 0 frames-from now))
          (let ((time (if running (+ watch (- now watch-from)) watch)))
            (multiple-value-bind (minutes rest) (floor time 60000)
              (multiple-value-bind (seconds thousandths) (floor rest 1000)
                (text-put 4 4 (format nil "~2,'0D:~2,'0D.~3,'0D  ~A" minutes seconds thousandths
                                      (if running "       " "stopped"))
                          #x0F))))
          (let ((place (marker-place now))
                (tick-place (marker-place (floor (* (- (ticks) first-tick) 65536 1000)
                                                 +chip-hz+))))
            (unless (= place last-place)
              (draw-marker 8 place #x0E)
              (setf last-place place))
            (unless (= tick-place last-tick-place)
              (draw-marker 11 tick-place #x0C)
              (setf last-tick-place tick-place)))
          (flet ((line (row attribute control &rest arguments)
                   (text-put 4 row (format nil "~72A" (apply #'format nil control arguments))
                             attribute)))
            (line 5 #x07 "The screen is drawn ~4D times a second~:[~;  (held back: the speed ~
                          is the same)~]" rate slow)
            (if (< seconds 1)
                (line 15 #x07 "waiting for its seconds to change...")
                (let ((ours (- ms1 ms0)) (bios (- ticks1 ticks0)))
                  (line 15 #x07 "In ~D second~:P by the clock chip:" seconds)
                  (line 16 #x0F "  our interrupts ~7D   ~7,1F a second   (~,2F expected)"
                        ours (/ ours seconds 1.0) (/ +chip-hz+ +divisor+ 1.0))
                  (line 17 #x0F "  BIOS ticks     ~7D   ~7,2F a second   (~,2F expected)"
                        bios (/ bios seconds 1.0) (/ +chip-hz+ 65536.0))
                  (when (plusp bios)
                    (line 18 #x0F "  ours for each BIOS tick  ~7,2F            (~,2F expected)"
                          (/ ours bios 1.0) (/ 65536.0 +divisor+)))))))
        ;; nothing more to do until the count moves on
        (loop while (= now (milliseconds)))))
    (values seconds (- ms1 ms0) (- ticks1 ticks0))))

(defun main ()
  "The timer at 1000 interrupts a second, with the BIOS clock still right."
  (multiple-value-bind (seconds ours bios)
      (unwind-protect (call-with-fast-timer #'show)
        (video-mode 3))
    (cond ((and (plusp seconds) (plusp bios))
           (format t "~&In ~D second~:P by the clock chip: ~D interrupts of ours (~,1F a second), ~
                      ~D BIOS ticks~%(~,2F a second), ~,2F of ours for each BIOS tick.~%"
                   seconds ours (/ ours seconds 1.0) bios (/ bios seconds 1.0) (/ ours bios 1.0)))
          (t (format t "~&Not long enough to measure anything.~%")))
    (format t "The timer is back at 18.2 interrupts a second.~%"))
  (values))

(doslib-examples:define-example "timer" 'main
  :category :system
  :description "The timer chip at 1000 interrupts a second: a millisecond clock.")
