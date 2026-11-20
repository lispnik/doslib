;;;; The real-time clock's interrupt: 1,024 a second, and an alarm
;;;;
;;;; The AT's battery-backed clock (a Motorola MC146818, at ports 70h and
;;;; 71h) does more than keep the date.  It has its own interrupt request,
;;;; number 8 -- the first on the second interrupt controller -- and three
;;;; reasons to raise it, each switched on by a bit of its register B:
;;;;
;;;;   bit 6  periodic     a steady rate set in register A: 1,024 a second
;;;;                       as the BIOS leaves it, 2 to 8,192 by choice
;;;;   bit 5  alarm        when the time equals the alarm time in registers
;;;;                       1, 3 and 5
;;;;   bit 4  update       once a second, after the time has moved on
;;;;
;;;; The handler must read register C, which says which reason it was and
;;;; clears it: until then the clock raises nothing more.  Here the
;;;; library's WITH-IRQ-HANDLER builds that handler -- write 0Ch to port
;;;; 70h, read port 71h, count -- and this program counts the periodic
;;;; interrupt for a second against the timer chip, then sets the alarm for
;;;; three seconds ahead and waits for it.
;;;;
;;;; This was how a DOS program got a clock finer than 18.2 ticks a second
;;;; without touching the timer everyone else depended on.  Under QEMU the
;;;; rate is the host's idea of time: expect a count near 1,024, not exact.
;;;;
;;;; It runs for about five seconds and returns; a key ends the wait early.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.alarm
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.alarm)

;;; The handler writes to port 70h too, so while it is in place the clock is
;;; only touched with its interrupt held off at the controller.
(defmacro quietly (&body body)
  `(let ((mask (inb #xA1)))
     (outb #xA1 (logior mask 1))
     (unwind-protect (progn ,@body)
       (outb #xA1 mask))))

(defun register (n) (quietly (cmos n)))
(defun (setf register) (value n)
  (quietly (outb #x70 n) (outb #x71 value))
  value)

(defun clock-time ()
  "Hours, minutes and seconds, read when the clock is not in the middle of changing."
  (loop while (logbitp 7 (register #x0A)))
  (let ((binary (logbitp 2 (register #x0B))))
    (flet ((value (n) (let ((byte (register n))) (if binary byte (bcd byte)))))
      (values (logand (value 4) #x7F) (value 2) (value 0)))))

(defun set-alarm (hours minutes seconds)
  (let ((binary (logbitp 2 (register #x0B))))
    (flet ((store (n value)
             (setf (register n) (if binary value (+ (* 16 (floor value 10)) (mod value 10))))))
      (store 5 hours) (store 3 minutes) (store 1 seconds))))

(defun wait-clocks (clocks hook &optional until-count)
  "Wait CLOCKS of the timer chip, or until a key, or until HOOK has counted UNTIL-COUNT."
  (let ((start (timer-count)))
    (loop until (or (>= (- (timer-count) start) clocks)
                    (and until-count (>= (irq-count hook) until-count)))
          do (when (key-pressed-p) (wait-key) (return)))
    (- (timer-count) start)))

(defun alarm ()
  (let ((b (cmos #x0B)) (a (cmos #x0A)))
    (multiple-value-bind (hours minutes seconds) (clock-time)
      (format t "~&The clock says ~2,'0D:~2,'0D:~2,'0D.  Register A is ~2,'0Xh (rate ~D), B is ~2,'0Xh.~%"
              hours minutes seconds a (logand a 15) b))
    (unwind-protect
         (with-irq-handler (hook 8 :write-ports '((#x70 #x0C)) :read-ports '(#x71))
           (register #x0C)                              ; clear anything left over
           ;; the periodic interrupt, for a second
           (setf (register #x0A) (logior (logand a #xF0) 6))    ; rate 6: 32,768 / 2^5 = 1,024
           (setf (irq-count hook) 0)
           (setf (register #x0B) (logior (logand b #x8F) #x40))
           (let ((clocks (wait-clocks +timer-hz+ hook)))
             (setf (register #x0B) (logand b #x8F))
             (format t "Periodic: ~:D interrupts in ~,2F seconds (1,024 a second expected).~%"
                     (irq-count hook) (/ clocks +timer-hz+ 1.0)))
           ;; the alarm, three seconds from now
           (multiple-value-bind (hours minutes seconds) (clock-time)
             (let ((then (mod (+ (* 3600 hours) (* 60 minutes) seconds 3) 86400)))
               (multiple-value-bind (h rest) (floor then 3600)
                 (multiple-value-bind (m s) (floor rest 60)
                   (set-alarm h m s)
                   (format t "Alarm set for ~2,'0D:~2,'0D:~2,'0D...~%" h m s)))))
           (register #x0C)
           (setf (irq-count hook) 0)
           (setf (register #x0B) (logior (logand b #x8F) #x20))
           (let ((clocks (wait-clocks (* 5 +timer-hz+) hook 1)))
             (setf (register #x0B) (logand b #x8F))
             (if (plusp (irq-count hook))
                 (progn (speaker-beep 880 2)
                        (format t "The alarm rang after ~,1F seconds: interrupt request 8, once.~%"
                                (/ clocks +timer-hz+ 1.0)))
                 (format t "No alarm came in ~,1F seconds.~%" (/ clocks +timer-hz+ 1.0)))))
      ;; the clock as it was
      (outb #x70 #x0A) (outb #x71 a)
      (outb #x70 #x0B) (outb #x71 b)
      (cmos #x0C))))

(defun main ()
  "The real-time clock's interrupt: 1,024 a second, and an alarm."
  (alarm))

(doslib-examples:define-example "alarm" 'main
  :category :system
  :description "The real-time clock's interrupt: 1,024 a second, and an alarm.")
