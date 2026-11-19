;;;; The CMOS memory, the real-time clock and the floppy controller: a tour
;;;; of the motherboard through its I/O ports.
;;;;
;;;; Nothing here asks the BIOS or DOS for an answer (except once, to show
;;;; DOS's idea of the time next to the clock chip's).  The chips are asked
;;;; themselves, with IN and OUT:
;;;;   70h/71h    the MC146818 real-time clock and its 128 bytes of
;;;;              battery-backed memory, where the BIOS keeps the setup;
;;;;   3F0h-3F7h  the floppy disk controller (looked at, not used: no motor
;;;;              is started and no head is moved);
;;;;   40h-43h    the 8254 timer that makes the 18.2 Hz tick;
;;;;   20h-A1h    the two 8259 interrupt controllers;
;;;;   64h        the keyboard controller;
;;;; and the table of serial and parallel ports the BIOS left at 0040:0000.
;;;;
;;;;   (doslib-examples:run "probes")        ; a key press between the pages
;;;;   (doslib-examples:run "probes" nil)    ; straight through, no pauses
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.probes
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.probes)

;;; ---- small things
(defun bit-p (value n) (logbitp n value))
(defun port (address) (logand (inb address) #xFF))

(defmacro within-ticks ((ticks) &body test)
  "Evaluate TEST over and over until it is true (return that) or TICKS timer
ticks have gone by (return NIL).  Whatever is waited for here may never
come -- a controller that is not there -- so nothing waits without this."
  (let ((start (gensym)) (value (gensym)))
    `(let ((,start (ticks)))
       (loop
         (let ((,value (progn ,@test)))
           (when ,value (return ,value)))
         (when (> (- (ticks) ,start) ,ticks) (return nil))))))

(defvar *pause* t)
(defun section (title)
  (format t "~&~%--- ~A ~V,,,'-A~%" title (max 0 (- 73 (length title))) ""))
(defun page ()
  "The end of a screenful: wait for a key, if we are pausing at all."
  (when *pause*
    (format t "~&-- press a key --")
    (finish-output)
    (wait-key)
    (format t "~C~18A~C" #\Return "" #\Return)))

(defun flags (value names)
  "NAMES of the bits set in VALUE (NAMES starts at bit 7), as one string."
  (let ((set (loop for name in names for n downfrom 7
                   when (and name (bit-p value n)) collect name)))
    (if set (format nil "~{~A~^, ~}" set) "none")))

;;; ---- the CMOS memory
;;; Port 70h selects one of 128 bytes, port 71h reads or writes it.  Bit 7
;;; of port 70h is not part of the address: it switches the NMI off, so it
;;; is left clear.
;;; (CMOS and BCD are the library's.)
(defun cmos16 (register) (+ (cmos register) (* 256 (cmos (1+ register)))))

;;; ---- the clock
;;; Once a second the chip copies its counters into bytes 0-9, and while it
;;; does (it says so in bit 7 of register A, "update in progress") they may
;;; read as anything.  So: wait for the bit to clear, read, and read again
;;; until two readings agree.
(defun raw-time ()
  (within-ticks (4) (not (bit-p (cmos #x0A) 7)))
  (list (cmos 0) (cmos 2) (cmos 4) (cmos 6) (cmos 7) (cmos 8) (cmos 9) (cmos #x32)))

(defun decode-time (raw b)
  "The bytes RAW of the clock, decoded as register B says they are stored:
second minute hour weekday day month year century."
  (let ((binary (bit-p b 2))            ; else every byte is BCD: 59h means 59
        (24-hour (bit-p b 1)))          ; else 1-12, with bit 7 of the hour set for PM
    (flet ((number (byte) (if binary byte (bcd byte))))
      (destructuring-bind (second minute hour weekday day month year century) raw
        (let ((pm (and (not 24-hour) (bit-p hour 7)))
              (h (number (logand hour (if 24-hour #xFF #x7F)))))
          (unless 24-hour               ; 12 AM is midnight, 12 PM is noon
            (setf h (+ (mod h 12) (if pm 12 0))))
          (values (number second) (number minute) h (number weekday) (number day)
                  (number month) (number year)
                  (bcd century)))))))   ; the BIOS keeps this one, in BCD, whatever B says

(defun rtc-time ()
  "The clock chip's time: second minute hour weekday day month year
century, and the raw bytes as a list."
  (let ((raw (loop repeat 10
                   for a = (raw-time) then b
                   for b = (raw-time)
                   when (equal a b) return a
                   finally (return b))))
    (multiple-value-call #'values (decode-time raw (cmos #x0B)) raw)))

(defparameter *weekdays* #("?" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defun periodic-rate (a)
  "The frequency in Hz that the low four bits of register A select, or NIL for none."
  (let ((rate (logand a 15)))
    (cond ((zerop rate) nil)
          ((<= rate 2) (ash 32768 (- (+ rate 6))))    ; 1 and 2 wrap round: 256 and 128 Hz
          (t (ash 32768 (- (1- rate)))))))

(defun show-clock ()
  (section "Real-time clock (MC146818, ports 70h/71h)")
  (multiple-value-bind (second minute hour weekday day month year century raw) (rtc-time)
    (let ((b (cmos #x0B)))
      (format t "Time  ~2,'0D:~2,'0D:~2,'0D    Date  ~A ~2,'0D~2,'0D-~2,'0D-~2,'0D~%"
              hour minute second (aref *weekdays* (if (<= 1 weekday 7) weekday 0))
              century year month day)
      (format t "  raw: sec ~2,'0X min ~2,'0X hour ~2,'0X weekday ~2,'0X day ~2,'0X ~
                 month ~2,'0X year ~2,'0X century ~2,'0X~%"
              (nth 0 raw) (nth 1 raw) (nth 2 raw) (nth 3 raw) (nth 4 raw) (nth 5 raw) (nth 6 raw)
              (nth 7 raw))
      (format t "  stored as ~:[BCD~;binary~], ~:[12-hour with a PM bit~;24-hour~]~%"
              (bit-p b 2) (bit-p b 1))
      (format t "  alarm ~2,'0X:~2,'0X:~2,'0X (raw; C0h-FFh in a field means \"any\")~%"
              (cmos 5) (cmos 3) (cmos 1))))
  ;; What DOS thinks.  It read the chip once, when it started, and has
  ;; counted timer ticks since: the two drift apart.
  (multiple-value-bind (ax bx cx dx) (int86 #x21 :ax #x2C00)
    (declare (ignore ax bx))
    (multiple-value-bind (ax2 bx2 cx2 dx2) (int86 #x21 :ax #x2A00)
      (declare (ignore ax2 bx2))
      (format t "DOS says  ~2,'0D:~2,'0D:~2,'0D  ~4,'0D-~2,'0D-~2,'0D  (its own count of timer ticks)~%"
              (hi cx) (lo cx) (hi dx) (word16 cx2) (hi dx2) (lo dx2))))
  (let ((a (cmos #x0A)) (b (cmos #x0B)) (c (cmos #x0C)) (d (cmos #x0D)))
    (format t "~%Register A = ~2,'0Xh: ~:[no update running~;update running~], divider ~D~A, ~
               rate select ~D~%"
            a (bit-p a 7) (ldb (byte 3 4) a)
            (if (= (ldb (byte 3 4) a) 2) " (32768 Hz)" "")
            (logand a 15))
    (let ((rate (periodic-rate a)))
      (if rate
          (format t "  periodic interrupt rate: 32768 >> (~D-1) = ~D Hz, one every ~,3F ms.~%"
                  (logand a 15) rate (/ 1000.0 rate))
          (format t "  periodic interrupt rate: none selected.~%"))
      (format t "  That is IRQ 8, and it only fires when PIE is set in register B (it is~:[ not~;~]).~%"
              (bit-p b 6)))
    (format t "Register B = ~2,'0Xh: ~A~%" b
            (flags b '("SET (updates halted)" "PIE (periodic interrupt)" "AIE (alarm interrupt)"
                       "UIE (update-ended interrupt)" "SQWE (square wave)" "DM (binary)" "24-hour"
                       "DSE (daylight saving)")))
    (format t "Register C = ~2,'0Xh: interrupt flags: ~A  (reading it clears them)~%" c
            (flags c '("IRQF" "PF periodic" "AF alarm" "UF update ended" nil nil nil nil)))
    (format t "Register D = ~2,'0Xh: ~:[the battery has FAILED: time and setup are not to be trusted~;~
               the battery is good (VRT)~]~%"
            d (bit-p d 7))))

(defun show-live-clock (&optional (seconds 5))
  "Watch the chip count: the line is rewritten when its seconds change."
  (section "The clock, live")
  (let ((start (ticks)) (last nil) (changes 0) (first-change nil) (last-change nil))
    (loop
      (multiple-value-bind (second minute hour) (rtc-time)
        (unless (eql second last)
          (let ((now (ticks)))
            (when last
              (incf changes)
              (unless first-change (setf first-change now))
              (setf last-change now))
            (setf last second)
            (format t "~C  ~2,'0D:~2,'0D:~2,'0D   register A = ~2,'0Xh   BIOS tick count = ~D   "
                    #\Return hour minute second (cmos #x0A) now)
            (finish-output))))
      (when (or (> changes seconds) (> (- (ticks) start) (* 19 (+ seconds 3)))) (return)))
    (terpri)
    (if (> changes 1)
        (format t "~D seconds on the chip took ~D timer ticks: ~,2F a second (the PIT makes 18.21).~%"
                (1- changes) (- last-change first-change)
                (/ (- last-change first-change) (float (1- changes))))
        (format t "The seconds did not change: is the clock stopped (SET in register B)?~%"))))

;;; ---- what the BIOS setup stored
(defparameter *floppy-types*
  #("none" "5.25\" 360 KB" "5.25\" 1.2 MB" "3.5\" 720 KB" "3.5\" 1.44 MB" "3.5\" 2.88 MB"))
(defun floppy-type (n) (if (< n (length *floppy-types*)) (aref *floppy-types* n) "unknown type"))
(defun disk-type (nibble extended-register)
  (case nibble
    (0 "none")
    (15 (format nil "type 15 -> register ~2,'0Xh: type ~D"
                extended-register (cmos extended-register)))
    (t (format nil "type ~D of the BIOS's table" nibble))))

(defun show-setup (&optional (machine-mb 512))
  (section "CMOS setup memory")
  (let* ((base (cmos16 #x15)) (extended (cmos16 #x17)) (extended-2 (cmos16 #x30))
         (above-16m (cmos16 #x34))      ; in 64 KB blocks: where newer BIOSes count on
         (above-4g (+ (cmos #x5B) (ash (cmos #x5C) 8) (ash (cmos #x5D) 16))))
    (format t "Base memory      (15h/16h) ~6D KB~%" base)
    (format t "Extended memory  (17h/18h) ~6D KB   as found at power-on (30h/31h) ~D KB~%"
            extended extended-2)
    (format t "  16 bits of KB end at 64 MB, so more is counted from 16 MB up in 64 KB blocks:~%")
    (format t "Above 16 MB      (34h/35h) ~6D blocks = ~D MB; above 4 GB (5Bh-5Dh) ~D blocks~%"
            above-16m (floor above-16m 16) above-4g)
    (let ((total (if (plusp above-16m)
                     (+ 16 (/ above-16m 16))
                     (/ (+ 1024 extended) 1024))))
      (format t "  so the machine has ~D MB~@[: ~A~]~%"
              (if (integerp total) total (float total))
              (and machine-mb
                   (if (= total machine-mb)
                       (format nil "the ~D MB expected" machine-mb)
                       (format nil "NOT the ~D MB expected" machine-mb))))))
  (let ((e (cmos #x14)))
    (format t "Equipment (14h) = ~2,'0Xh: ~:[no floppy drive~*~;~D floppy drive~:P~], ~
               ~:[no FPU~;FPU~], ~:[no PS/2 mouse~;PS/2 mouse~], display: ~A~%"
            e (bit-p e 0) (1+ (ldb (byte 2 6) e)) (bit-p e 1) (bit-p e 2)
            (aref #("EGA/VGA" "CGA 40 columns" "CGA 80 columns" "monochrome") (ldb (byte 2 4) e))))
  (let ((f (cmos #x10)) (h (cmos #x12)))
    (format t "Floppy drives (10h) = ~2,'0Xh: A: ~A, B: ~A~%"
            f (floppy-type (ash f -4)) (floppy-type (logand f 15)))
    (format t "Hard disks (12h) = ~2,'0Xh: first: ~A~%~24@Tsecond: ~A~%"
            h (disk-type (ash h -4) #x19) (disk-type (logand h 15) #x1A)))
  (format t "Diagnostic status (0Eh) = ~2,'0Xh, shutdown code (0Fh) = ~2,'0Xh~%" (cmos #x0E) (cmos #x0F))
  ;; The BIOS guards bytes 10h-2Dh with a 16-bit sum, high byte first.
  (let ((sum (logand (loop for r from #x10 to #x2D sum (cmos r)) #xFFFF))
        (stored (+ (* 256 (cmos #x2E)) (cmos #x2F))))
    (format t "Checksum of 10h-2Dh: computed ~4,'0Xh; in 2Eh/2Fh: ~4,'0Xh: ~A~%"
            sum stored
            (cond ((= sum stored) "good")
                  ((zerop stored) "this BIOS keeps none")
                  (t "BAD")))))

(defun show-dump ()
  (section "All 128 bytes")
  (format t "    ~{  ~1X~}~%" (loop for i below 16 collect i))
  (dotimes (row 8)
    (format t "~2,'0Xh:~{ ~2,'0X~}~%" (* row 16)
            (loop for i below 16 collect (cmos (+ (* row 16) i)))))
  (format t "00h-09h clock; 0Ah-0Dh registers A-D; 10h-2Fh setup and checksum; 32h century.~%"))

;;; ---- the floppy controller
;;; Its main status register (3F4h) says whether it will take a byte (RQM,
;;; bit 7) and which way the next byte goes (DIO, bit 6).  A command is a
;;; byte written to 3F5h when RQM=1, DIO=0; its answer is read from 3F5h
;;; when RQM=1, DIO=1.  VERSION (10h) is the one command that needs no
;;; drive, no interrupt and no DMA.
(defun fdc-wait (direction)
  "Wait until the controller is ready for a byte going in DIRECTION
(:in to it, :out of it).  NIL if it does not get ready in a few ticks."
  (within-ticks (3)
    (= (logand (port #x3F4) #xC0) (ecase direction (:in #x80) (:out #xC0)))))

(defun fdc-version ()
  "The answer to the VERSION command, or a keyword saying why there is none."
  (cond ((not (fdc-wait :in)) :not-ready)
        (t (outb #x3F5 #x10)
           (cond ((fdc-wait :out)
                  (prog1 (port #x3F5)
                    ;; one byte is all there should be; were there more,
                    ;; take them, or the controller is left stuck mid-answer
                    (loop repeat 16 while (= (logand (port #x3F4) #xC0) #xC0) do (port #x3F5))))
                 (t :no-answer)))))

(defun show-floppy ()
  (section "Floppy disk controller (ports 3F0h-3F7h)")
  (let ((msr (port #x3F4)) (dor (port #x3F2)) (dir (port #x3F7)))
    (cond ((= msr #xFF)
           (format t "Main status register (3F4h) = FFh: an empty bus.  There is no controller.~%"))
          (t
           (format t "Main status (3F4h) = ~2,'0Xh: ~A~%" msr
                   (flags msr '("RQM (ready for a byte)" "DIO (it has data for us)" "non-DMA"
                                "command in progress" "drive 3 seeking" "drive 2 seeking"
                                "drive 1 seeking" "drive 0 seeking")))
           (format t "Digital output (3F2h) = ~2,'0Xh: drive ~D, ~:[in RESET~;running~], ~
                      ~:[IRQ/DMA off~;IRQ 6/DMA 2 on~], motors: ~A~%"
                   dor (logand dor 3) (bit-p dor 2) (bit-p dor 3)
                   (flags dor '("D" "C" "B" "A" nil nil nil nil)))
           (format t "Digital input (3F7h) = ~2,'0Xh: disk-change line ~:[clear~;set~]~%" dir (bit-p dir 7))
           (format t "Status A/B (3F0h/3F1h, PS/2 only) = ~2,'0Xh ~2,'0Xh~%" (port #x3F0) (port #x3F1))
           (let ((version (fdc-version)))
             (case version
               (:not-ready
                (format t "VERSION: not asked, the controller was not ready for a command.~%"))
               (:no-answer
                (format t "VERSION: asked, but no answer came.~%"))
               (t (format t "VERSION command (10h) answers ~2,'0Xh: ~A~%" version
                          (case version
                            (#x90 "an 82077AA-class (enhanced) controller")
                            (#x80 "a NEC uPD765 or Intel 8272A, the original PC's")
                            (t "not a value I know"))))))
           (format t "No motor was started and no head moved.~%")))))

;;; ---- timer, interrupt controllers, keyboard controller, port table
(defparameter *irq-names*
  #("timer" "keyboard" "cascade to second 8259" "COM2" "COM1" "LPT2 or sound" "floppy" "LPT1"
    "real-time clock" "free (PCI)" "free (PCI)" "free (PCI)" "PS/2 mouse" "FPU" "first IDE" "second IDE"))

(defun show-timer ()
  (section "Timer (8254, ports 40h-43h), channel 0")
  ;; The read-back command, C2h: latch both the status and the count of
  ;; channel 0.  Then port 40h gives the status byte, count low, count high.
  (outb #x43 #xC2)
  (let* ((status (port #x40))
         (low (port #x40))
         (count (+ low (* 256 (port #x40))))
         (mode (ldb (byte 3 1) status)))
    (format t "Status ~2,'0Xh: mode ~D (~A), ~A, ~:[binary~;BCD~], output ~:[low~;high~]~%"
            status (if (> mode 5) (- mode 4) mode)      ; modes 6 and 7 are 2 and 3 again
            (aref #("interrupt on terminal count" "one-shot" "rate generator" "square wave"
                    "software strobe" "hardware strobe" "rate generator" "square wave")
                  mode)
            (aref #("latch" "low byte" "high byte" "low+high bytes") (ldb (byte 2 4) status))
            (bit-p status 0) (bit-p status 7))
    (format t "Count just now: ~D.  1193182 Hz / 65536 = 18.2 Hz: IRQ 0, the BIOS tick.~%"
            count)))

(defun show-interrupt-controllers ()
  (section "Interrupt controllers (8259 pair, ports 20h/21h and A0h/A1h)")
  (let ((mask (+ (port #x21) (* 256 (port #xA1)))))
    ;; OCW3: 0Bh makes the next read of the command port give the in-service
    ;; register, 0Ah the request register -- which is what it gives at
    ;; rest, so that is put back.
    (outb #x20 #x0B) (outb #xA0 #x0B)
    (let ((in-service (+ (port #x20) (* 256 (port #xA0)))))
      (outb #x20 #x0A) (outb #xA0 #x0A)
      (let ((requested (+ (port #x20) (* 256 (port #xA0)))))
        (format t "Masks: 21h = ~2,'0Xh, A1h = ~2,'0Xh   in service: ~4,'0Xh   requested: ~4,'0Xh~%"
                (lo mask) (hi mask) in-service requested)))
    (dotimes (row 8)
      (flet ((cell (irq)
               (format nil "IRQ ~2D ~:[on ~;off~]  ~A" irq (bit-p mask irq) (aref *irq-names* irq))))
        (format t "  ~38A~A~%" (cell row) (cell (+ row 8)))))))

(defun show-keyboard-controller ()
  (section "Keyboard controller (8042, port 64h)")
  (let ((status (port #x64)))
    (format t "Status ~2,'0Xh: ~A~%" status
            (flags status '("parity error" "timeout" "the waiting byte is the mouse's"
                            "keyboard not locked" "last write was a command" "self-test passed"
                            "input buffer full" "output buffer full (a byte is waiting)")))))

(defun show-port-table ()
  (section "Serial and parallel ports the BIOS found (0040:0000)")
  (flet ((row (name address)
           (format t "~{  ~12A~}~%"
                   (loop for i below 4
                         for base = (peek16 (+ address (* 2 i)))
                         collect (if (zerop base)
                                     (format nil "~A~D none" name (1+ i))
                                     (format nil "~A~D ~3,'0Xh" name (1+ i) base))))))
    (row "COM" #x400)
    (row "LPT" #x408))
  (format t "(On a PS/2-style BIOS the LPT4 slot is the segment of the extended BIOS data.)~%"))

(defun main (&optional (pause t) (machine-mb 512))
  "The CMOS memory, the real-time clock and the floppy controller.
PAUSE: wait for a key after each screenful.  MACHINE-MB: how much memory the
machine is known to have, to check the CMOS's figure against (or NIL)."
  (let ((*pause* pause))
    (show-clock)
    (page)
    (show-live-clock)
    (show-setup machine-mb)
    (page)
    (show-dump)
    (show-floppy)
    (page)
    (show-timer)
    (show-interrupt-controllers)
    (show-keyboard-controller)
    (show-port-table))
  (values))

(doslib-examples:define-example "probes" 'main
  :category :system
  :description "The CMOS memory, the real-time clock and the floppy controller.")
