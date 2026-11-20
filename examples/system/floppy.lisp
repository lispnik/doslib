;;;; Reading a floppy disk's first sector with no help from the BIOS: the
;;;; floppy disk controller and DMA channel 2, programmed from Lisp.
;;;;
;;;;   (doslib-examples:run "floppy")
;;;; It shows the sector; any key ends.  Without a drive, a controller or a
;;;; disk it says which is missing and returns (QEMU: put a disk in with
;;;; -drive file=disk.img,if=floppy,format=raw).
;;;;
;;;; The controller (an NEC 765, or its descendant) has four ports:
;;;;   3F2h  digital output: bits 0-1 choose the drive, bit 2 is 0 to hold
;;;;         the controller in reset, bit 3 lets it use its interrupt and
;;;;         DMA, bits 4-7 turn on the motors of drives A to D;
;;;;   3F4h  main status: bit 7 "ready for a byte", bit 6 "and it is one for
;;;;         you to read", bit 4 "a command is in progress";
;;;;   3F5h  data: commands go in here a byte at a time, results come out;
;;;;   3F7h  data rate: 0 is 500,000 bits a second, a 1.44MB disk's.
;;;; A command is a few bytes written to 3F5h, each when 3F4h says ready.
;;;; The slow ones -- moving the head, reading -- end with interrupt 6
;;;; (vector 0Eh): the handler here is machine code that counts it.  After
;;;; a head movement "sense interrupt status" (08h) must be asked, and
;;;; answers with status byte ST0 and the cylinder; after a read seven
;;;; result bytes wait at 3F5h: ST0, ST1, ST2 and where the head got to.
;;;;
;;;; What is done:  reset (3F2h: 0, then 0Ch) and four "sense interrupt"s,
;;;; one for each drive it might have;  "specify" (03h) the drive's timings;
;;;; motor on (3F2h: 1Ch) and half a second for it to gather speed;
;;;; "recalibrate" (07h): the head to cylinder 0;  "seek" (0Fh) to cylinder
;;;; 0, there already but it shows the command;  then "read data" (E6h)
;;;; for cylinder 0, head 0, sector 1, 512 bytes a sector.
;;;;
;;;; The controller does not hand the bytes to the processor: it asks DMA
;;;; channel 2 to take each one as it comes off the disk.  So before the
;;;; read the DMA controller is told where to put them and how many: mode
;;;; 46h (channel 2, one transfer, from the device to memory), the address
;;;; in ports 04h and 81h (the top bits), the count less one in port 05h.
;;;; After 512 bytes the DMA controller says "that was the last" and the
;;;; controller stops and interrupts.  The memory must be below 1MB (16MB,
;;;; strictly) and not cross a 64K boundary, so it comes from DOS.
;;;;
;;;; Every wait has a limit, by the BIOS's clock.  The BIOS's own floppy
;;;; code shares the controller: its motor time-out (0040:0040) is held
;;;; off while this runs, and at the end the controller is reset, the
;;;; channel masked, the vector and interrupt mask restored, the BIOS's
;;;; notes about the drive (0040:003E) cleared so that it recalibrates, and
;;;; the BIOS asked to reset the disk system: DOS can read A: afterwards.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.floppy
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:read-boot-sector))
(in-package #:doslib-examples.floppy)

(defconstant +output+ #x3F2 "The digital output register.")
(defconstant +status+ #x3F4 "The main status register.")
(defconstant +data+ #x3F5)
(defconstant +rate+ #x3F7)

(define-condition floppy-failure (error)
  ((text :initarg :text :reader failure-text))
  (:report (lambda (condition stream) (write-string (failure-text condition) stream))))
(defun fail (control &rest arguments)
  (error 'floppy-failure :text (apply #'format nil control arguments)))

(defun wait-until (test limit)
  "Call TEST until it is true (and return that) or LIMIT ticks have gone by (NIL)."
  (loop with start = (ticks)
        thereis (funcall test)
        while (<= (- (ticks) start) limit)))

;;; ---- bytes to and from the controller
(defun send (byte)
  "Give the controller a byte of a command, when it is ready for one."
  (unless (wait-until (lambda () (= (logand (inb +status+) #xC0) #x80)) 9)
    (fail "The floppy controller does not take a command: its status is ~2,'0Xh."
          (inb +status+)))
  (outb +data+ byte))
(defun receive ()
  "Take a byte of a result from the controller."
  (unless (wait-until (lambda () (= (logand (inb +status+) #xC0) #xC0)) 9)
    (fail "The floppy controller has no result to give: its status is ~2,'0Xh."
          (inb +status+)))
  (inb +data+))
(defun sense-interrupt ()
  "Ask what the interrupt was about.  Returns ST0 and the cylinder the head is at."
  (send #x08)
  (let ((st0 (receive)))
    (if (= st0 #x80)                    ; "no interrupt was pending": that is all it says
        (values st0 nil)
        (values st0 (receive)))))

;;; ---- interrupt 6: a handler from the library (src/irq.lisp) that counts
;;; and tells the interrupt controller
(defvar *hook* nil "The handler on interrupt 6, while a sector is being read.")
(defun interrupts () (irq-count *hook*))
(defun wait-interrupt (seen what)
  "Wait until more than SEEN interrupts have come: three seconds at most."
  (unless (wait-until (lambda () (> (interrupts) seen)) 55)
    (fail "No interrupt from the floppy controller after ~A~A" what
          (if (string= what "the read") ": is there a disk in drive A:?" "."))))

;;; ---- the steps
(defun reset-controller ()
  "Reset the controller and take the four interrupt reports that follow.  Returns the first ST0."
  (let ((seen (interrupts)) (first nil))
    (outb +output+ #x00)                                ; held in reset
    (wait-tick)
    (outb +output+ #x0C)                                ; let go; interrupt and DMA allowed
    (wait-interrupt seen "the reset")
    (dotimes (drive 4)
      (let ((st0 (sense-interrupt)))
        (unless first (setf first st0))))
    (outb +rate+ 0)                                     ; 500,000 bits a second
    (send #x03) (send #xDF) (send #x02)                 ; specify: step 3 ms, head load 16 ms, DMA
    first))

(defun move-head (seek)
  "Recalibrate (SEEK is NIL) or seek to cylinder 0.  Returns ST0 and the cylinder."
  (let ((seen (interrupts)))
    (cond (seek (send #x0F) (send #x00) (send #x00))    ; seek: head 0 drive 0; cylinder 0
          (t (send #x07) (send #x00)))                  ; recalibrate: drive 0
    (wait-interrupt seen (if seek "the seek" "the recalibrate"))
    (sense-interrupt)))

;;; (DMA channel 2 is set up by the library's DMA-START: one transfer, device
;;; to memory.  Its count register reads FFFFh when all has been moved.)

(defun read-sector-command ()
  "Read cylinder 0, head 0, sector 1.  Returns the seven result bytes."
  (let ((seen (interrupts)))
    (dolist (byte '(#xE6        ; read data: double density, skip deleted sectors, both heads
                    #x00        ; head 0, drive 0
                    0 0 1       ; cylinder, head, sector
                    2           ; 512 bytes a sector (128 * 2^2)
                    18          ; the last sector of a track
                    #x1B        ; the gap between sectors
                    #xFF))      ; (a length, used only for 128-byte sectors)
      (send byte))
    (wait-interrupt seen "the read")
    (loop repeat 7 collect (receive))))

(defun read-boot-sector ()
  "The first sector of the disk in drive A: as 512 bytes, with a list of what
the controller said on the way.  Signals FLOPPY-FAILURE if it cannot be read."
  (let* ((type (ash (cmos #x10) -4))
         (old-timer (peek #x440))
         (sector (make-array 512 :element-type '(unsigned-byte 8)))
         (said '()) (interrupts 0))
    (when (zerop type)
      (fail "There is no floppy drive A: (the CMOS set-up says so)."))
    (unless (<= 4 type 5)               ; 4 is 1.44MB; 5, 2.88MB, reads those disks too
      (fail "Drive A: is not a 1.44MB drive (CMOS type ~D): this reads only those." type))
    (when (= (inb +status+) #xFF)
      (fail "There is no floppy disk controller at port 3F0h."))
    (with-dos-memory (segment 1024)
      ;; 512 bytes that do not cross a 64K boundary: one half or the other
      (let ((address (dma-fit (linear segment) 512)))
        (with-irq-handler (hook 6)
          (setf *hook* hook)
          (unwind-protect
               (progn
                 (setf (sap-ref-8 (int-sap #x440) 0) #xFF)      ; the BIOS: leave the motor be
                 (push (format nil "reset ST0=~2,'0X" (reset-controller)) said)
                 (outb +output+ #x1C)                           ; drive A:, its motor on
                 (wait-tick 9)                                  ; half a second to gather speed
                 (multiple-value-bind (st0 cylinder) (move-head nil)
                   (when (logbitp 4 st0)                        ; not there after 79 steps: again
                     (setf (values st0 cylinder) (move-head nil)))
                   (push (format nil "recalibrate ST0=~2,'0X cyl ~D" st0 cylinder) said)
                   (unless (and (= (logand st0 #xF0) #x20) (eql cylinder 0))
                     (fail "Drive A: did not find cylinder 0 (ST0 = ~2,'0Xh): no drive?" st0)))
                 (multiple-value-bind (st0 cylinder) (move-head t)
                   (push (format nil "seek ST0=~2,'0X cyl ~D" st0 cylinder) said))
                 (dma-start 2 address 512 :direction :to-memory :repeat nil)
                 (destructuring-bind (st0 st1 st2 cylinder head number size)
                     (read-sector-command)
                   (declare (ignore size))
                   (push (format nil "read ST0=~2,'0X ST1=~2,'0X ST2=~2,'0X now at ~D/~D/~D"
                                 st0 st1 st2 cylinder head number)
                         said)
                   (unless (zerop (logand st0 #xC0))
                     (fail "Cannot read the sector (ST0 ~2,'0Xh, ST1 ~2,'0Xh, ST2 ~2,'0Xh): ~A"
                           st0 st1 st2
                           (cond ((logbitp 3 st0) "drive A: is not ready.")
                                 ((logbitp 0 st1) "no disk in A:, or blank?")
                                 ((logbitp 2 st1) "sector not found.")
                                 ((logbitp 5 st1) "the data is damaged.")
                                 (t "no disk in drive A:?")))))
                 (push (format nil "DMA count ~4,'0X" (dma-count 2)) said)
                 (setf interrupts (interrupts))
                 (dotimes (i 512)
                   (setf (aref sector i) (peek (+ address i)))))
            ;; Put it all back, whatever happened; nothing here may wait for long.
            (dma-stop 2)
            (ignore-errors (reset-controller))                  ; which also stops the motor
            ;; the BIOS's notes: no drive calibrated, no motor on, its timer as it was
            (setf (sap-ref-8 (int-sap #x43E) 0) (logand (peek #x43E) #x70)
                  (sap-ref-8 (int-sap #x43F) 0) (logand (peek #x43F) #xF0)
                  (sap-ref-8 (int-sap #x440) 0) (if (= old-timer #xFF) 0 old-timer))
            (int86 #x13 :ax 0 :dx 0)))))                        ; BIOS: reset the disk system
    (values sector (nreverse said) interrupts)))

;;; ---- showing it
(defun show-sector (sector)
  "The 512 bytes on 22 lines of 24: hexadecimal, alternately bright and dim
to tell the bytes apart, the OEM name and the signature in colour; then as text."
  (text-fill 0 0 80 25 7)
  (dotimes (i 512)
    (multiple-value-bind (row column) (floor i 24)
      (let ((byte (aref sector i)))
        (when (zerop column) (text-put 0 row (format nil "~3,'0X" i) 3))
        (text-put (+ 4 (* 2 column)) row (format nil "~2,'0X" byte)
                  (cond ((<= 3 i 10) 14)
                        ((>= i 510) (if (= byte (if (= i 510) #x55 #xAA)) 10 12))
                        ((evenp column) 15)
                        (t 7)))
        (text-put (+ 54 column) row (string (if (< 31 byte 127) (code-char byte) #\.))
                  (if (<= 3 i 10) 14 7))))))

(defun main ()
  "Read the boot sector of the floppy in drive A: by programming the controller and DMA."
  (multiple-value-bind (sector said interrupts)
      (handler-case (read-boot-sector)
        (floppy-failure (failure)
          (format t "~&~A~%" failure)
          (return-from main (values))))
    (let ((signature (and (= (aref sector 510) #x55) (= (aref sector 511) #xAA)))
          (name (map 'string (lambda (byte) (if (< 31 byte 127) (code-char byte) #\.))
                     (subseq sector 3 11))))
      (hide-text-cursor)
      (show-sector sector)
      (text-put 0 22 (format nil "~{~A~^; ~}" (subseq said 0 3)) 8)
      (text-put 0 23 (format nil "~{~A~^; ~}; ~D interrupts" (subseq said 3) interrupts) 8)
      (text-put 0 24 (format nil "Cylinder 0, head 0, sector 1.  Ends in 55 AA: ~:[no~;yes~].  ~
                                  OEM name ~S." signature name)
                15)
      (text-put 72 24 "Any key" 7)
      (wait-key)
      (video-mode 3)
      (show-text-cursor)
      (format t "~&Read the first sector of A: with the controller and DMA 2: ~
                 ~D interrupts.~%~
                 ~:[It does not end in 55 AA: not a boot sector~;It ends in 55 AA: a boot sector~]~
                 .  OEM name: ~S.~%"
              interrupts signature name)))
  (values))

(doslib-examples:define-example "floppy" 'main
  :category :system
  :description "A floppy's boot sector read with the controller and DMA channel 2, not the BIOS.")
