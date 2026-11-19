;;;; The Sound Blaster: where the card is, its processor (the "DSP"), and
;;;; its interrupt.
;;;;
;;;; A Sound Blaster is found by what the BLASTER environment variable says:
;;;; A220 I5 D1 H5 is the usual, the ports from 220h, interrupt 5, DMA
;;;; channel 1 for 8-bit sound and (a Sound Blaster 16) 5 for 16-bit.
;;;; BLASTER-SETTINGS reads it, BLASTER-PRESENT-P reads it and resets the
;;;; card to see whether one answers.
;;;;
;;;; The card's processor takes commands a byte at a time at port 2xCh, when
;;;; bit 7 there is clear, and answers at 2xAh, when bit 7 of 2xEh is set:
;;;; BLASTER-WRITE and BLASTER-READ.  What the commands are is the
;;;; program's business -- the examples "blaster", "dmascope", "record" and
;;;; "modplay" play and record with them -- and the samples reach the card
;;;; through a DMA channel (dma.lisp).
;;;;
;;;; The card interrupts when it has played, or recorded, a block of
;;;; samples.  A handler for a hardware interrupt cannot be Lisp: the
;;;; interrupt may arrive in the middle of anything, the garbage collector
;;;; included.  So the handler here is some thirty bytes of machine code
;;;; that tells the card and the interrupt controller the interrupt has been
;;;; seen and adds one to a counter; Lisp watches the counter
;;;; (BLASTER-INTERRUPTS) from inside WITH-BLASTER-INTERRUPT, which puts the
;;;; handler in place and takes it away again.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defvar *blaster-port* #x220 "The Sound Blaster's first port: BLASTER-SETTINGS sets it.")
(defvar *blaster-irq* 5 "The Sound Blaster's interrupt request line: BLASTER-SETTINGS sets it.")
(defvar *blaster-dma* 1 "The Sound Blaster's DMA channel for 8-bit sound: BLASTER-SETTINGS sets it.")
(defvar *blaster-dma16* 5
  "A Sound Blaster 16's DMA channel for 16-bit sound: BLASTER-SETTINGS sets it.")

(defun blaster-settings (&optional (text (sb-ext:posix-getenv "BLASTER")))
  "Take the card's port, interrupt and DMA channels from TEXT, which is the
BLASTER environment variable unless given: \"A220 I5 D1 H5\" is port 220h,
interrupt 5, and DMA channels 1 (8 bits) and 5 (16 bits).  What it does not
say, or all of it if it is not set, is as in that example.  Sets
*BLASTER-PORT*, *BLASTER-IRQ*, *BLASTER-DMA* and *BLASTER-DMA16*, and returns
the four."
  (setf *blaster-port* #x220 *blaster-irq* 5 *blaster-dma* 1 *blaster-dma16* 5)
  (when text
    (loop with start = 0
          for end = (or (position #\Space text :start start) (length text))
          do (when (> end (1+ start))
               (let ((number (subseq text (1+ start) end)))
                 (flet ((value (radix default)
                          (or (parse-integer number :radix radix :junk-allowed t) default)))
                   (case (char-upcase (char text start))
                     (#\A (setf *blaster-port* (value 16 *blaster-port*)))
                     (#\I (setf *blaster-irq* (value 10 *blaster-irq*)))
                     (#\D (setf *blaster-dma* (value 10 *blaster-dma*)))
                     (#\H (setf *blaster-dma16* (value 10 *blaster-dma16*)))))))
             (when (>= end (length text)) (return))
             (setf start (1+ end))))
  (values *blaster-port* *blaster-irq* *blaster-dma* *blaster-dma16*))

;;; ---- the card's processor
(defun blaster-write (byte)
  "Send BYTE, a command or part of one, to the card's processor, when it is
ready for it."
  (loop repeat 100000 while (logbitp 7 (inb (+ *blaster-port* #xC))))
  (outb (+ *blaster-port* #xC) byte))
(defun blaster-read ()
  "The next byte of the card's processor's answer, when there is one."
  (loop repeat 100000 until (logbitp 7 (inb (+ *blaster-port* #xE))))
  (inb (+ *blaster-port* #xA)))
(defun blaster-reset ()
  "Reset the card's processor: whatever it was playing or recording stops.
True if a card answered at *BLASTER-PORT*."
  (outb (+ *blaster-port* 6) 1)
  (dotimes (i 200) (inb (+ *blaster-port* 6)))          ; a few microseconds
  (outb (+ *blaster-port* 6) 0)
  (loop repeat 2000
        thereis (and (logbitp 7 (inb (+ *blaster-port* #xE)))
                     (= (inb (+ *blaster-port* #xA)) #xAA))))
(defun blaster-version ()
  "The version of the card's processor, as two values: 2 and up plays
without a gap (auto-initialized DMA), 3 is a Sound Blaster Pro, 4 a Sound
Blaster 16."
  (blaster-write #xE1)
  (let* ((major (blaster-read)) (minor (blaster-read)))
    (values major minor)))
(defun blaster-present-p ()
  "Is there a Sound Blaster this library can use?  Reads the settings
(BLASTER-SETTINGS) and resets the card: true if the interrupt is from 2 to
7, the 8-bit DMA channel is 0, 1 or 3, and a card answered."
  (blaster-settings)
  (and (<= 0 *blaster-dma* 3) (/= *blaster-dma* 2) (<= 2 *blaster-irq* 7) (blaster-reset)))

;;; ---- the interrupt.  The counter and the handler are in the process's
;;; memory, not the Lisp image's: DOSLIB-INIT forgets them, and they are
;;; made again when next wanted.
(defvar *blaster-counter* nil "Four bytes the handler counts interrupts in.")
(defvar *blaster-handler* nil "(port . code): the handler, made for the card at that port.")
(defun blaster-handler ()
  "The address of the interrupt handler: acknowledge, count, return."
  (unless *blaster-counter*
    (setf *blaster-counter* (alien-sap (make-alien (unsigned 32) 1))))
  (unless (eql (car *blaster-handler*) *blaster-port*)
    (let ((ds (data-selector)) (acknowledge (+ *blaster-port* #xE)))
      (setf *blaster-handler*
            (cons *blaster-port*
                  (apply #'machine-code
                         `(#x50 #x52 #x1E                   ; push eax / push edx / push ds
                           #x66 #xB8 ,(lo ds) ,(hi ds)      ; mov ax, our data selector
                           #x8E #xD8                        ; mov ds, ax
                           #xBA ,(lo acknowledge) ,(hi acknowledge) 0 0   ; mov edx, port+0Eh
                           #xEC                             ; in al, dx     the card: seen
                           #x42 #xEC                        ; inc edx / in al, dx   (16-bit: 2xFh)
                           #xFF #x05 ,@(bytes32 (sap-int *blaster-counter*))
                                                            ; inc dword [counter]
                           #xB0 #x20 #xE6 #x20              ; mov al,20h / out 20h,al
                                                            ;   the interrupt controller: done
                           #x1F #x5A #x58 #xCF))))))        ; pop ds / pop edx / pop eax / iretd
  (sap-int (cdr *blaster-handler*)))
(defun blaster-interrupts ()
  "How many times the card has interrupted since WITH-BLASTER-INTERRUPT
began: once for each block of samples played or recorded."
  (if *blaster-counter* (sap-ref-32 *blaster-counter* 0) 0))

(defun call-with-blaster-interrupt (function)
  (let ((vector (+ 8 *blaster-irq*)) (bit (ash 1 *blaster-irq*))
        (handler (blaster-handler))
        (old-mask (inb #x21)))
    (setf (sap-ref-32 *blaster-counter* 0) 0)
    (multiple-value-bind (old-selector old-offset) (get-vector vector)
      (unwind-protect
           (progn (set-vector vector (code-selector) handler)
                  (outb #x21 (logandc2 old-mask bit))   ; let the interrupt through
                  (funcall function))
        (outb #x21 (logior (logandc2 (inb #x21) bit) (logand old-mask bit)))  ; the bit as it was
        (set-vector vector old-selector old-offset)
        (blaster-reset)))))
(defmacro with-blaster-interrupt (() &body body)
  "Run BODY with the card's interrupts counted: BLASTER-INTERRUPTS, which
starts at 0, says how many there have been.  A handler in machine code is
put on the card's interrupt (*BLASTER-IRQ*, from 2 to 7) and the interrupt
controller is told to let it through; however BODY ends, the controller's
mask and the vector are put back as they were, and the card is reset.
Stopping what the card was doing (and its DMA channel) first is for BODY
to do, with UNWIND-PROTECT."
  `(call-with-blaster-interrupt (lambda () ,@body)))
