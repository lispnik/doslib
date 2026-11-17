;;;; A Lisp listener on COM1.  The 16550 UART is programmed
;;;; directly through its I/O ports (no BIOS, no DOS, no interrupts: the
;;;; line status register is polled), and whatever arrives is READ, EVALed
;;;; and PRINTed back down the wire.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.serial
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.serial)

(defconstant +com1+ #x3F8)
(defun serial-init (&optional (baud 115200))
  (let ((divisor (floor 115200 baud)))
    (outb (+ +com1+ 1) 0)               ; no interrupts
    (outb (+ +com1+ 3) #x80)            ; divisor latch on
    (outb +com1+ (lo divisor)) (outb (+ +com1+ 1) (hi divisor))
    (outb (+ +com1+ 3) #x03)            ; 8 data bits, no parity, 1 stop bit
    (outb (+ +com1+ 2) #xC7)            ; FIFOs on and cleared
    (outb (+ +com1+ 4) #x03)))
          ; DTR and RTS
(defun serial-write-byte (byte)
  (loop until (logbitp 5 (inb (+ +com1+ 5))))   ; transmit holding register empty?
  (outb +com1+ byte))
(defun serial-read-byte ()
  (loop until (logbitp 0 (inb (+ +com1+ 5))))   ; data ready?
  (inb +com1+))
(defun serial-write (string)
  (loop for c across string
        do (when (char= c #\Newline) (serial-write-byte 13))
           (serial-write-byte (logand (char-code c) #xFF))))
(defun serial-read-line ()
  (with-output-to-string (line)
    (loop for byte = (serial-read-byte)
          until (member byte '(10 13))
          do (write-char (code-char byte) line))))
(defun listener ()
  (serial-init)
  (format t "~&Listening on COM1 (~Xh), 115200 8N1.  Send :BYE to stop.~%" +com1+)
  (serial-write (format nil "~%~A ~A on ~A ~A~%" (lisp-implementation-type)
                        (lisp-implementation-version) (software-type) (software-version)))
  (loop
    (serial-write "COM1> ")
    (let ((line (serial-read-line)))
      (unless (string= line "")
        (format t "<- ~A~%" line)
        (let ((reply (handler-case
                         (let* ((form (read-from-string line))
                                (values nil)
                                (output (with-output-to-string (*standard-output*)
                                          (setf values (multiple-value-list (eval form))))))
                           (when (eq form :bye)
                             (serial-write (format nil "Goodbye.~%"))
                             (return))
                           (format nil "~A~{~S~%~}" output values))
                       (error (e) (format nil "ERROR (~A): ~A~%" (type-of e) e)))))
          (format t "-> ~A" reply)
          (serial-write reply))))))

(defun main ()
  "A Lisp listener on COM1."
  (listener))

(doslib-examples:define-example "serial" 'main
  :category :system
  :description "A Lisp listener on COM1.")
