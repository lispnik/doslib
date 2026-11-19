;;;; Serial ports, by polling the UART: no interrupts.
;;;;
;;;; A port is its base I/O address: #x3F8 for COM1, #x2F8 for COM2, or
;;;; whatever (serial-port n) finds in the BIOS's table.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defun serial-port (n)
  "The base address of COMn (1 to 4) as the BIOS found it, or NIL if there is none."
  (let ((address (peek16 (+ #x400 (* 2 (1- n))))))
    (and (plusp address) address)))
(defun serial-open (&key (port #x3F8) (baud 115200))
  "Set PORT up for BAUD, 8 data bits, no parity, 1 stop bit.  Returns PORT."
  (let ((divisor (floor 115200 baud)))
    (outb (+ port 1) 0)                 ; no interrupts
    (outb (+ port 3) #x80)              ; divisor latch on
    (outb port (lo divisor)) (outb (+ port 1) (hi divisor))
    (outb (+ port 3) #x03)              ; 8 data bits, no parity, 1 stop bit
    (outb (+ port 2) #xC7)              ; FIFOs on and cleared
    (outb (+ port 4) #x03))             ; DTR and RTS
  port)
(defun serial-byte-ready-p (port)
  "Has a byte arrived?"
  (logbitp 0 (inb (+ port 5))))
(defun serial-read-byte (port &optional (wait t))
  "The next byte received; with WAIT false, NIL if none has arrived."
  (if wait
      (loop until (serial-byte-ready-p port) do (poll-hotkeys))
      (unless (serial-byte-ready-p port) (return-from serial-read-byte nil)))
  (inb port))
(defun serial-write-byte (port byte)
  "Send BYTE, waiting until the port can take it."
  (loop until (logbitp 5 (inb (+ port 5))))     ; transmit holding register empty?
  (outb port byte)
  byte)
(defun serial-write-string (port string)
  "Send STRING, with a carriage return before every newline."
  (loop for c across string
        do (when (char= c #\Newline) (serial-write-byte port 13))
           (serial-write-byte port (logand (char-code c) #xFF)))
  string)
(defun serial-read-line (port)
  "Bytes received up to a carriage return or a line feed, as a string."
  (with-output-to-string (line)
    (loop for byte = (serial-read-byte port)
          until (member byte '(10 13))
          do (write-char (code-char byte) line))))
