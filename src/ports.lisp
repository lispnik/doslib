;;;; I/O ports, 8, 16 and 32 bits wide, and the CMOS memory.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; I/O ports
(defvar *inb* nil "Machine code: read a byte from a port.")
(defvar *outb* nil "Machine code: write a byte to a port.")
(defun inb (port)
  "Read a byte from I/O port PORT."
  (alien-funcall (sap-alien *inb* (function unsigned int)) port))
(defun outb (port value)
  "Write the byte VALUE to I/O port PORT."
  (alien-funcall (sap-alien *outb* (function void int int)) port value))

;;; 16-bit ports
(defvar *inw*)
(defvar *outw*)
(defun inw (port)
  "Read 16 bits from I/O port PORT."
  (alien-funcall (sap-alien *inw* (function unsigned int)) port))
(defun outw (port value)
  "Write the 16 bits of VALUE to I/O port PORT."
  (alien-funcall (sap-alien *outw* (function void int int)) port value))

;;; 32-bit ports (PCI configuration space)
(defvar *inl* nil "Machine code: read 32 bits from a port.")
(defvar *outl* nil "Machine code: write 32 bits to a port.")
(defun inl (port)
  "Read 32 bits from I/O port PORT."
  (alien-funcall (sap-alien *inl* (function unsigned int)) port))
(defun outl (port value)
  "Write the 32 bits of VALUE to I/O port PORT."
  (alien-funcall (sap-alien *outl* (function void int unsigned)) port value))

;;; The CMOS memory of the clock chip: a register number goes to port 70h,
;;; and the byte is then at port 71h.
(defun cmos (register)
  "Byte REGISTER (0 to 127) of the CMOS memory: the clock, and the BIOS's settings."
  (outb #x70 (logand register #x7F))
  (inb #x71))
(defun bcd (byte)
  "The number that BYTE holds as two decimal digits, as the clock keeps its."
  (+ (* 10 (ash byte -4)) (logand byte 15)))
