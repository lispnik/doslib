;;;; I/O ports, 8 and 32 bits wide.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; I/O ports
(defvar *inb*)
(defvar *outb*)
(defun inb (port)
  (alien-funcall (sap-alien *inb* (function unsigned int)) port))
(defun outb (port value)
  (alien-funcall (sap-alien *outb* (function void int int)) port value))

;;; 32-bit ports (PCI configuration space)
(defvar *inl*)
(defvar *outl*)
(defun inl (port)
  (alien-funcall (sap-alien *inl* (function unsigned int)) port))
(defun outl (port value)
  (alien-funcall (sap-alien *outl* (function void int unsigned)) port value))
