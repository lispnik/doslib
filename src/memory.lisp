;;;; Reading memory by address.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; Memory: the DPMI client's address space is flat and starts at zero, so
;;; the first megabyte (BIOS data, video memory, ROM) is just there.
(defun peek (address) (sap-ref-8 (int-sap address) 0))
(defun peek16 (address) (sap-ref-16 (int-sap address) 0))
(defun peek32 (address) (sap-ref-32 (int-sap address) 0))
