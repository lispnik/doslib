;;;; Real-mode interrupts through DPMI, and memory below 1MB.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; Real-mode interrupts.  INT86 goes through the DPMI host's protected-mode
;;; translation, which is no use when the answer is a real-mode segment
;;; (as with DOS's internal tables).  DPMI function 0300h runs the interrupt
;;; in real mode with exactly the registers given and hands them all back.
(defvar *rm*)
(defun real-int (n &key (ax 0) (bx 0) (cx 0) (dx 0) (si 0) (di 0) (es 0) (ds 0))
  "Real-mode interrupt N.  Returns AX BX CX DX ES DS FLAGS (16-bit values)."
  (dotimes (i 64) (setf (sap-ref-8 *rm* i) 0))
  (setf (sap-ref-32 *rm* #x1C) ax (sap-ref-32 *rm* #x10) bx
        (sap-ref-32 *rm* #x18) cx (sap-ref-32 *rm* #x14) dx
        (sap-ref-32 *rm* 4) si (sap-ref-32 *rm* 0) di
        (sap-ref-16 *rm* #x22) es (sap-ref-16 *rm* #x24) ds)
  (int86 #x31 :ax #x0300 :bx n :di (sap-int *rm*))
  (values (sap-ref-16 *rm* #x1C) (sap-ref-16 *rm* #x10) (sap-ref-16 *rm* #x18)
          (sap-ref-16 *rm* #x14) (sap-ref-16 *rm* #x22) (sap-ref-16 *rm* #x24)
          (sap-ref-16 *rm* #x20)))
(defun dos-alloc (bytes)
  "Memory below 1MB, which real-mode code (BIOS, DOS) can reach.  Returns its segment."
  (multiple-value-bind (ax bx cx dx di flags) (int86 #x31 :ax #x0100 :bx (ceiling bytes 16))
    (declare (ignore bx cx dx di))
    (when (logbitp 0 flags) (error "No DOS memory"))
    (word16 ax)))
(defun linear (segment &optional (offset 0)) (+ (* 16 segment) offset))
