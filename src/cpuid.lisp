;;;; The CPUID instruction.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; CPUID: void stub(unsigned leaf, unsigned out[4])
(defvar *cpuid* nil "Machine code: the CPUID instruction.")
(defun cpuid (leaf)
  "The processor's answer to CPUID with EAX = LEAF: EAX, EBX, ECX and EDX."
  (alien-funcall (sap-alien *cpuid* (function void unsigned system-area-pointer))
                 leaf *regs*)
  (values (sap-ref-32 *regs* 0) (sap-ref-32 *regs* 4)
          (sap-ref-32 *regs* 8) (sap-ref-32 *regs* 12)))
