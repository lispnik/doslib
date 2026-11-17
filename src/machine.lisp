;;;; Machine code made at run time: software interrupts and byte helpers.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defun machine-code (&rest bytes)
  (let ((p (alien-sap (make-alien (unsigned 8) (length bytes)))))
    (loop for b in bytes for i from 0 do (setf (sap-ref-8 p i) b))
    p))

;;; void stub(unsigned regs[6]): load EAX EBX ECX EDX EDI, INT n, store them
;;; back along with EFLAGS.
(defun interrupt-stub (n)
  (machine-code #x53 #x56 #x57                 ; push ebx / esi / edi
                #x8B #x74 #x24 #x10            ; mov esi,[esp+16]
                #x8B #x06 #x8B #x5E 4 #x8B #x4E 8 #x8B #x56 12 #x8B #x7E 16
                #xCD n                         ; int n
                #x89 #x06 #x89 #x5E 4 #x89 #x4E 8 #x89 #x56 12 #x89 #x7E 16
                #x9C #x58 #x89 #x46 20         ; pushfd / pop eax / mov [esi+20],eax
                #x5F #x5E #x5B #xC3))          ; pop edi / esi / ebx, ret
(defvar *regs*)
(defvar *stubs* (make-hash-table))

(defun int86 (n &key (ax 0) (bx 0) (cx 0) (dx 0) (di 0))
  "Software interrupt N.  Returns AX BX CX DX DI FLAGS as 32-bit values."
  (let ((stub (or (gethash n *stubs*)
                  (setf (gethash n *stubs*) (interrupt-stub n)))))
    (setf (sap-ref-32 *regs* 0) ax (sap-ref-32 *regs* 4) bx
          (sap-ref-32 *regs* 8) cx (sap-ref-32 *regs* 12) dx
          (sap-ref-32 *regs* 16) di)
    (alien-funcall (sap-alien stub (function void system-area-pointer)) *regs*)
    (values (sap-ref-32 *regs* 0) (sap-ref-32 *regs* 4) (sap-ref-32 *regs* 8)
            (sap-ref-32 *regs* 12) (sap-ref-32 *regs* 16) (sap-ref-32 *regs* 20))))
(defun lo (x) (logand x #xFF))
(defun hi (x) (logand (ash x -8) #xFF))
(defun word16 (x) (logand x #xFFFF))
