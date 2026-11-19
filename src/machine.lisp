;;;; Machine code made at run time: software interrupts and byte helpers.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defun machine-code (&rest bytes)
  "Put BYTES in memory as machine code.  Returns its address (a system area
pointer), to be called with ALIEN-FUNCALL.  The memory is the process's, not
the Lisp image's: see SAVED-CODE."
  (let ((p (alien-sap (make-alien (unsigned 8) (length bytes)))))
    (loop for b in bytes for i from 0 do (setf (sap-ref-8 p i) b))
    p))

;;; void stub(unsigned regs[6]): load EAX EBX ECX EDX EDI, INT n, store them
;;; back along with EFLAGS.
(defun interrupt-stub (n)
  "Machine code for the function INT86 calls to do interrupt N: it loads
EAX, EBX, ECX, EDX and EDI from a block of words, does INT N, and stores
them back with the flags."
  (machine-code #x53 #x56 #x57                 ; push ebx / esi / edi
                #x8B #x74 #x24 #x10            ; mov esi,[esp+16]
                #x8B #x06 #x8B #x5E 4 #x8B #x4E 8 #x8B #x56 12 #x8B #x7E 16
                #xCD n                         ; int n
                #x89 #x06 #x89 #x5E 4 #x89 #x4E 8 #x89 #x56 12 #x89 #x7E 16
                #x9C #x58 #x89 #x46 20         ; pushfd / pop eax / mov [esi+20],eax
                #x5F #x5E #x5B #xC3))          ; pop edi / esi / ebx, ret
(defvar *regs* nil "The block of words INT86 passes the registers in.")
(defvar *stubs* (make-hash-table) "Interrupt number -> its INTERRUPT-STUB, made when first wanted.")

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
(defun lo (x)
  "The low byte of X: AL of AX."
  (logand x #xFF))
(defun hi (x)
  "The second byte of X: AH of AX."
  (logand (ash x -8) #xFF))
(defun word16 (x)
  "The low 16 bits of X: AX of EAX."
  (logand x #xFFFF))

;;; Memory outside the Lisp heap -- machine code from MACHINE-CODE, DOS
;;; memory -- belongs to the process, not to the Lisp image: a core saved
;;; with SAVE-LISP-AND-DIE does not contain it, and an address kept in a
;;; variable is stale when that core starts.  Code that should work from a
;;; saved core is kept as its bytes, and put into memory again in each new
;;; process, when it is first wanted there.
(defvar *session* 0 "Which process of this image we are in: DOSLIB-INIT counts them.")
(defstruct (saved-code (:constructor saved-code (bytes)))
  "Machine code kept as its bytes, so that it survives in a saved core:
(saved-code bytes) makes one, and CODE-ADDRESS puts it in memory."
  bytes address (session -1))
(defun code-address (saved-code)
  "Where SAVED-CODE is in memory, in this process."
  (unless (= (saved-code-session saved-code) *session*)
    (setf (saved-code-address saved-code) (apply #'machine-code (saved-code-bytes saved-code))
          (saved-code-session saved-code) *session*))
  (saved-code-address saved-code))
