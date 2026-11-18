;;;; Protected-mode interrupt vectors, through DPMI.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defvar *get-cs*)                       ; mov eax,cs / ret
(defvar *get-ds*)                       ; mov eax,ds / ret
(defun code-selector ()
  "The selector our code runs in: what an interrupt vector must name."
  (alien-funcall (sap-alien *get-cs* (function unsigned))))
(defun data-selector ()
  "The selector of our (flat) data segment, for handlers that must load DS."
  (alien-funcall (sap-alien *get-ds* (function unsigned))))
(defun bytes32 (x)
  "X as four bytes, low first: for putting addresses into machine code."
  (loop for i below 4 collect (ldb (byte 8 (* 8 i)) x)))
(defun get-vector (n)
  "The handler of protected-mode interrupt N: its selector and offset (DPMI 0204h)."
  (multiple-value-bind (ax bx cx dx) (int86 #x31 :ax #x0204 :bx n)
    (declare (ignore ax bx))
    (values (word16 cx) dx)))
(defun set-vector (n selector offset)
  "Point protected-mode interrupt N at SELECTOR:OFFSET (DPMI 0205h)."
  (int86 #x31 :ax #x0205 :bx n :cx selector :dx offset))
