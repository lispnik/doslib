;;;; Real-mode interrupts through DPMI, memory below 1MB, Ctrl-C and Ctrl-Break.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; Real-mode interrupts.  INT86 goes through the DPMI host's protected-mode
;;; translation, which is no use when the answer is a real-mode segment
;;; (as with DOS's internal tables).  DPMI function 0300h runs the interrupt
;;; in real mode with exactly the registers given and hands them all back.
(defvar *rm* nil "The DPMI real-mode call structure REAL-INT passes the registers in.")
(defun real-int (n &key (ax 0) (bx 0) (cx 0) (dx 0) (si 0) (di 0) (es 0) (ds 0))
  "Real-mode interrupt N.  Returns AX BX CX DX ES DS FLAGS (16-bit values)."
  (dotimes (i 16) (setf (sap-ref-32 *rm* (* 4 i)) 0))
  (setf (sap-ref-32 *rm* #x1C) ax (sap-ref-32 *rm* #x10) bx
        (sap-ref-32 *rm* #x18) cx (sap-ref-32 *rm* #x14) dx
        (sap-ref-32 *rm* 4) si (sap-ref-32 *rm* 0) di
        (sap-ref-16 *rm* #x22) es (sap-ref-16 *rm* #x24) ds)
  (int86 #x31 :ax #x0300 :bx n :di (near-address (sap-int *rm*)))
  (values (sap-ref-16 *rm* #x1C) (sap-ref-16 *rm* #x10) (sap-ref-16 *rm* #x18)
          (sap-ref-16 *rm* #x14) (sap-ref-16 *rm* #x22) (sap-ref-16 *rm* #x24)
          (sap-ref-16 *rm* #x20)))
;;; Some services are not interrupts but routines to call: the extended
;;; memory manager, a mouse driver's handler.  DPMI function 0301h calls a
;;; real-mode routine that returns with RETF.
(defun real-call (segment offset &key (ax 0) (bx 0) (cx 0) (dx 0) (si 0) (di 0) (es 0) (ds 0))
  "Call the real-mode routine at SEGMENT:OFFSET, which returns with a far
return.  Returns AX BX CX DX ES DS FLAGS (16-bit values), as REAL-INT does."
  (dotimes (i 16) (setf (sap-ref-32 *rm* (* 4 i)) 0))
  (setf (sap-ref-32 *rm* #x1C) ax (sap-ref-32 *rm* #x10) bx
        (sap-ref-32 *rm* #x18) cx (sap-ref-32 *rm* #x14) dx
        (sap-ref-32 *rm* 4) si (sap-ref-32 *rm* 0) di
        (sap-ref-16 *rm* #x22) es (sap-ref-16 *rm* #x24) ds
        (sap-ref-16 *rm* #x2A) offset (sap-ref-16 *rm* #x2C) segment)
  (int86 #x31 :ax #x0301 :bx 0 :cx 0 :di (near-address (sap-int *rm*)))
  (values (sap-ref-16 *rm* #x1C) (sap-ref-16 *rm* #x10) (sap-ref-16 *rm* #x18)
          (sap-ref-16 *rm* #x14) (sap-ref-16 *rm* #x22) (sap-ref-16 *rm* #x24)
          (sap-ref-16 *rm* #x20)))

(defvar *dos-blocks* (make-hash-table) "Segment -> the selector DPMI gave it, for DOS-FREE.")
(defun dos-alloc (bytes)
  "Memory below 1MB, which real-mode code (BIOS, DOS) can reach.  Returns its
segment.  It belongs to this process: a saved core does not keep it."
  (multiple-value-bind (ax bx cx dx di flags) (int86 #x31 :ax #x0100 :bx (ceiling bytes 16))
    (declare (ignore bx cx di))
    (when (logbitp 0 flags) (error "No DOS memory"))
    (setf (gethash (word16 ax) *dos-blocks*) (word16 dx))
    (word16 ax)))
(defun dos-free (segment)
  "Give back memory that DOS-ALLOC returned."
  (let ((selector (gethash segment *dos-blocks*)))
    (unless selector
      (error "Segment ~4,'0Xh did not come from DOS-ALLOC in this process" segment))
    (remhash segment *dos-blocks*)
    (int86 #x31 :ax #x0101 :dx selector)
    nil))
(defmacro with-dos-memory ((segment bytes) &body body)
  "Run BODY with SEGMENT bound to BYTES of DOS memory, freed afterwards."
  `(let ((,segment (dos-alloc ,bytes)))
     (unwind-protect (progn ,@body) (dos-free ,segment))))
(defun linear (segment &optional (offset 0))
  "The address, for PEEK and SAP-REF, of real-mode SEGMENT:OFFSET."
  (+ (* 16 segment) offset))

;;; Most calls that need DOS memory need it only until they return: a file
;;; name to pass, a sector to receive.  The library keeps one block for
;;; that, so that nobody has to allocate and free around every call.
(defparameter *scratch-bytes* 16384)
(defvar *scratch* nil)
(defun dos-scratch ()
  "The segment of 16K of DOS memory for short-lived use: what is in it lasts
until the next thing that uses it (DOS-STRING, the functions in disk.lisp)."
  (or *scratch* (setf *scratch* (dos-alloc (+ 16 *scratch-bytes*)))))
(defun dos-string (string &key (segment (dos-scratch)) (offset 0))
  "Copy STRING into DOS memory with a zero after it, as DOS wants its file
names.  Returns the segment and the offset: DS:DX for REAL-INT, say."
  (let ((memory (int-sap (linear segment offset))))
    (loop for c across string for i from 0
          do (setf (sap-ref-8 memory i) (logand (char-code c) 255)))
    (setf (sap-ref-8 memory (length string)) 0))
  (values segment offset))
(defun dos-bytes (segment offset count)
  "COUNT bytes of memory at SEGMENT:OFFSET, as a vector."
  (let ((bytes (make-array count :element-type '(unsigned-byte 8)))
        (memory (int-sap (linear segment offset))))
    (dotimes (i count bytes) (setf (aref bytes i) (sap-ref-8 memory i)))))

;;; Ctrl-C.  With BREAK on -- FreeDOS starts that way -- DOS looks in the
;;; keyboard buffer for Ctrl-C during every call made to it, and ends the
;;; program if it finds one: no unwinding, no hot keys removed.  A program
;;; that reads Ctrl-C as a key, as an editor does, turns the checking off.
(defun break-checking ()
  "Is DOS's BREAK on?  SETF it to true or false to turn it on or off."
  (logbitp 0 (nth-value 3 (int86 #x21 :ax #x3300))))
(defun (setf break-checking) (on)
  "Turn DOS's BREAK on or off."
  (int86 #x21 :ax #x3301 :dx (if on 1 0))
  on)
(defmacro without-break-checking (&body body)
  "Run BODY with DOS's BREAK off, and put it back as it was afterwards."
  (let ((was (gensym "WAS")))
    `(let ((,was (break-checking)))
       (setf (break-checking) nil)
       (unwind-protect (progn ,@body)
         (setf (break-checking) ,was)))))

;;; Ctrl-Break.  The DOS port's runtime turns Ctrl-Break -- and Ctrl, Alt and
;;; Shift pressed together, for keyboards with no Break key -- into what
;;; Ctrl-C is to SBCL on other systems: the condition
;;; SB-SYS:INTERACTIVE-INTERRUPT is signalled in whatever is running, even a
;;; loop that calls nothing, and if nothing handles it the debugger opens
;;; with a restart that carries on.  (Ctrl-C does the same once DOS notices
;;; it, which is when the program next calls DOS.)
(deftype interrupt ()
  "The condition signalled when the user interrupts the program."
  'interrupt-condition)
(defun break-variable (name)
  (let ((address (foreign-variable-address name)))
    (and address (int-sap address))))
(defun interrupts-supported-p ()
  "Can this SBCL be interrupted with Ctrl-Break?  (One built before that was added cannot.)"
  (and (break-variable "dos_break_chord") t))
(defun interrupt-chord ()
  "Does pressing Ctrl, Alt and Shift together interrupt the program?  SETF
it to NIL for a program that wants those keys itself."
  (let ((variable (break-variable "dos_break_chord")))
    (and variable (/= 0 (sap-ref-32 variable 0)))))
(defun (setf interrupt-chord) (on)
  "Say whether Ctrl-Alt-Shift interrupts the program."
  (let ((variable (break-variable "dos_break_chord")))
    (when variable (setf (sap-ref-32 variable 0) (if on 1 0))))
  on)
(defun interrupt-count ()
  "How many times the program has been interrupted from the keyboard."
  (let ((variable (break-variable "dos_break_count")))
    (if variable (sap-ref-32 variable 0) 0)))
(defmacro catching-interrupt (&body body)
  "Run BODY and return its values -- or, if the user interrupts it, stop it
there (its UNWIND-PROTECT forms run) and return :INTERRUPTED."
  `(handler-case (progn ,@body)
     (interrupt-condition () :interrupted)))
