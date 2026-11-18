;;;; DOS critical errors: "Abort, Retry, Fail?" answered by the program.
;;;;
;;;; When a device fails -- no disk in the drive, a printer off line -- DOS
;;;; calls interrupt 24h, and the handler it finds there by default is the
;;;; one that prints "Abort, Ignore, Retry, Fail?" over whatever is on the
;;;; screen and waits.  (catch-critical-errors) points the interrupt at a
;;;; handler of ours instead.  It is real-mode code, since DOS calls it in
;;;; real mode: it notes the error code and answers "Fail", so the DOS call
;;;; in progress returns an error to the program like any other.  The
;;;; program then asks (critical-error) what went wrong, and decides for
;;;; itself what to do about it.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defvar *critical-stub* nil)            ; the handler's segment, in DOS memory
(defvar *critical-old-vector* nil)      ; (segment . offset) to put back

(defparameter *critical-error-names*
  #("disk is write-protected" "unknown unit" "drive not ready" "unknown command"
    "data error" "bad request" "seek error" "unknown media type" "sector not found"
    "printer out of paper" "write fault" "read fault" "general failure"))

(defun catch-critical-errors ()
  "From now on DOS device errors fail quietly, and (critical-error) tells of them."
  (unless *critical-stub*
    (let ((segment (dos-alloc 32)))
      (loop for byte in '(#x2E #x89 #x3E #x10 #x00        ; mov cs:[0010h],di   the error code
                          #x2E #xC6 #x06 #x12 #x00 #x01   ; mov byte cs:[0012h],1
                          #xB0 #x03 #xCF)                 ; mov al,3 (fail) / iret
            for i from 0
            do (setf (sap-ref-8 (int-sap (linear segment)) i) byte))
      (setf (sap-ref-32 (int-sap (linear segment #x10)) 0) 0)
      (multiple-value-bind (ax bx cx dx) (int86 #x31 :ax #x0200 :bx #x24)   ; the old vector
        (declare (ignore ax bx))
        (setf *critical-old-vector* (cons (word16 cx) (word16 dx))))
      (int86 #x31 :ax #x0201 :bx #x24 :cx segment :dx 0)
      (setf *critical-stub* segment)))
  t)

(defun release-critical-errors ()
  "Give interrupt 24h back to its previous handler."
  (when *critical-stub*
    (int86 #x31 :ax #x0201 :bx #x24 :cx (car *critical-old-vector*) :dx (cdr *critical-old-vector*))
    (setf *critical-stub* nil))
  nil)

(defun critical-error ()
  "If a device has failed since this was last asked, the description of the
failure (and its code); otherwise NIL.  Asking clears it."
  (when (and *critical-stub* (/= 0 (peek (linear *critical-stub* #x12))))
    (let ((code (lo (peek16 (linear *critical-stub* #x10)))))
      (setf (sap-ref-8 (int-sap (linear *critical-stub* #x12)) 0) 0)
      (values (if (< code (length *critical-error-names*))
                  (aref *critical-error-names* code)
                  "device error")
              code))))
