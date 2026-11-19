;;;; Hot keys: keys taken straight from the keyboard interrupt.
;;;;
;;;; (install-hotkey #x58 function) makes F12 call FUNCTION, whatever the
;;;; running program is doing with the keyboard -- the program never sees
;;;; the key.
;;;;
;;;; The BIOS's keyboard handler offers every scan code to interrupt 15h,
;;;; function 4Fh, before it does anything with it, and drops the key if
;;;; told to.  We put a few real-mode instructions on that interrupt: they
;;;; note a hot key in a table and have it dropped, and pass everything else
;;;; on.  Lisp acts on the note at the next safe moment, which is whenever
;;;; the program calls POLL-HOTKEYS -- and WAIT-TICK, KEY-PRESSED-P and
;;;; WAIT-KEY all do, so a program that uses those to pace itself or to look
;;;; for a key needs no change.
;;;;
;;;; (An earlier version took the keyboard interrupt itself and read the
;;;; scan code from the keyboard controller before passing the interrupt
;;;; on.  Reading it lets the controller deliver the next one, so the BIOS,
;;;; reading in its turn, could find a different code there: keys typed
;;;; quickly were lost, or lost their Shift.)
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defvar *hotkey-functions* (make-hash-table) "Scan code -> function.")
(defvar *hotkey-table* nil)             ; 256 bytes: which scan codes are hot keys
(defvar *hotkey-pending* nil)           ; 256 bytes: which have been pressed
(defvar *hotkey-any* nil)               ; a word that is non-zero if any has
(defvar *hotkey-old-vector* nil)        ; (segment . offset) while we are hooked in
(defvar *in-hotkey* nil)

(defun hotkeys-installed-p ()
  "Is any hot key installed?"
  (and *hotkey-old-vector* t))

(defvar *hotkey-segment* nil)           ; the handler and its tables, in DOS memory
(defun hook-keyboard ()
  ;; One block: the code at 0, the table of hot keys at 100h, the presses at
  ;; 200h, the "any" word at 300h, the old vector at 304h.
  (let* ((segment (or *hotkey-segment* (setf *hotkey-segment* (dos-alloc #x310))))
         (memory (int-sap (linear segment))))
    (dotimes (i #x310) (setf (sap-ref-8 memory i) 0))
    (loop for byte in '(#x80 #xFC #x4F                  ; cmp ah,4Fh    the keyboard intercept?
                        #x75 36                         ; jne chain
                        #x53 #x30 #xFF #x88 #xC3        ; push bx / xor bh,bh / mov bl,al
                        #x2E #x80 #xBF #x00 #x01 #x00   ; cmp byte cs:[bx+100h],0
                        #x74 22                         ; je not-hot
                        #x2E #xC6 #x87 #x00 #x02 #x01   ; mov byte cs:[bx+200h],1
                        #x2E #xC6 #x06 #x00 #x03 #x01   ; mov byte cs:[300h],1
                        #x5B                            ; pop bx
                        #x55 #x89 #xE5                  ; push bp / mov bp,sp
                        #x80 #x66 #x06 #xFE             ; and byte [bp+6],FEh   carry clear: drop the key
                        #x5D #xCF                       ; pop bp / iret
                        #x5B                            ; not-hot: pop bx
                        #x2E #xFF #x2E #x04 #x03)       ; chain: jmp far cs:[304h]
          for i from 0
          do (setf (sap-ref-8 memory i) byte))
    (setf *hotkey-table* (int-sap (linear segment #x100))
          *hotkey-pending* (int-sap (linear segment #x200))
          *hotkey-any* (int-sap (linear segment #x300)))
    (multiple-value-bind (ax bx cx dx) (int86 #x31 :ax #x0200 :bx #x15)   ; the real-mode vector
      (declare (ignore ax bx))
      (setf (sap-ref-16 memory #x304) (word16 dx) (sap-ref-16 memory #x306) (word16 cx))
      (int86 #x31 :ax #x0201 :bx #x15 :cx segment :dx 0)
      (setf *hotkey-old-vector* (cons (word16 cx) (word16 dx))))))

(defun install-hotkey (scan-code function)
  "Make the key with SCAN-CODE (F12 is #x58, F11 #x57, Scroll Lock #x46)
call FUNCTION, with no arguments, the next time the program polls."
  (unless *hotkey-old-vector* (hook-keyboard))
  (setf (gethash scan-code *hotkey-functions*) function
        (sap-ref-8 *hotkey-table* scan-code) 1)
  scan-code)

(defun remove-hotkeys ()
  "Give every hot key back to the keyboard, and interrupt 15h to its old handler."
  (when *hotkey-old-vector*
    (int86 #x31 :ax #x0201 :bx #x15 :cx (car *hotkey-old-vector*) :dx (cdr *hotkey-old-vector*))
    (setf *hotkey-old-vector* nil))
  (clrhash *hotkey-functions*)
  nil)

(defun remove-hotkey (scan-code)
  "Give the key with SCAN-CODE back to the keyboard."
  (when *hotkey-old-vector*
    (setf (sap-ref-8 *hotkey-table* scan-code) 0)
    (remhash scan-code *hotkey-functions*)
    (when (zerop (hash-table-count *hotkey-functions*)) (remove-hotkeys)))
  nil)

(defun hotkey-pending-p (scan-code)
  "Has the hot key been pressed, and not yet been acted on?"
  (and *hotkey-old-vector* (/= 0 (sap-ref-8 *hotkey-pending* scan-code))))

(defun clear-pending-hotkeys ()
  "Forget hot keys that have been pressed but not yet acted on."
  (when *hotkey-old-vector*
    (dotimes (i 256) (setf (sap-ref-8 *hotkey-pending* i) 0))
    (setf (sap-ref-32 *hotkey-any* 0) 0)))

(defun poll-hotkeys ()
  "Call the function of any hot key pressed since the last poll.  Cheap when none was."
  (when (and *hotkey-old-vector* (not *in-hotkey*) (/= 0 (sap-ref-32 *hotkey-any* 0)))
    (let ((*in-hotkey* t))
      (setf (sap-ref-32 *hotkey-any* 0) 0)
      (unwind-protect
           (dotimes (scan-code 128)
             (when (/= 0 (sap-ref-8 *hotkey-pending* scan-code))
               (setf (sap-ref-8 *hotkey-pending* scan-code) 0)
               (let ((function (gethash scan-code *hotkey-functions*)))
                 (when function (funcall function)))))
        (clear-pending-hotkeys)))       ; presses made meanwhile do not queue up
    t))
