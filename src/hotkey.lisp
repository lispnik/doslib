;;;; Hot keys: keys taken straight from the keyboard interrupt.
;;;;
;;;; (install-hotkey #x58 function) makes F12 call FUNCTION, whatever the
;;;; running program is doing with the keyboard -- the program never sees
;;;; the key.
;;;;
;;;; The keyboard interrupt (IRQ 1, vector 9) can arrive at any instant,
;;;; including in the middle of a garbage collection, so its handler cannot
;;;; be Lisp.  It is a few machine instructions, assembled here, that look
;;;; at the scan code: a hot key is noted in a table and swallowed, anything
;;;; else goes on to the BIOS as usual.  Lisp acts on the note at the next
;;;; safe moment, which is whenever the program calls POLL-HOTKEYS -- and
;;;; WAIT-TICK, KEY-PRESSED-P and WAIT-KEY all do, so a program that uses
;;;; those to pace itself or to look for a key needs no change.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defvar *hotkey-functions* (make-hash-table) "Scan code -> function.")
(defvar *hotkey-table* nil)             ; 256 bytes: which scan codes are hot keys
(defvar *hotkey-pending* nil)           ; 256 bytes: which have been pressed
(defvar *hotkey-any* nil)               ; a word that is non-zero if any has
(defvar *hotkey-old-vector* nil)        ; (selector . offset) while we are hooked in
(defvar *in-hotkey* nil)

(defun hotkeys-installed-p () (and *hotkey-old-vector* t))

(defun hook-keyboard ()
  (flet ((zeroed (n) (let ((p (alien-sap (make-alien (unsigned 8) n))))
                       (dotimes (i n) (setf (sap-ref-8 p i) 0))
                       p)))
    (setf *hotkey-table* (zeroed 256) *hotkey-pending* (zeroed 256) *hotkey-any* (zeroed 4)))
  (multiple-value-bind (old-selector old-offset) (get-vector 9)
    (let* ((ds (data-selector))
           (isr (apply #'machine-code
                       `(#x1E #x50                              ; push ds / push eax
                         #x66 #xB8 ,(lo ds) ,(hi ds) #x8E #xD8  ; mov ax,flat / mov ds,ax
                         #xE4 #x60                              ; in al,60h     the scan code
                         #x0F #xB6 #xC0                         ; movzx eax,al
                         #x80 #xB8 ,@(bytes32 (sap-int *hotkey-table*)) 0   ; cmp byte [table+eax],0
                         #x74 36                                ; je chain      not a hot key
                         #xC6 #x80 ,@(bytes32 (sap-int *hotkey-pending*)) 1 ; mov byte [pending+eax],1
                         #xC7 #x05 ,@(bytes32 (sap-int *hotkey-any*)) 1 0 0 0 ; mov dword [any],1
                         #xE4 #x61 #x88 #xC4 #x0C #x80 #xE6 #x61 ; acknowledge the key
                         #x88 #xE0 #xE6 #x61                    ;   at the keyboard controller
                         #xB0 #x20 #xE6 #x20                    ; mov al,20h / out 20h,al   end of interrupt
                         #x58 #x1F #xCF                         ; pop eax / pop ds / iretd
                         #x58 #x1F                              ; chain: pop eax / pop ds
                         #xEA ,@(bytes32 old-offset)            ; jmp far the old handler
                         ,(lo old-selector) ,(hi old-selector)))))
      (set-vector 9 (code-selector) (sap-int isr))
      (setf *hotkey-old-vector* (cons old-selector old-offset)))))

(defun install-hotkey (scan-code function)
  "Make the key with SCAN-CODE (F12 is #x58, F11 #x57, Scroll Lock #x46)
call FUNCTION, with no arguments, the next time the program polls."
  (unless *hotkey-old-vector* (hook-keyboard))
  (setf (gethash scan-code *hotkey-functions*) function
        (sap-ref-8 *hotkey-table* scan-code) 1)
  scan-code)

(defun remove-hotkeys ()
  "Give every hot key back to the keyboard, and the keyboard interrupt to its old handler."
  (when *hotkey-old-vector*
    (set-vector 9 (car *hotkey-old-vector*) (cdr *hotkey-old-vector*))
    (setf *hotkey-old-vector* nil))
  (clrhash *hotkey-functions*)
  nil)

(defun remove-hotkey (scan-code)
  (when *hotkey-old-vector*
    (setf (sap-ref-8 *hotkey-table* scan-code) 0)
    (remhash scan-code *hotkey-functions*)
    (when (zerop (hash-table-count *hotkey-functions*)) (remove-hotkeys)))
  nil)

(defun hotkey-pending-p (scan-code)
  "Has the hot key been pressed, and not yet been acted on?"
  (and *hotkey-old-vector* (/= 0 (sap-ref-8 *hotkey-pending* scan-code))))

(defun clear-pending-hotkeys ()
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
