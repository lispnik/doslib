;;;; Which keys are held down.
;;;;
;;;; The BIOS gives keys one at a time as they are typed, and says nothing
;;;; of a key let go: no good for a game, where left and fire are held
;;;; together.  The keyboard itself says more.  It raises interrupt request 1
;;;; for every key pressed and every key released, with a scan code at port
;;;; 60h: the key's number, with 80h added when it is released, and E0h
;;;; first for the keys the AT keyboard added (the separate arrows, right
;;;; Ctrl and Alt).  WITH-KEY-STATE puts a handler on that interrupt (made by
;;;; INSTALL-IRQ-HANDLER) that keeps the scan codes and then passes the
;;;; interrupt on, so the BIOS still sees every key -- Ctrl-Break still
;;;; works.  KEY-DOWN-P reads what has been kept and answers.
;;;;
;;;;   (with-key-state
;;;;     (loop until (key-down-p +key-escape+)
;;;;           do (when (key-down-p +key-left+) ...)
;;;;              (when (key-down-p +key-space+) ...)))
;;;;
;;;; The keys also pile up in the BIOS's buffer of sixteen, and it beeps
;;;; when that is full, so the buffer is emptied as the scan codes are read:
;;;; inside WITH-KEY-STATE, GET-KEY and WAIT-KEY have nothing to give.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defconstant +key-escape+ 1 "The scan code of Esc.")
(defconstant +key-enter+ 28 "The scan code of Enter.")
(defconstant +key-control+ 29 "The scan code of (left) Ctrl.")
(defconstant +key-left-shift+ 42 "The scan code of the left Shift key.")
(defconstant +key-right-shift+ 54 "The scan code of the right Shift key.")
(defconstant +key-alt+ 56 "The scan code of (left) Alt.")
(defconstant +key-space+ 57 "The scan code of the space bar.")
(defconstant +key-up+ 72 "The scan code of the up arrow (and 8 on the number pad).")
(defconstant +key-left+ 75 "The scan code of the left arrow (and 4 on the number pad).")
(defconstant +key-right+ 77 "The scan code of the right arrow (and 6 on the number pad).")
(defconstant +key-down+ 80 "The scan code of the down arrow (and 2 on the number pad).")

(defvar *key-hook* nil "The keyboard interrupt's hook while key state is being kept.")
(defvar *keys-down* (make-array 128 :element-type 'bit :initial-element 0)
  "Scan code -> 1 while that key is held down.")
(defvar *key-changes* 0 "How many presses and releases POLL-KEY-STATE has seen.")

(defun key-state-start ()
  "Begin keeping track of which keys are held down.  Returns true, or NIL
if it was already being done."
  (unless *key-hook*
    (fill *keys-down* 0)
    (setf *key-changes* 0
          *key-hook* (install-irq-handler 1 :buffer #x60 :chain t))
    t))

(defun key-state-stop ()
  "Stop keeping track of the keys, and empty the BIOS's keyboard buffer."
  (when *key-hook*
    (remove-irq-handler *key-hook*)
    (setf *key-hook* nil)
    (setf (sap-ref-16 (int-sap #x41A) 0) (sap-ref-16 (int-sap #x41C) 0)))
  nil)

(defun poll-key-state ()
  "Take in the scan codes that have arrived since last time.  KEY-DOWN-P
and KEYS-DOWN do this themselves.  Returns the number of presses and
releases seen since KEY-STATE-START."
  (when *key-hook*
    (loop for code = (irq-read-byte *key-hook*)
          while code
          do (when (< code #xE0)                ; E0h and E1h only announce what follows
               (setf (sbit *keys-down* (logand code 127)) (if (logbitp 7 code) 0 1))
               (incf *key-changes*)))
    ;; the BIOS has been given them too: empty its buffer (head = tail)
    (setf (sap-ref-16 (int-sap #x41A) 0) (sap-ref-16 (int-sap #x41C) 0)))
  *key-changes*)

(defun key-down-p (scan-code)
  "Is the key with SCAN-CODE (1 to 127: +KEY-LEFT+, +KEY-SPACE+ ...) held
down now?  Inside WITH-KEY-STATE only.  The keys the AT keyboard added
answer to the same codes as the ones they copy: either Ctrl is 29."
  (poll-key-state)
  (= 1 (sbit *keys-down* scan-code)))

(defun keys-down ()
  "The scan codes of all the keys held down now, in order."
  (poll-key-state)
  (loop for code from 1 below 128 when (= 1 (sbit *keys-down* code)) collect code))

(defmacro with-key-state (&body body)
  "Run BODY with the keyboard's presses and releases tracked, so that
KEY-DOWN-P and KEYS-DOWN work; the keyboard interrupt is put back however
BODY ends."
  (let ((started (gensym)))
    `(let ((,started (key-state-start)))
       (unwind-protect (progn ,@body)
         (when ,started (key-state-stop))))))
