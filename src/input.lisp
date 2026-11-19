;;;; The BIOS timer tick and keyboard.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; These three are where a program waits, so they are where hot keys
;;; (see hotkey.lisp) get their turn.
(defun ticks ()
  "The BIOS's count of timer ticks: 18.2 a second, zero at midnight."
  (peek32 #x46C))
(defun wait-tick (&optional (n 1))
  "Wait for N ticks of the BIOS timer: 18.2 to the second."
  (dotimes (i n)
    (let ((tick (peek32 #x46C)))
      (loop while (= tick (peek32 #x46C)) do (poll-hotkeys)))))
(defun key-pressed-p ()
  "Is a key waiting to be read?"
  (poll-hotkeys)
  (zerop (logand (nth-value 5 (int86 #x16 :ax #x0100)) #x40)))
(defun wait-key ()
  "Wait for a key.  Returns the BIOS's word for it: the scan code in the
high byte, the character in the low.  GET-KEY gives it a name."
  (when (hotkeys-installed-p)           ; the BIOS would wait without polling
    (loop until (key-pressed-p)))
  (int86 #x16 :ax 0))
(defun finish ()
  "Wait for a key, then go back to 80x25 text: how a picture ends."
  (wait-key) (video-mode 3))
