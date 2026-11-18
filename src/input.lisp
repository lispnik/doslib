;;;; The BIOS timer tick and keyboard.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; These three are where a program waits, so they are where hot keys
;;; (see hotkey.lisp) get their turn.
(defun wait-tick (&optional (n 1))      ; the BIOS timer ticks 18.2 times a second
  (dotimes (i n)
    (let ((tick (peek32 #x46C)))
      (loop while (= tick (peek32 #x46C)) do (poll-hotkeys)))))
(defun key-pressed-p ()                 ; INT 16h/01h: ZF set if no key
  (poll-hotkeys)
  (zerop (logand (nth-value 5 (int86 #x16 :ax #x0100)) #x40)))
(defun wait-key ()
  (when (hotkeys-installed-p)           ; the BIOS would wait without polling
    (loop until (key-pressed-p)))
  (int86 #x16 :ax 0))
(defun finish () (wait-key) (video-mode 3))
