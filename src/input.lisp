;;;; The BIOS timer tick and keyboard.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defun wait-tick (&optional (n 1))      ; the BIOS timer ticks 18.2 times a second
  (dotimes (i n)
    (let ((tick (peek32 #x46C))) (loop while (= tick (peek32 #x46C))))))
(defun key-pressed-p ()                 ; INT 16h/01h: ZF set if no key
  (zerop (logand (nth-value 5 (int86 #x16 :ax #x0100)) #x40)))
(defun wait-key () (int86 #x16 :ax 0))
(defun finish () (wait-key) (video-mode 3))
