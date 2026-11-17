;;;; Three old PC devices driven through their I/O ports: the
;;;; keyboard controller (to set the keyboard's lights), the parallel
;;;; printer port (to print a line), and the game port (to look for a
;;;; joystick).  No BIOS and no DOS: just IN and OUT.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.ports
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.ports)

;;; --- keyboard: controller status at port 64h, data at 60h
(defun keyboard-send (byte)
  (loop while (logbitp 1 (inb #x64)))           ; wait until it will take a byte
  (outb #x60 byte)
  (loop repeat 100000 until (logbitp 0 (inb #x64)))     ; wait for the answer
  (inb #x60))
                                   ; FAh means "acknowledged"
(defun keyboard-leds (scroll num caps)
  (let ((mask (inb #x21)))
    (outb #x21 (logior mask 2))                 ; keep the BIOS (IRQ 1) out of it
    (prog1 (list (keyboard-send #xED)           ; "set indicators", then which ones
                 (keyboard-send (+ (if scroll 1 0) (if num 2 0) (if caps 4 0))))
      (outb #x21 mask))))
;;; --- parallel port: data at base, status at base+1, control at base+2
(defun printer-status (base)
  (let ((status (inb (+ base 1))))
    (format nil "~:[busy~;ready~]~:[~;, out of paper~]~:[, off line~;, on line~]~:[, error~;~]"
            (logbitp 7 status) (logbitp 5 status) (logbitp 4 status) (logbitp 3 status))))
(defun print-string (base string)
  (loop for c across string
        do (loop until (logbitp 7 (inb (+ base 1))))    ; wait while busy
           (outb base (char-code c))
           (outb (+ base 2) #x0D) (outb (+ base 2) #x0C)))
      ; pulse the strobe line

;;; --- game port: writing starts four timers; a stick axis is how long its bit stays 1
(defun joystick ()
  (outb #x201 0)
  (loop for count from 0 below 20000
        for value = (inb #x201)
        when (zerop (logand value 3)) return (list :present count)
        finally (return (if (= value #xFF) :no-game-port :no-stick))))
(defun ports ()
  (format t "~&Keyboard controller status ~8,'0B~%" (inb #x64))
  (loop for (scroll num caps name) in '((nil nil t "Caps Lock") (nil t nil "Num Lock")
                                         (t nil nil "Scroll Lock") (t t t "all three")
                                         (nil nil nil "none"))
        do (format t "  lights: ~11A  keyboard answered ~{~2,'0Xh~^ ~}~%" name
                   (keyboard-leds scroll num caps))
           (wait-tick 4))
  (let ((lpt1 (peek16 #x408)))
    (format t "LPT1 is at ~3,'0Xh: ~A~%" lpt1 (printer-status lpt1))
    (outb lpt1 #x55)
    (format t "  data latch wrote 55h, read back ~2,'0Xh~%" (inb lpt1))
    (let ((line (format nil "Hello, printer.  This line was sent by ~A ~A on ~A.~C~C"
                        (lisp-implementation-type) (lisp-implementation-version)
                        (software-type) #\Return #\Newline)))
      (print-string lpt1 line)
      (format t "  printed ~D characters, one strobe pulse each~%" (length line))))
  (format t "Game port 201h: ~A~%"
          (let ((j (joystick)))
            (case j (:no-game-port "nothing there (reads FFh)")
                    (:no-stick "a port, but no stick attached")
                    (t (format nil "joystick, X axis count ~D" (second j)))))))

(defun main ()
  "Three old PC devices driven through their I/O ports."
  (ports))

(doslib-examples:define-example "ports" 'main
  :category :system
  :description "Three old PC devices driven through their I/O ports.")
