;;;; The joystick: four timers and four switches at port 201h
;;;;
;;;; The PC's game port is the cheapest analogue input ever built.  A
;;;; joystick is two variable resistors (and a second stick two more); each
;;;; is wired to a capacitor and a one-shot timer.  Writing anything to port
;;;; 201h discharges the capacitors and sets bits 0 to 3 of the port; each
;;;; bit drops back to 0 when its capacitor has charged again through its
;;;; resistor -- sooner with the stick one way, later with it the other.
;;;; There is no converter: the program reads a position by counting how
;;;; many times round a loop it gets before the bit drops.  Bits 4 to 7 are
;;;; the four buttons, 0 while pressed.
;;;;
;;;; So the numbers depend on the speed of the machine (which is why games
;;;; asked you to move the stick to its corners first), and reading a
;;;; joystick costs up to a millisecond of doing nothing else.  The loop
;;;; here is twenty bytes of machine code; this example keeps the smallest
;;;; and largest count it has seen on each axis and draws the stick between
;;;; them.
;;;;
;;;; Sound cards carried the game port, and QEMU has none: there this says
;;;; so and returns.  With a joystick: move it round its corners; Esc leaves.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.joystick
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.joystick)

(defconstant +port+ #x201)
(defconstant +never+ 65536)             ; the count when the bit did not drop

(defvar *axis-time* nil)

(defun assemble ()
  (setf *axis-time*
        (machine-code #x53                              ; push ebx
                      #x8B #x4C #x24 #x08               ; mov ecx,[esp+8]      the axis's bit
                      #xBA #x01 #x02 #x00 #x00          ; mov edx,201h
                      #x31 #xDB                         ; xor ebx,ebx
                      #xEE                              ; out dx,al            fire the one-shots
                      #xEC                              ; again: in al,dx
                      #x84 #xC8                         ; test al,cl
                      #x74 #x09                         ; jz done              the bit has dropped
                      #x43                              ; inc ebx
                      #x81 #xFB #x00 #x00 #x01 #x00     ; cmp ebx,65536
                      #x72 #xF2                         ; jb again
                      #x89 #xD8 #x5B #xC3)))            ; done: mov eax,ebx / pop ebx / ret

(defun axis-time (axis)
  "How many times round the loop before AXIS (0 to 3) timed out; +NEVER+ if it did not."
  ;; let the other one-shots finish first: a new write would start them again
  (loop repeat 20000 while (logtest (inb +port+) 15))
  (alien-funcall (sap-alien *axis-time* (function unsigned unsigned)) (ash 1 axis)))

(defun buttons ()
  "The four buttons as a list of booleans."
  (let ((byte (inb +port+)))
    (loop for bit from 4 to 7 collect (not (logbitp bit byte)))))

(defun joystick ()
  (assemble)
  (let ((present (loop for axis below 4 collect (< (axis-time axis) +never+))))
    (unless (or (first present) (second present))
      (format t "~&No joystick: port 201h reads ~2,'0Xh and its timers never run out.~%"
              (inb +port+))
      (return-from joystick nil))
    (let ((low (make-list 4 :initial-element +never+)) (high (make-list 4 :initial-element 0))
          (readings 0))
      (unwind-protect
           (progn
             (video-mode 3) (hide-text-cursor)
             (text-put 2 0 "The joystick at port 201h" 11)
             (text-put 2 1 "Move the stick round its corners; Esc leaves." 7)
             (text-box 2 3 43 19 :title " stick A ")
             (loop
               (when (and (key-pressed-p) (eql (get-key) #\Escape)) (return))
               (let ((times (loop for axis below 4
                                  collect (and (nth axis present) (axis-time axis)))))
                 (incf readings)
                 (loop for axis below 4 for time in times
                       when (and time (< time +never+))
                         do (setf (nth axis low) (min (nth axis low) time)
                                  (nth axis high) (max (nth axis high) time)))
                 (text-fill 3 4 41 17)
                 (flet ((place (axis cells)     ; where between the least and most seen
                          (let ((time (nth axis times)) (range (- (nth axis high) (nth axis low))))
                            (if (and time (plusp range))
                                (round (* (1- cells) (- (min time (nth axis high)) (nth axis low)))
                                       range)
                                (floor cells 2)))))
                   (text-put (+ 3 (place 0 41)) (+ 4 (place 1 17)) (string (code-char 254)) 14))
                 (loop for axis below 4 for time in times for row from 4
                       do (text-put 48 row
                                    (format nil "~A ~:[not connected~;~:*~5D  (~D to ~D)~]      "
                                            (nth axis '("A x" "A y" "B x" "B y"))
                                            (and time (< time +never+) time)
                                            (nth axis low) (nth axis high))
                                    15))
                 (loop for down in (buttons) for i from 1
                       do (text-put (+ 44 (* 8 i)) 9 (format nil "[~D]" i) (if down #x4F 8)))
                 (text-put 48 11 (format nil "~:D readings" readings) 7))
               (wait-tick 1)))
        (video-mode 3))
      (format t "~&~:D readings; the x axis ran from ~D to ~D times round the loop.~%"
              readings (first low) (first high)))))

(defun main ()
  "The joystick: four timers and four switches at port 201h."
  (joystick))

(doslib-examples:define-example "joystick" 'main
  :category :system
  :description "The joystick: four timers and four switches at port 201h.")
