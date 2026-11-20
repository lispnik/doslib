;;;; A terminal: the serial port by interrupt, and ANSI escape sequences
;;;;
;;;; This is what a DOS machine did with a modem: everything typed goes out
;;;; of COM1, everything that comes in goes on the screen.  Two things make
;;;; it more than a loop.
;;;;
;;;; Receiving by interrupt.  The serial chip (an 8250 or 16550) holds one
;;;; byte; at 115,200 baud the next arrives 87 millionths of a second later
;;;; and overwrites it.  A program that is busy scrolling the screen cannot
;;;; look often enough, so the chip is told to raise interrupt request 4
;;;; when a byte arrives (bit 0 of its register 1, and the OUT2 line that
;;;; gates its interrupt on a PC), and a handler built by the library's
;;;; WITH-IRQ-HANDLER reads each byte into a ring of 256.  Lisp empties the
;;;; ring when it gets round to it.
;;;;
;;;; ANSI escape sequences.  The other end positions the cursor and sets
;;;; colours with ESC [ numbers letter, the VT100's language that ANSI.SYS
;;;; and every bulletin board spoke.  Understood here: cursor position (H,
;;;; f), movement (A B C D), clearing (J, K), colours and bold (m), save and
;;;; restore (s, u), and "where is the cursor?" (6n), which is answered down
;;;; the line.
;;;;
;;;; To try it under QEMU, give the machine a serial port you can reach
;;;; (-serial tcp:127.0.0.1:4555,server,nowait), run this, and connect with
;;;; nc 127.0.0.1 4555; then type at both ends.  F10 leaves (Esc is a key
;;;; the other end wants).
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.term
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.term)

(defconstant +rows+ 24)                 ; the terminal's; the 25th is the status line
(defparameter *colours* #(0 4 2 6 1 5 3 7))     ; ANSI's order -> the PC's

(defvar *x* 0)
(defvar *y* 0)
(defvar *foreground* 7)
(defvar *background* 0)
(defvar *bold* nil)
(defvar *saved* '(0 . 0))
(defvar *state* :text)                  ; :text, :escape (ESC seen) or :sequence (ESC [ seen)
(defvar *numbers* '())                  ; the sequence's numbers so far, last first
(defvar *digits* nil)                   ; the number being read

(defun attribute ()
  (+ (* 16 *background*) *foreground* (if *bold* 8 0)))

(defun cell (x y) (* 2 (+ x (* 80 y))))

(defun clear-cells (from to)            ; cells FROM up to TO, in the current colours
  (loop with blank = (logior 32 (ash (attribute) 8))
        for i from from below to
        do (setf (sap-ref-16 *text-screen* (* 2 i)) blank)))

(defun scroll-up ()
  (sb-kernel:system-area-ub8-copy *text-screen* 160 *text-screen* 0 (* 160 (1- +rows+)))
  (clear-cells (* 80 (1- +rows+)) (* 80 +rows+)))

(defun line-feed ()
  (if (< *y* (1- +rows+)) (incf *y*) (scroll-up)))

(defun put-byte (byte)
  (setf (sap-ref-16 *text-screen* (cell *x* *y*)) (logior byte (ash (attribute) 8)))
  (when (= (incf *x*) 80)
    (setf *x* 0) (line-feed)))

(defun select-graphics (numbers)
  (dolist (n (or numbers '(0)))
    (cond ((= n 0) (setf *foreground* 7 *background* 0 *bold* nil))
          ((= n 1) (setf *bold* t))
          ((= n 22) (setf *bold* nil))
          ((= n 7) (rotatef *foreground* *background*))
          ((<= 30 n 37) (setf *foreground* (aref *colours* (- n 30))))
          ((= n 39) (setf *foreground* 7))
          ((<= 40 n 47) (setf *background* (aref *colours* (- n 40))))
          ((= n 49) (setf *background* 0)))))

(defun finish-sequence (letter port)
  (let* ((numbers (reverse (if *digits* (cons *digits* *numbers*) *numbers*)))
         (n (max 1 (or (first numbers) 1))))
    (flet ((clamp () (setf *x* (max 0 (min 79 *x*)) *y* (max 0 (min (1- +rows+) *y*)))))
      (case letter
        ((#\H #\f) (setf *y* (1- (max 1 (or (first numbers) 1)))
                         *x* (1- (max 1 (or (second numbers) 1))))
         (clamp))
        (#\A (decf *y* n) (clamp))
        (#\B (incf *y* n) (clamp))
        (#\C (incf *x* n) (clamp))
        (#\D (decf *x* n) (clamp))
        (#\J (let ((here (+ *x* (* 80 *y*))))
               (case (or (first numbers) 0)
                 (0 (clear-cells here (* 80 +rows+)))
                 (1 (clear-cells 0 (1+ here)))
                 (2 (clear-cells 0 (* 80 +rows+)) (setf *x* 0 *y* 0)))))
        (#\K (let ((row (* 80 *y*)))
               (case (or (first numbers) 0)
                 (0 (clear-cells (+ row *x*) (+ row 80)))
                 (1 (clear-cells row (+ row *x* 1)))
                 (2 (clear-cells row (+ row 80))))))
        (#\m (select-graphics numbers))
        (#\s (setf *saved* (cons *x* *y*)))
        (#\u (setf *x* (car *saved*) *y* (cdr *saved*)))
        (#\n (when (eql (first numbers) 6)              ; where is the cursor?
               (serial-write-string port (format nil "~C[~D;~DR" (code-char 27)
                                                 (1+ *y*) (1+ *x*)))))))))

(defun receive (byte port)
  "One byte from the line: part of an escape sequence, a control, or a character."
  (ecase *state*
    (:text
     (case byte
       (27 (setf *state* :escape))
       (13 (setf *x* 0))
       (10 (line-feed))
       (8 (when (plusp *x*) (decf *x*)))
       (9 (setf *x* (min 79 (* 8 (1+ (floor *x* 8))))))
       (7 (speaker-beep 880 1))
       (12 (clear-cells 0 (* 80 +rows+)) (setf *x* 0 *y* 0))
       (t (put-byte byte))))
    (:escape
     (if (= byte (char-code #\[))
         (setf *state* :sequence *numbers* '() *digits* nil)
         (setf *state* :text)))
    (:sequence
     (cond ((<= 48 byte 57) (setf *digits* (+ (* 10 (or *digits* 0)) (- byte 48))))
           ((= byte (char-code #\;)) (push (or *digits* 0) *numbers*) (setf *digits* nil))
           ((= byte (char-code #\?)))                   ; private sequences: read and ignored
           (t (setf *state* :text)
              (finish-sequence (code-char byte) port))))))

(defun send-key (key port)
  (flet ((escape (string) (serial-write-byte port 27) (serial-write-string port string)))
    (case key
      (:up (escape "[A")) (:down (escape "[B")) (:right (escape "[C")) (:left (escape "[D"))
      (:home (escape "[H")) (:end (escape "[F")) (:delete (serial-write-byte port 127))
      (t (when (characterp key) (serial-write-byte port (logand (char-code key) 255)))))))

(defun terminal (&optional (com 1))
  (let ((port (serial-port com)) (irq (if (oddp com) 4 3)) (received 0) (shown -1))
    (unless port
      (format t "~&There is no COM~D.~%" com)
      (return-from terminal nil))
    (setf *x* 0 *y* 0 *foreground* 7 *background* 0 *bold* nil *state* :text)
    (serial-open :port port :baud 115200)
    (outb (+ port 2) 0)                 ; no FIFO: an interrupt for every byte
    (inb port) (inb (+ port 5)) (inb (+ port 2))        ; nothing left over
    (unwind-protect
         (with-irq-handler (hook irq :buffer port)
           (outb (+ port 4) #x0B)       ; DTR, RTS, and OUT2: let the interrupt out
           (outb (+ port 1) 1)          ; interrupt when a byte has arrived
           (video-mode 3)
           (loop
             (loop for byte = (irq-read-byte hook)
                   while byte do (receive byte port) (incf received))
             (when (key-pressed-p)
               (let ((key (get-key)))
                 (when (eq key :f10) (return))
                 (send-key key port)))
             (unless (= received shown)
               (setf shown received)
               (text-put 0 24 (format nil " COM~D ~3,'0Xh  115200 8N1  IRQ ~D: ~:D interrupts, ~
                                           ~:D bytes received    F10 leaves~30A"
                                      com port irq (irq-count hook) received "")
                         #x70))
             (text-cursor *x* *y*)))
      (outb (+ port 1) 0)               ; the port quiet again
      (outb (+ port 4) 3)
      (video-mode 3))
    (format t "~&~:D bytes received.~%" received)))

(defun main (&optional (com 1))
  "A terminal: the serial port by interrupt, and ANSI escape sequences."
  (terminal com))

(doslib-examples:define-example "term" 'main
  :category :system
  :description "A terminal: the serial port by interrupt, and ANSI escape sequences.")
