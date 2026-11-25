;;;; Stop a program in the middle, and carry on another day.
;;;;
;;;; A Lisp image is the whole state of the program: every variable, every
;;;; function, the half-finished picture.  SAVE-LISP-AND-DIE writes that to
;;;; a file -- here, with the runtime in front of it, a program file,
;;;; RESUME.EXE.  Running RESUME.EXE is not running the program again: it
;;;; is the same computation going on from where it stopped, with nothing
;;;; saved or loaded by hand.
;;;;
;;;; The picture is a slow Mandelbrot set.  Press S while it draws; then,
;;;; at the DOS prompt, type RESUME.  (Esc abandons it.)
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.resume
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.resume)

(defconstant +rows+ 192)
(defvar *picture* (make-array (* 320 +rows+) :element-type '(unsigned-byte 8) :initial-element 0))
(defvar *row* 0 "The next row to compute.")
(defvar *sessions* '() "The rows at which each session started, newest first.")

(defun status (control &rest arguments) ; a line of text under the picture
  (int86 #x10 :ax #x0200 :bx 0 :dx (ash 24 8))
  (loop for c across (format nil "~38A" (apply #'format nil control arguments))
        do (int86 #x10 :ax (logior #x0E00 (char-code c)) :bx 15)))

(defun compute-row (row)
  (declare (optimize speed) (fixnum row))
  (let ((ci (- (* row (/ 2.4d0 +rows+)) 1.2d0)))
    (dotimes (column 320)
      (let ((cr (- (* column (/ 3.5d0 320)) 2.5d0)) (zr 0d0) (zi 0d0) (n 0))
        (declare (double-float cr zr zi) (fixnum n))
        (loop while (and (< n 3000) (< (+ (* zr zr) (* zi zi)) 4d0))
              do (psetf zr (+ (- (* zr zr) (* zi zi)) cr)
                        zi (+ (* 2 zr zi) ci))
                 (incf n))
        (let ((colour (if (= n 3000) 0 (1+ (mod (* n 3) 255)))))
          (setf (aref *picture* (+ column (* 320 row))) colour
                (sap-ref-8 *vga* (+ column (* 320 row))) colour))))))

(defun save-and-stop ()
  (video-mode 3)
  (format t "~&Stopping at row ~D of ~D.  Writing the whole program to RESUME.EXE...~%" *row* +rows+)
  (finish-output)
  (save-program #+sbcl "resume.exe" #+clisp "resume.mem" #'resume))

(defun render ()
  "Draw from *ROW* on.  Returns :DONE or :ABANDONED -- or does not return, if it is saved."
  (video-mode #x13)
  (rainbow-palette)
  (dotimes (i (* 320 +rows+))           ; what was drawn before, from the image
    (setf (sap-ref-8 *vga* i) (aref *picture* i)))
  (push *row* *sessions*)
  (loop
    (when (>= *row* +rows+) (return :done))
    (status "row ~D/~D  session ~D  S saves" *row* +rows+ (length *sessions*))
    (compute-row *row*)
    (incf *row*)
    (wait-tick 4)                       ; unhurried, so that there is time to press S
    (wait-tick 4)                       ; unhurried, so that there is time to press S
    (when (key-pressed-p)
      (let ((key (get-key)))
        (cond ((member key '(#\s #\S)) (save-and-stop))
              ((eql key (code-char 27)) (return :abandoned)))))))

(defun finish-up (ending)
  (when (eq ending :done)
    (status "done, in ~D session~:P.  Press a key." (length *sessions*))
    (wait-key))
  (video-mode 3)
  (format t "~&~:(~A~).  Sessions started at rows ~{~D~^, ~}.~%" ending (reverse *sessions*)))

(defun resume ()                        ; where RESUME.EXE starts
  (finish-up (render))
  (exit-lisp))

(defun main ()
  "Stop a computation in the middle; carry on later from RESUME.EXE."
  (fill *picture* 0)
  (setf *row* 0 *sessions* '())
  (finish-up (render)))

(doslib-examples:define-example "resume" 'main
  :category :lisp
  :description "Stop a computation in the middle; carry on later from RESUME.EXE.")
