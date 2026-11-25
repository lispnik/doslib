;;;; A formula, compiled to machine code while you watch, and plotted
;;;;
;;;; Type an expression in X, Y and TIME; the compiler -- which is part of
;;;; the running program, as it is of every Lisp program here -- turns it
;;;; into machine code for the processor, in a fraction of a second, and the
;;;; screen is painted with it: X and Y go from 0 to 1 across and down, TIME
;;;; is in seconds, and the value picks the colour.  The first screen shows
;;;; the machine code that was made.
;;;;
;;;; The formula is not called for each pixel: it is put inside the loop
;;;; that paints the screen and the two are compiled together, with X, Y and
;;;; TIME declared to be single floats -- so the arithmetic is the
;;;; processor's own, in its floating-point registers, and nothing is
;;;; allocated.
;;;;
;;;; Keys: 1 to 5 the formulas that come with it, E to type one, C to see
;;;; the code again, Esc to leave.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.formula
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main #:compile-formula))
(in-package #:doslib-examples.formula)

(defparameter *formulas*
  '("(* 40 (+ (sin (* 9 x)) (sin (* 7 y)) (sin (+ time (* 5 x y)))))"
    "(* 300 (sqrt (+ (expt (- x 0.5) 2) (expt (- y 0.5) 2))) (+ 1.2 (sin time)))"
    "(+ (* 255 x) (* 255 y) (* 40 time))"
    "(* 60 (sin (+ time (/ 1 (+ 0.05 (abs (- x y)))))))"
    "(* 128 (+ (sin (* 20 x (cos time))) (cos (* 20 y (sin time)))))"))

(defconstant +height+ 176 "Rows painted: the two lines of text below stay.")

(defun compile-formula (text)
  "The function that paints a screen with the formula in TEXT, or NIL and
what was wrong with it.  The function takes video memory and the time."
  (handler-case
      (let* ((*package* (find-package '#:doslib-examples.formula))
             (form (let ((*read-eval* nil)) (read-from-string text)))
             (painter
               `(lambda (screen time)
                  (declare (type system-area-pointer screen) (type single-float time)
                           (ignorable time) (optimize (speed 2) (safety 1)))
                  (dotimes (row +height+)
                    (let ((y (/ row ,(float +height+))))
                      (declare (type single-float y) (ignorable y))
                      (dotimes (column 320)
                        (let* ((x (/ column 320.0))
                               (value (float ,form 1.0)))
                          (declare (type single-float x value) (ignorable x))
                          (setf (sap-ref-8 screen (+ column (* 320 row)))
                                (logand 255 (the fixnum (truncate (max -1.0e6 (min 1.0e6 value)))))))))))))
        (let ((complaint nil))
          (multiple-value-bind (function warnings failed)
              ;; what the compiler has to say is kept, not printed: a
              ;; function that does not exist is only a warning to it
              (handler-bind ((compiler-note #'muffle-warning)
                             (warning (lambda (warning)
                                        (unless complaint (setf complaint (princ-to-string warning)))
                                        (muffle-warning warning))))
                (let ((*error-output* (make-broadcast-stream)))
                  (with-compilation-unit (:override t) (compile nil painter))))
            (declare (ignore warnings))
            (if (or failed complaint)
                (values nil (substitute #\Space #\Newline (or complaint "the compiler would not have it")))
                function))))
    (error (condition)
      (values nil (substitute #\Space #\Newline (princ-to-string condition))))))

(defun code-size (function)
  (code-bytes function))

(defun show-code (text function milliseconds &optional trouble)
  "The text screen: the formula and the machine code made from it."
  (video-mode 3)
  (when trouble
    (format t "~&The last formula stopped while it ran:~%  ~A~%~%"
            (subseq trouble 0 (min 76 (length trouble)))))
  (format t "~&The formula:~%  ~A~%~%" text)
  (format t "compiled to ~D bytes of machine code in ~D ms.  It begins:~%~%"
          (code-size function) milliseconds)
  (let ((listing (disassembly-string function))
        (shown 0))
    (with-input-from-string (in listing)
      (loop for line = (read-line in nil)
            while (and line (< shown 14))
            do (let ((colon (position #\: line)))
                 ;; "; 5B:       D9C1             FLD ST(1)" -> the bytes and the instruction
                 (when (and colon (> (length line) (+ colon 8)) (char= (char line 0) #\;)
                            (not (search "disassembly" line)) (not (search "Size:" line)))
                   (format t "  ~A~%" (string-trim " " (subseq line (1+ colon))))
                   (incf shown))))))
  (format t "~%Any key paints the screen with it.~%")
  (wait-key))

(defun say (column row colour string)   ; the BIOS writes text in the graphics modes too
  (int86 #x10 :ax #x0200 :bx 0 :dx (+ (* 256 row) column))
  (loop for c across string do (int86 #x10 :ax (+ #x0E00 (char-code c)) :bx colour)))

(defun ask-formula (current)
  (video-mode 3)
  (text-put 0 0 "A formula in X, Y (0 to 1) and TIME (seconds); its value picks the colour." 15)
  (text-put 0 1 "Enter compiles it; Esc keeps the one there was." 7)
  (text-input 0 3 80 :initial current))

(defun main (&optional formula)
  "A formula compiled to machine code while you watch, and plotted."
  (let ((text (or formula (first *formulas*))) (function nil) (milliseconds 0) (message nil))
    (flet ((take (new)
             (let ((start (get-internal-real-time)))
               (multiple-value-bind (made problem) (compile-formula new)
                 (cond (made (setf text new function made message nil
                                   milliseconds (- (get-internal-real-time) start))
                             t)
                       (t (setf message problem) nil))))))
      (unless (take text)
        (format t "~&~A~%" message)
        (return-from main (values)))
      (unwind-protect
           (loop
             (show-code text function milliseconds message)
             (setf message nil)
             (video-mode #x13)
             (rainbow-palette)
             (say 0 23 15 (subseq text 0 (min 40 (length text))))
             (say 0 24 7 "1-5 E:type one C:code Esc")
             (let ((start (ticks)) (frames 0) (again nil))
               (loop until again
                     do (handler-case
                            (funcall function *vga* (* 0.0549 (- (ticks) start)))
                          (error (condition)
                            (let ((trouble (substitute #\Space #\Newline (princ-to-string condition))))
                              (unless (take (first *formulas*)) (return-from main (values)))
                              (setf message trouble again t))))
                        (incf frames)
                        (when (zerop (mod frames 8))
                          (say 27 24 14 (format nil "~5,1F a second"
                                                (/ (* 18.2 frames) (max 1 (- (ticks) start))))))
                        (when (key-pressed-p)
                          (let ((key (get-key)))
                            (cond ((eql key (code-char 27)) (return-from main (values)))
                                  ((and (characterp key) (digit-char-p key)
                                        (<= 1 (digit-char-p key) (length *formulas*)))
                                   (take (nth (1- (digit-char-p key)) *formulas*))
                                   (setf again t))
                                  ((member key '(#\e #\E))
                                   (let ((typed (ask-formula text)))
                                     (when (and typed (not (take typed)))
                                       (text-put 0 5 "That will not do:" 12)
                                       (text-put 0 6 (subseq message 0 (min 79 (length message))) 7)
                                       (text-put 0 8 "Any key goes back to the last one." 7)
                                       (wait-key)))
                                   (setf again t))
                                  ((member key '(#\c #\C)) (setf again t))))))))
        (video-mode 3))))
  (values))

(doslib-examples:define-example "formula" 'main
  :category :graphics
  :description "A formula compiled to machine code while you watch, and plotted.")
