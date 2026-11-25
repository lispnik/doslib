;;;; Change a running program from the listener.  DRAW paints the
;;;; lower half of the text screen by asking CELL for the colour of each
;;;; character cell.  Redefine CELL at the prompt and call DRAW again: the
;;;; new definition is compiled to native code and takes effect at once.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.live
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.live)

(defvar *text* (int-sap #xB8000))
(defun cell (x y)                       ; the starting pattern: diagonal stripes
  (1+ (mod (floor (+ x y) 3) 6)))
(defun draw ()
  (loop for y from 12 to 24
        do (dotimes (x 80)
             (let ((offset (* 2 (+ x (* 80 y)))))
               (setf (sap-ref-8 *text* offset) 219
                     (sap-ref-8 *text* (1+ offset)) (cell x y))))))

(defun main ()
  "Change a running program from the listener."
  (draw)
  (format t "~&Now, in this package -- (in-package #:doslib-examples.live) --~%~
redefine CELL and call (draw) again.~%"))

(doslib-examples:define-example "live" 'main
  :category :lisp
  :description "Change a running program from the listener.")
