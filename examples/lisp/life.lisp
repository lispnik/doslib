;;;; Conway's Game of Life in 80x50 "pixels": half-height block
;;;; characters in text mode.  A key stops it.
;;;; (doslib-examples:build-executable "life" "life.exe") saves it as a
;;;; stand-alone program: the Lisp runtime with this image appended.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.life
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.life)

(defvar *text* (int-sap #xB8000))
(defconstant +w+ 80)
 
(defconstant +h+ 48)
(defun make-world () (make-array (* +w+ +h+) :element-type 'bit :initial-element 0))
(declaim (inline alive))
(defun alive (world x y)
  (aref world (+ (mod x +w+) (* +w+ (mod y +h+)))))
(defun seed (world)
  (dotimes (i (length world)) (setf (aref world i) (if (< (random 100) 28) 1 0)))
  ;; and one glider gun's worth of order in the chaos: a glider
  (loop for (x y) in '((1 0) (2 1) (0 2) (1 2) (2 2))
        do (setf (aref world (+ x 2 (* +w+ (+ y 2)))) 1)))
(defun step-world (world next)
  (declare (type simple-bit-vector world next) (optimize speed (safety 0)))
  (dotimes (y +h+)
    (dotimes (x +w+)
      (let ((n (+ (alive world (1- x) (1- y)) (alive world x (1- y)) (alive world (1+ x) (1- y))
                  (alive world (1- x) y) (alive world (1+ x) y)
                  (alive world (1- x) (1+ y)) (alive world x (1+ y)) (alive world (1+ x) (1+ y)))))
        (setf (aref next (+ x (* +w+ y)))
              (if (or (= n 3) (and (= n 2) (= 1 (alive world x y)))) 1 0))))))
(defun show (world generation)
  (dotimes (row 24)                     ; two cells per character: upper and lower half
    (dotimes (x +w+)
      (let ((top (alive world x (* 2 row))) (bottom (alive world x (1+ (* 2 row))))
            (offset (* 2 (+ x (* 80 row)))))
        (setf (sap-ref-8 *text* offset) (cond ((and (= top 1) (= bottom 1)) 219)
                                              ((= top 1) 223) ((= bottom 1) 220) (t 32))
              (sap-ref-8 *text* (1+ offset)) (+ 9 (mod (+ row (floor generation 4)) 6))))))
  (let ((status (format nil " Life on DOS, in Lisp   generation ~5D   population ~4D   any key quits "
                        generation (count 1 world))))
    (loop for c across status for x from 0
          do (setf (sap-ref-8 *text* (* 2 (+ x (* 80 24)))) (char-code c)
                   (sap-ref-8 *text* (1+ (* 2 (+ x (* 80 24))))) #x1F))))
(defun life ()
  (int86 #x10 :ax #x0100 :cx #x2000)    ; hide the cursor
  (dotimes (i 4000) (setf (sap-ref-8 *text* i) 0))
  (let ((world (make-world)) (next (make-world)))
    (seed world)
    (loop for generation from 0
          until (key-pressed-p)
          do (show world generation)
             (let ((tick (peek32 #x46C)))       ; pace it: one generation per
               (loop while (= tick (peek32 #x46C))))    ; BIOS timer tick (18.2/s)
             (step-world world next)
             (rotatef world next)))
  (wait-key)
  (video-mode 3))

(defun main ()
  "Conway's Game of Life in text mode."
  (life))

(doslib-examples:define-example "life" 'main
  :category :lisp
  :description "Conway's Game of Life in text mode.")
