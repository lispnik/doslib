;;;; Text-mode effects by writing to B800:0000
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.textfx
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.textfx)

(defvar *text* (int-sap #xB8000))
(defun put (x y char attribute)
  (when (and (< -1 x 80) (< -1 y 25))
    (let ((offset (* 2 (+ x (* 80 y)))))
      (setf (sap-ref-8 *text* offset) char
            (sap-ref-8 *text* (1+ offset)) attribute))))
(defun say (x y attribute string)
  (loop for c across string for i from x do (put i y (char-code c) attribute)))
(defun box (x y w h attribute)
  (dotimes (j h)
    (dotimes (i w)
      (put (+ x i) (+ y j)
           (cond ((and (= j 0) (= i 0)) 201) ((and (= j 0) (= i (1- w))) 187)
                 ((and (= j (1- h)) (= i 0)) 200)
                 ((and (= j (1- h)) (= i (1- w))) 188)
                 ((or (= j 0) (= j (1- h))) 205) ((or (= i 0) (= i (1- w))) 186)
                 (t 32))
           attribute))))
(defun textfx ()
  (int86 #x10 :ax #x0100 :cx #x2000)    ; hide the cursor
  (dotimes (y 25) (dotimes (x 80) (put x y 32 0)))
  (dotimes (x 80)                       ; "digital rain"
    (let ((length (+ 5 (random 16))) (head (random 30)))
      (dotimes (k length)
        (put x (- head k) (+ 33 (random 94))
             (cond ((= k 0) #x0F) ((< k 3) #x0A) (t #x02))))))
  (box 20 3 40 5 #x1F)
  (loop for c across "SBCL on FreeDOS" for i from 0
        do (put (+ 26 (* 2 i)) 4 (char-code c) (+ #x10 9 (mod i 7))))
  (say 22 6 #x1E "Lisp writing straight to B800:0000")
  (box 20 10 40 12 #x07)
  (say 22 10 #x0F " all 128 colour attributes ")
  (dotimes (bg 8)                       ; background x foreground table
    (dotimes (fg 16)
      (put (+ 24 (* 2 fg)) (+ 12 bg) 177 (+ (* 16 bg) fg))
      (put (+ 25 (* 2 fg)) (+ 12 bg) 254 (+ (* 16 bg) fg))))
  (finish))

(defun main ()
  "Text-mode effects by writing to B800:0000."
  (textfx))

(doslib-examples:define-example "textfx" 'main
  :category :graphics
  :description "Text-mode effects by writing to B800:0000.")
