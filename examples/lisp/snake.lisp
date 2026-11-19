;;;; A game: arrow keys steer, Esc quits.  Keys come from the
;;;; BIOS (INT 16h), timing from the BIOS tick counter at 0040:006C, and
;;;; the screen is text-mode video memory.  The snake wraps at the edges.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.snake
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.snake)

(defvar *text* (int-sap #xB8000))
(defun put (x y char attribute)
  (let ((offset (* 2 (+ x (* 80 y)))))
    (setf (sap-ref-8 *text* offset) char (sap-ref-8 *text* (1+ offset)) attribute)))
(defun say (x y attribute string)
  (loop for c across string for i from x do (put i y (char-code c) attribute)))
(defun read-key ()                      ; => BIOS scan code, or NIL
  (when (key-pressed-p) (hi (int86 #x16 :ax 0))))
(defun snake ()
  (int86 #x10 :ax #x0100 :cx #x2000)    ; hide the cursor
  (dotimes (y 25) (dotimes (x 80) (put x y 32 #x00)))
  (dotimes (x 80) (put x 0 32 #x1F))
  (let ((body (list '(12 . 12) '(11 . 12) '(10 . 12))) (dx 1) (dy 0) (score 0)
        (food (cons 40 12)) (alive t))
    (flet ((place-food ()
             (loop do (setf food (cons (random 80) (1+ (random 24))))
                   while (member food body :test #'equal))
             (put (car food) (cdr food) 4 #x0C)))        ; a red diamond
      (put (car food) (cdr food) 4 #x0C)
      (loop while alive
            do (say 1 0 #x1F (format nil "SNAKE in Lisp on DOS    score ~3D    length ~3D    arrows steer, Esc quits"
                                     score (length body)))
               (let ((next-move (+ (ticks) 2)))          ; about nine moves a second
                 (loop until (>= (ticks) next-move)
                       do (case (read-key)
                            (#x48 (when (zerop dy) (setf dx 0 dy -1)))   ; up
                            (#x50 (when (zerop dy) (setf dx 0 dy 1)))    ; down
                            (#x4B (when (zerop dx) (setf dx -1 dy 0)))   ; left
                            (#x4D (when (zerop dx) (setf dx 1 dy 0)))    ; right
                            (#x01 (setf alive nil next-move 0)))))       ; Esc
               (let ((head (cons (mod (+ (car (first body)) dx) 80)
                                 (1+ (mod (+ (cdr (first body)) dy -1) 24)))))
                 (cond ((member head body :test #'equal) (setf alive nil))
                       (t (push head body)
                          (put (car head) (cdr head) 219 #x0A)
                          (put (car (second body)) (cdr (second body)) 177 #x02)
                          (cond ((equal head food) (incf score 10) (place-food))
                                (t (let ((tail (car (last body))))
                                     (put (car tail) (cdr tail) 32 0)
                                     (setf body (butlast body)))))))))
      (say 30 12 #x4F (format nil "  GAME OVER   score ~D  " score))
      (wait-key)
      (video-mode 3))))

(defun main ()
  "Snake: arrow keys steer, Esc quits."
  (snake))

(doslib-examples:define-example "snake" 'main
  :category :lisp
  :description "Snake: arrow keys steer, Esc quits.")
