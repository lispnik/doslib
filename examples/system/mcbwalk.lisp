;;;; Walk DOS's chain of memory control blocks, the way MEM does.
;;;; INT 21h/52h returns a real-mode pointer to DOS's "list of lists"; the word
;;;; just below it is the segment of the first MCB.  Each MCB is 16 bytes:
;;;; 'M' (or 'Z' for the last), owner PSP segment, size in paragraphs, name.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.mcbwalk
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.mcbwalk)

(defun mcb-name (segment)
  (let ((a (linear segment 8)))
    (coerce (loop for i below 8 for c = (peek (+ a i))
                  while (<= 33 c 126) collect (code-char c)) 'string)))
(defun describe-owner (segment owner)
  (cond ((= owner 0) "(free)")
        ((= owner 8) (format nil "DOS ~A" (mcb-name segment)))
        ((= owner (1+ segment)) (mcb-name segment))     ; the program itself
        (t (format nil "~A (data/env)" (mcb-name (1- owner))))))
(defun walk (segment)
  (loop with free = 0 and used = 0
        for type = (peek (linear segment))
        for owner = (peek16 (linear segment 1))
        for paragraphs = (peek16 (linear segment 3))
        do (unless (member type '(77 90)) (return (values free used)))
           (format t "  ~4,'0X  ~4,'0X ~7:D  ~A~%" (1+ segment) owner
                   (* 16 paragraphs) (describe-owner segment owner))
           (if (zerop owner) (incf free paragraphs) (incf used paragraphs))
           (when (= type 90) (return (values free used)))
           (setf segment (+ segment paragraphs 1))))
(defun mcbwalk ()
  (multiple-value-bind (ax bx cx dx es) (real-int #x21 :ax #x5200)
    (declare (ignore ax cx dx))
    (let ((first (peek16 (linear es (- bx 2))))
          (link (lo (real-int #x21 :ax #x5802))))
      (format t "~&DOS list of lists at ~4,'0X:~4,'0X, first MCB at segment ~4,'0X~%" es bx first)
      (real-int #x21 :ax #x5803 :bx 1)          ; link the upper memory blocks in
      (format t "  seg   owner   bytes  what~%")
      (multiple-value-bind (free used) (walk first)
        (format t "~:D bytes in use, ~:D bytes free, in the DOS arena~%"
                (* 16 used) (* 16 free)))
      (real-int #x21 :ax #x5803 :bx link))))

(defun main ()
  "Walk DOS's chain of memory control blocks, the way MEM does."
      ; and put it back as it was
  (mcbwalk))

(doslib-examples:define-example "mcbwalk" 'main
  :category :system
  :description "Walk DOS's chain of memory control blocks, the way MEM does.")
