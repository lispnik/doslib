;;;; Turtle graphics with CLOS: a fractal tree and a dragon curve
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.turtle
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.turtle)

(defclass turtle ()
  ((x :initarg :x :accessor x) (y :initarg :y :accessor y)
   (heading :initarg :heading :initform 90.0 :accessor heading)
   (colour :initform 15 :accessor colour) (pen :initform t :accessor pen)))
(defgeneric forward (turtle distance))
(defgeneric turn (turtle degrees))
(defmethod forward ((tu turtle) distance)
  (let* ((a (* (heading tu) (/ pi 180)))
         (nx (+ (x tu) (* distance (cos a)))) (ny (- (y tu) (* distance (sin a)))))
    (when (pen tu)
      (draw-line (round (x tu)) (round (y tu)) (round nx) (round ny) (colour tu)))
    (setf (x tu) nx (y tu) ny)))
(defmethod turn ((tu turtle) degrees) (incf (heading tu) degrees))
(defun tree (tu length depth)
  (when (plusp depth)
    (setf (colour tu) (+ 1 depth))
    (forward tu length)
    (turn tu 24) (tree tu (* length 0.74) (1- depth))
    (turn tu -48) (tree tu (* length 0.74) (1- depth))
    (turn tu 24)
    (setf (pen tu) nil) (forward tu (- length)) (setf (pen tu) t)))
(defun dragon (tu steps length)         ; turn n: is the bit above n's lowest set bit 1?
  (loop for n from 1 to steps
        do (setf (colour tu) (+ 32 (floor (* n 220) steps)))
           (forward tu length)
           (turn tu (if (logbitp (integer-length (logand n (- n))) n) -90 90))))
(defun picture ()
  (video-mode #x13)
  (rainbow-palette 32 255)
  (loop for d from 1 to 11              ; tree colours: brown trunk to green tips
        do (set-palette (+ 1 d) (max 8 (- 40 (* 3 (- 11 d)))) (+ 20 (* 4 (- 11 d))) 6))
  (tree (make-instance (quote turtle) :x 52 :y 196) 26 11)
  (dragon (make-instance 'turtle :x 256 :y 128 :heading 0.0) 4095 2)
  (finish))

(defun main ()
  "Turtle graphics with CLOS."
  (picture))

(doslib-examples:define-example "turtle" 'main
  :category :graphics
  :description "Turtle graphics with CLOS.")
