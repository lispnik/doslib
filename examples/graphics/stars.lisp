;;;; Flying through a starfield.  Each star has a position in
;;;; space; every frame it comes closer and is drawn as a streak from where
;;;; it was to where it is, brighter as it nears.  Any key stops it.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.stars
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.stars)

(defstruct star x y z px py qx qy)
        ; P: where it is drawn; Q: where its streak starts
(defun new-star (&optional (z (+ 1.0 (random 15.0))))
  (make-star :x (- (random 16.0) 8.0) :y (- (random 10.0) 5.0) :z z))
(defun stars (&optional (count 220))
  (video-mode #x13)
  (dotimes (i 64) (set-palette i i i (min 63 (+ i (floor i 4)))))   ; blue-white greys
  (let ((field (loop repeat count collect (new-star))))
    (loop until (key-pressed-p)
          do (wait-tick)
             (dolist (s field)
               (when (star-qx s)        ; rub out last frame's streak
                 (draw-line (star-qx s) (star-qy s) (star-px s) (star-py s) 0))
               (let ((z (decf (star-z s) 0.22)))
                 (when (< z 0.3)        ; passed us: start again far away
                   (let ((fresh (new-star 16.0)))
                     (setf (star-x s) (star-x fresh) (star-y s) (star-y fresh)
                           (star-z s) 16.0 z 16.0 (star-px s) nil)))
                 (let ((x (round (+ 160 (/ (* 60 (star-x s)) z))))
                       (y (round (+ 100 (/ (* 50 (star-y s)) z)))))
                   (cond ((and (< -1 x 320) (< -1 y 200))
                          (let ((from-x (or (star-px s) x)) (from-y (or (star-py s) y)))
                            (when (> (+ (abs (- x from-x)) (abs (- y from-y))) 40)
                              (setf from-x x from-y y))   ; too close: no long streaks
                            (draw-line from-x from-y x y
                                       (max 10 (min 63 (round (- 72 (* 4 z))))))
                            (setf (star-qx s) from-x (star-qy s) from-y
                                  (star-px s) x (star-py s) y)))
                         (t (setf (star-qx s) nil (star-px s) nil))))))))
  (finish))

(defun main ()
  "Flying through a starfield."
  (stars))

(doslib-examples:define-example "stars" 'main
  :category :graphics
  :description "Flying through a starfield.")
