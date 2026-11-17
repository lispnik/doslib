;;;; Plants grown from grammars.  An L-system is a set of
;;;; rewriting rules; here both the rules and the strings they produce are
;;;; ordinary Lisp lists, rewritten a few times and then walked by a turtle:
;;;; F draws forward, + and - turn, [ and ] save and restore the position.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.lsystem
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.lsystem)

(defun rewrite (symbols rules)
  (loop for s in symbols append (or (cdr (assoc s rules)) (list s))))
(defun grow (axiom rules generations)
  (dotimes (i generations axiom) (setf axiom (rewrite axiom rules))))
(defun draw-plant (symbols x y step angle colour-base)
  (let ((heading 90.0) (stack '()) (depth 0))
    (dolist (s symbols)
      (case s
        (f (let ((nx (+ x (* step (cos (* heading (/ pi 180))))))
                 (ny (- y (* step (sin (* heading (/ pi 180)))))))
             (draw-line (round x) (round y) (round nx) (round ny)
                        (+ colour-base (min 7 depth)))
             (setf x nx y ny)))
        (+ (incf heading angle))
        (- (decf heading angle))
        ([ (push (list x y heading) stack) (incf depth))
        (] (destructuring-bind (px py ph) (pop stack) (setf x px y py heading ph))
           (decf depth))))))
(defun garden ()
  (video-mode #x13)
  (dotimes (i 8)                        ; three eight-step colour ramps, trunk to tip
    (set-palette (+ 16 i) (- 34 (* 3 i)) (+ 22 (* 5 i)) 6)
    (set-palette (+ 32 i) (- 40 (* 2 i)) (+ 18 (* 4 i)) (+ 10 (* 5 i)))
    (set-palette (+ 48 i) (+ 30 (* 4 i)) (+ 20 (* 3 i)) (- 30 (* 3 i))))
  (draw-line 0 199 319 199 20)
  ;; "fractal plant"
  (draw-plant (grow '(x) '((x f + [ [ x ] - x ] - f [ - f x ] + x) (f f f)) 5)
              70 198 2.6 25.0 16)
  ;; a bushier weed
  (draw-plant (grow '(f) '((f f [ + f ] f [ - f ] f)) 4) 172 198 2.2 25.7 32)
  ;; a sparse, wind-blown one
  (draw-plant (grow '(x) '((x f [ + x ] f [ - x ] + x) (f f f)) 6) 262 198 1.35 20.0 48)
  (finish))

(defun main ()
  "Plants grown from grammars."
  (garden))

(doslib-examples:define-example "lsystem" 'main
  :category :graphics
  :description "Plants grown from grammars.")
