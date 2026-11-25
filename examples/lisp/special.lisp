;;;; Programs that write programs, at run time, on DOS.
;;;; A formula arrives as data.  It can be walked by an interpreter each time
;;;; it is needed, or handed to COMPILE once, which turns it into native x86
;;;; code specialised for that formula.  Same answers, very different speed.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.special
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.special)

#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(defvar *formula* '(+ (* 3 (sin (* x 0.5))) (* 2 (cos (* y 0.25))) (/ (* x y) 100.0)))
(defun interpret (form x y)             ; a tree-walking evaluator
  (cond ((eq form 'x) x) ((eq form 'y) y) ((numberp form) form)
        (t (let ((args (mapcar (lambda (f) (interpret f x y)) (rest form))))
             (ecase (first form)
               (+ (apply #'+ args)) (* (apply #'* args)) (/ (apply #'/ args))
               (sin (sin (first args))) (cos (cos (first args))))))))
(defun specialise (form)                ; the compiler is just a function
  (compile nil `(lambda (x y)
                  (declare (single-float x y) (optimize speed (safety 0)))
                  ,form)))
(defun run (function)
  (let ((start (get-internal-real-time)) (sum 0.0))
    (dotimes (i 200)
      (dotimes (j 200)
        (incf sum (funcall function (float i) (float j)))))
    (values sum (/ (- (get-internal-real-time) start)
                   (/ internal-time-units-per-second 1000.0)))))
(defun demo ()
  (format t "~&formula: ~S~%" *formula*)
  (let ((compiled (specialise *formula*)))
    (multiple-value-bind (sum1 ms1) (run (lambda (x y) (interpret *formula* x y)))
      (multiple-value-bind (sum2 ms2) (run compiled)
        (format t "interpreted:  sum ~12,3F  in ~8,1F ms~%" sum1 ms1)
        (format t "compiled:     sum ~12,3F  in ~8,1F ms   (~,1Fx faster)~%"
                sum2 ms2 (/ ms1 (max ms2 0.001)))))
    (format t "The specialised function is ~D bytes of x86 made a moment ago:~%"
            (code-bytes compiled))
    (let ((*print-right-margin* 78))
      (disassemble (compile nil '(lambda (x) (declare (type (unsigned-byte 12) x)
                                                    (optimize speed (safety 0)))
                                  (+ (* x x) 1)))))))

(defun main ()
  "Programs that write programs, at run time, on DOS."
  (demo))

(doslib-examples:define-example "special" 'main
  :category :lisp
  :description "Programs that write programs, at run time, on DOS.")
