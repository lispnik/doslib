;;;; The same portable Lisp on DOS and on the build host.  The
;;;; times are not comparable (DOS is running under an emulator); the point
;;;; is the last column: both compilers must produce the same answers.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.bench
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.bench)

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(defun fib (n) (declare (fixnum n)) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(defun sieve (n)
  (let ((flags (make-array (1+ n) :element-type 'bit :initial-element 1)) (count 0))
    (loop for i from 2 to n
          when (= 1 (aref flags i))
            do (incf count) (loop for j from (* i i) to n by i do (setf (aref flags j) 0)))
    count))
(defun mandel-sum ()
  (let ((total 0))
    (dotimes (py 60)
      (dotimes (px 80)
        (let ((cr (- (* px 0.04d0) 2.2d0)) (ci (- (* py 0.04d0) 1.2d0)) (zr 0d0) (zi 0d0) (i 0))
          (declare (double-float cr ci zr zi) (fixnum i))
          (loop while (and (< i 200) (< (+ (* zr zr) (* zi zi)) 4d0))
                do (psetq zr (+ (- (* zr zr) (* zi zi)) cr) zi (+ (* 2d0 zr zi) ci))
                   (incf i))
          (incf total i))))
    total))
(defun factorial-digits (n)
  (length (princ-to-string (loop with f = 1 for i from 1 to n do (setf f (* f i)) finally (return f)))))
(defun hash-words (n)
  (let ((table (make-hash-table :test 'equal)))
    (dotimes (i n) (incf (gethash (format nil "~R" (mod (* i 7919) 1000)) table 0)))
    (list (hash-table-count table) (gethash "seven hundred seventy-seven" table))))
(defun cons-and-sort (n)
  (let ((list (loop for i below n collect (mod (* i 1103515245) 65521))))
    (reduce #'+ (subseq (sort list #'<) 0 10))))
(defclass shape () ())
 
(defclass circle (shape) ((r :initarg :r)))
(defclass square (shape) ((s :initarg :s)))
(defgeneric area (shape))
(defmethod area ((c circle)) (* 3 (slot-value c 'r) (slot-value c 'r)))
(defmethod area ((s square)) (* (slot-value s 's) (slot-value s 's)))
(defun clos-dispatch (n)
  (let ((shapes (list (make-instance 'circle :r 2) (make-instance 'square :s 3))) (sum 0))
    (dotimes (i n) (dolist (s shapes) (incf sum (area s))))
    sum))
(defun bench (name thunk)
  (let* ((start (get-internal-real-time)) (result (funcall thunk))
         (ms (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second)))
    (format t "~18A ~7D ms   ~A~%" name ms result)))

(defun main ()
  "The same portable Lisp on DOS and on the build host."
  (format t "~&~A ~A, ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version)
          (software-type) (machine-type))
  (bench "(fib 32)" (lambda () (fib 32)))
  (bench "sieve to 300000" (lambda () (sieve 300000)))
  (bench "mandelbrot 80x60" #'mandel-sum)
  (bench "digits of 1000!" (lambda () (factorial-digits 1000)))
  (bench "hash 20000 words" (lambda () (hash-words 20000)))
  (bench "sort 50000 conses" (lambda () (cons-and-sort 50000)))
  (bench "CLOS dispatch" (lambda () (clos-dispatch 100000)))
  (bench "full GC" (lambda () (gc :full t) :done)))

(doslib-examples:define-example "bench" 'main
  :category :lisp
  :description "The same portable Lisp on DOS and on the build host.")
