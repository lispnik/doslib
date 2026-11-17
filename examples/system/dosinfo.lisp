;;;; Asking DOS directly with INT 21h, bypassing the C library
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.dosinfo
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.dosinfo)

(defun dos (ah &key (al 0) (bx 0) (cx 0) (dx 0))
  (int86 #x21 :ax (+ (* 256 ah) al) :bx bx :cx cx :dx dx))
(defun dosinfo ()
  (multiple-value-bind (ax bx) (dos #x30)
    (format t "~&DOS version     ~D.~2,'0D   OEM ~2,'0Xh~A~%" (lo ax) (hi ax) (hi bx)
            (if (= (hi bx) #xFD) " (FreeDOS)" "")))
  (multiple-value-bind (ax bx) (dos #x33 :al 6)
    (declare (ignore ax))
    (format t "True version    ~D.~2,'0D~%" (lo bx) (hi bx)))
  (multiple-value-bind (ax bx cx dx) (dos #x2A)
    (declare (ignore bx))
    (format t "Date            ~A ~D-~2,'0D-~2,'0D~%"
            (nth (lo ax) '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
                           "Friday" "Saturday"))
            (word16 cx) (hi dx) (lo dx)))
  (multiple-value-bind (ax bx cx dx) (dos #x2C)
    (declare (ignore ax bx))
    (format t "Time            ~2,'0D:~2,'0D:~2,'0D.~2,'0D~%" (hi cx) (lo cx) (hi dx) (lo dx)))
  (format t "Current drive   ~C:    boot drive ~C:~%"
          (code-char (+ 65 (lo (dos #x19))))
          (code-char (+ 64 (lo (nth-value 3 (dos #x33 :al 5))))))
  (format t "Ctrl-Break      checking is ~:[off~;on~]~%"
          (plusp (lo (nth-value 3 (dos #x33 :al 0)))))
  (format t "Drives~%")
  (loop for drive from 3 to 26          ; C: onwards
        do (multiple-value-bind (ax bx cx dx) (dos #x36 :dx drive)
             (unless (= (word16 ax) #xFFFF)
               (let ((cluster (* (word16 ax) (word16 cx))))
                 (format t "  ~C:  ~9:D KB total  ~9:D KB free  (~D-byte clusters)~%"
                         (code-char (+ 64 drive))
                         (floor (* cluster (word16 dx)) 1024)
                         (floor (* cluster (word16 bx)) 1024) cluster)))))
  (format t "COMSPEC         ~A~%PATH            ~A~%"
          (posix-getenv "COMSPEC") (posix-getenv "PATH")))

(defun main ()
  "Asking DOS directly with INT 21h, bypassing the C library."
  (dosinfo))

(doslib-examples:define-example "dosinfo" 'main
  :category :system
  :description "Asking DOS directly with INT 21h, bypassing the C library.")
