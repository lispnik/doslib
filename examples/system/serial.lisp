;;;; A Lisp listener on COM1.  The 16550 UART is programmed
;;;; directly through its I/O ports (no BIOS, no DOS, no interrupts: the
;;;; line status register is polled -- that part is the library's
;;;; src/serial.lisp), and whatever arrives is READ, EVALed and PRINTed back
;;;; down the wire.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.serial
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.serial)

(defconstant +com1+ #x3F8)
(defun listener ()
  (serial-open :port +com1+)
  (format t "~&Listening on COM1 (~Xh), 115200 8N1.  Send :BYE to stop.~%" +com1+)
  (serial-write-string +com1+ (format nil "~%~A ~A on ~A ~A~%" (lisp-implementation-type)
                        (lisp-implementation-version) (software-type) (software-version)))
  (loop
    (serial-write-string +com1+ "COM1> ")
    (let ((line (serial-read-line +com1+)))
      (unless (string= line "")
        (format t "<- ~A~%" line)
        (let ((reply (handler-case
                         (let* ((form (read-from-string line))
                                (values nil)
                                (output (with-output-to-string (*standard-output*)
                                          (setf values (multiple-value-list (eval form))))))
                           (when (eq form :bye)
                             (serial-write-string +com1+ (format nil "Goodbye.~%"))
                             (return))
                           (format nil "~A~{~S~%~}" output values))
                       (error (e) (format nil "ERROR (~A): ~A~%" (type-of e) e)))))
          (format t "-> ~A" reply)
          (serial-write-string +com1+ reply))))))

(defun main ()
  "A Lisp listener on COM1."
  (listener))

(doslib-examples:define-example "serial" 'main
  :category :system
  :description "A Lisp listener on COM1.")
