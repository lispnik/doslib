;;;; A hardware inventory read from Lisp: CPUID, the BIOS
;;;; data area at 0040:0000, the BIOS ROM, and the CMOS clock via ports
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.sysinfo
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.sysinfo)

(defun reg-string (&rest words)         ; little-endian register dumps to text
  (with-output-to-string (s)
    (dolist (w words)
      (dotimes (i 4)
        (let ((c (ldb (byte 8 (* 8 i)) w)))
          (when (<= 32 c 126) (write-char (code-char c) s)))))))
(defun sysinfo ()
  (multiple-value-bind (max-leaf b c d) (cpuid 0)
    (format t "~&CPU vendor      ~A~%" (reg-string b d c))
    (multiple-value-bind (a b2 c2 d2) (cpuid 1)
      (declare (ignore b2 c2))
      (format t "CPU signature   family ~D, model ~D, stepping ~D   (CPUID leaves 0..~D)~%"
              (ldb (byte 4 8) a) (ldb (byte 4 4) a) (ldb (byte 4 0) a) max-leaf)
      (format t "CPU features   ~{ ~A~}~%"
              (loop for (bit name) in '((0 fpu) (4 tsc) (5 msr) (8 cx8) (15 cmov)
                                        (23 mmx) (24 fxsr) (25 sse) (26 sse2))
                    when (logbitp bit d2) collect name))))
  (when (>= (cpuid #x80000000) #x80000004)
    (format t "CPU name        ~A~%"
            (string-trim " " (apply #'concatenate 'string
                                    (loop for leaf from #x80000002 to #x80000004
                                          collect (multiple-value-call #'reg-string
                                                    (cpuid leaf)))))))
  (format t "BIOS date       ~A~%"
          (map 'string #'code-char (loop for i below 8 collect (peek (+ #xFFFF5 i)))))
  (let ((equipment (peek16 #x410)))
    (format t "Equipment word  ~4,'0X: ~D floppy drive~:P, ~D serial, ~D parallel port~:P~:[~;, FPU~]~%"
            equipment
            (if (logbitp 0 equipment) (1+ (ldb (byte 2 6) equipment)) 0)
            (ldb (byte 3 9) equipment) (ldb (byte 2 14) equipment)
            (logbitp 1 equipment)))
  (format t "I/O ports       COM1 ~3,'0Xh  COM2 ~3,'0Xh  LPT1 ~3,'0Xh~%"
          (peek16 #x400) (peek16 #x402) (peek16 #x408))
  (format t "Base memory     ~D KB (BIOS data area), ~D KB extended (CMOS)~%"
          (peek16 #x413) (+ (cmos #x30) (* 256 (cmos #x31))))
  (format t "Video           mode ~2,'0Xh, ~D x ~D text, CRTC at ~3,'0Xh~%"
          (peek #x449) (peek16 #x44A) (1+ (peek #x484)) (peek16 #x463))
  (format t "CMOS clock      ~2,'0D~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D~%"
          (bcd (cmos #x32)) (bcd (cmos 9)) (bcd (cmos 8)) (bcd (cmos 7))
          (bcd (cmos 4)) (bcd (cmos 2)) (bcd (cmos 0)))
  (format t "Timer ticks     ~D since midnight (18.2 per second)~%" (peek32 #x46C)))

(defun main ()
  "A hardware inventory read from Lisp."
  (sysinfo))

(doslib-examples:define-example "sysinfo" 'main
  :category :system
  :description "A hardware inventory read from Lisp.")
