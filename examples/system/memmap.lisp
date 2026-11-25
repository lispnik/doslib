;;;; The DPMI host's view of memory (INT 31h) next to SBCL's own
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.memmap
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.memmap)

#+sbcl
(defun foreign-variable (name)          ; a C variable of the runtime
  (sap-ref-32 (int-sap (find-foreign-symbol-address name)) 0))
(defun row (name start end)
  (format t "  ~22A ~8,'0X - ~8,'0X  ~10:D KB~%" name start end
          (ceiling (- end start) 1024)))
(defun memmap ()
  (multiple-value-bind (ax bx cx dx) (int86 #x31 :ax #x0400)
    (declare (ignore dx))
    (format t "~&DPMI host       version ~D.~D, ~D86 CPU, flags ~4,'0B~%"
            (hi ax) (lo ax) (lo cx) (word16 bx)))
  (let ((info (alien-sap (make-alien (unsigned 8) 48))))
    (int86 #x31 :ax #x0500 :di (near-address (sap-int info)))      ; ES:EDI -> buffer
    (flet ((pages (offset) (* 4 (sap-ref-32 info offset))))
      (format t "Physical memory ~:D KB total, ~:D KB free~%" (pages #x18) (pages #x14))
      (format t "Largest block   ~:D KB;  linear address space ~:D KB, ~:D KB free~%"
              (floor (sap-ref-32 info 0) 1024) (pages #x0C) (pages #x1C))))
#+clisp
  (let ((base (- (near-address 0))))
    (format t "~%CLISP's address space (a DJGPP program: its segment starts at ~8,'0Xh)~%"
            (ldb (byte 32 0) base))
    (row "BIOS data area" #x400 #x500)
    (row "VGA graphics memory" #xA0000 #xB0000)
    (row "VGA text memory" #xB8000 #xC0000)
    (format t "  ~22A ~:D KB in use of ~:D KB~%" "Lisp heap"
            (floor (heap-bytes-used) 1024) (floor (heap-bytes-limit) 1024)))
  #+sbcl
  (progn
    (format t "~%SBCL's address space (a flat, zero-based 4GB segment)~%")
  (row "BIOS data area" #x400 #x500)
  (row "VGA graphics memory" #xA0000 #xB0000)
  (row "VGA text memory" #xB8000 #xC0000)
  (let ((base (logandc2 (find-foreign-symbol-address "os_init") #xFFFF)))
    (row "C runtime, about" base (+ base #x50000)))
  (row "control stack" (sap-int (sb-di::descriptor-sap sb-vm:*control-stack-start*))
       (sap-int (sb-di::descriptor-sap sb-vm:*control-stack-end*)))
  (row "alien linkage table" sb-vm:alien-linkage-space-start
       (+ sb-vm:alien-linkage-space-start sb-vm:alien-linkage-space-size))
  (row "static space" sb-vm:static-space-start
       (+ sb-vm:static-space-start sb-vm::static-space-size))
  (let ((start (foreign-variable "DYNAMIC_SPACE_START")))
    (row "dynamic space" start (+ start (sb-ext:dynamic-space-size)))
    (format t "  ~22A ~:D KB in use; committed 64KB at a time on page faults~%"
            "" (floor (sb-kernel:dynamic-usage) 1024)))
  (format t "  ~22A ~8,'0X~%" "stack pointer now" (sap-int (sb-vm::current-sp)))))

(defun main ()
  "The DPMI host's view of memory (INT 31h) next to SBCL's own."
  (memmap))

(doslib-examples:define-example "memmap" 'main
  :category :system
  :description "The DPMI host's view of memory (INT 31h) next to SBCL's own.")
