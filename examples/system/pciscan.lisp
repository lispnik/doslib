;;;; Walk the PCI bus tree by hand.  Configuration space is
;;;; reached by writing a bus/device/function/register address to port CF8h
;;;; and reading or writing the 32-bit value at port CFCh.  For each function
;;;; this decodes the header, sizes the base address registers, follows the
;;;; capability list, and descends through PCI-to-PCI bridges.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.pciscan
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.pciscan)

(defun pci-address (bus device function register)
  (outl #xCF8 (logior #x80000000 (ash bus 16) (ash device 11) (ash function 8)
                      (logand register #xFC))))
(defun pci-read (bus device function register)
  (pci-address bus device function register) (inl #xCFC))
(defun pci-write (bus device function register value)
  (pci-address bus device function register) (outl #xCFC value))
(defun pci-byte (bus device function register)
  (ldb (byte 8 (* 8 (logand register 3))) (pci-read bus device function register)))
(defun pci-word (bus device function register)
  (ldb (byte 16 (* 8 (logand register 2))) (pci-read bus device function register)))
(defvar *vendors* '((#x8086 . "Intel") (#x1234 . "QEMU/Bochs") (#x1AF4 . "Red Hat virtio")
                    (#x10EC . "Realtek") (#x1022 . "AMD") (#x10DE . "NVIDIA")
                    (#x1B36 . "Red Hat QEMU") (#x1013 . "Cirrus Logic")
                    (#x1274 . "Ensoniq") (#x1033 . "NEC")))
(defvar *classes* '(((1 . 1) . "IDE controller") ((1 . 6) . "SATA controller")
                    ((2 . 0) . "Ethernet controller") ((3 . 0) . "VGA controller")
                    ((4 . 1) . "Audio device") ((6 . 0) . "Host bridge")
                    ((6 . 1) . "ISA bridge") ((6 . 4) . "PCI-to-PCI bridge")
                    ((6 . #x80) . "Bridge (other)") ((12 . 3) . "USB controller")
                    ((0 . #xFF) . "Unclassified") ((#xFF . 0) . "Unclassified")))
(defvar *capabilities* '((1 . "power management") (5 . "MSI") (9 . "vendor-specific")
                         (#x0C . "hot-plug") (#x0D . "bridge subsystem ID")
                         (#x10 . "PCI Express") (#x11 . "MSI-X") (#x12 . "SATA")))
(defun size-string (bytes)
  (cond ((>= bytes (ash 1 20)) (format nil "~DM" (ash bytes -20)))
        ((>= bytes 1024) (format nil "~DK" (ash bytes -10)))
        (t (format nil "~D" bytes))))
;;; The size of a BAR is found by writing all ones and seeing which bits
;;; stick, then putting the original value back.
(defun bar-size (bus device function register original)
  (pci-write bus device function register #xFFFFFFFF)
  (let ((mask (pci-read bus device function register)))
    (pci-write bus device function register original)
    (let ((bits (logand mask (if (logbitp 0 original) #xFFFC #xFFFFFFF0))))
      (if (zerop bits) 0
          (1+ (logxor bits (if (logbitp 0 original) #xFFFF #xFFFFFFFF)))))))
(defun show-bars (bus device function count indent)
  (loop with i = 0 while (< i count)
        do (let* ((register (+ #x10 (* 4 i)))
                  (bar (pci-read bus device function register)))
             (unless (zerop bar)
               (let ((size (bar-size bus device function register bar)))
                 (if (logbitp 0 bar)
                     (format t "~A  BAR~D  I/O ports ~4,'0Xh, ~A bytes~%" indent i
                             (logand bar #xFFFC) (size-string size))
                     (format t "~A  BAR~D  memory ~8,'0Xh, ~A, ~D-bit~:[~;, prefetchable~]~%"
                             indent i (logand bar #xFFFFFFF0) (size-string size)
                             (if (= (ldb (byte 2 1) bar) 2) 64 32) (logbitp 3 bar)))))
             (incf i (if (and (not (logbitp 0 bar)) (= (ldb (byte 2 1) bar) 2)) 2 1)))))
(defun capabilities (bus device function)
  (when (logbitp 4 (pci-word bus device function 6))    ; status: has a list
    (loop for pointer = (logand (pci-byte bus device function #x34) #xFC)
            then (logand (pci-byte bus device function (1+ pointer)) #xFC)
          for guard from 0 below 32
          until (zerop pointer)
          collect (let ((id (pci-byte bus device function pointer)))
                    (or (cdr (assoc id *capabilities*)) (format nil "cap ~2,'0Xh" id))))))
(defvar *count* 0)
(declaim (ftype function scan-bus))
(defun show (bus device function depth)
  (let* ((indent (make-string (* 2 depth) :initial-element #\Space))
         (id (pci-read bus device function 0))
         (class (pci-read bus device function 8))
         (header (logand (pci-byte bus device function #x0E) #x7F))
         (command (pci-word bus device function 4))
         (pin (pci-byte bus device function #x3D))
         (line (pci-byte bus device function #x3C))
         (subsystem (if (= header 0) (pci-read bus device function #x2C) 0))
         (caps (capabilities bus device function)))
    (incf *count*)
    (format t "~A~2,'0X:~2,'0X.~D ~4,'0X:~4,'0X rev ~2,'0X  ~A ~A~%" indent
            bus device function (logand id #xFFFF) (ash id -16) (logand class #xFF)
            (or (cdr (assoc (logand id #xFFFF) *vendors*)) "?")
            (or (cdr (assoc (cons (ldb (byte 8 24) class) (ldb (byte 8 16) class))
                            *classes* :test #'equal))
                (format nil "class ~2,'0X/~2,'0X" (ldb (byte 8 24) class)
                        (ldb (byte 8 16) class))))
    (format t "~A  ~:[~;I/O ~]~:[~;memory ~]~:[~;bus-master ~]~@[IRQ ~D ~]~@[(INT~C) ~]~@[subsystem ~A~]~%"
            indent (logbitp 0 command) (logbitp 1 command) (logbitp 2 command)
            (and (plusp pin) (< line 255) line)
            (and (plusp pin) (code-char (+ 64 pin)))
            (and (plusp subsystem)
                 (format nil "~4,'0X:~4,'0X" (logand subsystem #xFFFF) (ash subsystem -16))))
    (show-bars bus device function (if (= header 1) 2 6) indent)
    (when caps (format t "~A  capabilities: ~{~A~^, ~}~%" indent caps))
    (when (= header 1)                  ; a bridge: go down to the bus behind it
      (let ((secondary (pci-byte bus device function #x19)))
        (format t "~A  bridge to bus ~2,'0X (subordinate ~2,'0X)~%" indent secondary
                (pci-byte bus device function #x1A))
        (when (> secondary bus) (scan-bus secondary (1+ depth)))))))
(defun scan-bus (bus depth)
  (dotimes (device 32)
    (unless (= (logand (pci-read bus device 0 0) #xFFFF) #xFFFF)
      ;; bit 7 of the header type says whether functions 1-7 exist
      (dotimes (function (if (logbitp 7 (pci-byte bus device 0 #x0E)) 8 1))
        (unless (= (logand (pci-read bus device function 0) #xFFFF) #xFFFF)
          (show bus device function depth))))))
(defun pciscan ()
  (setf *count* 0)
  (scan-bus 0 0)
  (format t "~D PCI function~:P found~%" *count*))

(defun main ()
  "Walk the PCI bus tree by hand."
  (pciscan))

(doslib-examples:define-example "pciscan" 'main
  :category :system
  :description "Walk the PCI bus tree by hand.")
