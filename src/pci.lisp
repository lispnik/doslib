;;;; PCI configuration space, by the ports CF8h and CFCh.
;;;;
;;;; Every PCI device has 256 bytes of registers that say what it is and
;;;; where its memory and ports are.  A device is named by its bus (0 to
;;;; 255), its device number on the bus (0 to 31) and its function (0 to 7).
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defun pci-address (bus device function register)
  (outl #xCF8 (logior #x80000000 (ash bus 16) (ash device 11) (ash function 8)
                      (logand register #xFC))))
(defun pci-read (bus device function register)
  "The 32 bits at REGISTER (a multiple of 4) of a device's configuration space."
  (pci-address bus device function register) (inl #xCFC))
(defun pci-write (bus device function register value)
  "Write the 32 bits of VALUE at REGISTER of a device's configuration space."
  (pci-address bus device function register) (outl #xCFC value))
(defun pci-byte (bus device function register)
  "The byte at REGISTER of a device's configuration space."
  (ldb (byte 8 (* 8 (logand register 3))) (pci-read bus device function register)))
(defun pci-word (bus device function register)
  "The 16 bits at REGISTER (even) of a device's configuration space."
  (ldb (byte 16 (* 8 (logand register 2))) (pci-read bus device function register)))
(defun pci-present-p ()
  "Does the machine have PCI, as far as the configuration ports can tell?"
  (outl #xCF8 #x80000000)
  (= (inl #xCF8) #x80000000))
(defun pci-devices (&key (buses 256))
  "Every PCI function there is, as lists: bus, device, function, vendor id,
device id, and the 24-bit class code (class, subclass, interface)."
  (let ((found '()))
    (when (pci-present-p)
      (dotimes (bus buses)
        (dotimes (device 32)
          (unless (= (logand (pci-read bus device 0 0) #xFFFF) #xFFFF)
            (dotimes (function (if (logbitp 7 (pci-byte bus device 0 #x0E)) 8 1))
              (let ((id (pci-read bus device function 0)))
                (unless (= (logand id #xFFFF) #xFFFF)
                  (push (list bus device function (logand id #xFFFF) (ash id -16)
                              (ash (pci-read bus device function 8) -8))
                        found))))))))
    (nreverse found)))
