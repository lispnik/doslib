;;;; Two small device drivers, written against the hardware's own registers
;;;;
;;;; A driver is the code that knows a device's registers.  Finding the
;;;; device is the PCI bus's part: every card says who made it, what kind of
;;;; thing it is, and where its registers have been put (its "base address
;;;; registers": I/O ports, or memory at a physical address).  The rest is
;;;; reading the data sheet -- and in Lisp each step can be tried at the
;;;; listener, on the live card, before it becomes a function.
;;;;
;;;;   The network card: its station (MAC) address, read from the card
;;;;   itself.  Three cards are known here: AMD's PCnet (QEMU's and UTM's
;;;;   "pcnet", 86Box's "PCnet-PCI"), Realtek's RTL8139 and Intel's 8254x.
;;;;
;;;;   The display: QEMU's standard VGA card has, besides the VGA registers,
;;;;   a pair of I/O ports through which any size of screen can be asked
;;;;   for -- the "Bochs display interface".  DISPLAY-MODE sets 640x480 in
;;;;   256 colours with it, no BIOS involved; the picture memory is where
;;;;   the card's first base address register says, and MAP-PHYSICAL makes
;;;;   it addressable.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.driver
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:station-address #:display-mode #:display-off))
(in-package #:doslib-examples.driver)

;;; ---- finding a card
(defun find-card (class &optional subclass)
  "The first PCI function of CLASS (and SUBCLASS): bus, device, function, vendor, device id."
  (loop for (bus device function vendor id code) in (pci-devices :buses 8)
        when (and (= (ldb (byte 8 16) code) class)
                  (or (null subclass) (= (ldb (byte 8 8) code) subclass)))
          return (list bus device function vendor id)))

(defun base-address (card n)
  "Base address register N of CARD: the address, and whether it is I/O ports (or memory)."
  (destructuring-bind (bus device function &rest ids) card
    (declare (ignore ids))
    (let ((value (pci-read bus device function (+ #x10 (* 4 n)))))
      (if (logbitp 0 value)
          (values (logand value #xFFFC) t)
          (values (logand value #xFFFFFFF0) nil)))))

;;; ---- the network card
(defparameter *network-cards*
  '(((#x1022 #x2000) "AMD PCnet" :ports 0)             ; the address PROM is the first ports
    ((#x10EC #x8139) "Realtek RTL8139" :ports 0)       ; ID registers 0 to 5
    ((#x8086 #x100E) "Intel 82540EM" :memory #x5400)   ; receive address 0, low and high
    ((#x8086 #x100F) "Intel 82545EM" :memory #x5400)))

(defun station-address (card)
  "The six bytes of CARD's station address, its name, and how it was read;
NIL if the card is not one this driver knows."
  (destructuring-bind (bus device function vendor id) card
    (declare (ignore bus device function))
    (let ((known (cdr (assoc (list vendor id) *network-cards* :test #'equal))))
      (when known
        (destructuring-bind (name how offset) known
          (values (ecase how
                    (:ports (let ((base (base-address card 0)))
                              (loop for i below 6 collect (inb (+ base offset i)))))
                    (:memory (with-physical-memory (address (base-address card 0) #x6000)
                               (and address
                                    (loop for i below 6 collect (peek (+ address offset i)))))))
                  name how))))))

;;; ---- the display
(defconstant +index+ #x1CE)
(defconstant +data+ #x1CF)
(defun display-register (n) (outw +index+ n) (inw +data+))
(defun (setf display-register) (value n) (outw +index+ n) (outw +data+ value) value)
(defun display-present-p ()
  "Is there a Bochs display interface behind ports 1CEh and 1CFh?"
  (= (logand (display-register 0) #xFFF0) #xB0C0))
(defun display-mode (width height)
  "Set a WIDTH by HEIGHT screen of 256 colours, one byte a pixel, in one piece of memory."
  (setf (display-register 4) 0                  ; off while it is changed
        (display-register 1) width
        (display-register 2) height
        (display-register 3) 8                  ; bits a pixel
        (display-register 4) #x41))             ; on, with the memory all in one piece
(defun display-off ()
  "Back to what the VGA registers say, and to 80x25 text."
  (setf (display-register 4) 0)
  (video-mode 3))

(defun draw-picture (screen width height)
  (declare (type system-area-pointer screen) (fixnum width height))
  (dotimes (y height)
    (dotimes (x width)
      (setf (sap-ref-8 screen (+ x (* y width)))
            (1+ (mod (+ (ash (* x x) -9) (ash (* y y) -9) (logxor (ash x -3) (ash y -3))) 255))))))

(defun show-display (card)
  (let ((width 640) (height 480) (memory (base-address card 0)))
    (with-physical-memory (address memory (* 4096 (ceiling (* width height) 4096)))
      (cond ((null address)
             (format t "~&The DPMI host would not map the card's memory at ~8,'0Xh.~%" memory))
            (t (unwind-protect
                    (progn (display-mode width height)
                           (rainbow-palette)
                           (draw-picture (int-sap address) width height)
                           (wait-key))
                 (display-off))
               (format t "~&That was ~Dx~D in 256 colours, set through ports ~Xh and ~Xh~%~
                          and drawn in memory at ~8,'0Xh.~%" width height +index+ +data+ memory))))))

(defun main ()
  "Two small device drivers: a network card's address, and a display mode with no BIOS."
  (unless (pci-present-p)
    (format t "~&This machine has no PCI bus to ask.~%")
    (return-from main (values)))
  (let ((card (find-card 2)))
    (cond ((null card) (format t "~&No network card on the PCI bus.~%"))
          (t (destructuring-bind (bus device function vendor id) card
               (format t "~&Network card: vendor ~4,'0Xh, device ~4,'0Xh (bus ~D, device ~D), ~
                          interrupt ~D.~%"
                       vendor id bus device (pci-byte bus device function #x3C)))
             (multiple-value-bind (base ports) (base-address card 0)
               (format t "  Its registers are ~:[memory~;I/O ports~] at ~X.~%" ports base))
             (multiple-value-bind (address name) (station-address card)
               (if address
                   (format t "  It is an ~A.  Its station address, read from the card:~%    ~
                              ~{~2,'0X~^:~}~%"
                           name address)
                   (format t "  This driver does not know that card.~%"))))))
  (let ((card (find-card 3 0)))
    (cond ((null card) (format t "~&No VGA card on the PCI bus.~%"))
          ((not (display-present-p))
           (format t "~&The display card (vendor ~4,'0Xh, device ~4,'0Xh) has no Bochs display ~
                      interface:~%this driver is for QEMU's standard VGA.~%"
                   (fourth card) (fifth card)))
          (t (format t "~&Display card: vendor ~4,'0Xh, device ~4,'0Xh; display interface ~
                        version ~Xh.~%  ~D MB of picture memory at ~8,'0Xh.~%"
                     (fourth card) (fifth card) (display-register 0)
                     (floor (display-register #xA) 16) (base-address card 0))
             (format t "Press a key for 640x480 set through its own registers, and a key to come back.~%")
             (wait-key)
             (show-display card))))
  (values))

(doslib-examples:define-example "driver" 'main
  :category :system
  :description "Two small device drivers: a network card's address, and a display mode with no BIOS.")
