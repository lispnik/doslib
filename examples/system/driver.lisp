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
;;;;   Video memory: where a display card's picture memory is can be read
;;;;   off the PCI bus too.  (Not the old windows at A0000h and B8000h:
;;;;   every VGA card answers there, and no register says so.)  The bus
;;;;   says only "this much memory, at this address", not what it is for:
;;;;   which of a card's regions is the picture is the card's own business,
;;;;   so FRAMEBUFFER-ADDRESS guesses -- the first region marked
;;;;   "prefetchable", which registers never are -- and the guess is
;;;;   checked against the VESA BIOS, which knows its card.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.driver
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main #:station-address #:display-mode #:display-off
           #:memory-regions #:framebuffer-address))
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

;;; ---- video memory, from the PCI bus
;;; A base address register that is memory has, below the address, bit 0
;;; clear, bits 1 and 2 saying how wide the address is (2: 64 bits, the
;;; next register being its upper half), and bit 3 set if the memory is
;;; "prefetchable": reading it changes nothing, as with picture memory.
(defun memory-regions (card)
  "CARD's regions of memory, from its six base address registers: a list of
(register address prefetchable)."
  (destructuring-bind (bus device function &rest ids) card
    (declare (ignore ids))
    (let ((regions '()) (n 0))
      (loop while (< n 6)
            do (let ((value (pci-read bus device function (+ #x10 (* 4 n)))))
                 (unless (or (logbitp 0 value) (zerop (logand value #xFFFFFFF0)))
                   (push (list n (logand value #xFFFFFFF0) (logbitp 3 value)) regions))
                 (incf n (if (and (not (logbitp 0 value)) (= (ldb (byte 2 1) value) 2)) 2 1))))
      (nreverse regions))))

(defun framebuffer-address (card)
  "A guess at the physical address of CARD's picture memory: its first
prefetchable region of memory, or else its first region.  NIL if it has none."
  (let ((regions (memory-regions card)))
    (second (or (find-if #'third regions) (first regions)))))

(defun show-video-memory (card)
  (format t "  Its memory, by the PCI bus:~%")
  (loop for (n address prefetchable) in (memory-regions card)
        do (format t "    register ~D: ~8,'0Xh~:[~;, prefetchable~]~%" n address prefetchable))
  (let ((guess (framebuffer-address card))
        (bios (getf (vesa-mode-info #x101) :framebuffer)))
    (cond ((null guess) (format t "  None: the card has only the windows at A0000h and B8000h.~%"))
          ((null bios)
           (format t "  The picture memory should be at ~8,'0Xh; the VESA BIOS gives no address.~%"
                   guess))
          ((= guess bios)
           (format t "  The picture memory should be at ~8,'0Xh: the VESA BIOS says the same.~%"
                   guess))
          (t (format t "  A guess of ~8,'0Xh for the picture memory is wrong:~%  ~
                        the VESA BIOS says ~8,'0Xh.~%" guess bios)))))

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
  "Two small device drivers: a network card's address, video memory found on
the PCI bus, and a display mode with no BIOS."
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
    (when card
      (format t "~&Display card: vendor ~4,'0Xh, device ~4,'0Xh (bus ~D, device ~D).~%"
              (fourth card) (fifth card) (first card) (second card))
      (show-video-memory card))
    (cond ((null card) (format t "~&No VGA card on the PCI bus.~%"))
          ((not (display-present-p))
           (format t "  It has no Bochs display interface: the rest of this driver is for~%  ~
                      QEMU's standard VGA.~%"))
          (t (format t "  Bochs display interface version ~Xh, ~D MB of picture memory.~%"
                     (display-register 0) (floor (display-register #xA) 16))
             (format t "Press a key for 640x480 set through its own registers, and a key to come back.~%")
             (wait-key)
             (show-display card))))
  (values))

(doslib-examples:define-example "driver" 'main
  :category :system
  :description "Two small device drivers: a network card's address, and a display mode with no BIOS.")
