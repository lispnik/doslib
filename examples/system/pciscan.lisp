;;;; Everything the PCI bus will say about itself
;;;;
;;;; Every PCI function has 256 bytes of configuration registers, reached by
;;;; writing a bus/device/function/register address to port CF8h and reading
;;;; or writing 32 bits at port CFCh.  This walks the whole tree, descending
;;;; through PCI-to-PCI bridges, and prints all it can find:
;;;;
;;;;   - what the BIOS says: the PCI BIOS (INT 1Ah, AX = B101h), the BIOS32
;;;;     service directory, and the interrupt routing table ("$PIR") that
;;;;     says which interrupt pin of which slot is wired to which link;
;;;;   - for each function: who made it and what it is, the command and
;;;;     status registers bit by bit, timers, the interrupt pin and line,
;;;;     the subsystem, and the first 64 bytes as they stand;
;;;;   - its base address registers, each sized by the standard trick:
;;;;     write all ones, read back which bits stuck, put the value back;
;;;;   - its expansion ROM: where, how big, and -- by switching the ROM on
;;;;     for a moment and mapping it -- what the image in it is for;
;;;;   - a bridge's bus numbers and the windows of ports and memory it
;;;;     passes on;
;;;;   - the capability list, decoded: power management, MSI, MSI-X, PCI
;;;;     Express, AGP and the rest;
;;;;   - registers particular to chips this knows: how the PIIX's ISA
;;;;     bridge routes the four PCI interrupt links to IRQs;
;;;;   - and at the end the machine's map: every range of ports and memory
;;;;     claimed, and who shares which IRQ.
;;;;
;;;; What it cannot reach: PCI Express's extended capabilities live beyond
;;;; register 255, which ports CF8h/CFCh cannot address.
;;;;
;;;;   DEMO PCISCAN           the report, a screenful at a time
;;;;   DEMO PCISCAN FILE      the report written to PCISCAN.TXT
;;;;   DEMO PCISCAN DUMP      with all 256 bytes of each function, not 64
;;;;   DEMO PCISCAN NOROM     without switching expansion ROMs on to read them
;;;;
;;;; In the pager: Space the next screenful, Enter one line, A all the rest,
;;;; Esc or Q enough.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.pciscan
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.pciscan)

;;; (PCI-READ, PCI-WRITE, PCI-BYTE and PCI-WORD are the library's: src/pci.lisp.)

;;; ---- names
(defparameter *vendors*
  '((#x8086 . "Intel") (#x1234 . "QEMU/Bochs") (#x1AF4 . "Red Hat (virtio)")
    (#x1B36 . "Red Hat (QEMU)") (#x10EC . "Realtek") (#x1022 . "AMD") (#x10DE . "NVIDIA")
    (#x1002 . "ATI") (#x1013 . "Cirrus Logic") (#x1274 . "Ensoniq") (#x1033 . "NEC")
    (#x5333 . "S3") (#x102B . "Matrox") (#x121A . "3dfx") (#x1102 . "Creative Labs")
    (#x10B7 . "3Com") (#x9004 . "Adaptec") (#x9005 . "Adaptec") (#x1106 . "VIA")
    (#x1039 . "SiS") (#x1023 . "Trident") (#x100C . "Tseng Labs") (#x1000 . "NCR/Symbios")
    (#x1011 . "DEC") (#x14E4 . "Broadcom") (#x15AD . "VMware") (#x80EE . "VirtualBox")
    (#x125D . "ESS") (#x1073 . "Yamaha") (#x13F6 . "C-Media") (#x105A . "Promise")
    (#x104C . "Texas Instruments") (#x105D . "Number Nine") (#x10B9 . "ALi") (#x1045 . "OPTi")
    (#x1078 . "Cyrix") (#x0E11 . "Compaq") (#x1014 . "IBM") (#x10AD . "Winbond")
    (#x1163 . "Rendition") (#x12EB . "Aureal") (#x1050 . "Winbond") (#x10C8 . "NeoMagic")
    (#x1142 . "Alliance") (#x3D3D . "3Dlabs") (#x12D2 . "NVIDIA/SGS") (#x1414 . "Microsoft")))

(defparameter *devices*
  '(((#x8086 . #x1237) . "440FX host bridge") ((#x8086 . #x7000) . "PIIX3 ISA bridge")
    ((#x8086 . #x7010) . "PIIX3 IDE") ((#x8086 . #x7020) . "PIIX3 USB")
    ((#x8086 . #x7110) . "PIIX4 ISA bridge") ((#x8086 . #x7111) . "PIIX4 IDE")
    ((#x8086 . #x7112) . "PIIX4 USB") ((#x8086 . #x7113) . "PIIX4 power management")
    ((#x8086 . #x122E) . "PIIX ISA bridge") ((#x8086 . #x1230) . "PIIX IDE")
    ((#x8086 . #x7190) . "440BX host bridge") ((#x8086 . #x7191) . "440BX AGP bridge")
    ((#x8086 . #x100E) . "82540EM gigabit Ethernet") ((#x8086 . #x2415) . "AC'97 audio")
    ((#x8086 . #x29C0) . "Q35 host bridge") ((#x8086 . #x2918) . "ICH9 LPC bridge")
    ((#x8086 . #x2922) . "ICH9 AHCI") ((#x8086 . #x2930) . "ICH9 SMBus")
    ((#x1234 . #x1111) . "standard VGA") ((#x1022 . #x2000) . "PCnet (Am79C970)")
    ((#x10EC . #x8139) . "RTL8139 Ethernet") ((#x10EC . #x8029) . "RTL8029 (NE2000)")
    ((#x1013 . #x00B8) . "GD5446 VGA") ((#x1274 . #x5000) . "ES1370 AudioPCI")
    ((#x1274 . #x1371) . "ES1371 AudioPCI") ((#x121A . #x0001) . "Voodoo Graphics")
    ((#x121A . #x0002) . "Voodoo2") ((#x121A . #x0003) . "Voodoo Banshee")
    ((#x121A . #x0005) . "Voodoo3") ((#x1102 . #x0002) . "Sound Blaster Live!")
    ((#x1B36 . #x0001) . "PCI-to-PCI bridge") ((#x1B36 . #x000D) . "xHCI USB")
    ((#x1AF4 . #x1000) . "virtio network") ((#x1AF4 . #x1001) . "virtio block")
    ((#x15AD . #x0405) . "SVGA II")))

(defparameter *classes*
  '((0 "Built before classes" (0 . "not VGA") (1 . "VGA-compatible"))
    (1 "Mass storage" (0 . "SCSI") (1 . "IDE") (2 . "floppy") (3 . "IPI") (4 . "RAID")
     (5 . "ATA") (6 . "SATA") (7 . "SAS") (8 . "NVM"))
    (2 "Network" (0 . "Ethernet") (1 . "Token Ring") (2 . "FDDI") (3 . "ATM") (4 . "ISDN"))
    (3 "Display" (0 . "VGA") (1 . "XGA") (2 . "3D"))
    (4 "Multimedia" (0 . "video") (1 . "audio") (2 . "telephony") (3 . "audio device"))
    (5 "Memory" (0 . "RAM") (1 . "flash"))
    (6 "Bridge" (0 . "host") (1 . "ISA") (2 . "EISA") (3 . "MicroChannel") (4 . "PCI-to-PCI")
     (5 . "PCMCIA") (6 . "NuBus") (7 . "CardBus") (8 . "RACEway") (#x80 . "other"))
    (7 "Communication" (0 . "serial") (1 . "parallel") (2 . "multiport serial") (3 . "modem"))
    (8 "System peripheral" (0 . "interrupt controller") (1 . "DMA controller") (2 . "timer")
     (3 . "real-time clock") (4 . "hot-plug controller") (5 . "SD host"))
    (9 "Input" (0 . "keyboard") (1 . "digitiser") (2 . "mouse") (3 . "scanner")
     (4 . "game port"))
    (10 "Docking station") (11 "Processor" (0 . "386") (1 . "486") (2 . "Pentium"))
    (12 "Serial bus" (0 . "FireWire") (1 . "ACCESS.bus") (2 . "SSA") (3 . "USB")
     (4 . "Fibre Channel") (5 . "SMBus"))
    (13 "Wireless") (14 "Intelligent I/O") (15 "Satellite") (16 "Encryption")
    (17 "Signal processing") (255 "Unclassified")))

(defparameter *capabilities*
  '((1 . "power management") (2 . "AGP") (3 . "vital product data") (4 . "slot identification")
    (5 . "MSI") (6 . "CompactPCI hot swap") (7 . "PCI-X") (8 . "HyperTransport")
    (9 . "vendor-specific") (#x0A . "debug port") (#x0B . "CompactPCI resource control")
    (#x0C . "hot-plug") (#x0D . "bridge subsystem ID") (#x0E . "AGP 8x") (#x0F . "secure device")
    (#x10 . "PCI Express") (#x11 . "MSI-X") (#x12 . "SATA") (#x13 . "advanced features")
    (#x14 . "enhanced allocation")))

(defun vendor-name (vendor) (or (cdr (assoc vendor *vendors*)) "unknown maker"))
(defun device-name (vendor device)
  (cdr (assoc (cons vendor device) *devices* :test #'equal)))
(defun class-name-of (class subclass interface)
  (let ((entry (assoc class *classes*)))
    (format nil "~A~@[: ~A~]~@[ (~A)~]"
            (if entry (second entry) "Unknown class")
            (cdr (assoc subclass (cddr entry)))
            (cond ((and (= class 12) (= subclass 3))
                   (case interface (0 "UHCI") (#x10 "OHCI") (#x20 "EHCI") (#x30 "xHCI")))
                  ((and (= class 1) (= subclass 1) (plusp interface))
                   (format nil "~:[~;bus master; ~]primary ~:[at the old ports~;native~], ~
                                secondary ~:[too~;native~]"
                           (logbitp 7 interface) (logbitp 0 interface) (logbitp 2 interface)))
                  ((and (= class 1) (= subclass 6) (= interface 1)) "AHCI")))))

(defun size-string (bytes)
  (cond ((>= bytes (ash 1 30)) (format nil "~DG" (ash bytes -30)))
        ((>= bytes (ash 1 20)) (format nil "~DM" (ash bytes -20)))
        ((>= bytes 1024) (format nil "~DK" (ash bytes -10)))
        (t (format nil "~D bytes" bytes))))

(defun bits (word names)
  "The names of the bits set in WORD; NAMES is a list of (bit . name)."
  (loop for (bit . name) in names when (logbitp bit word) collect name))

;;; ---- what is gathered for the map at the end
(defvar *count* 0)
(defvar *ranges* '())                   ; (kind start size "who")
(defvar *interrupts* '())               ; (irq . "who")
(defvar *dump* 64)
(defvar *read-roms* t)

(defun who (bus device function)
  (format nil "~2,'0X:~2,'0X.~D" bus device function))

;;; ---- base address registers
;;; The size of a BAR is found by writing all ones and seeing which bits
;;; stick, then putting the original value back.
(defun probe (bus device function register)
  (let ((original (pci-read bus device function register)))
    (pci-write bus device function register #xFFFFFFFF)
    (prog1 (pci-read bus device function register)
      (pci-write bus device function register original))))

(defun show-bars (bus device function count)
  (loop with i = 0 while (< i count)
        do (let* ((register (+ #x10 (* 4 i)))
                  (bar (pci-read bus device function register))
                  (mask (probe bus device function register))
                  (wide (and (not (logbitp 0 bar)) (= (ldb (byte 2 1) bar) 2))))
             (cond ((zerop mask))       ; not implemented
                   ((logbitp 0 bar)
                    (let ((base (logand bar #xFFFC))
                          (size (1+ (logxor (logand mask #xFFFC) #xFFFF))))
                      (format t "  BAR~D  I/O ports ~4,'0Xh-~4,'0Xh (~A)~:[~;, not assigned~]~%"
                              i base (+ base size -1) (size-string size) (zerop base))
                      (when (plusp base)
                        (push (list :io base size (who bus device function)) *ranges*))))
                   (t
                    (let* ((high (if wide (pci-read bus device function (+ register 4)) 0))
                           (high-mask (if wide (probe bus device function (+ register 4)) 0))
                           (base (+ (logand bar #xFFFFFFF0) (ash high 32)))
                           (size (1+ (logxor (+ (logand mask #xFFFFFFF0) (ash high-mask 32))
                                             (if wide #xFFFFFFFFFFFFFFFF #xFFFFFFFF)))))
                      (format t "  BAR~D  memory ~8,'0Xh-~8,'0Xh (~A), ~A~
                                 ~:[~;, prefetchable~]~:[~;, not assigned~]~%"
                              i base (+ base size -1) (size-string size)
                              (case (ldb (byte 2 1) bar)
                                (0 "32-bit") (1 "below 1 MB") (2 "64-bit") (t "reserved type"))
                              (logbitp 3 bar) (zerop base))
                      (when (plusp base)
                        (push (list :memory base size (who bus device function)) *ranges*)))))
             (incf i (if wide 2 1)))))

;;; ---- the expansion ROM
(defun rom-images (address size)
  "Describe the image in a ROM that is switched on at ADDRESS."
  (with-physical-memory (rom address (min size 65536))
    (cond ((null rom) (format t "        (the DPMI host would not map it)~%"))
          ((/= (peek16 rom) #xAA55)
           (format t "        no image: it begins ~4,'0Xh, not AA55h~%" (peek16 rom)))
          (t
           (let ((data (+ rom (peek16 (+ rom #x18)))))
             (if (and (< (- data rom) (- (min size 65536) 24))
                      (= (peek32 data) #x52494350))       ; "PCIR"
                 (format t "        image: ~A for ~4,'0X:~4,'0X, ~A, revision ~D~
                            ~:[, more follow~;~]~%"
                         (case (peek (+ data #x14))
                           (0 "x86 BIOS code") (1 "Open Firmware") (2 "HP PA-RISC") (3 "EFI")
                           (t "code of an unknown kind"))
                         (peek16 (+ data 4)) (peek16 (+ data 6))
                         (size-string (* 512 (peek16 (+ data #x10)))) (peek16 (+ data #x12))
                         (logbitp 7 (peek (+ data #x15))))
                 (format t "        an image (AA55h) with no PCI data structure~%")))))))

(defun show-rom (bus device function register)
  (let* ((rom (pci-read bus device function register))
         (mask (logand (probe bus device function register) #xFFFFF800)))
    (unless (zerop mask)
      (let ((base (logand rom #xFFFFF800)) (size (1+ (logxor mask #xFFFFFFFF))))
        (format t "  ROM   ~:[not assigned~*~;~8,'0Xh~], ~A, ~:[switched off~;switched on~]~%"
                (plusp base) base (size-string size) (logbitp 0 rom))
        (when (plusp base)
          (push (list :memory base size (format nil "~A ROM" (who bus device function)))
                *ranges*)
          (when (and *read-roms* (logbitp 1 (pci-word bus device function 4)))
            (unwind-protect
                 (progn (pci-write bus device function register (logior rom 1))
                        (rom-images base size))
              (pci-write bus device function register rom))))))))

;;; ---- capabilities
(defun show-capability (bus device function at id)
  (flet ((byte-at (n) (pci-byte bus device function (+ at n)))
         (word-at (n) (pci-word bus device function (+ at n)))
         (long-at (n) (pci-read bus device function (+ at n))))
    (format t "  [~2,'0Xh] ~A" at
            (or (cdr (assoc id *capabilities*)) (format nil "capability ~2,'0Xh" id)))
    (case id
      (1 (let ((caps (word-at 2)) (control (word-at 4)))
           (format t " version ~D: in D~D now; supports D0~:[~; D1~]~:[~; D2~] D3; ~
                      ~:[no PME~;PME from~{ ~A~}~]"
                   (ldb (byte 3 0) caps) (ldb (byte 2 0) control)
                   (logbitp 9 caps) (logbitp 10 caps)
                   (plusp (ldb (byte 5 11) caps))
                   (bits (ldb (byte 5 11) caps)
                         '((0 . "D0") (1 . "D1") (2 . "D2") (3 . "D3hot") (4 . "D3cold"))))))
      (2 (let ((status (long-at 4)) (command (long-at 8)))
           (format t " ~D.~D: rates~{ ~A~}~:[~;, sideband addressing~]; ~:[disabled~;enabled~]"
                   (ldb (byte 4 4) (byte-at 2)) (ldb (byte 4 0) (byte-at 2))
                   (bits status '((0 . "1x") (1 . "2x") (2 . "4x")))
                   (logbitp 9 status) (logbitp 8 command))))
      (5 (let ((control (word-at 2)))
           (format t ": ~:[disabled~;enabled~], ~D of ~D message~:P, ~:[32~;64~]-bit address~
                      ~:[~;, maskable~]"
                   (logbitp 0 control) (ash 1 (ldb (byte 3 4) control))
                   (ash 1 (ldb (byte 3 1) control)) (logbitp 7 control) (logbitp 8 control))))
      (9 (format t ", ~D bytes" (byte-at 2)))
      (#x0D (format t " ~4,'0X:~4,'0X" (word-at 4) (word-at 6)))
      (#x10 (let ((caps (word-at 2)) (link (long-at #x0C)) (link-status (word-at #x12)))
              (flet ((speed (code)
                       (case code (1 "2.5 GT/s") (2 "5 GT/s") (3 "8 GT/s") (t "?"))))
                (format t " version ~D, ~A; link up to ~A x~D, now ~A x~D; ~
                           payload up to ~D bytes"
                        (ldb (byte 4 0) caps)
                        (case (ldb (byte 4 4) caps)
                          (0 "endpoint") (1 "legacy endpoint") (4 "root port")
                          (5 "upstream port") (6 "downstream port") (7 "PCIe-to-PCI bridge")
                          (8 "PCI-to-PCIe bridge") (9 "integrated endpoint") (t "port"))
                        (speed (ldb (byte 4 0) link)) (ldb (byte 6 4) link)
                        (speed (ldb (byte 4 0) link-status)) (ldb (byte 6 4) link-status)
                        (ash 128 (ldb (byte 3 0) (long-at 4)))))))
      (#x11 (let ((control (word-at 2)) (table (long-at 4)) (pending (long-at 8)))
              (format t ": ~:[disabled~;enabled~], ~D vector~:P; table in BAR~D at ~Xh, ~
                         pending bits in BAR~D at ~Xh"
                      (logbitp 15 control) (1+ (ldb (byte 11 0) control))
                      (logand table 7) (logandc2 table 7)
                      (logand pending 7) (logandc2 pending 7))))
      (#x13 (format t ":~{ ~A~}" (bits (byte-at 3) '((0 . "transactions pending")
                                                    (1 . "function-level reset"))))))
    (terpri)))

(defun show-capabilities (bus device function)
  (when (logbitp 4 (pci-word bus device function 6))    ; status: there is a list
    (format t "  capabilities:~%")
    (loop for at = (logand (pci-byte bus device function #x34) #xFC)
            then (logand (pci-byte bus device function (1+ at)) #xFC)
          for guard from 0 below 48
          until (< at #x40)
          do (show-capability bus device function at (pci-byte bus device function at)))))

;;; ---- chips this knows more about
(defun show-chip (bus device function vendor id)
  (when (and (= vendor #x8086) (member id '(#x7000 #x7110 #x122E)))
    (format t "  PIIX routes link")                 ; registers 60h-63h
    (loop for link below 4
          do (let ((route (pci-byte bus device function (+ #x60 link))))
               (format t " ~C ~:[to IRQ ~D~;nowhere~*~]~:[,~;~]" (code-char (+ 65 link))
                       (logbitp 7 route) (logand route 15) (= link 3))))
    (terpri))
  (when (and (= (pci-byte bus device function #x0B) 1) (= (pci-byte bus device function #x0A) 1))
    (let ((interface (pci-byte bus device function 9)))   ; an IDE controller's old places
      (unless (logbitp 0 interface)
        (format t "  primary channel: ports 01F0h-01F7h and 03F6h, IRQ 14~%")
        (push (list :io #x1F0 8 (format nil "~A primary" (who bus device function))) *ranges*)
        (push (cons 14 (who bus device function)) *interrupts*))
      (unless (logbitp 2 interface)
        (format t "  secondary channel: ports 0170h-0177h and 0376h, IRQ 15~%")
        (push (list :io #x170 8 (format nil "~A secondary" (who bus device function))) *ranges*)
        (push (cons 15 (who bus device function)) *interrupts*))))
  (when (and (= vendor #x8086) (= id #x1237))
    (format t "  440FX BIOS shadowing, registers 59h-5Fh:~{ ~2,'0X~}; SMRAM ~2,'0Xh~%"
            (loop for register from #x59 to #x5F
                  collect (pci-byte bus device function register))
            (pci-byte bus device function #x72))))

;;; ---- one function
(declaim (ftype function scan-bus))

(defun show-bridge (bus device function)
  "What a PCI-to-PCI bridge passes on to the bus behind it."
  (let ((io-base (pci-byte bus device function #x1C))
        (io-limit (pci-byte bus device function #x1D))
        (memory-base (pci-word bus device function #x20))
        (memory-limit (pci-word bus device function #x22))
        (prefetch-base (pci-word bus device function #x24))
        (prefetch-limit (pci-word bus device function #x26))
        (control (pci-word bus device function #x3E)))
    (format t "  bridge: primary bus ~2,'0X, secondary ~2,'0X, subordinate ~2,'0X, ~
               secondary latency ~D~%"
            (pci-byte bus device function #x18) (pci-byte bus device function #x19)
            (pci-byte bus device function #x1A) (pci-byte bus device function #x1B))
    (flet ((window (name base limit)
             (format t "    ~A ~:[nothing~*~*~;~Xh-~Xh~]~%" name (<= base limit) base limit)))
      (window "passes ports   "
              (+ (ash (logand io-base #xF0) 8) (ash (pci-word bus device function #x30) 16))
              (+ (ash (logand io-limit #xF0) 8) #xFFF
                 (ash (pci-word bus device function #x32) 16)))
      (window "passes memory  " (ash (logand memory-base #xFFF0) 16)
              (+ (ash (logand memory-limit #xFFF0) 16) #xFFFFF))
      (window "prefetchable   "
              (+ (ash (logand prefetch-base #xFFF0) 16)
                 (ash (pci-read bus device function #x28) 32))
              (+ (ash (logand prefetch-limit #xFFF0) 16) #xFFFFF
                 (ash (pci-read bus device function #x2C) 32))))
    (format t "    bridge control ~4,'0Xh:~:[ nothing~;~:*~{ ~A~^,~}~]~%" control
            (bits control '((0 . "parity response") (1 . "SERR") (2 . "ISA ports only")
                            (3 . "VGA passed on") (5 . "master abort mode")
                            (6 . "secondary bus held in reset") (7 . "fast back-to-back"))))))

(defun show (bus device function depth)
  (let* ((id (pci-read bus device function 0))
         (vendor (logand id #xFFFF)) (device-id (ash id -16))
         (class (pci-read bus device function 8))
         (header (pci-byte bus device function #x0E))
         (kind (logand header #x7F))
         (command (pci-word bus device function 4))
         (status (pci-word bus device function 6))
         (pin (pci-byte bus device function #x3D))
         (line (pci-byte bus device function #x3C)))
    (incf *count*)
    (format t "~%~A  ~4,'0X:~4,'0X rev ~2,'0X  ~A~@[ ~A~]~:[~;   (behind ~D bridge~:P)~]~%"
            (who bus device function) vendor device-id (logand class #xFF)
            (vendor-name vendor) (device-name vendor device-id) (plusp depth) depth)
    (format t "  class ~2,'0X/~2,'0X/~2,'0X  ~A~%"
            (ldb (byte 8 24) class) (ldb (byte 8 16) class) (ldb (byte 8 8) class)
            (class-name-of (ldb (byte 8 24) class) (ldb (byte 8 16) class)
                           (ldb (byte 8 8) class)))
    (format t "  header type ~D (~A), ~:[one function~;one of several functions~]~
               ~:[~*~*~;; subsystem ~4,'0X:~4,'0X~]~%"
            kind (case kind (0 "a device") (1 "a PCI-to-PCI bridge") (2 "a CardBus bridge")
                   (t "unknown"))
            (logbitp 7 header) (= kind 0)
            (pci-word bus device function #x2C) (pci-word bus device function #x2E))
    (format t "  command ~4,'0Xh:~:[ nothing switched on~;~:*~{ ~A~^,~}~]~%" command
            (bits command '((0 . "I/O") (1 . "memory") (2 . "bus master") (3 . "special cycles")
                            (4 . "write-and-invalidate") (5 . "VGA palette snoop")
                            (6 . "parity response") (8 . "SERR") (9 . "fast back-to-back")
                            (10 . "INTx off"))))
    (format t "  status  ~4,'0Xh: ~A DEVSEL~{, ~A~}~%" status
            (nth (ldb (byte 2 9) status) '("fast" "medium" "slow" "reserved"))
            (bits status '((3 . "interrupt pending") (4 . "capability list") (5 . "66 MHz")
                           (7 . "fast back-to-back") (8 . "data parity error")
                           (11 . "signalled target abort") (12 . "received target abort")
                           (13 . "received master abort") (14 . "signalled system error")
                           (15 . "parity error"))))
    (format t "  latency timer ~D, cache line ~D~:[~*~*~;, minimum grant ~D, maximum latency ~D~]~
               ~@[, self-test ~2,'0Xh~]~%"
            (pci-byte bus device function #x0D) (pci-byte bus device function #x0C)
            (= kind 0) (pci-byte bus device function #x3E) (pci-byte bus device function #x3F)
            (let ((bist (pci-byte bus device function #x0F))) (and (plusp bist) bist)))
    (if (plusp pin)
        (progn (format t "  interrupt: pin INT~C, ~:[no line assigned~*~;IRQ ~D~]~%"
                       (code-char (+ 64 pin)) (< line 255) line)
               (when (< line 255)
                 (push (cons line (who bus device function)) *interrupts*)))
        (format t "  interrupt: none~%"))
    (show-bars bus device function (case kind (0 6) (1 2) (t 1)))
    (case kind
      (0 (show-rom bus device function #x30))
      (1 (show-rom bus device function #x38)
       (show-bridge bus device function)))
    (show-capabilities bus device function)
    (show-chip bus device function vendor device-id)
    (loop for at from 0 below *dump* by 16
          do (format t "  ~2,'0X:~{ ~2,'0X~}~%" at
                     (loop for i below 16 collect (pci-byte bus device function (+ at i)))))
    (when (= kind 1)                    ; go down to the bus behind it
      (let ((secondary (pci-byte bus device function #x19)))
        (when (> secondary bus) (scan-bus secondary (1+ depth)))))))

(defun scan-bus (bus depth)
  (dotimes (device 32)
    (unless (= (logand (pci-read bus device 0 0) #xFFFF) #xFFFF)
      ;; bit 7 of the header type says whether functions 1-7 exist
      (dotimes (function (if (logbitp 7 (pci-byte bus device 0 #x0E)) 8 1))
        (unless (= (logand (pci-read bus device function 0) #xFFFF) #xFFFF)
          (show bus device function depth))))))

;;; ---- what the BIOS says
(defun find-signature (text step)
  "The address in the BIOS (E0000h-FFFFFh) of a table that begins with TEXT, or NIL."
  (loop for address from #xE0000 below #x100000 by step
        when (loop for c across text for i from 0
                   always (= (peek (+ address i)) (char-code c)))
          return address))

(defun show-routing-table (table)
  (let ((size (peek16 (+ table 6))))
    (format t "Interrupt routing table ($PIR) at ~5,'0Xh, version ~D.~D, ~D slot~:P; ~
               checksum ~:[WRONG~;right~]~%"
            table (peek (+ table 5)) (peek (+ table 4)) (floor (- size 32) 16)
            (zerop (logand 255 (loop for i below size sum (peek (+ table i))))))
    (format t "  router ~2,'0X:~2,'0X.~D (compatible with ~4,'0X:~4,'0X); ~
               IRQs kept for PCI:~:[ none~;~:*~{ ~D~}~]~%"
            (peek (+ table 8)) (ash (peek (+ table 9)) -3) (logand (peek (+ table 9)) 7)
            (peek16 (+ table 12)) (peek16 (+ table 14))
            (loop for irq below 16 when (logbitp irq (peek16 (+ table 10))) collect irq))
    (loop for entry from (+ table 32) below (+ table size) by 16
          do (format t "  ~2,'0X:~2,'0X ~:[on the board~*~;slot ~D~]:" (peek entry)
                     (ash (peek (+ entry 1)) -3) (plusp (peek (+ entry 14)))
                     (peek (+ entry 14)))
             (loop for pin below 4
                   do (let ((link (peek (+ entry 2 (* 3 pin))))
                            (possible (peek16 (+ entry 3 (* 3 pin)))))
                        (declare (ignorable possible))
                        (if (zerop link)
                            (format t " ~C none" (code-char (+ 65 pin)))
                            (format t " ~C link ~2,'0Xh" (code-char (+ 65 pin)) link))))
             (terpri))
    (when (> size 32)
      (format t "  a link can be made IRQ~{ ~D~}~%"
              (loop for irq below 16 when (logbitp irq (peek16 (+ table 32 3))) collect irq)))))

(defun show-bios ()
  (multiple-value-bind (ax bx cx dx) (int86 #x1A :ax #xB101)
    (if (and (zerop (hi ax)) (= dx #x20494350))         ; "PCI "
        (format t "PCI BIOS ~X.~2,'0X: buses 0 to ~D; configuration mechanism~{ ~A~}~%"
                (hi bx) (lo bx) (lo cx)
                (bits (lo ax) '((0 . "1 (ports CF8h/CFCh)") (1 . "2 (ports CF8h/CFAh)")
                                (4 . "special cycles by 1") (5 . "special cycles by 2"))))
        (format t "No PCI BIOS answers INT 1Ah (the ports are used all the same).~%")))
  (let ((bios32 (find-signature "_32_" 16)))
    (when bios32
      (format t "BIOS32 service directory at ~5,'0Xh: entry point ~8,'0Xh, revision ~D~%"
              bios32 (peek32 (+ bios32 4)) (peek (+ bios32 8)))))
  (let ((table (find-signature "$PIR" 16)))
    (if table
        (show-routing-table table)
        (format t "No interrupt routing table ($PIR) in the BIOS.~%"))))

;;; ---- the map
(defun show-map ()
  (format t "~%~D PCI function~:P found.~%" *count*)
  (dolist (kind '(:io :memory))
    (let ((ranges (sort (remove kind *ranges* :key #'first :test-not #'eq) #'< :key #'second))
          (digits (if (eq kind :io) 4 8)))
      (when ranges
        (format t "~%~:[Memory~;Ports~] claimed:~%" (eq kind :io))
        (loop for (nil start size owner) in ranges for previous = nil then end
              for end = (+ start size)
              do (format t "  ~V,'0Xh-~V,'0Xh  ~9A ~A~:[~;   OVERLAPS the one before~]~%"
                         digits start digits (1- end) (size-string size) owner
                         (and previous (< start previous)))))))
  (when *interrupts*
    (format t "~%Interrupt lines:~%")
    (dolist (irq (sort (remove-duplicates (mapcar #'car *interrupts*)) #'<))
      (let ((users (reverse (loop for (line . owner) in *interrupts*
                                  when (= line irq) collect owner))))
        (format t "  IRQ ~2D  ~{~A~^, ~}~:[~;   (shared)~]~%" irq users (rest users))))))

(defun report ()
  (setf *count* 0 *ranges* '() *interrupts* '())
  (show-bios)
  (scan-bus 0 0)
  (show-map))

;;; ---- a screenful at a time
(defun page (text)
  (let ((lines (with-input-from-string (in text)
                 (loop for line = (read-line in nil) while line collect line)))
        (room 24) (all nil))
    (dolist (line lines)
      (let ((rows (max 1 (ceiling (length line) 80))))
        (when (and (not all) (< room rows))
          (format t "-- more: Space, Enter (a line), A (all the rest), Esc --")
          (finish-output)
          (let ((key (get-key)))
            (format t "~C~56A~C" #\Return "" #\Return)
            (cond ((member key (list (code-char 27) #\q #\Q)) (return-from page nil))
                  ((member key '(#\a #\A)) (setf all t))
                  ((eql key #\Return) (setf room rows))
                  (t (setf room 24)))))
        (write-line line)
        (decf room rows))))
  t)

(defun pciscan (&rest options)
  (let ((options (mapcar (lambda (option) (string-upcase (princ-to-string option))) options)))
    (unless (pci-present-p)
      (format t "~&No PCI bus: nothing answers at port CF8h.~%")
      (return-from pciscan nil))
    (setf *dump* (if (member "DUMP" options :test #'string=) 256 64)
          *read-roms* (not (member "NOROM" options :test #'string=)))
    (let ((text (with-output-to-string (*standard-output*) (report))))
      (if (member "FILE" options :test #'string=)
          (progn (with-open-file (out "PCISCAN.TXT" :direction :output :if-exists :supersede)
                   (write-string text out))
                 (format t "~&~D PCI function~:P found; the report (~:D lines) is written ~
                            to PCISCAN.TXT.~%"
                         *count* (count #\Newline text)))
          (page text)))))

(defun main (&rest options)
  "Everything the PCI bus will say about itself."
  (apply #'pciscan options))

(doslib-examples:define-example "pciscan" 'main
  :category :system
  :description "Everything the PCI bus will say about itself.")
