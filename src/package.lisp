;;;; The DOSLIB package.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib
  (:use #:cl #:sb-alien #:sb-sys)
  (:documentation "BIOS, DOS, port and memory access for SBCL on DOS.
Everything is done from Lisp: the few instructions Lisp cannot express (INT,
IN, OUT, CPUID) are assembled by hand into malloc'ed memory and called with
ALIEN-FUNCALL.")
  (:export #:machine-code #:interrupt-stub #:*regs* #:*stubs* #:int86 #:lo
          #:hi #:word16 #:*inb* #:*outb* #:inb #:outb #:*inl* #:*outl* #:inl
          #:outl #:*rm* #:real-int #:dos-alloc #:linear #:break-checking
          #:without-break-checking #:*cpuid* #:cpuid
          #:peek #:peek16 #:peek32 #:*vga* #:video-mode #:set-palette
          #:rainbow-palette #:plot #:draw-line #:wait-tick #:key-pressed-p
          #:wait-key #:finish #:doslib-init
          #:code-selector #:data-selector #:bytes32 #:get-vector #:set-vector
          #:install-hotkey #:remove-hotkey #:remove-hotkeys #:poll-hotkeys
          #:hotkeys-installed-p #:hotkey-pending-p #:clear-pending-hotkeys
          #:*text-screen* #:text-put #:text-fill #:text-box #:text-cursor
          #:hide-text-cursor #:show-text-cursor #:save-text-screen
          #:restore-text-screen #:get-key #:text-input
          #:catch-critical-errors #:release-critical-errors #:critical-error
          #:text-wrap #:restart-dialog #:dialog-ask
          #:*session* #:saved-code #:code-address
          ;; DOS memory
          #:dos-free #:with-dos-memory #:dos-scratch #:dos-string #:dos-bytes
          ;; interrupting the program
          #:interrupt #:interrupts-supported-p #:interrupt-chord #:interrupt-count
          #:catching-interrupt
          ;; more ports
          #:inw #:outw #:cmos #:bcd
          #:ticks #:current-video-mode #:with-video-mode #:wait-retrace
          #:vesa-set-mode #:vesa-set-bank
          #:with-critical-errors-caught
          ;; the speaker
          #:+timer-hz+ #:speaker-on #:speaker-off #:speaker-divisor #:speaker-beep
          ;; the mouse
          #:mouse-driver-p #:mouse-init #:mouse-show #:mouse-hide #:with-mouse-hidden
          #:mouse-position #:mouse-cell #:mouse-pixel #:mouse-move #:mouse-range
          #:mouse-presses #:mouse-releases #:mouse-motion #:mouse-wheel-p
          ;; disks
          #:disk-error #:disk-error-drive #:disk-error-sector #:disk-error-code
          #:disk-error-reason #:current-drive #:drive-number #:drive-letter
          #:drive-read-method #:dos-read-sectors #:bios-read-sectors
          ;; PCI
          #:pci-read #:pci-write #:pci-byte #:pci-word #:pci-present-p #:pci-devices
          ;; serial ports
          #:serial-port #:serial-open #:serial-byte-ready-p #:serial-read-byte
          #:serial-write-byte #:serial-write-string #:serial-read-line))
