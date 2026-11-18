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
          #:outl #:*rm* #:real-int #:dos-alloc #:linear #:*cpuid* #:cpuid
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
          #:*session* #:saved-code #:code-address))
