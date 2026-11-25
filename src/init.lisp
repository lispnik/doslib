;;;; Setting the library up, at load time and when a saved core starts.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; The stubs live in malloc'ed memory, which a saved core does not keep, so
;;; they are (re)made by this function: when the library is loaded, and --
;;; through SB-EXT:*INIT-HOOKS* -- whenever a core saved with the library in
;;; it starts up (an application made with SAVE-LISP-AND-DIE, say).
(defun doslib-init ()
  "Set the library up in this process.  It runs when the library is loaded
and whenever a core saved with the library in it starts; a program saved
with its own :TOPLEVEL function calls it first."
  (incf *session*)
  (setf *code-room* nil)                ; machine code: the old process's memory is gone
  (clrhash *stubs*)
  (setf *regs* (alien-sap (make-alien (unsigned 32) 8)))
  (setf *inb* (machine-code #x8B #x54 #x24 4 #x31 #xC0 #xEC #xC3))
  (setf *outb* (machine-code #x8B #x54 #x24 4 #x8B #x44 #x24 8 #xEE #xC3))
  (setf *inl* (machine-code #x8B #x54 #x24 4 #xED #xC3))
  (setf *outl* (machine-code #x8B #x54 #x24 4 #x8B #x44 #x24 8 #xEF #xC3))
  (setf *inw* (machine-code #x8B #x54 #x24 4 #x31 #xC0 #x66 #xED #xC3))
  (setf *outw* (machine-code #x8B #x54 #x24 4 #x8B #x44 #x24 8 #x66 #xEF #xC3))
  (setf *rm* (alien-sap (make-alien (unsigned 8) 64)))
  (setf *cpuid* (machine-code #x53 #x57 #x8B #x44 #x24 12 #x8B #x7C #x24 16
          #x0F #xA2 #x89 #x07 #x89 #x5F 4 #x89 #x4F 8
          #x89 #x57 12 #x5F #x5B #xC3))
  (setf *get-cs* (machine-code #x8C #xC8 #xC3)
        *get-ds* (machine-code #x8C #xD8 #xC3))
  (setf *critical-stub* nil *critical-depth* 0)
  (clrhash *dos-blocks*)                ; DOS memory: none of it is ours any more
  (setf *scratch* nil)
  (clrhash *drive-methods*)
  (setf *timer-count* 0 *last-frame* 0) ; midnight may have passed, or the machine restarted
  (setf *blaster-hook* nil *key-segment* nil *key-old-vector* nil)
  (clrhash *irq-hooks*)                 ; the handlers were the old process's memory
  (setf *display-adapter* nil            ; a saved core may start on another machine
        *text-screen* (int-sap (text-memory-address)))
  (setf *hotkey-segment* nil)
  (setf *hotkey-old-vector* nil)        ; a new process: nothing is hooked
  (clrhash *hotkey-functions*)
  t)
(doslib-init)
(add-init-hook 'doslib-init)
(add-exit-hook 'remove-irq-handlers)
(add-exit-hook 'key-state-stop)
(add-exit-hook 'remove-hotkeys)   ; leave the keyboard as we found it
(defun release-all-critical-errors () (release-critical-errors t))
(add-exit-hook 'release-all-critical-errors)
