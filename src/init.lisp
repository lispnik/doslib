;;;; Setting the library up, at load time and when a saved core starts.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; The stubs live in malloc'ed memory, which a saved core does not keep, so
;;; they are (re)made by this function: when the library is loaded, and --
;;; through SB-EXT:*INIT-HOOKS* -- whenever a core saved with the library in
;;; it starts up (an application made with SAVE-LISP-AND-DIE, say).
(defun doslib-init ()
  (clrhash *stubs*)
  (setf *regs* (alien-sap (make-alien (unsigned 32) 8)))
  (setf *inb* (machine-code #x8B #x54 #x24 4 #x31 #xC0 #xEC #xC3))
  (setf *outb* (machine-code #x8B #x54 #x24 4 #x8B #x44 #x24 8 #xEE #xC3))
  (setf *inl* (machine-code #x8B #x54 #x24 4 #xED #xC3))
  (setf *outl* (machine-code #x8B #x54 #x24 4 #x8B #x44 #x24 8 #xEF #xC3))
  (setf *rm* (alien-sap (make-alien (unsigned 8) 64)))
  (setf *cpuid* (machine-code #x53 #x57 #x8B #x44 #x24 12 #x8B #x7C #x24 16
          #x0F #xA2 #x89 #x07 #x89 #x5F 4 #x89 #x4F 8
          #x89 #x57 12 #x5F #x5B #xC3))
  t)
(doslib-init)
(pushnew 'doslib-init sb-ext:*init-hooks*)
