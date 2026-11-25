;;;; A small x86 assembler in Lisp.  The other demos spell their
;;;; machine code as lists of bytes; here the same routines are written as
;;;; S-expressions, with labels, and assembled at run time.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.asm
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.asm)

(defvar *registers* '(eax ecx edx ebx esp ebp esi edi))
(defun reg (r) (or (position r *registers*) (error "not a register: ~S" r)))
(defun regp (x) (and (symbolp x) (member x *registers*)))
(defun imm32 (x) (loop for i below 4 collect (ldb (byte 8 (* 8 i)) x)))
(defun modrm (reg-field operand)        ; operand: register, or (base displacement)
  (if (regp operand)
      (list (logior #xC0 (ash reg-field 3) (reg operand)))
      (destructuring-bind (base &optional (disp 0)) operand
        `(,(logior #x40 (ash reg-field 3) (reg base))
          ,@(when (eq base 'esp) '(#x24))       ; ESP needs a SIB byte
          ,(logand disp #xFF)))))
(defun encode (instruction)
  (destructuring-bind (op &optional a b) instruction
    (ecase op
      (mov (cond ((and (regp a) (integerp b)) `(,(+ #xB8 (reg a)) ,@(imm32 b)))
                 ((regp b) `(#x89 ,@(modrm (reg b) a)))         ; register -> r/m
                 (t `(#x8B ,@(modrm (reg a) b)))))              ; memory -> register
      (add (if (integerp b) `(#x83 ,(+ #xC0 (reg a)) ,(logand b #xFF))
               `(#x01 ,@(modrm (reg b) a))))
      (sub (if (integerp b) `(#x83 ,(+ #xE8 (reg a)) ,(logand b #xFF))
               `(#x29 ,@(modrm (reg b) a))))
      (cmp `(#x83 ,(+ #xF8 (reg a)) ,(logand b #xFF)))
      (xor `(#x31 ,@(modrm (reg b) a)))
      (imul `(#x0F #xAF ,@(modrm (reg a) b)))
      (xchg `(#x87 ,@(modrm (reg b) a)))
      (inc `(,(+ #x40 (reg a)))) (dec `(,(+ #x48 (reg a))))
      (push `(,(+ #x50 (reg a)))) (pop `(,(+ #x58 (reg a))))
      (int `(#xCD ,a)) (ret '(#xC3)) (cld '(#xFC)) (cpuid '(#x0F #xA2))
      (in-al-dx '(#xEC)) (out-dx-al '(#xEE))
      ((jmp jz jnz jl) `(,(ecase op (jmp #xEB) (jz #x74) (jnz #x75) (jl #x7C)) (:label ,a))))))
(defun assemble (program)
  "Two passes: lay the code out to find the labels, then fill in the jumps."
  (let ((labels '()) (code '()))
    (dolist (item program)
      (if (symbolp item)
          (push (cons item (length code)) labels)
          (setf code (append code (encode item)))))
    (loop for byte in code for position from 1
          collect (if (consp byte)
                      (logand (- (cdr (assoc (second byte) labels)) position) #xFF)
                      byte))))
(defmacro defasm (name (&rest lisp-args) &body program)
  "Define a Lisp function whose body is machine code (cdecl arguments)."
  `(let ((code (saved-code (assemble ',program))))       ; (see SAVED-CODE in DOSLIB)
     (defun ,name ,lisp-args
       (alien-funcall (sap-alien (code-address code) (function unsigned ,@(mapcar (constantly 'unsigned)
                                                                  lisp-args)))
                      ,@lisp-args))))
(defun hex (bytes) (format nil "~{~2,'0X~^ ~}" bytes))
;;; The port-input stub from DOSLIB.LSP, this time from source:
(defvar *inb-source* '((mov edx (esp 4)) (xor eax eax) (in-al-dx) (ret)))
(defasm sum-to (n)                      ; 1 + 2 + ... + n, with a loop
  (mov ecx (esp 4)) (xor eax eax)
  again (add eax ecx) (dec ecx) (jnz again)
  (ret))
(defasm fib (n)                         ; iterative Fibonacci
  (mov ecx (esp 4)) (xor eax eax) (mov edx 1)
  next (add eax edx) (xchg eax edx) (dec ecx) (jnz next)
  (ret))
(defasm multiply (a b) (mov eax (esp 4)) (imul eax (esp 8)) (ret))
;;; (The address is an argument, not a number in the code: the address the
;;; program knows 0449h by is not 0449h in every Lisp.  See NEAR-ADDRESS.)
(defasm read-word (address)             ; BIOS data area byte 0449h, the hard way
  (mov edx (esp 4)) (xor eax eax) (mov eax (edx 0)) (ret))
(defun video-mode-now () (read-word (near-address #x449)))
(defasm dos-version ()                  ; INT 21h, AH=30h
  (push ebx) (mov eax #x3000) (int #x21) (pop ebx) (ret))

(defun main ()
  "A small x86 assembler in Lisp."
  (format t "~&inb:  ~A~%      same bytes as the hand-written stub: ~A~%"
          (hex (assemble *inb-source*))
          (equal (assemble *inb-source*) '(#x8B #x54 #x24 4 #x31 #xC0 #xEC #xC3)))
  (format t "sum-to:  ~A~%" (hex (assemble '((mov ecx (esp 4)) (xor eax eax)
                                               again (add eax ecx) (dec ecx) (jnz again) (ret)))))
  (format t "  (sum-to 100)        = ~D   (Lisp says ~D)~%" (sum-to 100) (loop for i to 100 sum i))
  (format t "  (sum-to 65535)      = ~D~%" (sum-to 65535))
  (format t "  (fib 40)            = ~D   (should be 102334155)~%" (fib 40))
  (format t "  (multiply 12345 6789) = ~D   (Lisp says ~D)~%" (multiply 12345 6789) (* 12345 6789))
  (format t "  video mode byte     = ~D~%" (logand (video-mode-now) #xFF))
  (let ((v (dos-version)))
    (format t "  DOS version via INT 21h from assembled code: ~D.~2,'0D~%" (lo v) (hi v))))

(doslib-examples:define-example "asm" 'main
  :category :system
  :description "A small x86 assembler in Lisp.")
