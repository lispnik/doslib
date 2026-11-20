;;;; Assembly language with Lisp macros.
;;;;
;;;; The assembler here takes instructions as lists, as the asm example
;;;; does -- and since a program is then just a list, Lisp can write it.
;;;; DEFINE-ASM-MACRO adds new "instructions" that expand into real ones:
;;;; REPEAT is a counted loop that makes up its own label, SAVING pushes
;;;; registers and pops them again in the right order.  Structured
;;;; assembly, with no assembler program in sight.
;;;;
;;;; The routine built that way paints the whole screen, a byte at a time;
;;;; the same thing written in Lisp, as well as Lisp can, is timed against
;;;; it.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.asmdsl
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.asmdsl)

;;; ---- the assembler
(defvar *registers* '(eax ecx edx ebx esp ebp esi edi))
(defun regp (x) (and (symbolp x) (member x *registers*)))
(defun reg (r) (or (position r *registers*) (error "~S is not a register" r)))
(defun modrm (field operand)            ; operand: a register, or (base displacement)
  (if (regp operand)
      (list (logior #xC0 (ash field 3) (reg operand)))
      (destructuring-bind (base &optional (displacement 0)) operand
        `(,(logior #x40 (ash field 3) (reg base))
          ,@(when (eq base 'esp) '(#x24))
          ,(logand displacement #xFF)))))
(defun encode (instruction)
  (destructuring-bind (op &optional a b) instruction
    (ecase op
      (mov (cond ((and (regp a) (integerp b)) `(,(+ #xB8 (reg a)) ,@(bytes32 b)))
                 ((regp b) `(#x89 ,@(modrm (reg b) a)))
                 (t `(#x8B ,@(modrm (reg a) b)))))
      (add `(#x01 ,@(modrm (reg b) a)))
      (xor `(#x31 ,@(modrm (reg b) a)))
      (and `(#x21 ,@(modrm (reg b) a)))
      (shr `(#xC1 ,(+ #xE8 (reg a)) ,b))
      (inc `(,(+ #x40 (reg a)))) (dec `(,(+ #x48 (reg a))))
      (push `(,(+ #x50 (reg a)))) (pop `(,(+ #x58 (reg a))))
      (stosb '(#xAA))                   ; [edi] <- al, edi <- edi + 1
      (cld '(#xFC)) (ret '(#xC3))
      ((jmp jz jnz) `(,(ecase op (jmp #xEB) (jz #x74) (jnz #x75)) (:label ,a))))))

;;; ---- macros: instructions that are made of instructions
(defvar *asm-macros* (make-hash-table))

(defmacro define-asm-macro (name lambda-list &body body)
  "Define NAME as a pseudo-instruction: BODY returns the list of items it stands for."
  `(setf (gethash ',name *asm-macros*) (lambda ,lambda-list ,@body)))

(defun expand (program)
  "PROGRAM with every macro replaced by what it stands for, all the way down."
  (loop for item in program
        append (let ((macro (and (consp item) (gethash (first item) *asm-macros*))))
                 (if macro
                     (expand (apply macro (rest item)))
                     (list item)))))

(define-asm-macro repeat (register count &rest body)   ; do BODY COUNT times
  (let ((again (gensym "AGAIN")))
    `((mov ,register ,count)
      ,again
      ,@body
      (dec ,register)
      (jnz ,again))))

(define-asm-macro saving (registers &rest body)        ; keep REGISTERS as they were
  `(,@(mapcar (lambda (r) `(push ,r)) registers)
    ,@body
    ,@(mapcar (lambda (r) `(pop ,r)) (reverse registers))))

(define-asm-macro clear (register) `((xor ,register ,register)))

(defun assemble (program)
  "Expand the macros; lay the code out to find the labels; fill in the jumps."
  (let ((labels '()) (code '()))
    (dolist (item (expand program))
      (if (symbolp item)
          (push (cons item (length code)) labels)
          (setf code (append code (encode item)))))
    (loop for byte in code for position from 1
          collect (if (consp byte)
                      (let ((distance (- (cdr (assoc (second byte) labels)) position)))
                        (unless (<= -128 distance 127) (error "A jump is too far"))
                        (logand distance #xFF))
                      byte))))
(defmacro define-machine-function (name (&rest arguments) &body program)
  "Define the Lisp function NAME, whose body is PROGRAM in machine code."
  `(let ((code (saved-code (assemble ',program))))       ; (see SAVED-CODE in DOSLIB)
     (setf (get ',name 'source) ',program)
     (defun ,name ,arguments
       (alien-funcall (sap-alien (code-address code) (function unsigned ,@(mapcar (constantly 'unsigned) arguments)))
                      ,@arguments))))

;;; ---- the routine: every pixel = (column XOR row) + time, for all 64,000
(define-machine-function paint-fast (time)
  (saving (ebx esi edi)
    (cld)
    (mov esi (esp 16))                  ; the argument, past the three saved registers
    (mov edi #xA0000)                   ; video memory
    (repeat edx 200
      (repeat ecx 320
        (mov eax ecx) (xor eax edx) (add eax esi)
        (stosb))))
  (ret))

;;; ---- and in Lisp, declared and optimized
(defun paint-lisp (time)
  (declare (fixnum time) (optimize speed (safety 0)))
  (let ((offset 0))
    (declare (fixnum offset))
    (loop for row fixnum from 200 above 0
          do (loop for column fixnum from 320 above 0
                   do (setf (sap-ref-8 *vga* offset) (logand 255 (+ (logxor column row) time)))
                      (incf offset)))))

(defun frames-per-second (function seconds)
  (let ((start (get-internal-real-time)) (frames 0))
    (loop until (>= (- (get-internal-real-time) start) (* seconds internal-time-units-per-second))
          do (funcall function frames) (incf frames))
    (/ frames (float seconds))))

(defun main ()
  "Assembly language with Lisp macros, timed against Lisp."
  (let ((source (get 'paint-fast 'source)))
    (format t "~&The routine, as written:~%~{  ~S~%~}" source)
    (format t "~%What the macros turn it into (~D items):~%  ~{~S~^ ~}~%"
            (length (expand source)) (expand source))
    (format t "~%As machine code (~D bytes):~%  ~{~2,'0X~^ ~}~%"
            (length (assemble source)) (assemble source))
    (format t "~%Press a key to run it: 3 seconds of Lisp, then 3 seconds of machine code.~%")
    (wait-key)
    (video-mode #x13)
    (rainbow-palette)
    (let ((lisp (frames-per-second #'paint-lisp 3))
          (fast (frames-per-second #'paint-fast 3)))
      (video-mode 3)
      (format t "~&Frames per second, 64,000 pixels each:~%")
      (format t "  Lisp, declared and optimized   ~8,1F~%" lisp)
      (format t "  machine code from the macros   ~8,1F   (~,1F times)~%" fast (/ fast (max lisp 0.01))))))

(doslib-examples:define-example "asmdsl" 'main
  :category :system
  :description "Assembly language with Lisp macros, timed against Lisp.")
