;;;; Single-stepping machine code: a small debugger.  A routine of a few
;;;; instructions is run one instruction at a time, with the disassembly,
;;;; the registers, the flags and the stack on the screen after each.
;;;;
;;;;   (doslib-examples:run "step")           the first routine
;;;;   (doslib-examples:run "step" "fact")    another: "?" lists them
;;;; Space (or S) does one instruction, R runs to the end, N goes on to the
;;;; next routine, A starts this one again, Esc leaves.
;;;;
;;;; How: the processor has a "trap flag".  With it set, an instruction is
;;;; followed at once by exception 1, whatever the instruction was.  The DOS
;;;; port's runtime has a function for exactly this, dos_single_step: given
;;;; a block of registers it loads them, sets the trap flag, jumps to the
;;;; code, and when the exception arrives hands the registers back as they
;;;; then are.  Everything else here is Lisp: the routines are bytes, put in
;;;; memory with MACHINE-CODE; SBCL's own disassembler says what they are;
;;;; and the routine's stack is a block of memory of ours, so that it can
;;;; be shown, and so that a routine that goes wrong harms nothing.
;;;;
;;;; The routines take their arguments on the stack and answer in EAX, as C
;;;; does.  Each is called with a return address that points at a marker;
;;;; arriving there is how the debugger knows it has finished.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.step
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.step)

(defparameter *routines*
  ;; name, what it does, its arguments, its bytes
  '(("gcd" "the greatest common divisor of 462 and 1071, by Euclid's algorithm" (462 1071)
     (#x8B #x44 #x24 #x04               ;     mov eax,[esp+4]
      #x8B #x4C #x24 #x08               ;     mov ecx,[esp+8]
      #x85 #xC9                         ; L:  test ecx,ecx
      #x74 #x0A                         ;     jz done
      #x31 #xD2                         ;     xor edx,edx
      #xF7 #xF1                         ;     div ecx
      #x89 #xC8                         ;     mov eax,ecx
      #x89 #xD1                         ;     mov ecx,edx
      #xEB #xF2                         ;     jmp L
      #xC3))                            ; done: ret
    ("sum" "1 + 2 + ... + 10, with the LOOP instruction" (10)
     (#x8B #x4C #x24 #x04               ;     mov ecx,[esp+4]
      #x31 #xC0                         ;     xor eax,eax
      #x01 #xC8                         ; L:  add eax,ecx
      #xE2 #xFC                         ;     loop L
      #xC3))                            ;     ret
    ("fact" "5 factorial, calling itself: watch the stack" (5)
     (#x8B #x44 #x24 #x04               ;     mov eax,[esp+4]
      #x83 #xF8 #x01                    ;     cmp eax,1
      #x76 #x0F                         ;     jbe one
      #x48                              ;     dec eax
      #x50                              ;     push eax
      #xE8 #xF0 #xFF #xFF #xFF          ;     call fact
      #x83 #xC4 #x04                    ;     add esp,4
      #xF7 #x64 #x24 #x04               ;     mul dword [esp+4]
      #xC3                              ;     ret
      #xB8 #x01 #x00 #x00 #x00          ; one: mov eax,1
      #xC3))))                          ;     ret

;;; ---- the machine
(defconstant +stack-bytes+ 4096)
(defvar *registers* nil "13 words: EAX ECX EDX EBX ESP EBP ESI EDI EIP EFLAGS, trap, error, address.")
(defvar *stack* nil "The stepped routine's stack: a SAP.")
(defparameter *register-names* '("EAX" "ECX" "EDX" "EBX" "ESP" "EBP" "ESI" "EDI" "EIP"))

(defun register (i) (sap-ref-32 *registers* (* 4 i)))
(defun (setf register) (value i) (setf (sap-ref-32 *registers* (* 4 i)) value))
(defun step-function ()
  "The runtime's dos_single_step, or NIL in a runtime that has none."
  (let ((address (find-foreign-symbol-address "dos_single_step")))
    (and address (int-sap address))))
(defun single-step ()
  "Run the instruction at EIP.  Returns the exception that ended it: 1 is the step itself."
  (alien-funcall (sap-alien (step-function) (function int system-area-pointer)) *registers*))

(defstruct program name description arguments code length marker lines)

(defun disassembly (code length)
  "The instructions at CODE as a list of (offset . text), from SBCL's disassembler."
  (let ((text (with-output-to-string (out)
                (sb-disassem:disassemble-memory code length :stream out)))
        (base (sap-int code)) (lines '()))
    (with-input-from-string (in text)
      (loop for line = (read-line in nil) while line
            do (let* ((start (position #\; line))
                      (colon (and start (position #\: line :start start)))
                      (number (and colon (ignore-errors
                                          (parse-integer line :start (1+ start) :end colon :radix 16)))))
                 (when number
                   ;; the address is printed whole, or as its last digits
                   (let ((offset (loop for o from 0 below length
                                       when (or (= (+ base o) number)
                                                (= (mod (+ base o) (expt 16 (- colon start 2))) number))
                                         return o)))
                     (when offset
                       (push (cons offset (string-trim " " (subseq line (1+ colon)))) lines)))))))
    (nreverse lines)))

(defun load-program (entry)
  (destructuring-bind (name description arguments bytes) entry
    ;; after the routine: a byte that is never executed, to return to
    (let ((code (apply #'machine-code (append bytes '(#xF4)))))
      (make-program :name name :description description :arguments arguments
                    :code code :length (length bytes)
                    :marker (+ (sap-int code) (length bytes))
                    :lines (disassembly code (length bytes))))))

(defun start (program)
  "Set the registers and the stack as they are when PROGRAM has just been called."
  (dotimes (i 13) (setf (register i) 0))
  (dotimes (i (floor +stack-bytes+ 4)) (setf (sap-ref-32 *stack* (* 4 i)) 0))
  (let ((sp (+ (sap-int *stack*) +stack-bytes+ -64)))
    (loop for argument in (reverse (program-arguments program))
          do (decf sp 4) (setf (sap-ref-32 (int-sap sp) 0) argument))
    (decf sp 4) (setf (sap-ref-32 (int-sap sp) 0) (program-marker program))
    (setf (register 4) sp
          (register 8) (sap-int (program-code program))
          (register 9) #x202)))                         ; interrupts on, nothing else
(defun finished-p (program) (= (register 8) (program-marker program)))

;;; ---- the screen
(defun show (program steps previous note)
  (text-fill 0 0 80 25 #x17)
  (text-put 1 0 (format nil " ~A: ~A " (program-name program) (program-description program)) #x70)
  ;; the instructions, the next one marked
  (let ((offset (- (register 8) (sap-int (program-code program)))))
    (loop for (at . text) in (program-lines program) for row from 2
          do (let ((here (= at offset)))
               (text-put 1 row (format nil "~:[  ~;=>~] ~2,'0X  ~A" here at
                                       (subseq text 0 (min 40 (length text))))
                         (if here #x1E #x17)))))
  ;; the registers: one that the last instruction changed is bright
  (loop for name in *register-names* for i from 0
        do (text-put 50 (+ 2 i) (format nil "~A  ~8,'0X  ~D" name (register i) (register i))
                     (if (and previous (/= (register i) (aref previous i))) #x1E #x17)))
  (let ((flags (register 9)))
    (text-put 50 12 (format nil "flags ~{~A~^ ~}"
                            (loop for (bit name) in '((0 "carry") (6 "zero") (7 "sign") (11 "overflow"))
                                  collect (if (logbitp bit flags) (string-upcase name) name)))
              (if (and previous (/= (logand flags #x8C1) (logand (aref previous 9) #x8C1))) #x1E #x17)))
  ;; the stack, from where ESP points
  (text-put 50 14 "the stack, from ESP:" #x13)
  (let ((top (+ (sap-int *stack*) +stack-bytes+ -64)))
    (loop for address from (register 4) below top by 4 for row from 15 below 23
          do (let ((word (sap-ref-32 (int-sap address) 0)))
               (text-put 50 row (format nil "~8,'0X  ~8,'0X~@[  ~A~]" address word
                                        (cond ((= word (program-marker program)) "the caller")
                                              ((<= (sap-int (program-code program)) word
                                                   (program-marker program))
                                               "a return address")))
                         #x17))))
  (text-put 1 22 (format nil "~D instruction~:P done.  ~A" steps note) #x1F)
  (text-put 1 24 " Space: one instruction   R: run to the end   A: again   N: next routine   Esc: leave "
            #x70))

(defun step-program (program)
  "Step PROGRAM as the keys ask.  Returns :NEXT or :QUIT."
  (start program)
  (let ((steps 0) (previous nil) (note "It has just been called."))
    (flet ((one-step ()
             (setf previous (coerce (loop for i below 10 collect (register i)) 'vector))
             (let ((trap (single-step)))
               (setf (register 9) (logandc2 (register 9) #x100))        ; the trap flag is ours
               (incf steps)
               (cond ((/= trap 1)
                      (setf note (format nil "Stopped by exception ~D at ~8,'0X." trap (register 8)))
                      nil)
                     ((finished-p program)
                      (setf note (format nil "It has returned: EAX is ~D." (register 0)))
                      nil)
                     (t (setf note "") t)))))
      (loop
        (show program steps previous note)
        (let* ((key (get-key))
               (stopped (or (finished-p program) (search "exception" note))))
          (case (if (characterp key) (char-downcase key) key)
            ((#\Space #\s) (unless stopped (one-step)))
            (#\r (unless stopped (loop repeat 100000 while (one-step))))
            (#\a (start program) (setf steps 0 previous nil note "It has just been called."))
            (#\n (return :next))
            (#\Escape (return :quit))))))))

(defun main (&optional name)
  "Single-stepping machine code: a small debugger."
  (cond ((equal name "?")
         (format t "~&Routines:~:{ ~A~*~*~*~}.~%" *routines*))
        ((null (step-function))
         (format t "~&This SBCL's runtime has no dos_single_step: it is older than this example.~%"))
        (t
         (setf *registers* (alien-sap (make-alien (unsigned 32) 13))
               *stack* (alien-sap (make-alien (unsigned 8) +stack-bytes+)))
         (let ((programs (mapcar #'load-program *routines*))
               (saved (save-text-screen)))
           (unwind-protect
                (progn
                  (hide-text-cursor)
                  (loop with rest = (or (member name programs :key #'program-name :test #'equalp)
                                        programs)
                        do (when (eq (step-program (first rest)) :quit) (return))
                           (setf rest (or (rest rest) programs))))
             (restore-text-screen saved)))))
  (values))

(doslib-examples:define-example "step" 'main
  :category :system
  :description "Single-stepping machine code: a small debugger.")
