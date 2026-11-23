;;;; Machine instructions in line: teaching the compiler new operations
;;;;
;;;; There are two ways to get at an instruction Lisp has no function for.
;;;; One is to put machine code in memory and call it, as the library does
;;;; for the I/O ports and the "asmdsl" example does with its own assembler:
;;;; every use is then a call to a foreign function.  The other is to tell
;;;; the compiler about the operation -- what it takes, what it gives, and
;;;; the instructions that do it -- and the compiler then puts those
;;;; instructions straight into whatever code uses it, with the arguments
;;;; already in registers.  SBCL calls such a definition a VOP, a "virtual
;;;; operation"; its own +, CAR and SAP-REF-8 are defined the same way.
;;;;
;;;; This is the compiler's internal interface (the packages SB-C, SB-VM and
;;;; SB-ASSEM), not a documented one: it changes between versions of SBCL.
;;;;
;;;; Three operations are defined here and timed against the other ways:
;;;;   FILL-WORDS   rep stosd: store a word many times
;;;;   SWAP-BYTES   bswap: turn the four bytes of a word round
;;;;   PORT-BYTE    in al,dx: read an I/O port
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.vop
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main #:fill-words #:swap-bytes #:port-byte))
(in-package #:doslib-examples.vop)

;;; ---- the operations
;;; DEFKNOWN tells the compiler the function exists and what its types are;
;;; DEFINE-VOP gives the instructions.  In a VOP:
;;;   :args and :results   say where each value may be: :SCS lists "storage
;;;                        classes", and UNSIGNED-REG is a general register
;;;                        holding a plain 32-bit number (not a tagged one)
;;;   :temporary           asks for a register to work in; with :OFFSET, a
;;;                        particular one, as REP STOSD wants EDI, EAX, ECX
;;;   :from and :to        say for how long, so that an argument can be put
;;;                        in that very register and the MOVE costs nothing
;;;   :generator           is the code: INST assembles one instruction
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown fill-words (system-area-pointer (unsigned-byte 32) (unsigned-byte 32)) (values)
      () :overwrite-fndb-silently t)
  (sb-c:defknown swap-bytes ((unsigned-byte 32)) (unsigned-byte 32)
      () :overwrite-fndb-silently t)
  (sb-c:defknown port-byte ((unsigned-byte 16)) (unsigned-byte 8)
      () :overwrite-fndb-silently t)

  (sb-c:define-vop (fill-words)
    (:translate fill-words)
    (:policy :fast-safe)
    (:args (to :scs (sb-vm::sap-reg) :target edi)
           (value :scs (sb-vm::unsigned-reg) :target eax)
           (count :scs (sb-vm::unsigned-reg) :target ecx))
    (:arg-types system-area-pointer sb-vm::unsigned-num sb-vm::unsigned-num)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::edi-offset :from (:argument 0)) edi)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::eax-offset :from (:argument 1)) eax)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::ecx-offset :from (:argument 2)) ecx)
    (:generator 10
      (sb-vm::move edi to)
      (sb-vm::move eax value)
      (sb-vm::move ecx count)
      (sb-assem:inst byte #xFC)         ; cld (an instruction the assembler has no name for)
      (sb-assem:inst rep)
      (sb-assem:inst stos eax)))        ; stosd: [edi] <- eax, edi + 4, ecx times

  (sb-c:define-vop (swap-bytes)
    (:translate swap-bytes)
    (:policy :fast-safe)
    (:args (word :scs (sb-vm::unsigned-reg) :target result))
    (:arg-types sb-vm::unsigned-num)
    (:results (result :scs (sb-vm::unsigned-reg)))
    (:result-types sb-vm::unsigned-num)
    (:generator 2
      (sb-vm::move result word)
      (sb-assem:inst bswap result)))

  (sb-c:define-vop (port-byte)
    (:translate port-byte)
    (:policy :fast-safe)
    (:args (port :scs (sb-vm::unsigned-reg) :target edx))
    (:arg-types sb-vm::unsigned-num)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::edx-offset :from (:argument 0)) edx)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::eax-offset :to (:result 0)) eax)
    (:results (value :scs (sb-vm::unsigned-reg)))
    (:result-types sb-vm::unsigned-num)
    (:generator 5
      (sb-vm::move edx port)
      (sb-assem:inst xor eax eax)
      (sb-assem:inst byte #xEC)         ; in al,dx
      (sb-vm::move value eax))))

;;; The functions themselves, for where the compiler does not know the
;;; types (the listener, FUNCALL): each is one use of its own operation.
(defun fill-words (to value count)
  "Store the 32 bits of VALUE at the address TO (a system area pointer), COUNT times."
  (declare (type system-area-pointer to) (type (unsigned-byte 32) value count))
  (fill-words to value count))
(defun swap-bytes (word)
  "The 32 bits of WORD with their four bytes in the other order."
  (declare (type (unsigned-byte 32) word))
  (swap-bytes word))
(defun port-byte (port)
  "Read a byte from I/O port PORT."
  (declare (type (unsigned-byte 16) port))
  (port-byte port))

;;; ---- the same things the other ways
(defvar *stosd* (saved-code '(#x57                       ; push edi
                              #x8B #x7C #x24 #x08        ; mov edi,[esp+8]    to
                              #x8B #x44 #x24 #x0C        ; mov eax,[esp+12]   what
                              #x8B #x4C #x24 #x10        ; mov ecx,[esp+16]   how many
                              #xFC #xF3 #xAB             ; cld / rep stosd
                              #x5F #xC3)))               ; pop edi / ret
(defvar *bswap* (saved-code '(#x8B #x44 #x24 #x04        ; mov eax,[esp+4]
                              #x0F #xC8 #xC3)))          ; bswap eax / ret

(defconstant +words+ 16000 "A mode 13h screen's worth.")

(defun fill-in-lisp (to value)
  (declare (type system-area-pointer to) (type (unsigned-byte 32) value)
           (optimize speed (safety 0)))
  (dotimes (i +words+) (setf (sap-ref-32 to (* 4 i)) value)))
(defun fill-by-call (to value)
  (alien-funcall (sap-alien (code-address *stosd*)
                            (function void system-area-pointer unsigned unsigned))
                 to value +words+))
(defun fill-in-line (to value)
  (declare (type system-area-pointer to) (type (unsigned-byte 32) value))
  (fill-words to value +words+))

(defun swap-in-lisp (count)
  (declare (fixnum count) (optimize speed (safety 0)))
  (let ((sum 0))
    (declare (type (unsigned-byte 32) sum))
    (dotimes (i count sum)
      (let ((word (logand (+ sum i) #xFFFFFFFF)))
        (setf sum (logior (ash (ldb (byte 8 0) word) 24) (ash (ldb (byte 8 8) word) 16)
                          (ash (ldb (byte 8 16) word) 8) (ldb (byte 8 24) word)))))))
(defun swap-by-call (count)
  (declare (fixnum count))
  (let ((sum 0) (code (code-address *bswap*)))
    (declare (type (unsigned-byte 32) sum))
    (dotimes (i count sum)
      (setf sum (alien-funcall (sap-alien code (function unsigned unsigned))
                               (logand (+ sum i) #xFFFFFFFF))))))
(defun swap-in-line (count)
  (declare (fixnum count) (optimize speed (safety 0)))
  (let ((sum 0))
    (declare (type (unsigned-byte 32) sum))
    (dotimes (i count sum)
      (setf sum (swap-bytes (logand (+ sum i) #xFFFFFFFF))))))

(defun port-by-call (count)
  (declare (fixnum count))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i count sum) (setf sum (logxor sum (inb #x61))))))
(defun port-in-line (count)
  (declare (fixnum count) (optimize speed (safety 0)))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i count sum) (setf sum (logxor sum (port-byte #x61))))))

;;; ---- timing
(defun microseconds (thunk)
  "How long THUNK takes: the best of three, by the timer chip."
  (loop repeat 3
        minimize (let ((start (timer-count)))
                   (funcall thunk)
                   (/ (* 1000000.0 (- (timer-count) start)) +timer-hz+))))

(defun report (what unit rows)
  "ROWS are (name microseconds each): print them, each against the first."
  (format t "~&~%~A~%" what)
  (let ((first (second (first rows))))
    (loop for (name each) in rows
          do (let ((ratio (/ first (max each 1e-6))))
               (format t "  ~22A ~10,3F us ~A~:[   ~,1F times ~:[slower~;as fast~]~;~]~%"
                       name each unit (eq each first) (if (>= ratio 1) ratio (/ ratio))
                       (>= ratio 1))))))

(defun main ()
  "Machine instructions in line: three operations added to the compiler, and timed."
  (let ((memory (alien-sap (make-alien (unsigned 32) +words+)))
        (swaps 200000) (reads 20000))
    (format t "~&Three instructions Lisp has no function for, used three ways.~%")
    (report "Filling 64,000 bytes with a word (rep stosd):" "a fill"
            (list (list "a Lisp loop" (microseconds (lambda () (fill-in-lisp memory #x01020304))))
                  (list "machine code, called" (microseconds (lambda () (fill-by-call memory #x01020304))))
                  (list "in line (a VOP)" (microseconds (lambda () (fill-in-line memory #x05060708))))))
    (unless (and (= (sap-ref-32 memory 0) #x05060708)
                 (= (sap-ref-32 memory (* 4 (1- +words+))) #x05060708))
      (error "FILL-WORDS did not fill the memory"))
    (unless (= (swap-in-lisp 1000) (swap-by-call 1000) (swap-in-line 1000))
      (error "The three ways of swapping bytes disagree"))
    (report "Turning a word's bytes round (bswap):" "a word"
            (list (list "Lisp: shifts and masks" (/ (microseconds (lambda () (swap-in-lisp swaps))) swaps))
                  (list "machine code, called" (/ (microseconds (lambda () (swap-by-call swaps))) swaps))
                  (list "in line (a VOP)" (/ (microseconds (lambda () (swap-in-line swaps))) swaps))))
    (report "Reading I/O port 61h (in al,dx):" "a read"
            (list (list "machine code, called" (/ (microseconds (lambda () (port-by-call reads))) reads))
                  (list "in line (a VOP)" (/ (microseconds (lambda () (port-in-line reads))) reads))))
    (format t "~&~%(swap-bytes #x11223344) is #x~8,'0X.  This is all of SWAP-BYTES, as the~%~
               compiler made it: the BSWAP is in the function, not called from it.~%"
            (swap-bytes #x11223344))
    (let ((listing (with-output-to-string (out) (disassemble 'swap-bytes :stream out))))
      (with-input-from-string (in listing)
        (loop for line = (read-line in nil)
              while line
              when (search "BSWAP" line) do (format t "  ~A~%" (string-trim " ;" line)))))
    (free-alien (sap-alien memory (* (unsigned 32)))))
  (values))

(doslib-examples:define-example "vop" 'main
  :category :system
  :description "Machine instructions in line: three operations added to the compiler, and timed.")
