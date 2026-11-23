;;;; Processor faults as Lisp conditions
;;;;
;;;; When machine code does something the processor will not have -- writes
;;;; where there is no memory, divides by zero, meets an instruction that
;;;; does not exist -- the processor raises an exception.  A DOS program
;;;; usually ends there, with a register dump from its extender.  Here the
;;;; exception arrives in Lisp as a condition, signalled in the middle of
;;;; the machine code that caused it: a handler can look at it, a restart
;;;; can put things right, and the program carries on.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.faults
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.faults)

(defparameter *faults*
  ;; what it does, and the machine code: a function of no arguments
  `(("writes to an address where there is no memory"
     ,(saved-code '(#xC7 #x05 #x00 #x00 #x00 #xF0 1 0 0 0   ; mov dword [F0000000h],1
                    #xC3)))                                  ; ret
    ("divides by zero"
     ,(saved-code '(#x31 #xC9 #x31 #xD2                     ; xor ecx,ecx / xor edx,edx
                    #xB8 1 0 0 0 #xF7 #xF1                  ; mov eax,1 / div ecx
                    #xC3)))
    ("runs an instruction that does not exist"
     ,(saved-code '(#x0F #x0B                               ; ud2
                    #xC3)))
    ("loads a segment register with nonsense"
     ,(saved-code '(#x06                                    ; push es
                    #x66 #xB8 #xFF #xFF                     ; mov ax,FFFFh
                    #x8E #xC0                               ; mov es,ax    (no such segment)
                    #x07 #xC3)))))                          ; pop es / ret

(defun call-code (code)
  (alien-funcall (sap-alien (code-address code) (function unsigned))))

(defun one-line (condition)
  (let ((text (substitute #\Space #\Newline (princ-to-string condition))))
    (if (> (length text) 64) (concatenate 'string (subseq text 0 61) "...") text)))

;;; ---- a fault put right while the program waits
;;; LOOK-UP reads a word through a pointer, in machine code.  Given a bad
;;; pointer it faults; the code that calls it offers a restart, USE-ADDRESS,
;;; and a handler -- further out, knowing nothing of machine code -- takes
;;; it.  Nothing is unwound: the call that failed is made again, from where
;;; it was, and the sum it was part of is finished.
(defvar *look-up* (saved-code '(#x8B #x44 #x24 #x04         ; mov eax,[esp+4]
                                #x8B #x00 #xC3)))           ; mov eax,[eax] / ret
(defun look-up (address)
  (loop
    (restart-case
        (return (alien-funcall (sap-alien (code-address *look-up*) (function unsigned unsigned))
                               address))
      (use-address (new)
        :report "Read from another address."
        (setf address new)))))

(defun main ()
  "Processor faults as Lisp conditions: caught, reported, and put right."
  (format t "~&Machine code that the processor stops, and what Lisp is told:~%")
  (loop for (what code) in *faults*
        do (format t "~&  It ~A:~%" what)
           (handler-case (progn (call-code code)
                                (format t "      nothing happened: this machine let it pass.~%"))
             (error (condition)
               (format t "      ~A~%      ~A~%" (type-of condition) (one-line condition)))))
  (format t "~&Still here: (+ 40 2) is ~D.~%" (+ 40 2))
  (let* ((table (alien-sap (make-alien (unsigned 32) 4)))
         (good (sap-int table))
         (faults 0))
    (dotimes (i 4) (setf (sap-ref-32 table (* 4 i)) (* 100 (1+ i))))
    (format t "~&~%Adding up four words through pointers, the third of them bad:~%")
    (let ((sum (handler-bind ((error (lambda (condition)
                                        (incf faults)
                                        (format t "  fault: ~A~%  -> reading from ~8,'0Xh instead, and going on~%"
                                                (one-line condition) (+ good 8))
                                        (invoke-restart 'use-address (+ good 8)))))
                 (loop for address in (list good (+ good 4) #xF0000000 (+ good 12))
                       sum (look-up address)))))
      (format t "  The sum is ~D, with ~D fault~:P on the way.~%" sum faults))
    (free-alien (sap-alien table (* (unsigned 32)))))
  (values))

(doslib-examples:define-example "faults" 'main
  :category :lisp
  :description "Processor faults as Lisp conditions: caught, reported, and put right.")
