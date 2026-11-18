;;;; A Lisp function as an interrupt handler.
;;;;
;;;; Part 1: interrupt vector 60h is pointed at a Lisp function.  A few bytes
;;;; of glue save the registers, pass AX BX CX DX to the function (an alien
;;;; callback), put its result in EAX and IRET.  Anything that executes
;;;; "INT 60h" -- here a little machine-code client that knows nothing about
;;;; Lisp -- now gets its answer computed by Lisp.
;;;;
;;;; Part 2: a hardware interrupt.  The timer tick (IRQ 0, vector 1Ch) can
;;;; arrive at any instant, including in the middle of a garbage collection,
;;;; so its handler cannot be Lisp code; it is machine code, assembled from
;;;; Lisp, that counts ticks in a memory cell and chains to the old handler.
;;;; Lisp just watches the cell.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.intvec
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.intvec)

;;; ---- part 1: the handler, in Lisp
(defun nth-prime (n)
  (loop with found = 0 for k from 2
        when (loop for d from 2 to (isqrt k) never (zerop (mod k d)))
          do (when (= (incf found) n) (return k))))
(defun service (ax bx cx dx)
  (declare (ignorable dx))
  (let ((result (case ax
                  (0 #x4C495350)                        ; "LISP": are you there?
                  (1 (+ bx cx))
                  (2 (nth-prime bx))
                  (3 (logand (reduce #'* (loop for i from 1 to bx collect i))
                             #xFFFFFFFF))               ; factorial, low 32 bits
                  (t #xFFFFFFFF))))
    (format t "  [Lisp handler]  INT 60h  AX=~D BX=~D CX=~D  ->  EAX=~8,'0Xh~%"
            ax bx cx result)
    result))
;;; The code below is made when the demo runs, not when this file is
;;; loaded: memory outside the Lisp heap does not survive into a saved core.
(defvar *callback*)                     ; SERVICE, as something C could call
(defvar *int60*)                        ; the glue the vector points at
(defvar *client*)                       ; unsigned client(void): asks for the 1000th
                                        ; prime, knowing only "INT 60h"
(defun make-code ()
  (setf *callback*
        (alien-sap (sb-alien::alien-lambda unsigned ((ax unsigned) (bx unsigned)
                                                     (cx unsigned) (dx unsigned))
                     (service ax bx cx dx))))
  (setf *int60*
        (apply #'machine-code
               `(#x60 #x52 #x51 #x53 #x50 #xFC          ; pushad; push edx,ecx,ebx,eax; cld
                 #xB8 ,@(bytes32 (sap-int *callback*))  ; mov eax, callback
                 #xFF #xD0 #x83 #xC4 16                 ; call eax; add esp,16
                 #x89 #x44 #x24 28 #x61 #xCF)))         ; mov [esp+28],eax; popad; iretd
  (setf *client* (machine-code #x53 #xB8 2 0 0 0 #xBB #xE8 3 0 0   ; mov eax,2; mov ebx,1000
                               #xCD #x60 #x5B #xC3)))            ; int 60h; pop ebx; ret
(defun demo ()
  (make-code)
  (multiple-value-bind (old-selector old-offset) (get-vector #x60)
    (format t "~&INT 60h was ~4,'0X:~8,'0X~%" old-selector old-offset)
    (set-vector #x60 (code-selector) (sap-int *int60*))
    (multiple-value-bind (s o) (get-vector #x60)
      (format t "INT 60h is now ~4,'0X:~8,'0X, glue for #'SERVICE~%" s o))
    (format t "From Lisp, with INT86:~%")
    (format t "  installation check returned ~S~%"
            (map 'string #'code-char (reverse (bytes32 (int86 #x60 :ax 0)))))
    (format t "  40 + 2 = ~D~%" (int86 #x60 :ax 1 :bx 40 :cx 2))
    (format t "  12! = ~D~%" (int86 #x60 :ax 3 :bx 12))
    (format t "From a machine-code client:~%")
    (format t "  it got ~D back in EAX~%"
            (alien-funcall (sap-alien *client* (function unsigned))))
    (set-vector #x60 old-selector old-offset))

  ;; ---- part 2: the timer tick
  (let* ((counter (alien-sap (make-alien (unsigned 32) 1)))
         (ds (data-selector)))
    (setf (sap-ref-32 counter 0) 0)
    (multiple-value-bind (old-selector old-offset) (get-vector #x1C)
      (let ((isr (apply #'machine-code
                        `(#x1E #x50 #x66 #xB8 ,(lo ds) ,(hi ds) #x8E #xD8   ; push ds,eax; mov ds,flat
                          #xFF #x05 ,@(bytes32 (sap-int counter))           ; inc dword [counter]
                          #x58 #x1F                                         ; pop eax, ds
                          #xEA ,@(bytes32 old-offset) ,(lo old-selector) ,(hi old-selector)))))
        (format t "Timer tick (INT 1Ch) hooked at ~8,'0X; counting for 2 seconds...~%"
                (sap-int isr))
        (set-vector #x1C (code-selector) (sap-int isr))
        (let ((sum 0d0))                ; Lisp is busy; the ticks arrive anyway
          (loop with start = (get-internal-real-time)
                until (>= (- (get-internal-real-time) start)
                          (* 2 internal-time-units-per-second))
                do (incf sum (sin sum)))
          (set-vector #x1C old-selector old-offset)
          (format t "  ~D ticks arrived while Lisp was computing (18.2 per second expected)~%"
                  (sap-ref-32 counter 0)))))))

(defun main ()
  "A Lisp function as an interrupt handler."
  (demo))

(doslib-examples:define-example "intvec" 'main
  :category :system
  :description "A Lisp function as an interrupt handler.")
