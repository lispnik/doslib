;;;; Handlers for hardware interrupts, built to order.
;;;;
;;;; A device says it wants attention by raising an interrupt request -- IRQ
;;;; 0 is the timer, 1 the keyboard, 5 usually a sound card, 6 the floppy
;;;; controller -- and the processor drops what it is doing and runs the
;;;; handler for it.  That handler cannot be Lisp: the interrupt may come in
;;;; the middle of anything, the garbage collector included.  What nearly
;;;; every handler has to do is small and always the same, though: tell the
;;;; device it has been heard (reading or writing one of its ports), note
;;;; that it happened, tell the interrupt controller it is done, and return.
;;;; Lisp then looks at the note when it is ready to.
;;;;
;;;; INSTALL-IRQ-HANDLER makes such a handler in machine code from a
;;;; description -- which ports to read and write, and whether to pass the
;;;; interrupt on to the handler that was there before -- and puts it in
;;;; place; the note is a count, which IRQ-COUNT reads.  WITH-IRQ-HANDLER
;;;; does that around some code and takes the handler out again however
;;;; the code ends.
;;;;
;;;;   (with-irq-handler (hook 6)                   ; the floppy controller
;;;;     ... start something ...
;;;;     (loop until (plusp (irq-count hook))))     ; it has finished
;;;;
;;;; A device that hands over a byte with each interrupt -- a serial port --
;;;; can have them kept: with :BUFFER and its data port, the
;;;; handler reads the port and puts the byte in a ring of 256, and
;;;; IRQ-READ-BYTE takes them out in the order they came.
;;;;
;;;; Passing the interrupt on (:CHAIN) is for interrupts that others need
;;;; too: the timer, above all.  :CHAIN T passes on every one.  :CHAIN with
;;;; a number is for a timer made to run fast (see examples/system/timer.lisp):
;;;; it is added to a 16-bit sum at each interrupt, and the interrupt is
;;;; passed on only when the sum overflows -- with the timer's divisor as the
;;;; number, that is exactly as often as the timer used to interrupt, so the
;;;; BIOS's clock keeps time.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

;;; A hook's memory outside the Lisp heap: the count (4 bytes), the 16-bit
;;; sum for :CHAIN n (at 4), the previous handler as a far jump through
;;; memory wants it (offset at 6, selector at 10), and for :BUFFER where the
;;; next byte goes (one byte, at 12) in the ring of 256 that starts at 16.
(defstruct (irq-hook (:constructor %make-irq-hook))
  "A handler made by INSTALL-IRQ-HANDLER: what it is on, and what was there before."
  irq memory code chain unmask installed old-selector old-offset old-mask (tail 0))

(defvar *irq-hooks* (make-hash-table :test #'equal)
  "Description -> hook: a handler asked for again is the same one, not made again.")

(defun irq-vector (irq)
  "The interrupt vector of hardware interrupt request IRQ (0 to 15)."
  (if (< irq 8) (+ 8 irq) (+ #x70 (- irq 8))))

(defun irq-handler-bytes (irq memory &key read-ports write-ports buffer chain)
  "The machine code of a handler for IRQ, as a list of bytes: write each
(port value) of WRITE-PORTS, read each of READ-PORTS, read port BUFFER if
given and put the byte in the ring at MEMORY + 16, add one to the four
bytes at MEMORY, and then either tell the interrupt controller and return,
or (CHAIN, as INSTALL-IRQ-HANDLER describes it) jump to the handler whose
address is at MEMORY + 6."
  (let ((ds (data-selector))
        (done `(#xB0 #x20 ,@(when (>= irq 8) '(#xE6 #xA0)) #xE6 #x20))  ; mov al,20h / out (A0h,) 20h
        (leave '(#x1F #x5A #x58))                       ; pop ds / pop edx / pop eax
        (pass-on `(#x2E #xFF #x2D ,@(bytes32 (+ memory 6)))))           ; jmp far cs:[old handler]
    `(#x50 #x52 #x1E                                    ; push eax / push edx / push ds
      #x66 #xB8 ,(lo ds) ,(hi ds) #x8E #xD8             ; mov ax, our data selector / mov ds, ax
      ,@(loop for (port value) in write-ports
              append `(#xBA ,@(bytes32 port) #xB0 ,value #xEE))         ; mov edx, port / mov al, value / out dx, al
      ,@(loop for port in read-ports
              append `(#xBA ,@(bytes32 port) #xEC))     ; mov edx, port / in al, dx
      ,@(when buffer
          `(#xBA ,@(bytes32 buffer) #xEC                ; mov edx, port / in al, dx
            #x0F #xB6 #x15 ,@(bytes32 (+ memory 12))    ; movzx edx, byte [where the next goes]
            #x88 #x82 ,@(bytes32 (+ memory 16))         ; mov [ring + edx], al
            #xFE #x05 ,@(bytes32 (+ memory 12))))       ; inc byte [where the next goes]
      #xFF #x05 ,@(bytes32 memory)                      ; inc dword [count]
      ,@(cond ((null chain) `(,@done ,@leave #xCF))     ; ... / iretd
              ((eq chain t) `(,@leave ,@pass-on))
              (t `(#x66 #x81 #x05 ,@(bytes32 (+ memory 4)) ,(lo chain) ,(hi chain)  ; add word [sum], n
                   #x72 ,(+ (length done) (length leave) 1)            ; jc pass-on
                   ,@done ,@leave #xCF
                   ,@leave ,@pass-on))))))

(defun install-irq-handler (irq &key read-ports write-ports buffer chain (unmask (not chain)))
  "Put a handler on hardware interrupt request IRQ (0 to 15) that writes
each (port value) of WRITE-PORTS and then reads each of READ-PORTS --
whatever tells the device it has been heard -- and counts.  With BUFFER, a
port, it also reads that port and keeps the byte for IRQ-READ-BYTE (the
last 256 of them).  Without CHAIN it then tells the
interrupt controller the interrupt is done; with CHAIN true it passes every
interrupt on to the handler that was there before, and with CHAIN a number
only when a 16-bit sum of that number overflows.  With UNMASK (the default
unless chaining) the interrupt controller is told to let the interrupt
through.  Returns a hook, for IRQ-COUNT and REMOVE-IRQ-HANDLER."
  (let* ((key (list irq read-ports write-ports buffer chain))
         (hook (or (gethash key *irq-hooks*)
                   (let ((memory (alien-sap (make-alien (unsigned 8) 272))))
                     (setf (gethash key *irq-hooks*)
                           (%make-irq-hook
                            :irq irq :memory memory :chain chain
                            :code (apply #'machine-code
                                         (irq-handler-bytes irq (near-address (sap-int memory))
                                                            :read-ports read-ports
                                                            :write-ports write-ports
                                                            :buffer buffer
                                                            :chain chain)))))))
         (memory (irq-hook-memory hook))
         (port (if (< irq 8) #x21 #xA1)) (bit (ash 1 (logand irq 7))))
    (when (irq-hook-installed hook)
      (error "There is already such a handler on interrupt request ~D" irq))
    (multiple-value-bind (selector offset) (get-vector (irq-vector irq))
      (setf (irq-hook-old-selector hook) selector (irq-hook-old-offset hook) offset
            (irq-hook-old-mask hook) (inb port) (irq-hook-unmask hook) unmask
            (sap-ref-32 memory 0) 0 (sap-ref-16 memory 4) 0
            (sap-ref-8 memory 12) 0 (irq-hook-tail hook) 0
            (sap-ref-32 memory 6) offset (sap-ref-16 memory 10) selector))
    (set-vector (irq-vector irq) (code-selector) (near-address (sap-int (irq-hook-code hook))))
    (when unmask (outb port (logandc2 (inb port) bit)))
    (setf (irq-hook-installed hook) t)
    hook))

(defun remove-irq-handler (hook)
  "Take HOOK's handler out: the interrupt controller's mask bit and the
vector go back to what they were when it was installed."
  (when (irq-hook-installed hook)
    (let* ((irq (irq-hook-irq hook))
           (port (if (< irq 8) #x21 #xA1)) (bit (ash 1 (logand irq 7))))
      (when (irq-hook-unmask hook)
        (outb port (logior (logandc2 (inb port) bit) (logand (irq-hook-old-mask hook) bit))))
      (set-vector (irq-vector irq) (irq-hook-old-selector hook) (irq-hook-old-offset hook))
      (setf (irq-hook-installed hook) nil)))
  nil)

(defun remove-irq-handlers ()
  "Take out every handler INSTALL-IRQ-HANDLER has put in: done when Lisp exits."
  (maphash (lambda (key hook) (declare (ignore key)) (remove-irq-handler hook)) *irq-hooks*))

(defun irq-count (hook)
  "How many interrupts HOOK's handler has seen since it was installed.  SETF it to start again."
  (sap-ref-32 (irq-hook-memory hook) 0))
(defun (setf irq-count) (count hook)
  "Set the number of interrupts HOOK's handler has counted."
  (setf (sap-ref-32 (irq-hook-memory hook) 0) count))

(defun irq-read-byte (hook)
  "The next byte the handler of HOOK (installed with :BUFFER) has kept, or
NIL when there is none waiting.  More than 255 left unread are lost."
  (let ((tail (irq-hook-tail hook)) (memory (irq-hook-memory hook)))
    (unless (= tail (sap-ref-8 memory 12))
      (setf (irq-hook-tail hook) (logand (1+ tail) 255))
      (sap-ref-8 memory (+ 16 tail)))))

(defmacro with-irq-handler ((hook irq &rest options) &body body)
  "Run BODY with a handler on hardware interrupt request IRQ, made as
INSTALL-IRQ-HANDLER makes it from OPTIONS, and HOOK bound to its hook: (irq-count
hook) is the number of interrupts so far.  The handler is taken out again
however BODY ends."
  `(let ((,hook (install-irq-handler ,irq ,@options)))
     (unwind-protect (progn ,@body)
       (remove-irq-handler ,hook))))
