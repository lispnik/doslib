;;;; How fast is this machine?  The processor's clock rate from its
;;;; time-stamp counter, and a chart of eight ways to move a megabyte
;;;;
;;;; A Pentium or later counts its own clock cycles in a 64-bit register
;;;; that RDTSC reads (CPUID says whether it is there: feature bit 4).
;;;; Counting cycles across a known stretch of the timer chip's 1,193,182 Hz
;;;; clock gives the processor's speed in MHz, which is how every "speed
;;;; test" of the time did it.
;;;;
;;;; Then the memory: a megabyte is copied over and over for a quarter of a
;;;; second each way, and the rates drawn as bars --
;;;;
;;;;   Lisp, a byte at a time      a loop over SAP-REF-8, compiled for speed
;;;;   Lisp, 32 bits at a time     the same loop over SAP-REF-32
;;;;   REPLACE on Lisp vectors     what portable Lisp would write
;;;;   the C library's memmove     SB-KERNEL:SYSTEM-AREA-UB8-COPY
;;;;   REP MOVSB, REP MOVSD        the processor's string instructions, from
;;;;                               four lines of hand-assembled machine code
;;;;   REP STOSD                   a fill: writes only, nothing read
;;;;   REP MOVSD to the screen     64,000 bytes at a time into the VGA's
;;;;                               memory at A0000h, in mode 13h
;;;;
;;;; On real hardware of the era the last line is the telling one: memory on
;;;; an ISA video card is many times slower than main memory, which is why
;;;; games drew as little as they could.  Under an emulator the numbers say
;;;; more about the emulator: QEMU's time-stamp counter follows the host's
;;;; clock, not instructions executed, and its video memory is ordinary
;;;; memory.  The order of the bars is still instructive.
;;;;
;;;; Run it and wait: two or three seconds.  Any key leaves.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.speed
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.speed)

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(defconstant +size+ (* 1024 1024))              ; what is copied each time
(defconstant +screen+ 64000)                    ; a mode 13h screenful
(defconstant +turn+ (floor +timer-hz+ 4))       ; how long each way is run: a quarter second

;;; The machine code.  All are C functions: arguments on the stack, the
;;; result (RDTSC's 64 bits) in EDX:EAX.
(defvar *rdtsc* nil)
(defvar *movsb* nil)
(defvar *movsd* nil)
(defvar *stosd* nil)

(defun assemble ()
  (flet ((copier (instruction)
           (machine-code #x56 #x57              ; push esi / push edi
                         #x8B #x7C #x24 #x0C    ; mov edi,[esp+12]   to
                         #x8B #x74 #x24 #x10    ; mov esi,[esp+16]   from
                         #x8B #x4C #x24 #x14    ; mov ecx,[esp+20]   how many
                         #xFC #xF3 instruction  ; cld / rep movsb or movsd
                         #x5F #x5E #xC3)))      ; pop edi / pop esi / ret
    (setf *rdtsc* (machine-code #x0F #x31 #xC3) ; rdtsc / ret
          *movsb* (copier #xA4)
          *movsd* (copier #xA5)
          *stosd* (machine-code #x57            ; push edi
                                #x8B #x7C #x24 #x08     ; mov edi,[esp+8]    to
                                #x8B #x44 #x24 #x0C     ; mov eax,[esp+12]   what
                                #x8B #x4C #x24 #x10     ; mov ecx,[esp+16]   how many
                                #xFC #xF3 #xAB  ; cld / rep stosd
                                #x5F #xC3))))   ; pop edi / ret

(defun time-stamp-counter-p ()
  (and (plusp (cpuid 0)) (logbitp 4 (nth-value 3 (cpuid 1)))))

(defun cycles ()
  (alien-funcall (sap-alien *rdtsc* (function (unsigned 64)))))

(defun string-copy (stub to from count)
  (alien-funcall (sap-alien stub (function void system-area-pointer system-area-pointer unsigned))
                 to from count))

(defun string-fill (to value count)
  (alien-funcall (sap-alien *stosd* (function void system-area-pointer unsigned unsigned))
                 to value count))

;;; The Lisp loops
(defun copy-bytes (to from count)
  (declare (system-area-pointer to from) (fixnum count) (optimize speed (safety 0)))
  (dotimes (i count) (setf (sap-ref-8 to i) (sap-ref-8 from i))))

(defun copy-words (to from count)
  (declare (system-area-pointer to from) (fixnum count) (optimize speed (safety 0)))
  (loop for i fixnum from 0 below count by 4
        do (setf (sap-ref-32 to i) (sap-ref-32 from i))))

(defun copy-vector (to from)
  (declare (type (simple-array (unsigned-byte 8) (*)) to from) (optimize speed))
  (replace to from))

;;; Measuring
(defun megahertz ()
  "The processor's clock rate: cycles counted across a quarter of a second."
  (let* ((start (timer-count)) (first (cycles)))
    (loop until (>= (- (timer-count) start) +turn+))
    (let ((last (cycles)) (clocks (- (timer-count) start)))
      (/ (* (- last first) +timer-hz+) clocks 1000000))))

(defun measure (bytes thunk counter)
  "Call THUNK, which moves BYTES, for a quarter of a second.  Millions of
bytes a second, and processor cycles to the byte (NIL without COUNTER)."
  (funcall thunk)                               ; once to fetch the pages
  (let ((start (timer-count)) (first (and counter (cycles))) (calls 0) (clocks 0))
    (loop do (funcall thunk) (incf calls)
             (setf clocks (- (timer-count) start))
          until (or (>= clocks +turn+) (minusp clocks)))        ; (midnight: give up)
    (let ((moved (* calls bytes)))
      (values (if (plusp clocks) (/ (* moved +timer-hz+) clocks 1000000) 0)
              (and counter (/ (- (cycles) first) moved))))))

;;; The chart
(defconstant +bar-x+ 50)
(defconstant +bar-width+ 28)

(defun processor-name ()
  (if (and (plusp (cpuid 0)) (>= (cpuid #x80000000) #x80000004))
      (string-trim " " (with-output-to-string (s)
                         (loop for leaf from #x80000002 to #x80000004
                               do (dolist (word (multiple-value-list (cpuid leaf)))
                                    (dotimes (i 4)
                                      (let ((c (ldb (byte 8 (* 8 i)) word)))
                                        (when (<= 32 c 126) (write-char (code-char c) s))))))))
      (multiple-value-bind (leaves b c d) (cpuid 0)     ; no name: the maker and the model
        (declare (ignore leaves))
        (format nil "~A, family ~D model ~D"
                (with-output-to-string (s)
                  (dolist (word (list b d c))
                    (dotimes (i 4) (write-char (code-char (ldb (byte 8 (* 8 i)) word)) s))))
                (ldb (byte 4 8) (cpuid 1)) (ldb (byte 4 4) (cpuid 1))))))

(defun draw-chart (results)
  "RESULTS is a list of (name rate cycles), in the order measured."
  (let ((best (reduce #'max results :key #'second :initial-value 1)))
    (loop for (name rate per-byte) in results for row from 6
          do (text-put 2 row name 15)
             (text-put 27 row (format nil "~7:D MB/s" (round rate)) 14)
             (when per-byte
               (text-put 40 row (format nil "~7,2F" per-byte) 7))
             (text-put +bar-x+ row
                       (make-string (max 1 (round (* +bar-width+ rate) best))
                                    :initial-element (code-char 219))
                       (if (= rate best) 10 2)))))

(defun speed-test ()
  (assemble)
  (let* ((counter (time-stamp-counter-p))
         (from (make-alien (unsigned 8) +size+)) (to (make-alien (unsigned 8) +size+))
         (from-sap (alien-sap from)) (to-sap (alien-sap to))
         (from-vector (make-array +size+ :element-type '(unsigned-byte 8) :initial-element 1))
         (to-vector (make-array +size+ :element-type '(unsigned-byte 8)))
         (results '()) screen (same t))
    (unwind-protect
         (flet ((try (name bytes thunk)
                  (multiple-value-bind (rate per-byte) (measure bytes thunk counter)
                    (setf results (append results (list (list name rate per-byte))))
                    (draw-chart results))))
           (dotimes (i +size+) (setf (sap-ref-8 from-sap i) (logand (* i 37) 255)))
           ;; the screen first, while there is nothing to spoil; drawn last
           (with-video-mode (#x13)
             (setf screen (multiple-value-list
                           (measure +screen+
                                    (lambda () (string-copy *movsd* *vga* from-sap
                                                            (floor +screen+ 4)))
                                    counter))))
           (video-mode 3)
           (text-fill 0 0 80 25 7)
           (text-put 2 0 "How fast is this machine?" 11)
           (text-put 2 2 (processor-name) 15)
           (text-put 2 3 (if counter
                             (format nil "~:D MHz by its time-stamp counter, against the timer chip"
                                     (round (megahertz)))
                             "No time-stamp counter: a 486 or older, so no clock rate to give")
                     7)
           (text-put 2 5 "Moving a megabyte" 7)
           (text-put 31 5 "rate" 7)
           (when counter (text-put 40 5 "cycles/byte" 7))
           (try "Lisp, a byte at a time" +size+ (lambda () (copy-bytes to-sap from-sap +size+)))
           (try "Lisp, 32 bits at a time" +size+ (lambda () (copy-words to-sap from-sap +size+)))
           (try "REPLACE on Lisp vectors" +size+ (lambda () (copy-vector to-vector from-vector)))
           (try "C library's memmove" +size+
                (lambda () (sb-kernel:system-area-ub8-copy from-sap 0 to-sap 0 +size+)))
           (try "REP MOVSB" +size+ (lambda () (string-copy *movsb* to-sap from-sap +size+)))
           (try "REP MOVSD" +size+
                (lambda () (string-copy *movsd* to-sap from-sap (floor +size+ 4))))
           (dotimes (i +size+)                  ; did the last copy arrive?
             (unless (= (sap-ref-8 to-sap i) (sap-ref-8 from-sap i)) (return (setf same nil))))
           (try "REP STOSD (a fill)" +size+
                (lambda () (string-fill to-sap #x55AA55AA (floor +size+ 4))))
           (setf results (append results (list (cons "REP MOVSD to the screen" screen))))
           (draw-chart results)
           (text-put 2 15 (format nil "The copies agree: ~:[NO~;yes~]"
                                  (and same (every (lambda (byte) (= byte 1)) to-vector)))
                     7)
           (text-put 2 17 "MB/s is millions of bytes a second.  Under an emulator these measure" 8)
           (text-put 2 18 "the emulator; on a real machine the screen is the slow one." 8)
           (text-put 2 20 "Any key leaves." 7)
           (wait-key)
           (video-mode 3)
           (format t "~&Fastest: ~A, ~:D million bytes a second.~%"
                   (first (first (sort (copy-list results) #'> :key #'second)))
                   (round (reduce #'max results :key #'second))))
      (free-alien from) (free-alien to))))

(defun main ()
  "The processor's clock rate, and a chart of eight ways to move a megabyte."
  (speed-test))

(doslib-examples:define-example "speed" 'main
  :category :system
  :description "The processor's clock rate, and a chart of eight ways to move a megabyte.")
