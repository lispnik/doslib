;;;; Tests of the DOSLIB library that run on the DOS machine itself, and
;;;; the wrapper the test runner calls the examples through.
;;;;
;;;; tests/run.py copies this file to D:\SBCL\T.LSP and loads it into a
;;;; Lisp that has the examples in it.  Then, typed at the listener:
;;;;   (u)                 run the tests below; the tally goes to the screen
;;;;                       and one line per test to UNIT.TXT
;;;;   (r 7 "fire")        run an example and print "=== 7 ok", or the error
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-tests
  (:use #:cl #:doslib)
  (:export #:u #:r))
(in-package #:doslib-tests)

(defvar *tests* '() "(name . function), in the order they were defined.")
(defmacro deftest (name &body body)
  `(progn (setf *tests* (append (remove ',name *tests* :key #'car)
                                (list (cons ',name (lambda () ,@body)))))
          ',name))
(defmacro is (form)
  `(unless ,form (error "~S is false" ',form)))
(defmacro is= (expected form)
  `(let ((value ,form))
     (unless (equal value ,expected) (error "~S is ~S, not ~S" ',form value ,expected))))

(defun one-line (thing)
  (let ((text (substitute #\Space #\Newline (princ-to-string thing))))
    (subseq text 0 (min 70 (length text)))))

(defun text-mode ()                     ; whatever was running, the tally must be readable
  (unless (and (member (peek #x449) '(2 3)) (= (peek #x484) 24))
    (video-mode 3)))

(defun u ()
  "Run the tests."
  (let ((failed 0))
    (with-open-file (log "UNIT.TXT" :direction :output :if-exists :supersede)
      (dolist (test *tests*)
        (let ((problem (handler-case (progn (funcall (cdr test)) nil)
                         (error (e) (one-line e)))))
          (text-mode)
          (format log "~:[ok~;FAIL~] ~(~A~)~@[: ~A~]~%" problem (car test) problem)
          (when problem
            (incf failed)
            (format t "~&FAIL ~(~A~): ~A~%" (car test) problem)))))
    (format t "~&=== unit ~D passed, ~D failed~%" (- (length *tests*) failed) failed)
    (values)))

(defun r (number name &rest arguments)
  "Run the example NAME; say how it ended on a line the runner looks for."
  (let ((result (handler-case (progn (apply #'doslib-examples:run name arguments) "ok")
                  (error (e) (format nil "ERROR ~A" (one-line e))))))
    (text-mode)
    (format t "~&=== ~D ~A~%" number result)
    (values)))

(import '(u r) '#:cl-user)

;;; An example that never ends, for the tests of interrupting one.
(defun spin ()
  (format t "~&spinning~%")
  (finish-output)
  (loop))
(doslib-examples:define-example "spin" 'spin
  :category :other
  :description "A loop that never ends (tests).")

;;; ---- machine.lisp, dos.lisp, memory.lisp, cpuid.lisp, ports.lisp
(deftest bytes
  (is= #x34 (lo #x1234))
  (is= #x12 (hi #x1234))
  (is= #x2345 (word16 #x12345))
  (is= '(#x78 #x56 #x34 #x12) (bytes32 #x12345678))
  (is= #xB8000 (linear #xB800))
  (is= #x46C (linear #x40 #x6C)))

(deftest machine-code                   ; mov eax,42 / ret
  (is= 42 (sb-alien:alien-funcall
           (sb-alien:sap-alien (machine-code #xB8 42 0 0 0 #xC3) (function sb-alien:int)))))

(deftest int86
  (is (>= (lo (int86 #x21 :ax #x3000)) 5))              ; the DOS version
  (is (<= 500 (word16 (int86 #x12)) 640)))              ; KB of conventional memory

(deftest real-int
  (is= (word16 (int86 #x21 :ax #x3000)) (real-int #x21 :ax #x3000)))

(deftest dos-memory
  (let ((segment (dos-alloc 64)))
    (is (< 0 segment #xA000))
    (setf (sb-sys:sap-ref-32 (sb-sys:int-sap (linear segment)) 0) #xCAFEF00D)
    (is= #xCAFEF00D (peek32 (linear segment)))
    (is= #xF00D (peek16 (linear segment)))
    (is= #x0D (peek (linear segment)))
    ;; DOS writes into it, in real mode: the current directory (INT 21h/47h)
    (real-int #x21 :ax #x4700 :dx 0 :ds segment :si 0)
    (is= "SBCL" (coerce (loop for i from 0
                              for byte = (peek (linear segment i))
                              until (zerop byte) collect (code-char byte))
                        'string))))

(deftest bios-data
  (is= 80 (peek16 #x44A))                               ; columns
  (is= 3 (peek #x449)))                                 ; video mode

(deftest cpuid
  (multiple-value-bind (highest b c d) (cpuid 0)
    (is (>= highest 1))
    (is (every (lambda (word)                           ; the vendor: twelve letters
                 (every (lambda (byte) (graphic-char-p (code-char byte))) (bytes32 word)))
               (list b d c)))))

(deftest ports
  (outb #x70 0)                                         ; the clock chip's seconds, in BCD
  (let ((seconds (inb #x71)))
    (is (<= (ldb (byte 4 0) seconds) 9))
    (is (<= (ldb (byte 4 4) seconds) 5)))
  (outl #xCF8 #x80000000)                               ; PCI: the first device's identity
  (let ((identity (inl #xCFC)))
    (outl #xCF8 0)
    (is (/= identity #xFFFFFFFF))
    (is (/= identity 0))))

(deftest break-checking
  (let ((was (break-checking)))
    (unwind-protect
         (progn (setf (break-checking) nil)
                (is (not (break-checking)))
                (setf (break-checking) t)
                (is (break-checking))
                (without-break-checking (is (not (break-checking))))
                (is (break-checking)))
      (setf (break-checking) was))))

;;; ---- interrupts.lisp
(deftest vectors
  (is (plusp (code-selector)))
  (is (/= (code-selector) (data-selector)))
  (multiple-value-bind (selector offset) (get-vector #x60)
    (set-vector #x60 selector offset)
    (is= (list selector offset) (multiple-value-list (get-vector #x60)))))

;;; ---- input.lisp, text.lisp: the keyboard, with keys put into the BIOS's buffer
(defun stuff-key (scan ascii) (int86 #x16 :ax #x0500 :cx (logior (ash scan 8) ascii)))

(deftest ticks
  (let ((start (peek32 #x46C)))
    (wait-tick 3)
    (is (<= 3 (- (peek32 #x46C) start) 4))))

(deftest keyboard
  (is (not (key-pressed-p)))
  (stuff-key #x1E 97)
  (is (key-pressed-p))
  (is= 97 (lo (wait-key)))
  (is (not (key-pressed-p)))
  (stuff-key #x1E 97) (is= #\a (get-key))
  (stuff-key #x48 0) (is= :up (get-key))
  (stuff-key #x3B 0) (is= :f1 (get-key))
  (stuff-key #x85 0) (is= #x85 (get-key)))              ; F11 has no name

(deftest text-input
  (let ((saved (save-text-screen)))
    (unwind-protect
         (progn (stuff-key #x23 104) (stuff-key #x17 105) (stuff-key #x2D 120)
                (stuff-key #x0E 8) (stuff-key #x1C 13)  ; h i x Backspace Enter
                (is= "hi" (text-input 0 0 20))
                (stuff-key #x01 27)
                (is= nil (text-input 0 0 20)))
      (restore-text-screen saved))))

;;; ---- text.lisp
(deftest text-screen
  (let ((saved (save-text-screen))
        (before (peek16 #xB8000)))
    (unwind-protect
         (progn (text-put 0 0 "Hello" #x1F)
                (is= 72 (peek #xB8000))
                (is= #x1F (peek #xB8001))
                (is= 111 (peek #xB8008))
                (text-put 78 0 "abcdef")                ; clipped at the right edge
                (is= 98 (peek (+ #xB8000 (* 2 79))))
                (text-put 0 25 "off the screen")
                (text-fill 2 2 3 2 #x70 #\#)
                (is= #x7023 (peek16 (+ #xB8000 (* 2 (+ 4 (* 80 3))))))
                (text-box 10 5 20 4 :attribute #x4E :title "T")
                (flet ((cell (x y) (peek (+ #xB8000 (* 2 (+ x (* 80 y)))))))
                  (is= 201 (cell 10 5)) (is= 187 (cell 29 5))
                  (is= 200 (cell 10 8)) (is= 188 (cell 29 8))
                  (is= 186 (cell 10 6)) (is= 32 (cell 11 6))
                  (is= 84 (cell 13 5))))
      (restore-text-screen saved))
    (is= before (peek16 #xB8000))))

(deftest text-wrap
  (is= '("aa bb" "cc") (text-wrap "aa bb cc" 5))
  (is= '("abc" "def" "gh") (text-wrap "abcdefgh" 3))
  (is= '("  aa" "  bb") (text-wrap "  aa bb" 4))
  (is= '("a" "b") (text-wrap (format nil "a~%b") 10))
  (is= '("") (text-wrap "" 10))
  (is (every (lambda (line) (<= (length line) 17))
             (text-wrap "The quick brown fox jumps over the lazy dog, repeatedly and at length." 17))))

;;; ---- video.lisp
(deftest mode-13h
  (unwind-protect
       (progn (video-mode #x13)
              (is= #x13 (peek #x449))
              (plot 10 20 5)
              (is= 5 (peek (+ #xA0000 10 (* 320 20))))
              (draw-line 0 0 9 0 7)
              (is (loop for x below 10 always (= 7 (peek (+ #xA0000 x)))))
              (is= 0 (peek (+ #xA0000 10)))
              (draw-line 0 10 9 19 9)                   ; a diagonal
              (is (loop for i below 10 always (= 9 (peek (+ #xA0000 i (* 320 (+ 10 i)))))))
              (set-palette 1 63 0 21)
              (outb #x3C7 1)
              (is= '(63 0 21) (list (inb #x3C9) (inb #x3C9) (inb #x3C9))))
    (video-mode 3))
  (is= 3 (peek #x449)))

;;; ---- critical.lisp
(deftest critical-errors
  (catch-critical-errors)
  (unwind-protect
       (progn (critical-error)                          ; forget any old one
              (is= nil (critical-error))
              ;; no disk in drive A: -- DOS would stop and ask "Abort, Retry, Fail?"
              (ignore-errors (with-open-file (in "A:\\NOSUCH.TXT") (read-line in)))
              (multiple-value-bind (reason code) (critical-error)
                (is (stringp reason))
                (is (integerp code)))
              (is= nil (critical-error)))               ; asking cleared it
    (release-critical-errors))
  (is= nil (critical-error)))

;;; ---- hotkey.lisp.  The BIOS hands every scan code to INT 15h, function
;;; 4Fh, before it queues the key; calling that by hand is a key press as
;;; far as the hot key code can tell.
(deftest hotkeys
  (let ((count 0))
    (is (not (hotkeys-installed-p)))
    (unwind-protect
         (progn (install-hotkey #x58 (lambda () (incf count)))
                (is (hotkeys-installed-p))
                (is (not (hotkey-pending-p #x58)))
                (poll-hotkeys)
                (is= 0 count)
                (real-int #x15 :ax #x4F58)              ; F12 goes down
                (is (hotkey-pending-p #x58))
                (poll-hotkeys)
                (is= 1 count)
                (is (not (hotkey-pending-p #x58)))
                (real-int #x15 :ax #x4F1E)              ; A goes down: not ours
                (poll-hotkeys)
                (is= 1 count)
                (real-int #x15 :ax #x4F58)
                (clear-pending-hotkeys)
                (poll-hotkeys)
                (is= 1 count))
      (remove-hotkey #x58))
    (is (not (hotkeys-installed-p)))
    ;; with the handler gone the keyboard still works
    (stuff-key #x1E 97)
    (is= #\a (get-key))))

;;; ---- the parts of the examples that are plain functions
(deftest calculator
  (flet ((calc (text) (doslib-examples.calc:calculate text)))
    (is= 1025 (calc "2^10 + 1"))
    (is= 3 (calc "0xFF % 7"))
    (is= -4 (calc "-2^2"))
    (is= 1/3 (calc "1/3"))
    (is= 4d0 (calc "sqrt(16)"))
    (is= 14 (calc "2 + 3 * 4"))
    (is= 20 (calc "4 * (2 + 3)"))
    (is= (expt 2 100) (calc "(expt 2 100)"))
    (is (handler-case (progn (calc "2 +") nil) (error () t)))))

(deftest registry
  (is (member "fire" (doslib-examples:example-names) :test #'string=))
  (is (> (length (doslib-examples:example-names)) 50))
  (is (handler-case (progn (doslib-examples:run "no-such-example") nil) (error () t))))

;;; ---- DOS memory given back, and the scratch block
(deftest dos-free
  (let ((first (dos-alloc 256)))
    (dos-free first)
    (is= first (dos-alloc 256))                         ; the same place is free again
    (dos-free first)
    (is (handler-case (progn (dos-free first) nil) (error () t))))   ; not twice
  (let ((seen nil))
    (with-dos-memory (segment 100)
      (setf seen segment)
      (is (< 0 segment #xA000)))
    (is (handler-case (progn (dos-free seen) nil) (error () t)))))

(deftest dos-scratch
  (is= (dos-scratch) (dos-scratch))
  (multiple-value-bind (segment offset) (dos-string "D:\\SBCL")
    (is= (dos-scratch) segment)
    (is= 0 offset)
    (is= '(68 58 92 83 66 67 76 0) (coerce (dos-bytes segment 0 8) 'list))
    ;; DOS reads it, in real mode: change directory to where we are already
    (is (not (logbitp 0 (nth-value 6 (real-int #x21 :ax #x3B00 :ds segment :dx offset)))))))

(deftest more-ports
  (let ((word (inw #x40)))                              ; the timer's counter, 16 bits at once
    (is (<= 0 word #xFFFF)))
  (is= 59 (bcd #x59))
  (is (<= 0 (bcd (cmos 0)) 59))                         ; the clock's seconds
  (is (<= 1 (bcd (cmos 7)) 31))                         ; and the day of the month
  (let ((before (ticks)))
    (wait-tick 2)
    (is (<= 2 (- (ticks) before) 3))))

(deftest video-modes
  (is= 3 (current-video-mode))
  (is= :done (with-video-mode (#x13)
               (is= #x13 (current-video-mode))
               (wait-retrace)
               :done))
  (is= 3 (current-video-mode))
  ;; left by a non-local exit as well
  (catch 'out (with-video-mode (#x13) (throw 'out nil)))
  (is= 3 (current-video-mode))
  (unwind-protect
       (progn (is (vesa-set-mode #x101))                ; 640x480, 256 colours
              (vesa-set-bank 2)
              (setf (sb-sys:sap-ref-8 *vga* 5) 77)
              (vesa-set-bank 0)
              (setf (sb-sys:sap-ref-8 *vga* 5) 11)
              (vesa-set-bank 2)
              (is= 77 (sb-sys:sap-ref-8 *vga* 5)))      ; another part of the picture
    (video-mode 3)))

(deftest speaker
  (unwind-protect
       (progn (speaker-on 440)
              (is= 3 (logand (inb #x61) 3))
              (is= (round +timer-hz+ 440) (speaker-divisor (round +timer-hz+ 440))))
    (speaker-off))
  (is= 0 (logand (inb #x61) 3))
  (speaker-beep 660 1)
  (is= 0 (logand (inb #x61) 3)))

(deftest mouse
  (is (mouse-driver-p))
  (is (member (mouse-init) '(2 3)))
  (mouse-range 0 0 639 199)
  (mouse-move 320 96)
  (multiple-value-bind (x y buttons) (mouse-position)
    (is= 320 x) (is= 96 y) (is= 0 buttons))
  (multiple-value-bind (column row) (mouse-cell)
    (is= 40 column) (is= 12 row))
  (is= 320 (mouse-pixel))                               ; text mode: no halving
  (with-video-mode (#x13)
    (mouse-init)
    (mouse-move 100 50)
    (is= 50 (mouse-pixel)))                             ; 320 across: half the driver's 100
  (mouse-init)
  (is= 0 (mouse-presses 0))
  (is= 0 (mouse-releases 0))
  (multiple-value-bind (dx dy) (mouse-motion)
    (is (integerp dx)) (is (integerp dy)))
  (mouse-show)
  (with-mouse-hidden (text-put 0 0 " "))
  (mouse-hide))

(deftest disks
  (is= 3 (drive-number "d:"))
  (is= 2 (drive-number #\c))
  (is= 3 (current-drive))
  (is= #\D (drive-letter nil))
  (let ((boot (dos-read-sectors "C" 0)))                ; a volume's boot sector
    (is= 512 (length boot))
    (is= #x55 (aref boot 510)) (is= #xAA (aref boot 511))
    (is (member (aref boot 0) '(#xEB #xE9))))           ; it starts with a jump
  (is (member (drive-read-method "C") '(:small :large :fat32)))
  (is= 2048 (length (dos-read-sectors nil 1 :count 4)))
  ;; more than the scratch block holds at once
  (let ((many (dos-read-sectors nil 0 :count 40)))
    (is= (* 40 512) (length many))
    (is (equalp (subseq many 0 512) (dos-read-sectors nil 0))))
  (let ((mbr (bios-read-sectors #x80 0)))               ; the first hard disk's partition table
    (is= #x55 (aref mbr 510)) (is= #xAA (aref mbr 511)))
  (is (typep (handler-case (dos-read-sectors "Q" 0) (disk-error (e) e)) 'disk-error))
  (is (typep (handler-case (bios-read-sectors #x9F 0) (disk-error (e) e)) 'disk-error)))

(deftest pci
  (is (pci-present-p))
  (let ((devices (pci-devices :buses 2)))
    (is (plusp (length devices)))
    (destructuring-bind (bus device function vendor id class) (first devices)
      (is= '(0 0 0) (list bus device function))
      (is= vendor (pci-word 0 0 0 0))
      (is= id (pci-word 0 0 0 2))
      (is= class (ash (pci-read 0 0 0 8) -8))
      (is= (ldb (byte 8 16) class) (pci-byte 0 0 0 #x0B)))
    (is (find #x0300 devices :key (lambda (d) (ash (sixth d) -8))))))   ; a VGA card

(deftest serial-ports
  (is= #x3F8 (serial-port 1))
  (is= #x3F8 (serial-open :port #x3F8 :baud 9600))
  (is= #x03 (logand (inb (+ #x3F8 3)) #x7F))            ; 8 data bits, no parity, 1 stop
  (is= nil (serial-read-byte #x3F8 nil))                ; nothing has arrived
  (is (not (serial-byte-ready-p #x3F8))))

(deftest critical-errors-nest
  (catch-critical-errors)
  (catch-critical-errors)
  (release-critical-errors)                             ; the inner one: still caught
  (ignore-errors (with-open-file (in "A:\\NOSUCH.TXT") (read-line in)))
  (is (stringp (critical-error)))
  (release-critical-errors)
  (is= nil (critical-error))
  (is= :value (with-critical-errors-caught
                (ignore-errors (with-open-file (in "A:\\NOSUCH.TXT") (read-line in)))
                (is (stringp (critical-error)))
                :value)))

(deftest interrupts
  (is (interrupts-supported-p))
  (is (interrupt-chord))
  (setf (interrupt-chord) nil)
  (is (not (interrupt-chord)))
  (setf (interrupt-chord) t)
  (is (interrupt-chord))
  (is= '(1 2) (multiple-value-list (catching-interrupt (values 1 2))))
  (is= :interrupted (catching-interrupt (signal 'sb-sys:interactive-interrupt)))
  (is (subtypep 'interrupt 'condition))
  (is (integerp (interrupt-count))))

(deftest dma
  (is= #x12340 (dma-fit #x12340 4096))                  ; no boundary in the way
  (is= #x20000 (dma-fit #x1F800 4096))                  ; one: start after it
  (is= #x1F000 (dma-fit #x1F000 4096))                  ; ending exactly on it is fine
  (with-dma-memory (address 2048)
    (is (< 0 address #xA0000))
    (is (<= (+ (logand address #xFFFF) 2048) #x10000))
    ;; channel 3 has no device here: what is set can be read back, and nothing moves
    (unwind-protect
         (progn (dma-start 3 address 2048 :direction :to-memory :repeat nil)
                (is= (logand address #xFFFF) (dma-address 3))
                (is= 2047 (dma-count 3)))
      (dma-stop 3))))
