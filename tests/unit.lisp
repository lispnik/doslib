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
