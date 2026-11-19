;;;; A printer that is not ready, as restarts.
;;;;
;;;; The companion of examples/lisp/restarts.lisp, with the other device
;;;; that made DOS say "Abort, Retry, Fail?": the printer.  A short report
;;;; is printed on LPT1.  The code that prints first asks the BIOS how the
;;;; printer is (INT 17h, function 2) and, if it is off line, out of paper
;;;; or not there at all, signals PRINTER-NOT-READY -- offering to try
;;;; again, to print into a file instead, or to skip the job.  Should the
;;;; printer fail later, while DOS is writing to it, DOS's critical error
;;;; is caught (see src/critical.lisp) and becomes the same condition with
;;;; the same restarts.
;;;;
;;;; Part 1 prints the report and asks you what to do if it cannot.
;;;; Part 2 prints it again under a policy: two more tries, then a file.
;;;;
;;;;   (doslib-examples:run "printer")
;;;;   (doslib-examples:run "printer" :printer 1)            ; LPT2
;;;;   (doslib-examples:run "printer" :file "REPORT.TXT")    ; the file the policy uses
;;;;
;;;; To try it in QEMU: "-parallel none" is a machine with no printer port,
;;;; "-parallel file:lpt.txt" one whose printer is always ready, and whose
;;;; paper is that file.  On a machine with one port, :printer 1 is a
;;;; printer that is not there.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.printer
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.printer)

(define-condition print-error (error)
  ((device :initarg :device :reader device)
   (reason :initarg :reason :reader reason)))
(define-condition printer-not-ready (print-error) ()
  (:report (lambda (c s) (format s "The printer ~A is not ready: ~A." (device c) (reason c)))))
(define-condition file-not-writable (print-error) ()
  (:report (lambda (c s) (format s "Cannot print to the file ~A: ~A." (device c) (reason c)))))

;;; ---- the printer, as the BIOS sees it
(defun device-name (printer) (format nil "LPT~D" (1+ printer)))

(defun printer-status (printer)
  "The status byte of PRINTER (0 is LPT1), or NIL if there is no such port."
  ;; The BIOS keeps the ports it found at 0040:0008.  Asked about a port
  ;; that is not there it answers nothing at all -- AH comes back as it
  ;; went in -- so the table is looked at first.
  (if (zerop (peek16 (+ #x408 (* 2 printer))))
      nil
      ;; A printer is busy for a moment after every character it is sent,
      ;; so "busy" and nothing else is believed only once it has lasted a second.
      (loop repeat 19
            for status = (hi (int86 #x17 :ax #x0200 :dx printer))
            while (= (logand status #xB9) #x10)
            do (wait-tick)
            finally (return status))))

(defun status-text (status)
  "The bits of a printer status byte, in words."
  (let ((set (loop for name in '("not busy" "acknowledge" "out of paper" "selected" "I/O error"
                                 nil nil "timeout")
                   for bit downfrom 7
                   when (and name (logbitp bit status)) collect name)))
    (format nil "~2,'0Xh: ~:[no bits set~;~:*~{~A~^, ~}~]" status set)))

(defun status-problem (status)
  "Why a printer with this status byte cannot print, or NIL if it can."
  (cond ((null status) "there is no such printer port")
        ((logbitp 0 status) "it does not answer (timeout)")
        ((logbitp 5 status) "it is out of paper")
        ((logbitp 3 status) "it reports an I/O error")
        ((not (logbitp 4 status)) "it is off line")
        ((not (logbitp 7 status)) "it is busy")))

;;; ---- the low level: it knows what can be done, not what should be
(defun report-bytes (lines &optional form-feed)
  "LINES as the bytes to send: each ends with CR LF; FORM-FEED ejects the page."
  (let ((text (format nil "~{~A~C~C~}~@[~C~]" (loop for line in lines
                                                    append (list line #\Return #\Newline))
                      (and form-feed #\Page))))
    (map '(vector (unsigned-byte 8)) (lambda (c) (logand (char-code c) #xFF)) text)))

(defun write-to-printer (lines printer)
  "Send LINES to PRINTER through DOS.  A printer that fails on the way is a
critical error to DOS; here it is signalled as PRINTER-NOT-READY."
  (let* ((name (device-name printer))
         (bytes (report-bytes lines t))
         (fd (sb-unix:unix-open name sb-unix:o_wronly 0)))
    (unless fd
      (error 'printer-not-ready :device name :reason "DOS has no such device"))
    (critical-error)                    ; forget any old one
    (let* ((written (unwind-protect (sb-unix:unix-write fd bytes 0 (length bytes))
                      (sb-unix:unix-close fd)))
           (reason (critical-error)))
      (cond (reason (error 'printer-not-ready :device name :reason reason))
            ((not (eql written (length bytes)))
             (error 'printer-not-ready :device name
                                       :reason (format nil "only ~D of ~D bytes were taken"
                                                       (or written 0) (length bytes))))))))

(defun write-to-file (lines file)
  (handler-case
      (with-open-file (out file :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
        (write-sequence (report-bytes lines) out))
    (error ()
      (error 'file-not-writable :device file :reason "it cannot be written"))))

(defparameter *default-file* "PRINTOUT.TXT")

(defun send-report (lines printer)
  "Print LINES on PRINTER.  Returns where they went: the device's name or a
file's.  If the printer will not print, signals an error with restarts to
try again or to print to a file."
  (let ((file nil))                     ; set once somebody has chosen a file
    (loop
      (restart-case
          (cond (file (write-to-file lines file)
                      (return file))
                (t (let ((problem (status-problem (printer-status printer))))
                     (when problem
                       (error 'printer-not-ready :device (device-name printer) :reason problem)))
                   (write-to-printer lines printer)
                   (return (device-name printer))))
        (retry ()
          :report (lambda (s)
                    (format s (if file "Try the file again." "Try again (when the printer is ready)."))))
        (print-to-file (name)
          :report "Print to a file instead."
          :interactive (lambda ()
                         (let ((name (string-trim " " (dialog-ask (format nil "File (Enter for ~A):"
                                                                          *default-file*)))))
                           (list (if (string= name "") *default-file* name))))
          (setf file name))))))

(defun print-report (lines printer)
  "Where the report went, or NIL if it was skipped."
  (restart-case (send-report lines printer)
    (skip ()
      :report "Skip printing."
      nil)))

;;; ---- the report: a few lines about this machine
(defun report-lines ()
  (multiple-value-bind (ax bx cx dx) (int86 #x21 :ax #x2A00)        ; DOS: the date
    (declare (ignore ax bx))
    (multiple-value-bind (ax2 bx2 cx2) (int86 #x21 :ax #x2C00)      ; and the time
      (declare (ignore ax2 bx2))
      (list "DOSLIB PRINTER REPORT"
            "====================="
            ""
            (format nil "Printed on ~4,'0D-~2,'0D-~2,'0D at ~2,'0D:~2,'0D."
                    (word16 cx) (hi dx) (lo dx) (hi cx2) (lo cx2))
            (format nil "Lisp:   ~A ~A" (lisp-implementation-type) (lisp-implementation-version))
            (format nil "Memory: ~D KB of conventional memory, by the BIOS's count."
                    (word16 (int86 #x12)))
            (format nil "Heap:   ~:D bytes of Lisp objects." (sb-kernel:dynamic-usage))
            ""
            "A printer that is not ready is not the end of the program:"
            "the code that prints offers RETRY, PRINT-TO-FILE and SKIP,"
            "and the code that asked for the printing chooses."))))

;;; ---- the screen: a log down the page
(defvar *row* 0)
(defun note (attribute control &rest arguments)
  (dolist (line (text-wrap (apply #'format nil control arguments) 76))
    (when (< *row* 24) (text-put 2 *row* line attribute))
    (incf *row*)))

(defun print-and-tell (lines printer)
  (let ((status (printer-status printer)))
    (note 7 "Printing ~D lines on ~A (~:[no such port~;~:*BIOS status ~A~]) ..."
          (length lines) (device-name printer) (and status (status-text status))))
  (let ((where (print-report lines printer)))
    (cond ((null where) (note 12 "    (skipped)"))
          ((string= where (device-name printer)) (note 10 "    printed on ~A." where))
          (t (note 10 "    printed to the file ~A." where)))))

(defun main (&key (printer 0) (file *default-file*))
  "A printer that is not ready, as restarts.  PRINTER: 0 for LPT1, 1 for
LPT2...  FILE: where the policy of part 2 prints when the printer will not."
  (catch-critical-errors)
  (unwind-protect
       (let ((lines (report-lines)))
         (text-fill 0 0 80 25 7)
         (hide-text-cursor)
         (setf *row* 0)
         (note 15 "Part 1: you decide.  If the printer is not ready, choose a restart.")
         (handler-bind ((print-error (lambda (c) (restart-dialog c :title "Lisp asks" :through 'skip))))
           (print-and-tell lines printer))
         (incf *row*)
         (note 15 "Part 2: a policy decides.  The same code, and nobody is asked:")
         (note 7 "a printer that is not ready is given two more tries, then the report goes ~
                  to the file ~A; if that cannot be written either, it is skipped." file)
         (let ((tries 0))
           (handler-bind ((printer-not-ready
                            (lambda (c)
                              (cond ((< (incf tries) 3)
                                     (note 14 "    policy: ~A  Trying again." c)
                                     (wait-tick 9)      ; give it half a second
                                     (invoke-restart 'retry))
                                    (t (note 14 "    policy: ~A  Printing to ~A." c file)
                                       (invoke-restart 'print-to-file file)))))
                          (file-not-writable
                            (lambda (c)
                              (note 14 "    policy: ~A  Skipping." c)
                              (invoke-restart 'skip))))
             (print-and-tell lines printer)))
         (incf *row*)
         (note 15 "Press a key.")
         (get-key))
    (release-critical-errors)
    (text-fill 0 0 80 25 7)
    (text-cursor 0 0)
    (show-text-cursor))
  (values))

(doslib-examples:define-example "printer" 'main
  :category :system
  :description "A printer that is not ready, as restarts.")
