;;;; "Abort, Retry, Fail?" -- and what Common Lisp does instead.
;;;;
;;;; When a drive is not ready DOS stops the world and offers three or four
;;;; fixed answers, to the user, whatever the program was doing.  In Common
;;;; Lisp the code that hits the problem says what the ways forward are
;;;; (restarts), and code further up decides which to take -- by asking the
;;;; user, or by policy, without anyone being asked.  The low-level code is
;;;; the same in both cases, and the stack is not unwound until the choice
;;;; has been made, so "retry" really does carry on from where it was.
;;;;
;;;; Part 1 reads three files and asks you what to do about each failure.
;;;; Part 2 reads them again under a policy written as three handlers.
;;;; To see a drive that is not ready, run it with no disk in drive A:.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.restarts
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.restarts)

(define-condition device-not-ready (file-error)
  ((reason :initarg :reason :reader reason))
  (:report (lambda (c s) (format s "Cannot read ~A: ~A." (file-error-pathname c) (reason c)))))
(define-condition file-missing (file-error) ()
  (:report (lambda (c s) (format s "There is no file ~A." (file-error-pathname c)))))

;;; ---- the low level: it knows what can be done, not what should be
(defun open-file (path)
  "Open PATH for reading.  On failure, signals an error with restarts to
try again or to use another file."
  (loop
    (restart-case
        (progn
          (critical-error)              ; forget any old one
          (let ((fd (sb-unix:unix-open path sb-unix:o_rdonly 0)))
            (when fd
              (return (make-fd-stream fd :input t :buffering :full :external-format :latin-1)))
            (let ((reason (critical-error)))
              (if reason
                  (error 'device-not-ready :pathname path :reason reason)
                  (error 'file-missing :pathname path)))))
      (retry ()
        :report "Try again (when the drive is ready).")
      (use-file (other)
        :report "Read a different file instead."
        :interactive (lambda () (list (dialog-ask "File:")))
        (setf path other)))))

(defun first-lines (path &optional (count 2))
  "The first COUNT lines of the file, or NIL if it was skipped."
  (restart-case
      (with-open-stream (in (open-file path))
        (loop repeat count
              for line = (read-line in nil)
              while line collect (string-right-trim '(#\Return) line)))
    (skip-file ()
      :report "Skip this file."
      nil)))

;;; ---- the screen: a log down the page
(defvar *row* 0)
(defun note (attribute control &rest arguments)
  (dolist (line (text-wrap (apply #'format nil control arguments) 76))
    (when (< *row* 24) (text-put 2 *row* line attribute))
    (incf *row*)))
(defun read-all (files)
  (dolist (file files)
    (note 7 "Reading ~A ..." file)
    (let ((lines (first-lines file)))
      (if lines
          (dolist (line lines) (note 10 "    ~A" (subseq line 0 (min 66 (length line)))))
          (note 12 "    (skipped)")))))

(defun main (&optional (files '("A:\\README.TXT" "D:\\SBCL\\NOSUCH.TXT" "D:\\SBCL\\README.TXT")))
  "DOS errors as restarts: Abort, Retry, Fail? done properly."
  (catch-critical-errors)
  (unwind-protect
       (progn
         (text-fill 0 0 80 25 7)
         (hide-text-cursor)
         (setf *row* 0)
         (note 15 "Part 1: you decide.  Each failure offers its restarts.")
         (restart-case
             (handler-bind ((file-error (lambda (c) (restart-dialog c :title "Lisp asks" :through 'give-up))))
               (read-all files))
           (give-up ()
             :report "Give up on the rest of the files."
             (note 12 "    (gave up)")))
         (incf *row*)
         (note 15 "Part 2: a policy decides.  The same code, and nobody is asked:")
         (note 7 "a file that is missing is skipped; a drive that is not ready is given two more tries and then skipped.")
         (let ((tries (make-hash-table :test 'equal)))
           (handler-bind ((file-missing (lambda (c)
                                          (note 14 "    policy: ~A  Skipping." c)
                                          (invoke-restart 'skip-file)))
                          (device-not-ready (lambda (c)
                                              (let ((path (file-error-pathname c)))
                                                (cond ((< (incf (gethash path tries 0)) 3)
                                                       (note 14 "    policy: ~A  Trying again." c)
                                                       (invoke-restart 'retry))
                                                      (t (note 14 "    policy: ~A  Skipping." c)
                                                         (invoke-restart 'skip-file)))))))
             (read-all files)))
         (incf *row*)
         (note 15 "Press a key.")
         (get-key))
    (release-critical-errors)
    (text-fill 0 0 80 25 7)
    (text-cursor 0 0)
    (show-text-cursor)))

(doslib-examples:define-example "restarts" 'main
  :category :lisp
  :description "DOS errors as restarts: Abort, Retry, Fail? done properly.")
