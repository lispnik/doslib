;;;; Commands that tie the editor to the rest: run an example, load a Quicklisp system, the calculator on F12.
;;;;
;;;;   M-x run-example   (C-c x)   run one of the DOSLIB examples (TAB completes its name, an
;;;;                               empty or unknown name lists them); the editor's screen comes
;;;;                               back when the example ends, and an error in it is a message
;;;;   M-x quickload     (C-c q)   load a system with Quicklisp (TAB completes over the systems
;;;;                               Quicklisp knows); what is printed goes to the buffer *quickload*
;;;;   F12                         the pop-up calculator of the "calc" example, while editing
;;;;                               and inside an example run from the editor;
;;;;                               M-x calculator-hotkey takes it off the key, or puts it back
;;;;   M-x restart-dialog-mode     errors in evaluated code are offered in DOSLIB's restart
;;;;                               dialog, a box over the text, instead of the *debugger* buffer
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

;;; ---- things the commands share

(defvar *calculator-enabled* t "Should the editor have the pop-up calculator on F12?")
(defvar *calculator-session* -1 "The DOSLIB session the calculator was last installed in.")

(defun commands-append (b text)
  "Add TEXT at the end of buffer B and leave point after it, in every window that shows B."
  (let ((last (1- (nlines b))) (*inhibit-read-only* t))
    (insert-at last (length (line-text last b)) text :b b :undo nil))
  (setf (buf-line b) (1- (nlines b)) (buf-col b) (length (line-text (1- (nlines b)) b))
        (buf-modified b) nil)
  (dolist (w *windows*) (when (eq (win-buf w) b) (setf (win-point w) nil))))

(defun commands-screen-blank-p ()
  "Is there nothing on the text screen?"
  (loop for i from 0 below 4000 by 2
        always (member (sap-ref-8 *screen* i) '(0 32))))

;;; ---- M-x run-example

(defun run-example-program (name function)
  "Give the screen to FUNCTION, then take it back.  An error in it is a message."
  (let ((failure nil))
    (text-mode)                      ; a clean screen, the cursor at the top
    (unwind-protect
         (progn
           (handler-case (funcall function)
             (error (e) (setf failure e)))
           (ignore-errors (finish-output))
           ;; An example that printed its results has nothing to wait for and
           ;; returns at once: keep them on the screen until a key is pressed.
           ;; (One that drew pictures has waited for its own key and left a
           ;; blank text screen behind.)
           (when (and (= (peek #x449) 3) (not (commands-screen-blank-p)))
             (put-string 0 24 (format nil " ~A ~:[finished~;stopped with an error~].  ~
                                           Press a key to return to the editor."
                                      name failure)
                         #x70 80)
             (wait-key)))
      ;; Whatever it did to the mode, the palette, the font or the cursor,
      ;; setting the mode again undoes; the command loop then draws everything.
      (text-mode)
      (setf *calculator-session* -1))   ; an example may have taken F12: see below
    (if failure
        (message "~A stopped with an error: ~A" name failure)
        (message "~A finished" name))))

(define-command run-example ()
  "Run one of the DOSLIB examples and come back to the editor."
  (let* ((names (doslib-examples:example-names))
         (typed (string-trim " " (read-string "Run example: " :complete (complete-from names))))
         (starts (remove-if-not (lambda (name) (and (>= (length name) (length typed))
                                                    (string-equal typed name :end2 (length typed))))
                                names))
         (name (cond ((find typed names :test #'string-equal))
                     ((and (string/= typed "") (null (rest starts))) (first starts)))))   ; enough to tell
    (cond (name (run-example-program name (lambda () (doslib-examples:run name))))
          (t (show-in-help "*examples*"
                           (with-output-to-string (out)
                             (format out "The DOSLIB examples.  M-x run-example (C-c x) runs one by name.~%")
                             (doslib-examples:list-examples out)))
             (if (string= typed "")
                 (message "Which example?  These are the ones there are.")
                 (message "There is no example named ~A.  These are the ones there are." typed))))))

;;; ---- the calculator on F12
;;;
;;; F12 is a DOSLIB hot key (INSTALL-HOTKEY), as in the "calc" example: the
;;; editor's key wait polls it, and so do the examples, so the calculator
;;; also comes up over one started with M-x run-example.  The hot key is a
;;; handler in DOS memory that has to be put in place in the process that is
;;; running, not when this file is loaded (the editor may be started from a
;;; saved core, or as EMACS.EXE).  So it is an idle hook that installs it,
;;; the first time the editor waits for a key in each process -- and again
;;; after an example has run, since one may have used F12 itself.  DOSLIB
;;; takes the handler out when Lisp exits.

(defun calculator-idle ()
  (when (and *calculator-enabled* (/= *calculator-session* *session*))
    (doslib-examples.calc:install)
    (setf *calculator-session* *session*))
  nil)                                  ; no buffer changed
(when *calculator-enabled* (add-idle-hook 'calculator-idle))

(define-command calculator-hotkey ()
  "Put the pop-up calculator on F12, or take it off again."
  (cond (*calculator-enabled*
         (setf *calculator-enabled* nil *calculator-session* -1)
         (remove-idle-hook 'calculator-idle)
         (doslib-examples.calc:uninstall)
         (message "F12 is an ordinary key again"))
        (t (setf *calculator-enabled* t)
           (add-idle-hook 'calculator-idle)
           (calculator-idle)
           (message "F12 is the calculator (Esc or F12 closes it)"))))

;;; ---- M-x quickload
;;;
;;; Quicklisp is not something this file can depend on: it is looked for
;;; when the command runs, and its functions are found by name.

(defvar *quickload-systems* nil "The names of the systems Quicklisp can load, once asked for.")

(defun quickload-call (package name &rest arguments)
  (let ((symbol (and (find-package package) (find-symbol name package))))
    (unless (and symbol (fboundp symbol)) (editor-error "This Quicklisp has no ~A:~A" package name))
    (apply symbol arguments)))

(defun quickload-setup-file ()
  "Quicklisp's setup.lisp, if it is installed: in HOME, where QL.BAT puts it."
  (dolist (home (list (environment-variable "HOME") (ignore-errors (namestring (user-homedir-pathname))) "D:/HOME"))
    (when (and home (string/= home ""))
      (let ((file (concatenate 'string (string-right-trim "/\\" home) "/quicklisp/setup.lisp")))
        (when (ignore-errors (probe-file file)) (return file))))))

(defun quickload-ensure ()
  "Load Quicklisp if it is not loaded yet; an error if it is not installed."
  (unless (find-package "QUICKLISP-CLIENT")
    (let ((setup (quickload-setup-file)))
      (unless setup
        (editor-error "Quicklisp is not installed: leave the editor and type QL at the DOS prompt"))
      (draw-echo (format nil "Loading Quicklisp from ~A ..." setup))
      (let* ((out (make-string-output-stream))
             (*standard-output* out) (*error-output* out))
        (handler-case (load setup)
          (error (e) (editor-error "Quicklisp did not load: ~A" e))))
      (unless (find-package "QUICKLISP-CLIENT")
        (editor-error "~A was loaded, but there is still no Quicklisp" setup)))))

(defun quickload-system-names ()
  (or *quickload-systems*
      (progn
        (draw-echo "Reading Quicklisp's list of systems...")    ; thousands of them: it takes a while
        (setf *quickload-systems*
              (sort (remove-duplicates
                     (append (ignore-errors (quickload-call "QUICKLISP-CLIENT" "LIST-LOCAL-SYSTEMS"))
                             (mapcar (lambda (system) (quickload-call "QL-DIST" "NAME" system))
                                     (quickload-call "QL-DIST" "PROVIDED-SYSTEMS" t)))
                     :test #'string=)
                    #'string<)))))

(defun quickload-complete (text)
  "Completion over the systems; the list is only read when TAB is first pressed."
  (multiple-value-bind (new matches)
      (handler-case (funcall (complete-from (quickload-system-names)) text)
        (error () (values text nil)))   ; no list (no dist installed?): the name can still be typed
    ;; hundreds may match: show only as many as leave the prompt on the screen
    (let ((shown '()) (room (- 60 (length new))))
      (loop for match in matches
            do (decf room (1+ (length match)))
               (when (minusp room) (push "..." shown) (return))
               (push match shown))
      (values new (if (rest matches) (nreverse shown) matches)))))

;;; What Quicklisp and the compiler print goes into the buffer as it comes,
;;; so that a long load can be watched.
(defclass quickload-stream (#+sbcl sb-gray:fundamental-character-output-stream #+clisp gray:fundamental-character-output-stream)
  ((buffer :initarg :buffer :reader quickload-stream-buffer)
   (pending :initform (make-string-output-stream) :reader quickload-stream-pending)
   (column :initform 0 :accessor quickload-stream-column)
   (drawn :initform -1 :accessor quickload-stream-drawn)))     ; the timer tick of the last redraw
(defun quickload-stream-flush (stream)
  (let ((text (get-output-stream-string (quickload-stream-pending stream))))
    (when (string/= text "")
      (commands-append (quickload-stream-buffer stream) text)
      (let ((tick (peek32 #x46C)))      ; drawing is slow: not more than once a tick
        (unless (= tick (quickload-stream-drawn stream))
          (setf (quickload-stream-drawn stream) tick)
          (ignore-errors (redisplay)))))))
(defmethod #+sbcl sb-gray:stream-write-char #+clisp gray:stream-write-char ((stream quickload-stream) char)
  (write-char char (quickload-stream-pending stream))
  (cond ((char= char #\Newline) (setf (quickload-stream-column stream) 0) (quickload-stream-flush stream))
        (t (incf (quickload-stream-column stream))))
  char)
(defmethod #+sbcl sb-gray:stream-line-column #+clisp gray:stream-line-column ((stream quickload-stream)) (quickload-stream-column stream))
(defmethod #+sbcl sb-gray:stream-force-output #+clisp gray:stream-force-output ((stream quickload-stream)) (quickload-stream-flush stream) nil)
(defmethod #+sbcl sb-gray:stream-finish-output #+clisp gray:stream-finish-output ((stream quickload-stream)) (quickload-stream-flush stream) nil)

(define-command quickload ()
  "Load a system with Quicklisp, downloading it first if need be."
  (quickload-ensure)
  (let ((name (string-downcase
               (string-trim " " (read-string "Quickload system: " :complete 'quickload-complete)))))
    (when (string= name "") (editor-error "No system was named"))
    (let* ((b (or (find-buf "*quickload*") (make-buf "*quickload*")))
           (stream (make-instance 'quickload-stream :buffer b))
           (failure nil)
           (start (get-universal-time)))
      (select-buffer b)
      (commands-append b (format nil "~:[~%~;~]; (ql:quickload ~S)~%"
                                 (string= (line-text (1- (nlines b)) b) "") name))
      (message "Loading ~A with Quicklisp..." name)
      (redisplay)
      (handler-case
          (let ((*standard-output* stream) (*error-output* stream) (*trace-output* stream)
                (*query-io* *minibuffer-stream*) (*standard-input* *minibuffer-stream*))
            (quickload-call "QUICKLISP-CLIENT" "QUICKLOAD" name))
        (error (e) (setf failure e)))
      (quickload-stream-flush stream)
      (unless (string= (line-text (1- (nlines b)) b) "") (commands-append b (string #\Newline)))
      (let ((seconds (- (get-universal-time) start)))
        (cond (failure
               (commands-append b (format nil "; ~A was NOT loaded: ~A~%" name
                                          (one-line (princ-to-string failure))))
               (message "Quickload of ~A failed: ~A" name failure))
              (t (commands-append b (format nil "; ~A is loaded (~D second~:P)~%" name seconds))
                 (message "~A is loaded (~D second~:P)" name seconds)))))))

;;; ---- M-x restart-dialog-mode
;;;
;;; An error in code the editor evaluates (C-x C-e, the REPL, M-:) goes to
;;; the function EDITOR-DEBUGGER with the stack still in place.  That
;;; function is looked up by name each time, so it can be wrapped: with the
;;; mode on, the wrapper shows the condition and the same restarts in
;;; DOSLIB's RESTART-DIALOG, where E also evaluates a form before choosing;
;;; with it off, the editor's own debugger buffer is used as before.
;;; (Errors in the editor's own commands are caught by its command loop
;;; after the stack has unwound, restarts and all: they stay messages.)

(defvar *restart-dialog-mode* nil "True: evaluation errors are offered in a dialog box.")
(defvar *restart-dialog-original* nil "The editor's own debugger function.")
(defvar *restart-dialog-wrapper* nil "The function this file put in its place.")

(defun restart-dialog-debugger (condition &optional hook)
  (cond ((not *restart-dialog-mode*) (funcall *restart-dialog-original* condition hook))
        ((or (typep condition 'quit-command) (null *editor-restart*)) nil)
        (t (setf *debugged* condition)
           (ignore-errors (redisplay))  ; text mode, and the editor under the box
           (let ((all (compute-restarts condition)))
             (restart-dialog condition
                             ;; ours and those inside it, as in the debugger buffer
                             :restarts (ldiff all (rest (member *editor-restart* all)))
                             :title (format nil " ~A " (type-of condition))
                             :eval-package *repl-package*)))))

;;; (loading this file again must not take the wrapper for the original)
(unless (and *restart-dialog-wrapper* (eq (fdefinition 'editor-debugger) *restart-dialog-wrapper*))
  (setf *restart-dialog-original* (fdefinition 'editor-debugger)))
(setf *restart-dialog-wrapper* #'restart-dialog-debugger
      (fdefinition 'editor-debugger) *restart-dialog-wrapper*)

(define-command restart-dialog-mode ()
  "Offer errors in evaluated code in a dialog box, or in the debugger buffer again."
  (setf *restart-dialog-mode* (not *restart-dialog-mode*))
  (message "Errors in evaluated code now open ~:[the debugger buffer~;a restart dialog~]"
           *restart-dialog-mode*))

;;; ---- keys (C-c x and C-c q are free in the editor's own bindings)
(bind-key "C-c x" 'run-example)
(bind-key "C-c q" 'quickload)
