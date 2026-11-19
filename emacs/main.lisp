;;;; The command loop
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun read-prefix ()
  "Read a numeric prefix argument, if one is being typed: C-u (4), C-u C-u
(16), C-u 12, M-1 M-2.  Returns the number or NIL; the key after it is left
to be read again."
  (let ((key (read-key)) (n nil) (explicit nil))
    (flet ((digit (k)
             (cond ((= (length k) 1) (digit-char-p (char k 0)))
                   ((and (= (length k) 3) (string= "M-" k :end2 2)) (digit-char-p (char k 2))))))
      (cond ((equal key "C-u") (setf n 4))
            ((and (= (length key) 3) (digit key)) (setf n (digit key) explicit t))
            (t (push key *unread*) (return-from read-prefix nil)))
      (loop
        (draw-echo (format nil "C-u ~D-" n))
        (let ((k (read-key)))
          (cond ((and (equal k "C-u") (not explicit)) (setf n (* n 4)))
                ((digit k) (setf n (if explicit (+ (* n 10) (digit k)) (digit k)) explicit t))
                (t (push k *unread*) (return n))))))))
(defun run-command (command keys)
  "Run COMMAND -- as many times as the prefix argument says, unless it is a
command that takes the argument itself (its TAKES-PREFIX property)."
  (let ((b *buf*))
    (unless (or (eq (first (buf-undo b)) :boundary)
                (and (null command) (null *last-command*)))    ; typing: undo a run at a time
      (push :boundary (buf-undo b))))
  (setf *this-command* command)
  (handler-case
      (dotimes (i (if (and *prefix-argument* (not (and (symbolp command) (get command 'takes-prefix))))
                      *prefix-argument*
                      1))
        (cond (command (funcall command))
              ((not (or (= (length keys) 1) (equal keys "SPC")))
               (message "~A is not bound to a command" keys) (return))
              ((buf-read-only *buf*) (message "~A is read-only" (buf-name *buf*)) (return))
              (t (self-insert keys))))
    (quit-command ()
      (setf (buf-mark-active *buf*) nil *unread* '() *defining-macro* nil)
      (message "Quit"))
    (error (e) (setf *unread* '()) (message "~A" e)))   ; an error also stops a macro
  (setf *last-command* *this-command*))
(defun emacs (&optional file)
  "Run the editor until C-x C-c."
  (unless (find-buf "*scratch*")
    (let ((b (make-buf "*scratch*")))
      (setf (buf-mode b) 'lisp-mode)
      (insert-at 0 0 (format nil ";; Lisp goes here.  C-x C-e evaluates the expression before the cursor,~%~
;; C-c C-z opens the REPL, F1 lists the keys, C-x C-c leaves.~%~%") :b b)
      (setf (buf-line b) 3 (buf-undo b) nil (buf-modified b) nil)))
  (repl-buffer)
  (setf *window* (make-win :buf (find-buf "*scratch*")) *windows* (list *window*) *buf* (win-buf *window*)
        *message* nil *unread* '() *defining-macro* nil)
  (layout-windows)
  (reset-display)
  (video-mode 3)
  (handler-case (when (probe-file "emacsrc.lsp")
                  (let ((*package* *repl-package*)) (load "emacsrc.lsp")))
    (error (e) (message "Error in EMACSRC.LSP: ~A" e)))
  (when file (visit (namestring file)))
  (catch 'exit-editor
    (loop
      (handler-case (redisplay) (error (e) (draw-echo (format nil "Redisplay: ~A" e))))
      (setf *macro-mark* (length *macro-keys*))
      (let ((*prefix-argument* (let ((*idle* t)) (read-prefix))))
        (multiple-value-bind (command keys) (let ((*idle* t)) (read-command))
          (setf *message* nil)
          (run-command command keys)))))
  (when *leave-display* (funcall *leave-display*))
  (video-mode 3)
  (values))
(push (lambda (file) (emacs file) t) sb-ext:*ed-functions*)     ; (ed "file") comes here
(defun build-emacs (&optional (name "emacs.exe"))
  "Save the editor as a stand-alone program: EMACS [file]."
  (setf *buffers* '() *windows* '() *kill-ring* '())     ; start clean
  (save-lisp-and-die name :executable t
                          :toplevel (lambda ()
                                      (doslib-init)
                                      (emacs (second sb-ext:*posix-argv*))
                                      (sb-ext:exit))))
