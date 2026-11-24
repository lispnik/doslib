;;;; Keymaps
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defvar *global-map* (make-hash-table :test 'equal))
(defvar *modes* '() "The modes that have a keymap, for the help commands.")
(defun mode-map (mode)
  (or (get mode 'keymap)
      (progn (pushnew mode *modes*)
             (setf (get mode 'keymap) (make-hash-table :test 'equal)))))
(defun split-keys (string)
  (loop with start = 0
        for space = (position #\Space string :start start)
        collect (subseq string start space)
        while space do (setf start (1+ space))))
(defun bind-key (keys command &optional (map *global-map*))
  "Make the key sequence KEYS (\"C-x C-f\", \"M-q\", \"<f5>\") run COMMAND.
MAP may be a keymap or the name of a mode, as in (bind-key \"C-c t\" 'foo 'lisp-mode)."
  (when (symbolp map) (setf map (mode-map map)))
  (let ((keys (split-keys keys)))
    (loop for key in (butlast keys)
          do (setf map (let ((next (gethash key map)))
                         (if (hash-table-p next)
                             next
                             (setf (gethash key map) (make-hash-table :test 'equal))))))
    (setf (gethash (car (last keys)) map) command)))
;;; ---- major modes
;;; A buffer has one major mode, which says what kind of thing it holds.  A
;;; mode is a name with properties (see buffers.lisp); DEFINE-MAJOR-MODE
;;; sets them, and may derive the mode from another, whose keys, colours
;;; and hooks it then has as well as its own.  SET-MAJOR-MODE puts a buffer
;;; in a mode, and that is when the mode's own code and its hooks run.
(defvar *major-modes* '() "The major modes made with DEFINE-MAJOR-MODE, in the order they were defined.")
(defvar *auto-modes* '()
  "Which major mode a file gets from its name: an alist of (extension . mode),
the extension without its dot.  A file with none of them is in FUNDAMENTAL-MODE.")
(defun mode-for-file (name)
  (or (cdr (assoc (or (pathname-type name) "") *auto-modes* :test #'string-equal))
      'fundamental-mode))
(defun set-major-mode (mode &optional (b *buf*))
  "Put buffer B in the major mode MODE.  As in GNU Emacs that is a fresh
start: the buffer's minor modes are turned off and its buffer-local values
forgotten (but the permanent ones), then the code of the mode and of the
modes it is derived from is run, the oldest first, and then their hooks
\(TEXT-MODE-HOOK before the hook of a mode derived from TEXT-MODE)."
  (let ((*buf* b))
    (dolist (minor (copy-list (buf-minor-modes b)))
      (unless (get minor 'permanent-local) (ignore-errors (set-minor-mode minor nil b))))
    (kill-all-local-variables b)
    (setf (buf-mode b) mode (buf-lex b) nil)
    (let ((chain (reverse (mode-chain mode))))
      (dolist (m chain)
        (let ((function (get m 'mode-function)))
          (when function (funcall function b))))
      (dolist (m chain) (run-hooks (format nil "~A-HOOK" m))))
    (run-hooks 'after-change-major-mode-hook)
    mode))
(defun ensure-major-mode (mode &optional (b *buf*))
  "Put buffer B in MODE unless it is in it already."
  (unless (eq (buf-mode b) mode) (set-major-mode mode b))
  mode)
(defmacro define-major-mode (name parent (&key lighter keys extensions (command t)) documentation
                             &body body)
  "Define the major mode NAME, derived from PARENT (NIL for a mode of its own
kind).  A derived mode has its parent's keys where it binds none itself,
is coloured and indented as its parent is, and runs its parent's hooks
before its own.  LIGHTER is its name in the mode line, KEYS a list of
\(keys command), EXTENSIONS the file extensions that are in this mode when
visited.  BODY is run when a buffer enters the mode, with BUFFER bound to
it and after the parent's body.  Unless COMMAND is false there is a command
NAME that puts the current buffer in the mode; and NAME-HOOK is run then.
  (define-major-mode scheme-mode lisp-mode (:lighter \"Scheme\" :extensions (\"scm\"))
    \"For Scheme source.\")"
  `(progn
     (setf (get ',name 'parent-mode) ',parent
           (get ',name 'mode-name) ,(or lighter (string-capitalize (string name)))
           (get ',name 'mode-doc) ,documentation
           (get ',name 'mode-function) (lambda (buffer) (declare (ignorable buffer)) ,@body))
     (setf *major-modes* (append (remove ',name *major-modes*) (list ',name)))
     (dolist (binding ',keys) (bind-key (first binding) (second binding) ',name))
     (dolist (extension ',extensions)
       (setf *auto-modes* (acons extension ',name
                                 (remove extension *auto-modes* :key #'car :test #'string-equal))))
     ,@(when command
         `((define-command ,name ()
             ,(format nil "Put this buffer in ~(~A~): ~A" name documentation)
             (set-major-mode ',name)
             (message "~A mode" (mode-name ',name)))))
     ',name))

;;; ---- minor modes
;;; A minor mode is something extra that can be on or off whatever the major
;;; mode is -- being looked at and not changed (View), hiding the dull files
;;; of a directory (Dired's Omit).  Each has a keymap that is looked in
;;; before the major mode's, a word for the mode line, a hook, and a command
;;; of its own name.  As in GNU Emacs the name is also a variable: true where
;;; the mode is on.  Most are on or off buffer by buffer; a global one is on
;;; everywhere; and a globalized one is a global switch that turns a
;;; buffer's own mode on in every buffer it suits, those made later too.
(defvar *minor-modes* '() "Every minor mode there is, in the order they were defined.")
(defvar *global-minor-modes* '() "The global minor modes that are on.")
(defvar *globalized-modes* '()
  "The globalized minor modes: lists of (global-mode mode predicate).")
(defun minor-mode-p (mode &optional (b *buf*))
  "Is the minor mode MODE on in buffer B?"
  (and (or (and b (member mode (buf-minor-modes b))) (member mode *global-minor-modes*)) t))
(defun active-minor-modes (&optional (b *buf*))
  "The minor modes that are on in buffer B: the newest first, then the global ones."
  (append (buf-minor-modes b) *global-minor-modes*))
(defun set-minor-mode (mode on &optional (b *buf*))
  "Turn the minor mode MODE on (or, with ON false, off) in buffer B --
everywhere, if it is a global one -- and run its hook, MODE-HOOK.  Returns
what the mode's own code returned, which may be a message to show."
  (unless (member mode *minor-modes*) (editor-error "There is no minor mode ~(~A~)" mode))
  (unless (eq (and on t) (minor-mode-p mode b))
    (if (get mode 'global-mode)
        (setf *global-minor-modes* (if on (cons mode *global-minor-modes*) (remove mode *global-minor-modes*)))
        (setf (buf-minor-modes b) (if on (cons mode (buf-minor-modes b)) (remove mode (buf-minor-modes b)))))
    (let ((*buf* b))
      (prog1 (let ((function (get mode 'minor-mode-function)))
               (and function (funcall function b (and on t))))
        (run-hooks (format nil "~A-HOOK" mode))))))
(defun minor-mode-state (mode) (minor-mode-p mode))
(defun (setf minor-mode-state) (on mode) (set-minor-mode mode on) on)
(defmacro define-minor-mode (name (&key lighter global keys permanent) documentation &body body)
  "Define the minor mode NAME: the command NAME, the variable NAME (true in a
buffer where the mode is on, and SETF-able), and the hook NAME-HOOK, run
when the mode has been turned on or off.  LIGHTER is the word for it in the
mode line (\"\" for none); KEYS is a list of (keys command) that work while
it is on, before the major mode's; GLOBAL makes it one mode for every
buffer; PERMANENT keeps it on when the buffer's major mode changes.  BODY
is run when it has been turned on or off, with BUFFER bound to the buffer
and ON to true or false: if it returns a string, that is the message shown.
The command turns the mode on and off when it is typed or run with M-x;
called from Lisp -- a hook, say -- it turns the mode on, or does what its
argument says: true for on, NIL or a number below 1 for off.
  (define-minor-mode shout-mode (:lighter \"Shout\" :keys ((\"<f9>\" upcase-word)))
    \"Capitals at a key.\")
  (add-hook 'lisp-mode-hook 'shout-mode)"
  `(progn
     (setf *minor-modes* (append (remove ',name *minor-modes*) (list ',name)))
     (setf (get ',name 'mode-name) ,(or lighter (string-capitalize (string name)))
           (get ',name 'mode-doc) ,documentation
           (get ',name 'global-mode) ,global
           (get ',name 'permanent-local) ,permanent
           (get ',name 'minor-mode-function)
           (lambda (buffer on) (declare (ignorable buffer on)) ,@body))
     (dolist (binding ',keys) (bind-key (first binding) (second binding) ',name))
     (define-symbol-macro ,name (minor-mode-state ',name))
     (define-command ,name (&optional (argument :default))
       ,(format nil "Turn ~(~A~) on or off: ~A" name documentation)
       (let* ((on (cond ((not (eq argument :default))
                         (and argument (not (and (realp argument) (< argument 1)))))
                        ((eq *this-command* ',name) (not (minor-mode-p ',name)))
                        (t t)))
              (said (set-minor-mode ',name on)))
         (when (eq *this-command* ',name)
           (if (stringp said)
               (message "~A" said)
               (message "~A mode is ~:[off~;on~]" (mode-name ',name) on)))
         on))))
(defmacro define-globalized-minor-mode (global-name mode &optional (predicate '(constantly t))
                                        documentation)
  "Define GLOBAL-NAME, a global minor mode that turns the minor mode MODE on
in every buffer for which PREDICATE (a function of the buffer) is true:
the buffers there are, and each one made or given a major mode later.
Turned off, it turns MODE off in them all."
  `(progn
     (define-minor-mode ,global-name (:global t :lighter "")
       ,(or documentation (format nil "Turns ~(~A~) on in every buffer it suits." mode))
       (dolist (b *buffers*)
         (when (or (not on) (funcall ,predicate b))
           (set-minor-mode ',mode on b))))
     (setf *globalized-modes*
           (cons (list ',global-name ',mode ,predicate)
                 (remove ',global-name *globalized-modes* :key #'first)))
     ',global-name))
(defun turn-on-globalized-modes ()
  ;; (run for a buffer that has just been given its major mode)
  (loop for (global mode predicate) in *globalized-modes*
        when (and (minor-mode-p global) (funcall predicate *buf*))
          do (set-minor-mode mode t *buf*)))
(add-hook 'after-change-major-mode-hook 'turn-on-globalized-modes)
(defun read-command ()
  "Read keys until they name a command.  Returns it (or NIL) and the keys typed."
  ;; (looked up in this order: the minor modes that are on, the major mode
  ;; and the modes it is derived from, and then the keys there are everywhere)
  (let ((maps (append (mapcar #'mode-map (active-minor-modes))
                      (mapcar #'mode-map (mode-chain (buf-mode *buf*)))
                      (list *global-map*)))
        (typed '()))
    (loop
      (let ((key (read-key)))
        (push key typed)
        (let ((found (remove nil (mapcar (lambda (map) (gethash key map)) maps)))
              (keys (format nil "~{~A~^ ~}" (reverse typed))))
          (cond ((null found) (return (values nil keys)))
                ((hash-table-p (first found))
                 (setf maps (remove-if-not #'hash-table-p found))
                 (draw-echo (concatenate 'string keys "-")))
                (t (return (values (first found) keys)))))))))
