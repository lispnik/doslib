;;;; Buffers: a vector of lines, a point, a mark
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defstruct (buf (:constructor %make-buf))
  name (lines (make-array 16 :adjustable t :fill-pointer 0))
  (line 0) (col 0) goal mark mark-active file modified (mode 'fundamental-mode)
  undo redo (crlf t) prompt history (history-index -1)
  read-only                             ; true: commands may not change the text
  minor-modes                           ; the minor modes that are on here, newest first
  locals                                ; buffer-local variables: an alist of (name . value)
  view                                  ; View mode: (made-for-viewing buffer-to-go-back-to was-read-only)
  (omitted 0)                           ; Dired: how many files Omit mode is hiding
  lex                                   ; what LINE-START-STATE remembers
  dir entries (sort :name))             ; Dired: the directory, its DENTRYs, their order
;;; A window that is not selected remembers its own point in POINT; the
;;; selected window's point is its buffer's.
(defstruct win buf (top 0) (hscroll 0) row height point)
;;; What a mode adds to the editor's own behaviour hangs on the mode's
;;; name, as properties holding functions:
;;;   line-attributes  (buffer line-number text)  set the colours of one line
;;;                    in *ATTRS*, an attribute byte per character, as it is drawn
;;;   kill-function    (buffer)  the buffer is being killed
;;;   ask-before-kill  true: a modified buffer is worth a question even
;;;                    though it has no file
;;; A mode can be derived from another (its PARENT-MODE property), and then
;;; whatever it does not say for itself is its parent's: these properties,
;;; its keys, and the hooks that run when a buffer enters it.
(defun mode-parent (mode) (and (symbolp mode) (get mode 'parent-mode)))
(defun mode-chain (mode)
  "MODE and the modes it is derived from, the nearest first."
  (loop for m = mode then (mode-parent m)
        repeat 20                       ; (a mode made its own ancestor by mistake must not hang the editor)
        while (and m (symbolp m))
        collect m))
(defun mode-derived-p (mode ancestor)
  "Is MODE the mode ANCESTOR, or derived from it?"
  (and (member ancestor (mode-chain mode)) t))
(defun base-mode (mode)
  "The built-in kind of buffer MODE is, for the code that draws and indents:
LISP-MODE, REPL-MODE or DIRED-MODE if it is one or is derived from one, or else MODE."
  (or (find-if (lambda (m) (member m '(lisp-mode repl-mode dired-mode))) (mode-chain mode)) mode))
(defun mode-hook (mode name)
  "What MODE says for the property NAME, or the nearest mode it is derived from does."
  (loop for m in (mode-chain mode) thereis (get m name)))
(defun buffer-precious-p (b)
  "Would killing B lose work?"
  (and (buf-modified b) (or (buf-file b) (mode-hook (buf-mode b) 'ask-before-kill))))
(defvar *buffers* '())
(defvar *buf* nil "The current buffer.")
(defvar *windows* '())
(defvar *window* nil "The selected window.")
(defvar *commands* '() "Every function defined with DEFINE-COMMAND.")
(defvar *message* nil)
(defvar *last-message* nil "The most recent echo-area message, for programs to look at.")
(defvar *kill-ring* '())
(defvar *this-command* nil)
(defvar *last-command* nil)
(defvar *unread* '())
(defvar *prefix-argument* nil "The number typed with C-u or M-digits before this command, or NIL.")
(defvar *defining-macro* nil)
(defvar *macro-keys* '() "The keys of the keyboard macro being defined, newest first.")
(defvar *macro-mark* 0)
(defvar *last-macro* '() "The keys of the last keyboard macro defined.")
(defvar *repl-package* (find-package '#:doslib-emacs-user)
  "The package the REPL and the evaluation commands read and evaluate in.")
(define-condition quit-command (error) ())
(defvar *inhibit-read-only* nil "True while a read-only buffer may be changed.")

(defmacro define-command (name lambda-list &body body)
  "Define a function that can be bound to a key or run with M-x."
  `(progn (defun ,name ,lambda-list ,@body) (pushnew ',name *commands*) ',name))
(defvar *message-log* '() "The echo area's messages, newest first (C-h e shows them).")
(defun message (control &rest arguments)
  "Show a line in the echo area."
  (setf *message* (one-line (apply #'format nil control arguments))
        *last-message* *message*)
  (unless (or (string= *message* "") (equal *message* (first *message-log*)))
    (push *message* *message-log*)
    (when (> (length *message-log*) 250) (setf (cdr (nthcdr 199 *message-log*)) nil)))
  *message*)
(defun one-line (string)                ; newlines and runs of spaces -> one space
  (with-output-to-string (out)
    (let ((gap nil))
      (loop for c across string
            do (if (member c '(#\Space #\Newline #\Tab))
                   (setf gap t)
                   (progn (when gap (write-char #\Space out) (setf gap nil))
                          (write-char c out)))))))
(defun editor-error (control &rest arguments) (apply #'error control arguments))
(defun buffer-mode-p (mode &optional (b *buf*))
  "Is buffer B in MODE, or in a mode derived from it?"
  (mode-derived-p (buf-mode b) mode))

;;; ---- hooks
;;; A hook is a name and a list of functions to call when something
;;; happens: LISP-MODE-HOOK when a buffer becomes a Lisp buffer, VIEW-MODE-HOOK
;;; when View mode is turned on or off.  Hooks are known by the spelling of
;;; their names, so 'LISP-MODE-HOOK means the same hook whatever package
;;; EMACSRC.LSP is read in.
(defvar *hooks* (make-hash-table :test 'equal) "Hook name (a string) -> its functions, in order.")
(defun hook-functions (hook) (gethash (string-upcase (string hook)) *hooks*))
(defun add-hook (hook function &optional at-end)
  "Have FUNCTION called whenever HOOK is run: (add-hook 'lisp-mode-hook 'my-setup).
It goes before the functions already there, or with AT-END after them."
  (let ((key (string-upcase (string hook))))
    (unless (member function (gethash key *hooks*))
      (setf (gethash key *hooks*)
            (if at-end
                (append (gethash key *hooks*) (list function))
                (cons function (gethash key *hooks*)))))
    function))
(defun remove-hook (hook function)
  "Stop FUNCTION being called when HOOK is run."
  (let ((key (string-upcase (string hook))))
    (setf (gethash key *hooks*) (remove function (gethash key *hooks*)))
    nil))
(defun run-hooks (hook &rest arguments)
  "Call the functions of HOOK, each with ARGUMENTS.  One that signals an
error is reported in the echo area, and the rest are still called."
  (dolist (function (hook-functions hook))
    (handler-case (apply function arguments)
      (quit-command (c) (error c))
      (error (e) (message "Error in ~(~A~): ~A" hook e)))))

;;; ---- buffer-local variables
;;; A variable of the editor's that can have a value of its own in each
;;; buffer.  DEFVAR-LOCAL makes the name stand for "its value in the
;;; current buffer" (a symbol macro on BUFFER-LOCAL), so it is read and
;;; SETF like any variable: a buffer that has not set it has the default.
;;; As in GNU Emacs, changing a buffer's major mode forgets the buffer's
;;; own values, except those of variables declared permanent.
(defvar *local-variables* '() "Every variable defined with DEFVAR-LOCAL.")
(defun default-value (name)
  "The value the buffer-local variable NAME has in a buffer that has not set it."
  (get name 'local-default))
(defun (setf default-value) (value name) (setf (get name 'local-default) value))
(defun local-variable-p (name &optional (b *buf*))
  "Has buffer B a value of its own for the buffer-local variable NAME?"
  (and b (assoc name (buf-locals b)) t))
(defun buffer-local (name &optional (b *buf*))
  "The value of the buffer-local variable NAME in buffer B: its own, or the default."
  (let ((cell (and b (assoc name (buf-locals b)))))
    (if cell (cdr cell) (get name 'local-default))))
(defun (setf buffer-local) (value name &optional (b *buf*))
  (let ((cell (and b (assoc name (buf-locals b)))))
    (cond (cell (setf (cdr cell) value))
          (b (push (cons name value) (buf-locals b)))
          (t (setf (get name 'local-default) value)))  ; no buffer yet: the default
    value))
(defun kill-local-variable (name &optional (b *buf*))
  "Forget buffer B's own value of NAME: it has the default again."
  (setf (buf-locals b) (remove name (buf-locals b) :key #'car))
  nil)
(defun kill-all-local-variables (&optional (b *buf*))
  "Forget every value of its own that buffer B has, but those of permanent variables."
  (setf (buf-locals b) (remove-if-not (lambda (cell) (get (car cell) 'permanent-local)) (buf-locals b)))
  nil)
(defmacro defvar-local (name default &optional documentation &key permanent)
  "Define NAME as a buffer-local variable whose value is DEFAULT in a buffer
that has not set it.  NAME is then read and SETF as a variable, and means
its value in the current buffer.  With PERMANENT, a change of major mode
does not forget a buffer's value.
  (defvar-local fill-column 70 \"Where lines are broken.\")
  (setf fill-column 60)                 ; in this buffer only
  (setf (default-value 'fill-column) 72)"
  `(progn
     (setf (get ',name 'local-default) ,default
           (get ',name 'local-documentation) ,documentation
           (get ',name 'permanent-local) ,permanent)
     (pushnew ',name *local-variables*)
     (define-symbol-macro ,name (buffer-local ',name))
     ',name))
(defun make-buf (name)
  (let ((b (%make-buf :name name)))
    (vector-push-extend "" (buf-lines b))
    (setf *buffers* (append *buffers* (list b)))
    b))
(defun find-buf (name) (find name *buffers* :key #'buf-name :test #'string-equal))
(defun nlines (&optional (b *buf*)) (length (buf-lines b)))
(defun line-text (i &optional (b *buf*)) (aref (buf-lines b) i))
(defun current-line () "The text of the line point is on." (line-text (buf-line *buf*)))
(defun point-line () (buf-line *buf*))
(defun point-col () (buf-col *buf*))
(defun goto (line col)
  "Move point, staying inside the buffer."
  (let* ((line (max 0 (min line (1- (nlines)))))
         (col (max 0 (min col (length (line-text line))))))
    (setf (buf-line *buf*) line (buf-col *buf*) col)))

;;; The two primitives every change goes through; they keep the undo list.
(defun split-lines (string)
  (loop with start = 0
        for newline = (position #\Newline string :start start)
        collect (subseq string start newline)
        while newline do (setf start (1+ newline))))
(defun vector-insert (vector index item)
  (vector-push-extend item vector)
  (loop for i from (1- (length vector)) above index
        do (setf (aref vector i) (aref vector (1- i))))
  (setf (aref vector index) item))
(defun vector-delete (vector start end)
  (loop for i from end below (length vector)
        do (setf (aref vector (- i (- end start))) (aref vector i)))
  (decf (fill-pointer vector) (- end start)))
(defun insert-at (line col string &key (b *buf*) (undo t))
  "Insert STRING at LINE, COL; returns the position after it."
  (when (and undo (buf-read-only b) (not *inhibit-read-only*))
    (editor-error "~A is read-only" (buf-name b)))
  (let* ((lines (buf-lines b)) (text (aref lines line)) (parts (split-lines string))
         (head (subseq text 0 col)) (tail (subseq text col))
         (end-line line) (end-col (+ col (length string))))
    (cond ((null (rest parts))
           (setf (aref lines line) (concatenate 'string head string tail)))
          (t (setf (aref lines line) (concatenate 'string head (first parts)))
             (loop for part in (rest parts) for i from (1+ line)
                   do (vector-insert lines i part) (setf end-line i))
             (setf end-col (length (aref lines end-line))
                   (aref lines end-line) (concatenate 'string (aref lines end-line) tail))))
    (when undo (push (list :insert line col end-line end-col) (buf-undo b)) (setf (buf-redo b) nil))
    (setf (buf-modified b) t (buf-mark-active b) nil)
    (values end-line end-col)))
(defun text-between (l1 c1 l2 c2 &optional (b *buf*))
  (if (= l1 l2)
      (subseq (line-text l1 b) c1 c2)
      (with-output-to-string (out)
        (write-string (subseq (line-text l1 b) c1) out)
        (loop for i from (1+ l1) below l2 do (terpri out) (write-string (line-text i b) out))
        (terpri out)
        (write-string (subseq (line-text l2 b) 0 c2) out))))
(defun delete-between (l1 c1 l2 c2 &key (b *buf*) (undo t))
  "Delete the text between two positions and return it."
  (when (and undo (buf-read-only b) (not *inhibit-read-only*))
    (editor-error "~A is read-only" (buf-name b)))
  (let ((text (text-between l1 c1 l2 c2 b)) (lines (buf-lines b)))
    (setf (aref lines l1)
          (concatenate 'string (subseq (aref lines l1) 0 c1) (subseq (aref lines l2) c2)))
    (when (> l2 l1) (vector-delete lines (1+ l1) (1+ l2)))
    (when undo (push (list :delete l1 c1 text) (buf-undo b)) (setf (buf-redo b) nil))
    (setf (buf-modified b) t (buf-mark-active b) nil)
    text))
(defun insert (string)
  "Insert STRING at point."
  (multiple-value-bind (line col) (insert-at (point-line) (point-col) string)
    (goto line col)))
(defun buffer-string (&optional (b *buf*))
  (let ((last (1- (nlines b)))) (text-between 0 0 last (length (line-text last b)) b)))
