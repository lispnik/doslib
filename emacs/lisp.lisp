;;;; Lisp: parentheses, indentation, evaluation
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun escaped-p (line col)             ; is the character at LINE, COL part of #\x ?
  (and (>= col 2) (char= (char-at line (- col 1)) #\\) (char= (char-at line (- col 2)) #\#)))
(defun token-end (text c)
  "Where the symbol or number that starts at column C of TEXT ends.  |Bars|
and a backslash make what they hold part of it, spaces and parentheses too.
A bar with no partner takes the rest of the line."
  (let ((n (length text)) (in-bars nil))
    (loop while (< c n)
          do (let ((ch (char text c)))
               (cond ((char= ch #\\) (incf c 2))
                     ((char= ch #\|) (setf in-bars (not in-bars)) (incf c))
                     ((and (not in-bars) (delimiter-p ch)) (return))
                     (t (incf c)))))
    (min c n)))
(defun token-start (text c)
  "Where the symbol or number that ends at column C of TEXT starts."
  (loop while (> c 0)
        do (let ((ch (char text (1- c))))
             (cond ((and (>= c 2) (char= (char text (- c 2)) #\\)) (decf c 2))
                   ((char= ch #\|)
                    (let ((open (position #\| text :end (1- c) :from-end t))) (setf c (or open (1- c)))))
                   ((delimiter-p ch) (return))
                   (t (decf c)))))
  c)
(defun token-around (text col)
  "The symbol or number of the line TEXT that column COL is in or just after:
where it starts and where it ends, or NIL."
  (let ((c 0) (n (length text)))
    (loop while (< c n)
          do (let ((ch (char text c)))
               (cond ((char= ch #\")    ; over a string
                      (incf c)
                      (loop while (and (< c n) (char/= (char text c) #\"))
                            do (incf c (if (char= (char text c) #\\) 2 1)))
                      (incf c))
                     ((char= ch #\;) (return nil))
                     ((delimiter-p ch) (incf c))
                     (t (let* ((start c)
                               (end (if (and (char= ch #\#) (< (1+ c) n) (char= (char text (1+ c)) #\\))
                                        (or (position-if #'delimiter-p text :start (min n (+ c 3))) n)
                                        (max (1+ c) (token-end text c)))))
                          (cond ((<= start col end) (return (values start end)))
                                ((> start col) (return nil)))
                          (setf c end))))))))
(defun backslashed-p (text p)
  "Is the character at column P of TEXT made part of a symbol by a backslash?"
  (and (>= p 1) (char= (char text (1- p)) #\\)
       (not (and (>= p 2) (char= (char text (- p 2)) #\#)))            ; #\( is a character
       (not (and (>= p 3) (char= (char text (- p 2)) #\\) (char= (char text (- p 3)) #\#)))))
(defun safe-start (line)
  "A line at or before LINE that starts outside any list: the REPL prompt, or
the nearest line that starts a top-level form."
  (or (and (eq (buf-mode *buf*) 'repl-mode)
           (let ((prompt (repl-prompt-line))) (and prompt (<= prompt line) prompt)))
      (loop for i from line downto (max 0 (- line 500))
            when (top-level-line-p i) return i)
      (max 0 (- line 500))))
(defun scan-parens (from to-line to-col function)
  "Call FUNCTION with true (open) or false (close), the line and the column for
every parenthesis that is code -- not in a string, a comment or a character --
from the start of line FROM up to TO-LINE, TO-COL.  Returns true if that
position is inside a string."
  (let ((states (lex-states)) (in-string nil) (in-symbol nil) (block-comment 0))
    (loop for l from from to (min to-line (1- (nlines)))
          do (let* ((text (line-text l)) (c 0)
                    (start (if states (aref states l) (cond (in-string :string) (in-symbol :symbol) (t block-comment))))
                    (n (if (= l to-line) (min to-col (length text)) (length text))))
               (setf in-string (eq start :string) in-symbol (eq start :symbol)
                     block-comment (if (integerp start) start 0))
               (loop while (< c n)
                     do (let ((ch (char text c))
                              (next (if (< (1+ c) (length text)) (char text (1+ c)) #\Newline)))
                          (cond (in-string (cond ((char= ch #\\) (incf c))
                                                 ((char= ch #\") (setf in-string nil))))
                                (in-symbol (cond ((char= ch #\\) (incf c))
                                                 ((char= ch #\|) (setf in-symbol nil))))
                                ((plusp block-comment)
                                 (cond ((and (char= ch #\|) (char= next #\#)) (decf block-comment) (incf c))
                                       ((and (char= ch #\#) (char= next #\|)) (incf block-comment) (incf c))))
                                ((char= ch #\") (setf in-string t))
                                ((char= ch #\;) (return))
                                ((char= ch #\\) (incf c))
                                ((and (char= ch #\#) (char= next #\\)) (incf c 2))
                                ((and (char= ch #\#) (char= next #\|)) (incf block-comment) (incf c))
                                ((char= ch #\|) (setf in-symbol t))     ; |a symbol|: its parentheses are not code
                                ((char= ch #\() (funcall function t l c))
                                ((char= ch #\)) (funcall function nil l c)))
                          (incf c)))))
    in-string))
(defun find-open-paren (line col)
  "The position of the parenthesis that the one just before LINE, COL closes."
  (let ((stack '()))
    (scan-parens (safe-start line) line col
                 (lambda (open l c)
                   (if open
                       (push (cons l c) stack)
                       (let ((match (pop stack)))
                         (when (and (= l line) (= c (1- col)))
                           (return-from find-open-paren match))))))
    nil))
(defun find-close-paren (line col)
  "The position after the parenthesis that closes the one at LINE, COL."
  (let ((depth nil))
    (scan-parens (safe-start line) (min (1- (nlines)) (+ line 500)) most-positive-fixnum
                 (lambda (open l c)
                   (cond ((and open (= l line) (= c col)) (setf depth 1))
                         ((null depth))
                         (open (incf depth))
                         ((zerop (decf depth)) (return-from find-close-paren (cons l (1+ c)))))))
    nil))
(defun matching-paren-position ()
  (let ((line (point-line)) (col (point-col)))
    (cond ((and (> col 0) (char= (char-at line (1- col)) #\)) (not (escaped-p line (1- col))))
           (find-open-paren line col))
          ((char= (char-at line col) #\()
           (let ((end (find-close-paren line col))) (and end (cons (car end) (1- (cdr end)))))))))
(defun sexp-start-before-point ()
  "Where the expression that ends at point begins."
  (let ((l (point-line)) (c (point-col)))
    (loop while (and (or (> c 0) (> l 0)) (whitespace-p (if (> c 0) (char-at l (1- c)) #\Newline)))
          do (if (> c 0) (decf c) (setf l (1- l) c (length (line-text l)))))
    (when (= c 0) (editor-error "No expression before point"))
    (let ((ch (char-at l (1- c))))
      (cond ((char= ch #\))
             (let ((open (or (find-open-paren l c) (editor-error "Unbalanced parentheses"))))
               (setf l (car open) c (cdr open))
               (loop while (and (> c 0) (find (char-at l (1- c)) "'`,@#")) do (decf c))))
            ((char= ch #\")
             (decf c)
             (loop do (decf c)
                   until (or (<= c 0) (and (char= (char-at l c) #\")
                                           (char/= (char-at l (1- c)) #\\)))))
            (t (setf c (token-start (line-text l) c))
               (loop while (and (> c 0) (find (char-at l (1- c)) "'`,@#")) do (decf c)))))
    (values l c)))
(defvar *body-forms*
  '("let" "let*" "flet" "labels" "macrolet" "when" "unless" "lambda" "progn" "prog1" "block"
    "case" "ecase" "typecase" "etypecase" "handler-case" "handler-bind" "restart-case" "unwind-protect"
    "multiple-value-bind" "destructuring-bind" "eval-when" "catch" "tagbody" "ignore-errors"
    "symbol-macrolet" "locally" "define-command"))
(defun body-form-p (word)
  (flet ((prefix-p (prefix) (and (>= (length word) (length prefix))
                                 (string-equal prefix word :end2 (length prefix)))))
    (or (prefix-p "def") (prefix-p "with-") (prefix-p "do")
        (member word *body-forms* :test #'string-equal))))
(defun indent-under (text col)          ; COL is where the enclosing ( is, in TEXT
  (let* ((start (1+ col))
         (end (or (position-if #'delimiter-p text :start start) (length text)))
         (word (subseq text start end)))
    (cond ((string= word "") (1+ col))
          ((body-form-p word) (+ col 2))
          ((string-equal word "if") (+ col 4))
          (t (let ((argument (position-if-not #'whitespace-p text :start end)))
               (if (and argument (char/= (char text argument) #\;)) argument (1+ col)))))))
(defun lisp-indentation (target)
  "The column line TARGET should start in, or NIL to leave it alone."
  (let* ((stack '())
         (in-string (scan-parens (safe-start (max 0 (1- target))) target 0
                                 (lambda (open l c) (if open (push (cons l c) stack) (pop stack))))))
    (cond (in-string nil)
          ((null stack) 0)
          (t (indent-under (line-text (car (first stack))) (cdr (first stack)))))))
(define-command indent-line ()
  "Indent the current line as Lisp code."
  (let* ((line (point-line)) (text (current-line))
         (old (or (position-if-not #'whitespace-p text) (length text)))
         (new (lisp-indentation line)))
    (when (and new (/= new old))
      (delete-between line 0 line old)
      (insert-at line 0 (make-string new :initial-element #\Space))
      (goto line (max new (+ (point-col) (- new old)))))
    (when (and new (< (point-col) new)) (goto line new))))
(define-command newline-and-indent () "Insert a line break and indent the new line."
  (newline) (indent-line))


;;; Input for the code being evaluated comes from the minibuffer: this
;;; stream is *QUERY-IO* and *STANDARD-INPUT* during an evaluation.  What is
;;; written to it becomes the prompt for what is next read from it, so
;;; (y-or-n-p "Really?"), (read) and the questions restarts ask all work.
(defclass minibuffer-stream (sb-gray:fundamental-character-input-stream
                             sb-gray:fundamental-character-output-stream)
  ((prompt :initform "" :accessor stream-prompt)
   (pending :initform "" :accessor stream-pending)
   (index :initform 0 :accessor stream-index)))
(defmethod sb-gray:stream-write-char ((stream minibuffer-stream) char)
  (setf (stream-prompt stream) (concatenate 'string (stream-prompt stream) (string char)))
  char)
(defmethod sb-gray:stream-line-column ((stream minibuffer-stream)) nil)
(defmethod sb-gray:stream-read-char ((stream minibuffer-stream))
  (when (>= (stream-index stream) (length (stream-pending stream)))
    (let* ((asked (string-trim '(#\Space #\Newline) (stream-prompt stream)))
           (prompt (subseq asked (1+ (or (position #\Newline asked :from-end t) -1)))))
      (redisplay)
      (setf (stream-pending stream)
            (format nil "~A~%" (read-string (if (string= prompt "") "Input: "
                                                (concatenate 'string prompt " "))))
            (stream-index stream) 0 (stream-prompt stream) "")))
  (prog1 (char (stream-pending stream) (stream-index stream))
    (incf (stream-index stream))))
(defmethod sb-gray:stream-unread-char ((stream minibuffer-stream) char)
  (declare (ignore char))
  (when (plusp (stream-index stream)) (decf (stream-index stream)))
  nil)
(defmethod sb-gray:stream-clear-input ((stream minibuffer-stream))
  (setf (stream-pending stream) "" (stream-index stream) 0)
  nil)
(defvar *minibuffer-stream* (make-instance 'minibuffer-stream))

;;; The debugger.  An error in code being evaluated does not end the
;;; evaluation: the stack stays as it is while the buffer *debugger* shows
;;; the condition, the restarts on offer and a backtrace.  Typing a
;;; restart's number invokes it (asking in the minibuffer for any value it
;;; needs); q abandons the evaluation.
(defvar *editor-restart* nil "The restart that abandons the current evaluation.")
(defvar *debugged* nil "The condition the debugger was last entered for, in this evaluation.")
(defun editor-debugger (condition &optional hook)
  (declare (ignore hook))
  (when (or (typep condition 'quit-command) (null *editor-restart*))
    (return-from editor-debugger nil))
  (setf *debugged* condition)
  (let* ((restarts (let ((all (compute-restarts condition)))   ; ours and those inside it
                     (ldiff all (rest (member *editor-restart* all)))))
         (previous *buf*)
         (chosen nil))
    (show-in-help
     "*debugger*"
     (format nil "~A~%   [Condition of type ~A]~%~%Restarts:~%~{~A~%~}~%~
Type a restart's number to invoke it, or q to abandon the evaluation.~%~%Backtrace:~%~A"
             condition (type-of condition)
             (loop for restart in restarts for i from 0
                   collect (format nil " ~2D: [~A] ~A" i (restart-name restart) restart))
             (or (ignore-errors
                  (with-output-to-string (out)
                    (sb-debug:print-backtrace :stream out :count 40)))
                 "(not available)")))
    (unwind-protect
         (loop
           (message "Debugger: restart number (0-~D), or q to abandon" (1- (length restarts)))
           (redisplay)
           (let* ((key (read-key)) (digit (and (= (length key) 1) (digit-char-p (char key 0)))))
             (cond ((and digit (< digit (length restarts)))
                    (setf chosen (nth digit restarts)) (return))
                   ((member key '("q" "a" "C-g") :test #'equal)
                    (setf chosen *editor-restart*) (return))
                   (t (let ((command (gethash key *global-map*)))      ; look around
                        (when (member command '(next-line previous-line scroll-up scroll-down
                                                forward-char backward-char beginning-of-line
                                                end-of-line beginning-of-buffer end-of-buffer recenter))
                          (ignore-errors (funcall command))))))))
      (setf *buffers* (remove (find-buf "*debugger*") *buffers*))
      (select-buffer previous)
      (setf *message* nil))
    (invoke-restart-interactively chosen)))
(defun eval-captured (form)
  "Evaluate FORM.  Returns its values as a list, what it printed, and the
condition if the evaluation was abandoned.  Errors enter the debugger."
  (let ((out (make-string-output-stream)) (values '()) (condition nil) (*debugged* nil))
    (let ((*standard-output* out) (*error-output* out) (*trace-output* out)
          (*query-io* *minibuffer-stream*) (*standard-input* *minibuffer-stream*)
          (sb-ext:*invoke-debugger-hook* #'editor-debugger)    ; BREAK and the like
          (*package* *repl-package*))
      (restart-case
          (let ((*editor-restart* (find-restart 'abort)))
            (handler-bind ((warning (lambda (w)
                                      (unless (typep w 'style-warning) (format out "; WARNING: ~A~%" w))
                                      (muffle-warning w)))
                           (error #'editor-debugger))
              (setf values (multiple-value-list (eval form)))))
        (abort ()
          :report "Abandon the evaluation and return to the editor."
          (setf condition (or *debugged*
                              (make-condition 'simple-error
                                              :format-control "the evaluation was abandoned")))))
      (setf *repl-package* *package*))
    (values values (get-output-stream-string out) condition)))
(defun read-forms (text)
  "All the forms in TEXT, then :OK, :INCOMPLETE, or an error."
  (let ((*package* *repl-package*) (forms '()) (position 0) (eof '#:eof))
    (handler-case
        (loop (multiple-value-bind (form end) (read-from-string text nil eof :start position)
                (when (eq form eof) (return (values (nreverse forms) :ok)))
                (push form forms) (setf position end)))
      (end-of-file () (values nil :incomplete))
      (error (e) (values nil e)))))
(defun printed (values)
  (let ((*package* *repl-package*))
    (if values (format nil "~{~S~^~%~}" values) "; No value")))
(defun eval-and-show (text)
  "Evaluate the forms in TEXT; show the result in the echo area, and
anything longer in the buffer *output*."
  (multiple-value-bind (forms status) (read-forms text)
    (unless (eq status :ok)
      (editor-error "Can't read that: ~A" (if (eq status :incomplete) "incomplete expression" status)))
    (let ((all (make-string-output-stream)) (last ""))
      (dolist (form forms)
        (multiple-value-bind (values output condition) (eval-captured form)
          (write-string output all)
          (setf last (if condition
                         (format nil "~A: ~A" (type-of condition) condition)
                         (printed values)))
          (format all "~A~%" last)))
      (let ((everything (string-right-trim '(#\Newline) (get-output-stream-string all))))
        (if (or (find #\Newline everything) (> (length everything) 76))
            (let ((b (or (find-buf "*output*") (make-buf "*output*"))))
              (insert-at (1- (nlines b)) (length (line-text (1- (nlines b)) b))
                         (format nil "~A~%" everything) :b b)
              (setf (buf-line b) (1- (nlines b)) (buf-col b) 0 (buf-modified b) nil)
              (dolist (w *windows*) (when (eq (win-buf w) b) (setf (win-point w) nil)))
              (message "=> ~A   (more in *output*)" last))
            (message "=> ~A" everything))))))
;;; Code in a file is read in the file's package: the one named by the
;;; nearest (in-package ...) above point.  A buffer with no file (*scratch*,
;;; the REPL) uses the REPL's package, which evaluating an IN-PACKAGE form
;;; there changes.
(defun buffer-package (&optional line)
  "The package named by the nearest (in-package ...) at or above LINE of a
buffer that has a file, if there is such a package."
  (when (and *buf* (buf-file *buf*))
    (loop for i from (min (or line (point-line)) (1- (nlines))) downto 0
          do (let ((text (line-text i)))
               (when (and (> (length text) 12) (string-equal "(in-package" text :end2 11))
                 (return (ignore-errors
                          (let ((*read-eval* nil) (*package* (find-package "KEYWORD")))
                            (find-package (string (read-from-string text t nil :start 11)))))))))))
(defun current-package ()
  "The package the code at point is read in."
  (or (buffer-package) *repl-package*))
(defmacro in-buffer-package (&body body)
  "Do BODY with the evaluation commands reading in the buffer's package, if it names one."
  (let ((package (gensym)))
    `(let ((,package (buffer-package)))
       (progv (and ,package '(*repl-package*)) (and ,package (list ,package))
         ,@body))))
(define-command eval-print-last-sexp ()
  "Evaluate the Lisp expression before point and insert what it prints and returns."
  (multiple-value-bind (line col) (sexp-start-before-point)
    (in-buffer-package
      (multiple-value-bind (forms status) (read-forms (text-between line col (point-line) (point-col)))
        (unless (eq status :ok)
          (editor-error "Can't read that: ~A" (if (eq status :incomplete) "incomplete expression" status)))
        (dolist (form forms)
          (multiple-value-bind (values output condition) (eval-captured form)
            (insert (format nil "~%~A~&~A~%" output
                            (if condition
                                (format nil "; ~A: ~A" (type-of condition)
                                        (one-line (princ-to-string condition)))
                                (printed values))))))))))
(define-command eval-last-sexp ()
  "Evaluate the Lisp expression before point; with C-u, insert the result as well."
  (if *prefix-argument*
      (eval-print-last-sexp)
      (multiple-value-bind (line col) (sexp-start-before-point)
        (in-buffer-package
          (eval-and-show (text-between line col (point-line) (point-col)))))))
(setf (get 'eval-last-sexp 'takes-prefix) t)
(define-command eval-region ()
  "Evaluate the Lisp expressions between point and the mark."
  (multiple-value-bind (l1 c1 l2 c2) (region-bounds)
    (in-buffer-package (eval-and-show (text-between l1 c1 l2 c2)))))
(defun defvar-again (text)
  "TEXT with a (defvar name value ...) at its start made a DEFPARAMETER: evaluating
a DEFVAR on purpose is asking for the new value."
  (if (and (> (length text) 8) (string-equal "(defvar " text :end2 8)
           (ignore-errors (> (length (let ((*package* (current-package)) (*read-eval* nil))
                                       (read-from-string text)))
                             2)))
      (concatenate 'string "(defparameter " (subseq text 8))
      text))
(define-command eval-defun ()
  "Evaluate the top-level form around point.  A DEFVAR gives its variable the value again."
  (let* ((start (or (loop for i from (point-line) downto 0 when (top-level-line-p i) return i)
                    (editor-error "No top-level form here")))
         (end (or (find-close-paren start 0) (editor-error "Unbalanced parentheses"))))
    (in-buffer-package
      (eval-and-show (defvar-again (text-between start 0 (car end) (cdr end)))))))
(define-command eval-buffer () "Evaluate the whole buffer." (eval-and-show (buffer-string)))
(define-command eval-expression () "Read a Lisp expression in the minibuffer and evaluate it."
  (eval-and-show (read-string "Eval: ")))

;;; M-. goes to where something is defined; M-, comes back.  The definition
;;; is looked for as a line that begins "(def... NAME": in this buffer, in
;;; the other buffers, in the file the compiler remembers for a function
;;; that is loaded, and in the Lisp files beside the one being edited.
(defvar *definition-stack* '() "Where M-. was typed from: (buffer line col), newest first.")
(defun name-at-point ()
  "The symbol's name that point is in or just after, as it is written; or NIL."
  (let ((text (current-line)))
    (multiple-value-bind (start end) (token-around text (min (point-col) (length text)))
      (and start (< start end) (string-left-trim "#'`,@" (subseq text start end))))))
(defun bare-name (name)
  "NAME without a package prefix."
  (let ((colon (position #\: name :from-end t)))
    (if (and colon (< (1+ colon) (length name))) (subseq name (1+ colon)) name)))
(defun definition-line-p (text name)
  "Does the line TEXT begin a definition of NAME?  (defun name, (defstruct (name,
(defun (setf name), (define-command name, (define-example \"name\" ..."
  (when (and (> (length text) 5) (string-equal "(def" text :end2 4))
    (let* ((space (position #\Space text))
           (start (and space (position #\Space text :start space :test-not #'char=))))
      (when start
        (when (char= (char text start) #\() (incf start))
        (when (and (< (+ start 5) (length text)) (string-equal "setf " text :start2 start :end2 (+ start 5)))
          (incf start 5))
        (when (and (< start (length text)) (char= (char text start) #\")) (incf start))
        (let ((end (or (position-if #'delimiter-p text :start start) (length text))))
          (string-equal name (bare-name (subseq text start end))))))))
(defun definition-in-buffer (name b)
  "The number of the line of buffer B that defines NAME, or NIL."
  (loop for i from 0 below (nlines b)
        when (definition-line-p (line-text i b) name) return i))
(defun definition-in-file (name file)
  (ignore-errors
   (with-open-file (in file :external-format :latin-1)
     (loop for line = (read-line in nil) for i from 0
           while line
           when (definition-line-p line name) return i))))
(defun remembered-source-file (name)
  "The file a loaded function or macro called NAME was compiled from, if the
compiler recorded one and it is still there."
  (ignore-errors
   (let* ((colon (position #\: name))
          (package (if (and colon (plusp colon))
                       (find-package (string-upcase (subseq name 0 colon)))
                       *repl-package*))
          (symbol (and package (find-symbol (string-upcase (bare-name name)) package)))
          (function (and symbol (or (macro-function symbol)
                                    (and (fboundp symbol) (fdefinition symbol)))))
          (code (and function (sb-kernel:fun-code-header (sb-kernel:%fun-fun function))))
          (file (and code (sb-c::debug-source-namestring
                           (sb-c::debug-info-source (sb-kernel:%code-debug-info code))))))
     (and file (probe-file file) (namestring file)))))
(defun find-definition-place (name)
  "Where NAME is defined: a buffer and a line number, or NIL."
  (let ((bare (bare-name name)))
    (flet ((in-buffer (b) (let ((line (definition-in-buffer bare b))) (and line (cons b line))))
           (in-file (file)
             (let ((line (definition-in-file bare file)))
               (and line (cons (find-file-buffer (namestring file)) line)))))
      (or (in-buffer *buf*)
          (some #'in-buffer (remove *buf* *buffers*))
          (let ((file (remembered-source-file name))) (and file (in-file file)))
          (and (buf-file *buf*)
               (some #'in-file
                     (ignore-errors
                      (append (directory (merge-pathnames "*.lisp" (buf-file *buf*)))
                              (directory (merge-pathnames "*.lsp" (buf-file *buf*)))))))))))
(define-command find-definition ()
  "Go to the definition of a function, macro, variable or command: by
default the one whose name point is on.  M-, comes back."
  (let* ((default (name-at-point))
         (name (string-trim " " (read-string "Find definition of: " :initial (or default ""))))
         (place (and (string/= name "") (find-definition-place name))))
    (unless place (editor-error "No definition of ~A found" name))
    (push (list *buf* (point-line) (point-col)) *definition-stack*)
    (select-buffer (car place))
    (goto (cdr place) 0)
    (recenter)
    (message "~A, line ~D" (buf-name *buf*) (1+ (cdr place)))))
(define-command pop-definition ()
  "Go back to where M-. was last typed."
  (let ((place (pop *definition-stack*)))
    (unless place (editor-error "No place to go back to"))
    (destructuring-bind (b line col) place
      (unless (member b *buffers*) (editor-error "That buffer has been killed"))
      (select-buffer b)
      (goto line col)
      (recenter))))
(bind-key "M-." 'find-definition)
(bind-key "M-," 'pop-definition)
