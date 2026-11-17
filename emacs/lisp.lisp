;;;; Lisp: parentheses, indentation, evaluation
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun escaped-p (line col)             ; is the character at LINE, COL part of #\x ?
  (and (>= col 2) (char= (char-at line (- col 1)) #\\) (char= (char-at line (- col 2)) #\#)))
(defun safe-start (line)
  "A line at or before LINE that starts outside any string, comment or list:
the REPL prompt, or the nearest line with a parenthesis in the first column."
  (or (and (eq (buf-mode *buf*) 'repl-mode)
           (let ((prompt (repl-prompt-line))) (and prompt (<= prompt line) prompt)))
      (loop for i from line downto (max 0 (- line 500))
            when (let ((text (line-text i))) (and (plusp (length text)) (char= (char text 0) #\()))
              return i)
      (max 0 (- line 500))))
(defun scan-parens (from to-line to-col function)
  "Call FUNCTION with true (open) or false (close), the line and the column for
every parenthesis that is code -- not in a string, a comment or a character --
from the start of line FROM up to TO-LINE, TO-COL.  Returns true if that
position is inside a string."
  (let ((in-string nil) (block-comment 0))
    (loop for l from from to (min to-line (1- (nlines)))
          do (let* ((text (line-text l)) (c 0)
                    (n (if (= l to-line) (min to-col (length text)) (length text))))
               (loop while (< c n)
                     do (let ((ch (char text c))
                              (next (if (< (1+ c) (length text)) (char text (1+ c)) #\Newline)))
                          (cond (in-string (cond ((char= ch #\\) (incf c))
                                                 ((char= ch #\") (setf in-string nil))))
                                ((plusp block-comment)
                                 (cond ((and (char= ch #\|) (char= next #\#)) (decf block-comment) (incf c))
                                       ((and (char= ch #\#) (char= next #\|)) (incf block-comment) (incf c))))
                                ((char= ch #\") (setf in-string t))
                                ((char= ch #\;) (return))
                                ((and (char= ch #\#) (char= next #\\)) (incf c 2))
                                ((and (char= ch #\#) (char= next #\|)) (incf block-comment) (incf c))
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
            (t (loop while (and (> c 0) (not (delimiter-p (char-at l (1- c))))) do (decf c))
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
(define-command eval-last-sexp ()
  "Evaluate the Lisp expression before point."
  (multiple-value-bind (line col) (sexp-start-before-point)
    (eval-and-show (text-between line col (point-line) (point-col)))))
(define-command eval-defun ()
  "Evaluate the top-level form around point."
  (flet ((top-p (i) (let ((text (line-text i))) (and (plusp (length text)) (char= (char text 0) #\()))))
    (let* ((start (or (loop for i from (point-line) downto 0 when (top-p i) return i)
                      (editor-error "No top-level form here")))
           (end (or (find-close-paren start 0) (editor-error "Unbalanced parentheses"))))
      (eval-and-show (text-between start 0 (car end) (cdr end))))))
(define-command eval-buffer () "Evaluate the whole buffer." (eval-and-show (buffer-string)))
(define-command eval-expression () "Read a Lisp expression in the minibuffer and evaluate it."
  (eval-and-show (read-string "Eval: ")))
