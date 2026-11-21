;;;; Lisp from the running image: completion, arguments, documentation,
;;;; macro expansion, compiling a file
;;;;
;;;; The editor is in the same Lisp as the code it edits, so it can ask:
;;;;
;;;;   M-TAB (or M-/, C-M-i)   complete the symbol before point, from the
;;;;                           symbols the buffer's package can see
;;;;   SPC                     after a function's or macro's name, shows its
;;;;                           arguments in the echo area
;;;;   C-c C-d d  (C-h o)      everything DESCRIBE says about a symbol
;;;;   C-c C-d a  (C-h a)      apropos: the symbols whose names contain a text
;;;;   C-c RET                 expand the macro call at point, once
;;;;   C-c M-m                 expand it and everything inside it
;;;;   C-c C-k                 save, compile and load the buffer's file
;;;;
;;;; In a buffer that has a file, "the buffer's package" is the one named by
;;;; the nearest (in-package ...) above point; elsewhere it is the REPL's.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

;;; ---- symbols by name

(defun name-package (name)
  "The package a symbol written as NAME is looked for in, and the part after the colons."
  (let* ((colon (position #\: name))
         (last (position #\: name :from-end t)))
    (values (cond ((null colon) (current-package))
                  ((zerop colon) (find-package "KEYWORD"))
                  (t (find-package (string-upcase (subseq name 0 colon)))))
            (if last (subseq name (1+ last)) name))))
(defun find-named-symbol (name)
  "The symbol written as NAME, and true if there is one."
  (multiple-value-bind (package bare) (name-package name)
    (if package
        (multiple-value-bind (symbol status) (find-symbol (string-upcase bare) package)
          (values symbol (and status t)))
        (values nil nil))))
(defun arglist-string (name)
  "The function or macro NAME with its arguments, as it would be called; or NIL."
  (ignore-errors
   (multiple-value-bind (symbol found) (find-named-symbol name)
     (when (and found (fboundp symbol) (not (special-operator-p symbol)))
       (let* ((function (or (macro-function symbol) (fdefinition symbol)))
              (arguments (if (typep function 'generic-function)
                             (sb-mop:generic-function-lambda-list function)
                             (sb-kernel:%fun-lambda-list function)))
              (*package* (current-package)) (*print-case* :downcase) (*print-pretty* nil))
         (and (listp arguments) (format nil "(~(~A~)~{ ~S~})" name arguments)))))))

;;; ---- completion

(defun symbol-completions (prefix)
  "The symbols PREFIX could be the start of, written as PREFIX is written:
foo, package:foo (exported ones) or package::foo."
  (multiple-value-bind (package bare) (name-package prefix)
    (let* ((colon (position #\: prefix))
           (head (subseq prefix 0 (- (length prefix) (length bare))))
           (start (string-upcase bare))
           (found '()))
      (labels ((starts-p (name) (and (>= (length name) (length start))
                                     (string= start name :end2 (length start))))
               (consider (symbol)
                 (when (starts-p (symbol-name symbol))
                   (push (concatenate 'string head (string-downcase (symbol-name symbol))) found))))
        (cond ((null package))
              ((null colon)
               (when (string/= start "")
                 (do-symbols (symbol package) (consider symbol))
                 (dolist (p (list-all-packages))
                   (dolist (name (cons (package-name p) (package-nicknames p)))
                     (when (starts-p name)
                       (push (concatenate 'string (string-downcase name) ":") found))))))
              ((or (zerop colon) (search "::" prefix))
               (do-symbols (symbol package)
                 (when (eq (symbol-package symbol) package) (consider symbol))))
              (t (do-external-symbols (symbol package) (consider symbol)))))
      (sort (remove-duplicates found :test #'string=) #'string<))))
(defun shown-candidates (candidates room)
  "As many of CANDIDATES as fit in ROOM columns, and ... if that is not all of them."
  (let ((shown '()))
    (dolist (candidate candidates (nreverse shown))
      (decf room (1+ (length candidate)))
      (when (minusp room) (return (nreverse (cons "..." shown))))
      (push candidate shown))))
(defun complete-symbol-name (text)
  "A completion function for the minibuffer, over Lisp's symbols."
  (multiple-value-bind (new matches) (funcall (complete-from (symbol-completions text)) text)
    (values new (if (rest matches) (shown-candidates matches (- 56 (length new))) matches))))
(define-command complete-symbol ()
  "Complete the Lisp symbol before point."
  (let* ((text (current-line)) (end (point-col))
         (start (loop for i downfrom end above 0
                      when (delimiter-p (char text (1- i))) return i
                      finally (return 0))))
    (loop while (and (< start end) (find (char text start) ",@#`")) do (incf start))
    (let ((prefix (subseq text start end)))
      (when (string= prefix "") (editor-error "There is no symbol before point to complete"))
      (multiple-value-bind (new matches) (funcall (complete-from (symbol-completions prefix)) prefix)
        (when (> (length new) (length prefix)) (insert (subseq new (length prefix))))
        (cond ((null matches) (message "Nothing starts with ~A" prefix))
              ((rest matches)
               (message "~D: ~{~A~^ ~}" (length matches) (shown-candidates matches 68)))
              (t (message "~A" (or (arglist-string new) "The only completion"))))))))
(define-command indent-or-complete ()
  "Indent the line; if that changes nothing and point follows a symbol, complete it."
  (let ((before (current-line)) (col (point-col)))
    (indent-line)
    (when (and (string= before (current-line)) (= col (point-col)) (plusp col)
               (not (delimiter-p (char before (1- col)))))
      (complete-symbol))))

;;; ---- arguments, as they are needed

(define-command lisp-space ()
  "Insert a space; after the name of a function or a macro, show its arguments."
  (insert " ")
  (ignore-errors
   (let ((open (enclosing-open)))
     (when open
       (let* ((text (line-text (car open))) (start (1+ (cdr open)))
              (end (or (position-if #'delimiter-p text :start start) (length text)))
              (arglist (and (< start end) (arglist-string (subseq text start end)))))
         (when arglist (message "~A" arglist)))))))

;;; ---- documentation

(define-command describe-symbol ()
  "Show what Lisp knows about a symbol: by default the one point is on."
  (let* ((package (current-package))
         (name (string-trim " " (read-string "Describe symbol: " :initial (or (name-at-point) "")
                                                                 :complete #'complete-symbol-name))))
    (multiple-value-bind (symbol found) (find-named-symbol name)
      (unless found (editor-error "There is no symbol ~A" name))
      (show-in-help "*help*"
                    (with-output-to-string (out)
                      (let ((*package* package) (arglist (arglist-string name)))
                        (when arglist (format out "~A~%~%" arglist))
                        (describe symbol out)))))))
(defun symbol-kinds (symbol)
  "What SYMBOL names, as a list of words."
  (remove nil (list (cond ((not (fboundp symbol)) nil)
                          ((special-operator-p symbol) "special operator")
                          ((macro-function symbol) "macro")
                          ((typep (fdefinition symbol) 'generic-function) "generic function")
                          (t "function"))
                    (cond ((keywordp symbol) nil)
                          ((constantp symbol) "constant")
                          ((boundp symbol) "variable"))
                    (and (find-class symbol nil) "class")
                    (and (member symbol *commands*) "command"))))
(define-command lisp-apropos ()
  "List the symbols whose names contain some text, and what each one names."
  (let* ((package (current-package))
         (text (string-trim " " (read-string "Apropos: "))))
    (when (string= text "") (editor-error "Apropos of what?"))
    (let* ((symbols (remove-duplicates (append (apropos-list text package)
                                               (apropos-list text nil t))))
           (rows (loop for symbol in symbols
                       for kinds = (ignore-errors (symbol-kinds symbol))
                       when kinds
                         collect (cons (let ((*package* package)) (string-downcase (prin1-to-string symbol)))
                                       kinds))))
      (setf rows (sort rows #'string< :key #'car))
      (show-in-help "*apropos*"
                    (format nil "~D symbol~:P with ~A in the name~:[~;, the first 500 shown~].  ~
                                 C-c C-d d describes one.~%~%~:{~36A ~{~A~^, ~}~%~}"
                            (length rows) text (> (length rows) 500)
                            (mapcar (lambda (row) (list (car row) (cdr row)))
                                    (subseq rows 0 (min 500 (length rows)))))))))

;;; ---- macro expansion

(defun sexp-around-point ()
  "The text of the expression that starts at point; failing that, of the one that ends there."
  (let ((line (point-line)) (col (point-col)))
    (if (find (char-at line col) "('`,#")
        (multiple-value-bind (l c) (sexp-end line col) (text-between line col l c))
        (multiple-value-bind (l c) (sexp-start line col)
          (multiple-value-bind (el ec) (sexp-end l c) (text-between l c el ec))))))
(defun expand-all (form)
  "FORM with every macro call in it expanded."
  (let ((flag (find-symbol "*WALK-FORM-EXPAND-MACROS-P*" "SB-WALKER"))
        (walk (find-symbol "WALK-FORM" "SB-WALKER")))
    (if (and flag walk (fboundp walk))
        (progv (list flag) (list t) (funcall walk form))
        (macroexpand form))))
(defun show-expansion (expander)
  (let ((package (current-package)) (text (sexp-around-point)))
    (multiple-value-bind (forms status) (let ((*repl-package* package)) (read-forms text))
      (unless (and (eq status :ok) forms) (editor-error "That expression can't be read"))
      (let* ((form (first forms))
             (expansion (let ((*package* package)) (funcall expander form))))
        (show-in-help "*macroexpansion*"
                      (let ((*package* package) (*print-case* :downcase) (*print-pretty* t)
                            (*print-right-margin* 78))
                        (format nil "~S~%" expansion)))
        (setf (buf-mode *buf*) 'lisp-mode)
        (message "~:[Expanded~;That is not a macro call: nothing to expand~]" (equal form expansion))))))
(define-command expand-macro ()
  "Expand the macro call at point, once, into the buffer *macroexpansion*."
  (show-expansion #'macroexpand-1))
(define-command expand-macro-all ()
  "Expand the macro call at point and every one inside it, into the buffer *macroexpansion*."
  (show-expansion #'expand-all))

;;; ---- compiling a file

(defun fill-buffer (name text)
  "Put TEXT in the buffer NAME without showing it."
  (let ((b (or (find-buf name) (make-buf name))))
    (setf (fill-pointer (buf-lines b)) 0)
    (dolist (line (split-lines text)) (vector-push-extend line (buf-lines b)))
    (setf (buf-line b) 0 (buf-col b) 0 (buf-undo b) nil (buf-modified b) nil)
    (dolist (w *windows*) (when (eq (win-buf w) b) (setf (win-point w) nil (win-top w) 0)))
    b))
(defun count-text (text in)
  (loop with start = 0
        for at = (search text in :start2 start)
        while at count t do (setf start (1+ at))))
(define-command compile-and-load-file ()
  "Save the buffer, compile its file and load the result.  What the compiler
said is in the buffer *compilation*."
  (unless (buf-file *buf*) (editor-error "This buffer has no file to compile"))
  (when (buf-modified *buf*) (save-buffer))
  (let* ((file (buf-file *buf*)) (out (make-string-output-stream))
         (start (get-universal-time)) (loaded nil) (failure nil))
    (draw-echo (format nil "Compiling ~A ..." file))
    (let ((*standard-output* out) (*error-output* out) (*trace-output* out)
          (*package* *repl-package*))
      (handler-case
          (let ((fasl (compile-file file)))
            (cond (fasl (load fasl) (setf loaded t))
                  (t (setf failure "the compiler gave up"))))
        (error (e)
          (setf failure (one-line (princ-to-string e)))
          (format out "~&; ~A~%" e))))
    (let* ((said (get-output-stream-string out))
           (warnings (count-text "caught WARNING" said))
           (notes (count-text "caught STYLE-WARNING" said)))
      (fill-buffer "*compilation*" said)
      (if loaded
          (message "~A compiled and loaded in ~D second~:P~[~:;, ~:*~D warning~:P~]~[~:;, ~:*~D style ~
                    warning~:P~]~:[~;  (see *compilation*)~]"
                   (file-namestring file) (- (get-universal-time) start) warnings notes
                   (plusp (+ warnings notes)))
          (message "~A was NOT loaded: ~A  (see *compilation*)" (file-namestring file) failure)))))

;;; ---- the definitions in this buffer

(defun definition-name (text)
  "The name the line TEXT begins a definition of, or NIL: (defun name, (defstruct (name,
(defmethod name, (define-command name ..."
  (when (and (> (length text) 5) (string-equal "(def" text :end2 4))
    (let* ((space (position #\Space text))
           (start (and space (position #\Space text :start space :test-not #'char=))))
      (when start
        (when (char= (char text start) #\() (incf start))
        (when (and (< start (length text)) (char= (char text start) #\")) (incf start))
        (let ((end (or (position-if #'delimiter-p text :start start) (length text))))
          (and (< start end) (subseq text start end)))))))
(defun buffer-definitions (&optional (b *buf*))
  "The definitions in buffer B: a list of (name . line), in order."
  (loop for i from 0 below (nlines b)
        for name = (definition-name (line-text i b))
        when (and name (top-level-line-p i b)) collect (cons name i)))
(define-command goto-definition-in-buffer ()
  "Go to one of this buffer's definitions, by name (TAB completes; M-, comes back)."
  (let* ((definitions (or (buffer-definitions) (editor-error "There are no definitions in this buffer")))
         (names (mapcar #'car definitions))
         (name (string-trim " " (read-string
                                 (format nil "Go to definition (~D here): " (length names))
                                 :complete (lambda (text)
                                             (multiple-value-bind (new matches)
                                                 (funcall (complete-from names) text)
                                               (values new (if (rest matches)
                                                               (shown-candidates matches (- 44 (length new)))
                                                               matches)))))))
         (place (or (assoc name definitions :test #'string-equal)
                    (find-if (lambda (d) (and (>= (length (car d)) (length name)) (string/= name "")
                                              (string-equal name (car d) :end2 (length name))))
                             definitions)
                    (editor-error "No definition of ~A in this buffer" name))))
    (push (list *buf* (point-line) (point-col)) *definition-stack*)
    (goto (cdr place) 0)
    (recenter)
    (message "~A, line ~D" (car place) (1+ (cdr place)))))

;;; ---- tracing, disassembling, undefining

(defun read-function-name (prompt)
  "Ask for the name of a function, by default the one at point; returns the symbol."
  (let ((name (string-trim " " (read-string prompt :initial (or (name-at-point) "")
                                                   :complete #'complete-symbol-name))))
    (multiple-value-bind (symbol found) (find-named-symbol name)
      (unless (and found (fboundp symbol)) (editor-error "There is no function ~A" name))
      symbol)))
(define-command toggle-trace ()
  "Trace a function -- its calls and results are printed with what the code
prints -- or stop tracing it."
  (let ((symbol (read-function-name "Trace, or stop tracing: ")))
    (cond ((member symbol (eval '(trace)))
           (eval `(untrace ,symbol))
           (message "~(~S~) is no longer traced" symbol))
          (t (eval `(trace ,symbol))
             (message "~(~S~) is traced: its calls appear with the output of what you evaluate" symbol)))))
(define-command untrace-all () "Stop tracing every function."
  (eval '(untrace))
  (message "Nothing is traced"))
(define-command disassemble-function ()
  "Show the machine code of a function in the buffer *disassembly*."
  (let ((symbol (read-function-name "Disassemble: ")))
    (show-in-help "*disassembly*"
                  (with-output-to-string (out) (disassemble symbol :stream out)))))
(define-command undefine-function ()
  "Make a function or a macro undefined."
  (let ((symbol (read-function-name "Undefine: ")))
    (fmakunbound symbol)
    (message "~(~S~) is no longer defined" symbol)))

;;; ---- the REPL's package

(define-command set-repl-package ()
  "Put the REPL in a package: by default the one this buffer's code is in."
  (let* ((names (sort (mapcar (lambda (p) (string-downcase (package-name p))) (list-all-packages))
                      #'string<))
         (name (string-trim " " (read-string "REPL package: "
                                             :initial (string-downcase (package-name (current-package)))
                                             :complete (lambda (text)
                                                         (multiple-value-bind (new matches)
                                                             (funcall (complete-from names) text)
                                                           (values new (if (rest matches)
                                                                           (shown-candidates matches 40)
                                                                           matches)))))))
         (package (or (find-package (string-upcase name)) (editor-error "There is no package ~A" name))))
    (setf *repl-package* package)
    (let ((repl (find-buf "*repl*")))
      (when repl (let ((*buf* repl)) (repl-new-prompt))))
    (message "The REPL is in ~A" (package-name package))))
