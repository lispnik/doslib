;;;; Help
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

;;;; The editor documents itself: every command has a documentation
;;;; string, and the keymaps say which keys run it, so the help commands
;;;; only have to look.  As in GNU Emacs they are behind C-h:
;;;;
;;;;   C-h k  what a key does                C-h c  the same, in one line
;;;;   C-h f  a function    C-h x  a command    C-h v  a variable
;;;;   C-h w  where a command is on the keyboard
;;;;   C-h a  apropos: the commands with some words in their names
;;;;   C-h d  apropos of the documentation: commands and variables
;;;;   C-h m  the mode of this buffer, and its keys
;;;;   C-h b  every key (F1)                 C-h l  the last keys typed
;;;;   C-h e  the messages there have been   C-h ?  this list
;;;;
;;;; (C-h o and C-c C-d a, in lisptools.lisp, ask about any Lisp symbol.)

(defun first-line (string) (subseq string 0 (position #\Newline string)))
(defun command-doc (command)
  (first-line (or (and (symbolp command) (documentation command 'function)) "")))

;;; ---- which keys
(defun map-bindings (function map &optional prefix)
  "Call FUNCTION with the keys (\"C-x 2\") and the command of every binding in MAP."
  (maphash (lambda (key value)
             (let ((keys (if prefix (concatenate 'string prefix " " key) key)))
               (if (hash-table-p value)
                   (map-bindings function value keys)
                   (funcall function keys value))))
           map))
(defun command-keys (command)
  "The keys that run COMMAND: a list of strings, a mode's keys with its name
after them (\"L [Dired]\")."
  (let ((found '()))
    (map-bindings (lambda (keys value) (when (eq value command) (push keys found))) *global-map*)
    (dolist (mode (reverse *modes*))
      (map-bindings (lambda (keys value)
                      (when (eq value command)
                        (push (format nil "~A [~A]" keys (mode-name mode)) found)))
                    (mode-map mode)))
    (sort found (lambda (a b) (or (< (length a) (length b))
                                  (and (= (length a) (length b)) (string< a b)))))))
(defun keys-phrase (command)
  (let ((keys (command-keys command)))
    (if keys (format nil "~{~A~^, ~}" keys) "M-x only")))

;;; ---- one thing
(defun function-help (symbol)
  "What there is to say about the function or command SYMBOL."
  (with-output-to-string (out)
    (let ((command (member symbol *commands*)))
      (format out "~(~A~) is ~:[a function~;a command~]~@[ of the package ~A~].~%"
              symbol command
              (and (symbol-package symbol) (package-name (symbol-package symbol))))
      (when command (format out "Keys: ~A~%" (keys-phrase symbol)))
      (let ((arguments (ignore-errors (function-arguments (fdefinition symbol)))))
        (when arguments (format out "Arguments: ~(~A~)~%" arguments)))
      (format out "~%~A~%" (or (documentation symbol 'function) "Not documented.")))))
(defun read-command-name (prompt)
  (let* ((name (string-trim " " (read-string prompt :complete (complete-from
                                                              (mapcar #'string-downcase *commands*)))))
         (command (find name *commands* :test #'string-equal)))
    (unless command (editor-error "No command named ~A" name))
    command))
(define-command describe-key-briefly ()
  "Say in the echo area what a key does."
  (draw-echo "Describe key briefly: ")
  (multiple-value-bind (command keys) (read-command)
    (if command
        (message "~A runs ~(~A~): ~A" keys command (command-doc command))
        (message "~A ~:[is not bound~;inserts itself~]" keys (= (length keys) 1)))))
(define-command describe-key ()
  "Show everything about what a key does."
  (draw-echo "Describe key: ")
  (multiple-value-bind (command keys) (read-command)
    (cond ((and command (symbolp command))
           (show-in-help "*help*" (format nil "~A runs the command ~(~A~).~%~%~A"
                                          keys command (function-help command))))
          (command (message "~A runs ~A" keys command))
          (t (message "~A ~:[is not bound~;inserts itself~]" keys (= (length keys) 1))))))
(define-command describe-command ()
  "Show the documentation of a command, and its keys."
  (show-in-help "*help*" (function-help (read-command-name "Describe command: "))))
(define-command describe-function ()
  "Show the documentation of a function."
  (let* ((name (read-string "Describe function: "
                            :complete (complete-from (mapcar #'string-downcase *commands*))))
         (symbol (or (find name *commands* :test #'string-equal)
                     (let ((*package* *repl-package*)) (read-from-string name)))))
    (unless (and (symbolp symbol) (fboundp symbol)) (editor-error "~A is not a function" name))
    (show-in-help "*help*" (function-help symbol))))
(define-command where-is ()
  "Say which keys run a command."
  (let* ((command (read-command-name "Where is command: "))
         (keys (command-keys command)))
    (if keys
        (message "~(~A~) is on ~{~A~^, ~}" command keys)
        (message "~(~A~) is not on any key: M-x ~(~A~)" command command))))

;;; ---- variables
(defun editor-variables ()
  "The editor's own variables: the special variables of the packages its commands are in."
  (let ((found '()))
    (dolist (package (remove-duplicates (remove nil (mapcar #'symbol-package *commands*))))
      (do-symbols (symbol package)
        (when (and (eq (symbol-package symbol) package) (boundp symbol)
                   (not (constantp symbol))
                   (special-variable-p symbol))
          (push symbol found))))
    found))
(define-command describe-variable ()
  "Show the value and the documentation of one of the editor's variables."
  (let* ((variables (append *local-variables* (editor-variables)))
         (name (string-trim " " (read-string "Describe variable: "
                                             :complete (complete-from
                                                        (mapcar #'string-downcase variables)))))
         (symbol (or (find name variables :test #'string-equal)
                     (let ((*package* *repl-package*)) (ignore-errors (read-from-string name))))))
    ;; a buffer-local variable: this buffer's value, and the one a buffer has that has not set it
    (when (member symbol *local-variables*)
      (let ((*package* (or (symbol-package symbol) *package*)) (*print-length* 30) (*print-level* 4))
        (show-in-help "*help*"
                      (format nil "~(~A~) is a buffer-local variable~:[~;, kept when the major mode changes~].~%~%~
Its value in ~A: ~S~:[  (the default)~;~]~%Its default: ~S~%~%~A~%"
                              symbol (get symbol 'permanent-local)
                              (buf-name *buf*) (buffer-local symbol) (local-variable-p symbol)
                              (default-value symbol)
                              (or (get symbol 'local-documentation) "Not documented."))))
      (return-from describe-variable))
    (unless (and symbol (symbolp symbol) (boundp symbol)) (editor-error "No variable named ~A" name))
    (show-in-help "*help*"
                  (let ((*package* (or (symbol-package symbol) *package*))
                        (*print-length* 30) (*print-level* 4) (*print-right-margin* 76))
                    (format nil "~(~A~) is a variable of the package ~A.~%~%Its value:~%~S~%~%~A~%"
                            symbol (package-name *package*) (symbol-value symbol)
                            (or (documentation symbol 'variable) "Not documented."))))))

;;; ---- apropos
(defun apropos-words (prompt)
  (let ((words (split-keys (string-trim " " (read-string prompt)))))
    (setf words (remove "" words :test #'string=))
    (unless words (editor-error "Apropos of what?"))
    words))
(defun mentions-p (words text)
  (every (lambda (word) (search word text :test #'char-equal)) words))
(defun command-rows (commands)
  (mapcar (lambda (command) (list command (keys-phrase command) (command-doc command)))
          (sort (copy-list commands) #'string<)))
(define-command apropos-command ()
  "List the commands whose names contain some words, with their keys."
  (let* ((words (apropos-words "Apropos command (words): "))
         (found (remove-if-not (lambda (command) (mentions-p words (string command))) *commands*)))
    (if (null found)
        (message "No command has ~{~A~^ and ~} in its name (C-h d looks in the documentation)" words)
        (show-in-help "*apropos*"
                      (format nil "~D command~:P with ~{~A~^ and ~} in the name.  ~
                                   C-h x describes one.~%~%~:{~(~A~)   (~A)~%    ~A~%~}"
                              (length found) words (command-rows found))))))
(define-command apropos-documentation ()
  "List the commands and variables whose documentation contains some words."
  (let* ((words (apropos-words "Apropos documentation (words): "))
         (commands (remove-if-not (lambda (command)
                                    (mentions-p words (or (documentation command 'function) "")))
                                  *commands*))
         (variables (sort (remove-if-not (lambda (variable)
                                           (mentions-p words (or (documentation variable 'variable) "")))
                                         (editor-variables))
                          #'string<)))
    (if (and (null commands) (null variables))
        (message "Nothing documented mentions ~{~A~^ and ~}" words)
        (show-in-help "*apropos*"
                      (format nil "Documentation that mentions ~{~A~^ and ~}: ~D command~:P, ~
                                   ~D variable~:P.~%~%~:{~(~A~)   (~A)~%    ~A~%~}~
                                   ~:{~(~A~)   (variable)~%    ~A~%~}"
                              words (length commands) (length variables) (command-rows commands)
                              (mapcar (lambda (variable)
                                        (list variable (first-line (documentation variable 'variable))))
                                      variables))))))

;;; ---- everything at once
(defun binding-rows (map)
  (let ((rows '()))
    (map-bindings (lambda (keys command) (push (list keys command (command-doc command)) rows)) map)
    (sort rows #'string< :key #'first)))
(define-command describe-bindings ()
  "List every key and what it does."
  (let ((mode (buf-mode *buf*)) (minor (active-minor-modes)))
    (show-in-help "*help*"
                  (with-output-to-string (out)
                    (format out "Key           Command                   What it does~%")
                    ;; in the order the keys are looked up: minor modes, the major mode, the rest
                    (dolist (m (append minor (mode-chain mode)))
                      (let ((rows (binding-rows (mode-map m))))
                        (when rows
                          (format out "~%In ~A mode:~%~:{~13A ~(~25A~) ~A~%~}" (mode-name m) rows))))
                    (format out "~%Everywhere:~%~:{~13A ~(~25A~) ~A~%~}" (binding-rows *global-map*))))))
(define-command describe-mode ()
  "Say what the modes of this buffer are for, major and minor, and list their keys."
  (let* ((mode (buf-mode *buf*)) (chain (mode-chain mode)) (minors (active-minor-modes))
         (locals (buf-locals *buf*)))
    (show-in-help
     "*help*"
     (with-output-to-string (out)
       ;; first what there is, in a few lines; then the keys, which are many
       (format out "This buffer, ~A, is in ~A mode (~(~A~)).~%~@[~%~A~%~]"
               (buf-name *buf*) (mode-name mode) mode (get mode 'mode-doc))
       (dolist (parent (rest chain))
         (format out "~%It is derived from ~A mode (~(~A~)).~%" (mode-name parent) parent))
       (dolist (minor minors)
         (format out "~%The minor mode ~A is on (M-x ~(~A~) turns it off).~%~@[~A~%~]"
                 (if (string= (mode-name minor) "") (string-downcase minor) (mode-name minor))
                 minor (get minor 'mode-doc)))
       (let ((hooks (loop for m in (append chain minors)
                          for functions = (hook-functions (format nil "~A-HOOK" m))
                          when functions collect (list m functions))))
         (when hooks
           (format out "~%Hooks:~%~:{  ~(~A~)-hook: ~{~(~A~)~^, ~}~%~}" hooks)))
       (when locals
         (format out "~%This buffer's own values:~%~:{  ~(~A~) = ~S~%~}"
                 (mapcar (lambda (cell) (list (car cell) (cdr cell))) (reverse locals))))
       (let ((off (remove-if #'minor-mode-p *minor-modes*)))
         (when off (format out "~%Minor modes that are off: ~{~(~A~)~^, ~}.~%" off)))
       ;; the keys, in the order they are looked up
       (dolist (minor minors)
         (let ((rows (binding-rows (mode-map minor))))
           (when rows
             (format out "~%The keys of ~A mode:~%~:{~13A ~(~25A~) ~A~%~}" (mode-name minor) rows))))
       (let ((rows (binding-rows (mode-map mode))))
         (format out "~%~:[It has no keys of its own.~%~;Its own keys:~%~]~:{~13A ~(~25A~) ~A~%~}" rows rows))
       (dolist (parent (rest chain))
         (let ((rows (binding-rows (mode-map parent))))
           (when rows
             (format out "~%The keys of ~A mode, which it is derived from:~%~:{~13A ~(~25A~) ~A~%~}"
                     (mode-name parent) rows))))
       (format out "~%The keys that work everywhere are listed by C-h b.~%")))))
(define-command view-lossage ()
  "Show the last keys typed."
  (show-in-help "*help*"
                (with-output-to-string (out)
                  (format out "The last ~D keys typed, oldest first:~%~%" (length *lossage*))
                  (let ((column 0))
                    (dolist (key (reverse *lossage*))
                      (when (> (+ column (length key)) 72) (terpri out) (setf column 0))
                      (format out "~A " key)
                      (incf column (1+ (length key)))))
                  (terpri out))))
(define-command view-echo-area-messages ()
  "Show the messages there have been in the echo area."
  (show-in-help "*messages*" (format nil "~{~A~%~}" (reverse *message-log*)))
  (end-of-buffer))
(define-command help-for-help ()
  "List the help commands."
  (show-in-help "*help*" (format nil "The editor's help.  Each of these is C-h and then a key:~%~%~
~:{  C-h ~3A ~(~26A~) ~A~%~}~%F1 lists every key.  M-x runs any command by name, with TAB to complete it.~%"
                                 (mapcar (lambda (row)
                                           (list (subseq (first row) 4) (second row) (third row)))
                                         (remove-if-not (lambda (row)
                                                          (and (> (length (first row)) 4)
                                                               (string= "C-h " (first row) :end2 4)))
                                                        (binding-rows *global-map*))))))
(define-command execute-command ()
  "Run a command by name."
  (let* ((name (read-string "M-x " :complete (complete-from (mapcar #'string-downcase *commands*))))
         (command (find name *commands* :test #'string-equal)))
    (unless command (editor-error "No command named ~A" name))
    (setf *this-command* command)
    (funcall command)))
(define-command exit-editor ()
  "Leave the editor."
  (when (and (find-if #'buffer-precious-p *buffers*)
             (not (ask-y-or-n "Modified buffers exist; exit anyway?")))
    (error 'quit-command))
  (throw 'exit-editor nil))
