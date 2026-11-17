;;;; Help
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun first-line (string) (subseq string 0 (position #\Newline string)))
(defun command-doc (command)
  (first-line (or (and (symbolp command) (documentation command 'function)) "")))
(define-command describe-key ()
  "Say what a key does."
  (draw-echo "Describe key: ")
  (multiple-value-bind (command keys) (read-command)
    (if command
        (message "~A runs ~(~A~): ~A" keys command (command-doc command))
        (message "~A ~:[is not bound~;inserts itself~]" keys (= (length keys) 1)))))
(define-command describe-bindings ()
  "List every key and what it does."
  (let ((rows '()))
    (labels ((walk (map prefix where)
               (maphash (lambda (key value)
                          (let ((keys (if prefix (concatenate 'string prefix " " key) key)))
                            (if (hash-table-p value)
                                (walk value keys where)
                                (push (list keys value where) rows))))
                        map)))
      (walk *global-map* nil "")
      (walk (mode-map (buf-mode *buf*)) nil (mode-name (buf-mode *buf*))))
    (show-in-help "*help*"
                  (format nil "Key           Command                   What it does~%~
~:{~13A ~(~25A~) ~A~@[  [~A]~]~%~}"
                          (mapcar (lambda (row)
                                    (list (first row) (second row) (command-doc (second row))
                                          (and (string/= (third row) "") (third row))))
                                  (sort rows #'string< :key #'first))))))
(define-command describe-function ()
  "Show the documentation of a function."
  (let* ((name (read-string "Describe function: "
                            :complete (complete-from (mapcar #'string-downcase *commands*))))
         (symbol (let ((*package* *repl-package*)) (read-from-string name))))
    (show-in-help "*help*"
                  (format nil "~(~A~)~@[ ~(~A~)~]~%~%~A~%"
                          symbol
                          (ignore-errors (sb-kernel:%fun-lambda-list (fdefinition symbol)))
                          (or (documentation symbol 'function) "Not documented.")))))
(define-command execute-command ()
  "Run a command by name."
  (let* ((name (read-string "M-x " :complete (complete-from (mapcar #'string-downcase *commands*))))
         (command (find name *commands* :test #'string-equal)))
    (unless command (editor-error "No command named ~A" name))
    (setf *this-command* command)
    (funcall command)))
(define-command exit-editor ()
  "Leave the editor."
  (when (and (find-if (lambda (b) (and (buf-modified b) (buf-file b))) *buffers*)
             (not (ask-y-or-n "Modified buffers exist; exit anyway?")))
    (error 'quit-command))
  (throw 'exit-editor nil))
