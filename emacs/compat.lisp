;;;; What the editor needs from the Lisp it runs on, beyond what the
;;;; library does: SBCL or CLISP.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS.

(in-package #:doslib-emacs)

;;; ---- functions and variables
(defun function-arguments (function)
  "The lambda list of FUNCTION."
  #+sbcl (if (typep function 'generic-function)
             (sb-mop:generic-function-lambda-list function)
             (sb-kernel:%fun-lambda-list function))
  #+clisp (ext:arglist function))
(defun special-variable-p (symbol)
  "Has SYMBOL been proclaimed special?"
  #+sbcl (eq (sb-int:info :variable :kind symbol) :special)
  #+clisp (ext:special-variable-p symbol))
(defun function-source-file (symbol function)
  "The file the compiler says FUNCTION, the definition of SYMBOL, came from, or NIL."
  #+sbcl (declare (ignore symbol))
  #+sbcl (let ((code (sb-kernel:fun-code-header (sb-kernel:%fun-fun function))))
           (and code (sb-c::debug-source-namestring
                      (sb-c::debug-info-source (sb-kernel:%code-debug-info code)))))
  #+clisp (declare (ignore function))
  #+clisp (let ((places (documentation symbol 'sys::file)))    ; ((kind file from to) ...)
            (loop for place in places
                  for file = (and (consp place) (second place))
                  ;; a compiled file is named: its source is beside it
                  when file
                    return (namestring (or (find-if #'probe-file
                                                    (list (make-pathname :type "lisp" :defaults file)
                                                          (make-pathname :type "lsp" :defaults file)))
                                           file)))))

(defun external-apropos (text)
  "The external symbols, of any package, with TEXT in their names."
  #+sbcl (apropos-list text nil t)
  #+clisp (remove-if-not (lambda (symbol)
                           (let ((package (symbol-package symbol)))
                             (and package
                                  (eq (nth-value 1 (find-symbol (symbol-name symbol) package))
                                      :external))))
                         (apropos-list text nil)))

;;; ---- the debugger
(defun backtrace-string (count)
  "The top COUNT frames of the stack, as text."
  (with-output-to-string (out)
    #+sbcl (sb-debug:print-backtrace :stream out :count count)
    #+clisp (let ((*standard-output* out)) (ext:show-stack 4 count))))

;;; ---- vectors
(defun first-difference (old new end)
  "The first index below END at which the vectors OLD and NEW do not hold
the same (EQ) object, or END."
  #+sbcl (let ((old (sb-ext:array-storage-vector old))
               (new (sb-ext:array-storage-vector new)))
           (declare (type simple-vector old new) (type fixnum end)
                    (optimize (speed 2) (safety 0)))
           (or (loop for i of-type fixnum from 0 below end
                     unless (eq (svref old i) (svref new i)) return i)
               end))
  #+clisp (or (mismatch old new :end1 end :end2 end :test #'eq) end))

;;; ---- the program
(defun command-line-file ()
  "The file named on the command line, if one was."
  (first (command-line)))
