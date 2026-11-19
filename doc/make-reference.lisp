;;;; Make doc/reference.md from the sources: every exported name, with its
;;;; arguments and its documentation string, file by file.
;;;;
;;;;   sbcl --script doc/make-reference.lisp          (any SBCL: nothing is loaded,
;;;;                                                   the sources are only read)
;;;;
;;;; A name that is exported but has no documentation string is listed at
;;;; the end of the run, and the script exits with status 1.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

;;; The sources name symbols in these packages.  (Whatever ASDF has to say
;;; about this computer's own libraries is not wanted here.)
(let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (require :asdf)
    (require :sb-bsd-sockets)))
(defpackage #:doslib-reference (:use #:cl))
(in-package #:doslib-reference)

(defparameter *root*
  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(defparameter *parts*
  ;; title, package, the file that defines the package, directory, files in order
  ;; (the editor's commands say what they do; its other names are listed bare)
  '(("The library" "DOSLIB" "src/package.lisp" "src/"
     ("machine" "ports" "dos" "cpuid" "memory" "interrupts" "hotkey" "video" "gtext" "modex" "input" "text"
      "critical" "dialog" "sound" "mouse" "disk" "dma" "blaster" "pci" "serial" "init"))
    ("The examples" "DOSLIB-EXAMPLES" "examples/package.lisp" "examples/" ("package"))
    ("The editor" "DOSLIB-EMACS" "emacs/package.lisp" "emacs/"
     ("buffers" "screen" "keyboard" "keys" "keymaps" "minibuffer" "movement" "editing" "search"
      "files" "windows" "help" "macros" "lisp" "repl" "dired" "main"))))

(defun read-forms (file)
  (with-open-file (in file)
    (let ((eof (gensym)))
      (loop for form = (read in nil eof) until (eq form eof) collect form))))

(defun file-title (file)
  "The first comment line of FILE, without its semicolons."
  (with-open-file (in file)
    (string-trim " ;" (read-line in))))

(defun definition (form)
  "If FORM defines something: its name, kind, arguments (or :NONE) and documentation."
  (when (consp form)
    (destructuring-bind (operator &optional name &rest rest) form
      (flet ((doc (candidate) (and (stringp candidate) candidate)))
        (case operator
          ((defun defmacro)
           (values name (if (eq operator 'defun) "Function" "Macro") (first rest)
                   (and (cddr rest) (doc (second rest)))))
          ((defvar defparameter defconstant)
           (values name (if (eq operator 'defconstant) "Constant" "Variable") :none (doc (second rest))))
          (deftype (values name "Type" :none (and (cddr rest) (doc (second rest)))))
          (define-condition
           (values name "Condition" :none
                   (second (find :documentation (cddr rest) :key (lambda (o) (and (consp o) (car o)))))))
          (defstruct
           (values (if (consp name) (car name) name) "Structure" :none (doc (first rest))))
          ;; the editor's own defining forms
          (t (when (and (symbolp operator) (string= operator "DEFINE-COMMAND"))
               (values name "Command" (first rest) (and (cddr rest) (doc (second rest)))))))))))

(defun show-name (name)
  (let ((*package* (find-package "KEYWORD")) (*print-case* :downcase))
    (if (consp name)                    ; (setf x)
        (format nil "(setf ~(~A~))" (symbol-name (second name)))
        (string-downcase (symbol-name name)))))

(defun show-arguments (arguments)
  (let ((*print-case* :downcase) (*print-pretty* nil) (*package* (find-package "DOSLIB-REFERENCE")))
    (labels ((plain (x)                 ; without package prefixes
               (cond ((null x) "()")
                     ((keywordp x) (format nil ":~(~A~)" x))
                     ((symbolp x) (string-downcase (symbol-name x)))
                     ((consp x) (format nil "(~{~A~^ ~})" (mapcar #'plain x)))
                     (t (format nil "~S" x)))))
      (if (null arguments) "" (format nil " ~{~A~^ ~}" (mapcar #'plain arguments))))))

(defun exported-p (name package)
  (let ((symbol (if (consp name) (second name) name)))
    (and (symbolp symbol)
         (eq (nth-value 1 (find-symbol (symbol-name symbol) package)) :external)
         (eq (symbol-package symbol) package))))

(defvar *missing* '())

(defun write-part (out title package-name package-file directory files)
  (dolist (form (read-forms (merge-pathnames package-file *root*)))     ; make the package
    (when (and (consp form) (eq (car form) 'defpackage))
      (handler-bind ((warning #'muffle-warning)) (eval form))))
  (let ((package (find-package package-name)) (*package* (find-package package-name)))
    (format out "~%## ~A~%~%Package `~(~A~)`.~%" title package-name)
    (dolist (file files)
      (let* ((path (merge-pathnames (format nil "~A~A.lisp" directory file) *root*))
             (entries '()))
        (dolist (form (read-forms path))
          (when (and (consp form) (eq (car form) 'in-package)) (setf *package* (find-package (second form))))
          (multiple-value-bind (name kind arguments doc) (definition form)
            (when (and name (exported-p name package))
              (push (list name kind arguments doc) entries))
            (when (equal kind "Condition")      ; and the readers of its slots
              (dolist (slot (fourth form))
                (let ((reader (and (consp slot) (getf (rest slot) :reader))))
                  (when (and reader (exported-p reader package))
                    (push (list reader "Function" '(condition)
                                (format nil "The ~(~A~) of a ~(~A~) condition." (first slot) name))
                          entries)))))))
        (when entries
          (format out "~%### `~A~A.lisp`: ~A~%" directory file (file-title path))
          (dolist (entry (nreverse entries))
            (destructuring-bind (name kind arguments doc) entry
              (format out "~%**`~A`**~:[ `~A`~;~*~] — ~A  ~%" (show-name name) (eq arguments :none)
                      (if (eq arguments :none) "" (string-trim " " (show-arguments arguments))) kind)
              (if doc
                  (format out "~A~%" (substitute #\Space #\Newline doc))
                  (unless (string= package-name "DOSLIB-EMACS")
                    (push (format nil "~A~A.lisp: ~A" directory file (show-name name)) *missing*))))))))
    ;; exported, but defined by none of the forms we know
    (let ((defined (make-hash-table)))
      (dolist (file files)
        (dolist (form (read-forms (merge-pathnames (format nil "~A~A.lisp" directory file) *root*)))
          (let ((name (definition form)))
            (when name (setf (gethash (if (consp name) (second name) name) defined) t))
            (when (and (consp form) (eq (car form) 'define-condition))
              (dolist (slot (fourth form))
                (let ((reader (and (consp slot) (getf (rest slot) :reader))))
                  (when reader (setf (gethash reader defined) t))))))))
      (do-external-symbols (symbol package)
        (unless (or (gethash symbol defined) (not (eq (symbol-package symbol) package))
                    (string= package-name "DOSLIB-EMACS"))
          (push (format nil "~A: ~A (exported; no definition found)" directory (show-name symbol)) *missing*))))))

(with-open-file (out (merge-pathnames "doc/reference.md" *root*) :direction :output :if-exists :supersede)
  (format out "# doslib reference~%~%Every exported name, file by file, with its arguments and what its~%~
documentation string says.  This file is made by `doc/make-reference.lisp`~%~
from the sources: change the documentation string, not this file.~%")
  (dolist (part *parts*) (apply #'write-part out part)))

(when *missing*
  (format t "~D exported name~:P without documentation:~%~{  ~A~%~}" (length *missing*) (reverse *missing*))
  (sb-ext:exit :code 1))
(format t "doc/reference.md written.~%")
