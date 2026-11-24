;;;; The examples: a registry, so that they can be listed and run by name.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples
  (:use #:cl)
  (:documentation "The DOSLIB examples.  Each one lives in a package of its
own, DOSLIB-EXAMPLES.<NAME>, with a MAIN function, and is registered here:
  (doslib-examples:list-examples)
  (doslib-examples:run \"plasma\")
  (doslib-examples:menu)")
  (:export #:define-example #:run #:run-arguments #:list-examples #:list-names #:example-names #:menu #:data-file
           #:build-executable #:save-core))
(in-package #:doslib-examples)

(defstruct example name function category description)
(defvar *examples* '() "Every example defined, as EXAMPLE structures.")

(defun define-example (name function &key (category :other) (description ""))
  "Make the example NAME known: FUNCTION (a symbol) runs it."
  (setf *examples* (remove name *examples* :key #'example-name :test #'string-equal))
  (push (make-example :name name :function function :category category :description description)
        *examples*)
  name)

(defun find-example (name)
  (or (find (string name) *examples* :key #'example-name :test #'string-equal)
      (error "There is no example named ~A.  (doslib-examples:list-examples) shows them." name)))

(defun run (name &rest arguments)
  "Run the example NAME (a string or symbol), passing ARGUMENTS to its MAIN.
Ctrl-Break (or Ctrl-Alt-Shift) stops it: the screen goes back to text, the
speaker is silenced, and RUN returns."
  (let ((example (find-example name)))
    (when (and (eq (example-category example) :graphics)
               (not (member (doslib:display-adapter) '(:vga :mcga))))
      (format t "~&~A draws in the VGA's modes, and this machine's display card is ~:[an~;a~] ~A.~%"
              (example-name example) (member (doslib:display-adapter) '(:cga :mda :hercules))
              (doslib:display-adapter))
      (return-from run (values)))
    (when (eq (doslib:catching-interrupt (apply (example-function example) arguments) nil)
              :interrupted)
      (doslib:speaker-off)
      (unless (= (doslib:current-video-mode) 3) (doslib:video-mode 3))
      (doslib:show-text-cursor)
      (format t "~&~A was interrupted.~%" (example-name example))))
  (values))

(defun command-line-arguments ()
  "What follows --end-toplevel-options on the command line.  (SBCL takes its
own options out of *POSIX-ARGV*, that one included, when it has read them.)"
  (let ((marked (member "--end-toplevel-options" sb-ext:*posix-argv* :test #'string=)))
    (rest (or marked sb-ext:*posix-argv*))))

(defun run-arguments (&optional (arguments (command-line-arguments)))
  "Run the example named by the first of ARGUMENTS, passing it the rest: by
default, what follows --end-toplevel-options on the command line, which is
how DEMO.BAT passes on what was typed after DEMO.  An argument that is a
whole number is passed as a number, anything else as a string."
  (if arguments
      (apply #'run (first arguments)
             (mapcar (lambda (argument)
                       (if (and (plusp (length argument)) (every #'digit-char-p argument))
                           (parse-integer argument)
                           argument))
                     (rest arguments)))
      (menu)))

(defun list-examples (&optional (stream *standard-output*))
  "Print the examples, by category."
  (dolist (category '(:graphics :system :lisp :sound :network :other))
    (let ((examples (sort (remove category *examples* :key #'example-category :test-not #'eq)
                          #'string< :key #'example-name)))
      (when examples
        (format stream "~&~%~:(~A~)~%" category)
        (dolist (example examples)
          (format stream "  ~10A ~A~%" (example-name example) (example-description example))))))
  (values))

(defun example-names ()
  "The names of all the examples, in alphabetical order."
  (sort (mapcar #'example-name *examples*) #'string<))

(defun list-names (&optional (stream *standard-output*))
  "Print just the names of the examples, by category: it fits on one screen."
  (dolist (category '(:graphics :system :lisp :sound :network :other))
    (let ((names (sort (mapcar #'example-name
                               (remove category *examples* :key #'example-category :test-not #'eq))
                       #'string<)))
      (when names
        (format stream "~&~:(~A~)~%" category)
        (loop while names
              do (format stream "  ~{~12A~}~%"
                         (loop repeat 6 while names collect (pop names)))))))
  (values))

(defun menu ()
  "Run the examples asked for by name, until an empty line or q.
? lists their names again, and ?? lists them with descriptions."
  (list-names)
  (loop
    (format t "~&~%Example to run (? lists them, ?? describes them, Enter leaves): ")
    (finish-output)
    ;; (a line typed at the DOS console ends with a carriage return)
    (let ((name (string-trim '(#\Space #\Tab #\Return)
                             (or (read-line *standard-input* nil) ""))))
      (cond ((member name '("" "q" "quit" "exit") :test #'string-equal) (return))
            ((string= name "?") (list-names))
            ((string= name "??") (list-examples))
            ((find name *examples* :key #'example-name :test #'string-equal)
             (handler-case (run name)
               (error (e) (format t "~&~A stopped with an error: ~A~%" name e))))
            (t (format t "~&There is no example named ~A.~%" name))))))

(defun data-file (name)
  "The native name of a file in the examples' data directory."
  (sb-ext:native-namestring
   (asdf:system-relative-pathname "doslib/examples" (concatenate 'string "examples/data/" name))))

(defun build-executable (name file)
  "Save the example NAME as a stand-alone program in FILE.  This ends the Lisp session."
  (let ((function (example-function (find-example name))))
    (sb-ext:save-lisp-and-die file :executable t
                                   :toplevel (lambda () (funcall function) (sb-ext:exit)))))

(defun save-core (&optional (file "doslib.cor"))
  "Save a core with the examples already loaded, so that they start quickly:
sbcl --core doslib.cor.  This ends the Lisp session."
  ;; (a core saved by "sbcl --non-interactive" would otherwise remember that
  ;; errors are to end the session, not open the debugger)
  (sb-ext:enable-debugger)
  (sb-ext:save-lisp-and-die file))
