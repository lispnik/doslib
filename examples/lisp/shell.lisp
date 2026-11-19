;;;; A DOS shell in Lisp.  Lines that look like DOS commands
;;;; (dir *.exe, cd \, type file, copy a b ...) are run as commands; lines
;;;; that start with a parenthesis are Lisp.  The two share everything: FILES
;;;; returns the directory as a list of objects, so "which is the biggest
;;;; file" is an ordinary Lisp expression instead of a batch-file puzzle.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.shell
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.shell)

(defstruct entry name size time directory)
(defun name (e) (entry-name e))
(defun size (e) (entry-size e))
(defun cwd ()                           ; "D:\SBCL", or "D:\" at a root
  (let ((d (sb-unix:posix-getcwd)))
    (if (= (length d) 2) (concatenate 'string d "\\") d)))
(defun match-p (pattern string)         ; DOS wildcards: * and ?
  (cond ((string= pattern "") (string= string ""))
        ((char= (char pattern 0) #\*)
         (or (match-p (subseq pattern 1) string)
             (and (string/= string "") (match-p pattern (subseq string 1)))))
        ((string= string "") nil)
        ((or (char= (char pattern 0) #\?) (char-equal (char pattern 0) (char string 0)))
         (match-p (subseq pattern 1) (subseq string 1)))))
(defun files (&optional (pattern "*"))
  "The current directory's entries matching PATTERN, as a list of ENTRY objects."
  (when (string= pattern "*.*") (setf pattern "*"))
  (let ((result '()))
    (sb-impl::map-directory
     (lambda (path)
       (let* ((directory (null (pathname-name path)))
              (name (if directory
                        (car (last (pathname-directory path)))
                        (file-namestring path))))
         (when (match-p pattern name)
           (push (make-entry :name name :directory directory
                             :time (or (ignore-errors (file-write-date path)) 0)
                             :size (if directory 0
                                       (with-open-file (in path :element-type '(unsigned-byte 8))
                                         (file-length in))))
                 result))))
     (concatenate 'string (string-right-trim "\\" (cwd)) "\\"))
    (sort result #'string-lessp :key #'entry-name)))
(defun free-bytes ()
  (multiple-value-bind (ax bx cx) (int86 #x21 :ax #x3600 :dx 0)
    (* (word16 ax) (word16 bx) (word16 cx))))
;;; ---- the commands
(defvar *commands* '())
(defmacro defcommand (name (&rest args) help &body body)
  ;; DIR becomes the function CMD-DIR: several command names (TYPE, for one)
  ;; are already Common Lisp symbols
  (let ((function (intern (format nil "CMD-~A" name))))
    `(progn (defun ,function ,args ,@body)
            (pushnew (list ,(string name) ',function ,help) *commands*
                     :key #'car :test #'string=))))
(defcommand dir (&optional (pattern "*")) "list files:  dir [pattern]"
  (let ((list (files pattern)))
    (dolist (e list)
      (multiple-value-bind (s mi h d mo y) (decode-universal-time (entry-time e) 0)
        (declare (ignore s))
        (format t "~:[~12:D~;       <DIR>~*~]  ~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D  ~A~%"
                (entry-directory e) (entry-size e) y mo d h mi (entry-name e))))
    (format t "~8D file~:P ~14:D bytes~%~22T~14:D bytes free~%"
            (count-if-not #'entry-directory list) (reduce #'+ (mapcar #'entry-size list))
            (free-bytes))
    (values)))
(defcommand cd (&optional path) "change or show the directory:  cd [path]"
  (when path
    (when (and (> (length path) 1) (char= (char path 1) #\:))      ; a drive letter too
      (int86 #x21 :ax #x0E00 :dx (- (char-code (char-upcase (char path 0))) 65)))
    (when (logbitp 0 (nth-value 6 (real-int #x21 :ax #x3B00 :ds (dos-string path) :dx 0)))
      (format t "No such directory: ~A~%" path))
    (setf *default-pathname-defaults*
          (pathname (concatenate 'string (string-right-trim "\\" (cwd)) "\\"))))
  (unless path (format t "~A~%" (cwd)))
  (values))
(defcommand type (file) "show a text file:  type file"
  (with-open-file (in file)
    (loop for line = (read-line in nil) while line do (write-line (string-right-trim '(#\Return) line))))
  (values))
(defcommand copy (from to) "copy a file:  copy from to"
  (with-open-file (in from :element-type '(unsigned-byte 8))
    (with-open-file (out to :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
      (let ((buffer (make-array 16384 :element-type '(unsigned-byte 8))) (total 0))
        (loop for n = (read-sequence buffer in) while (plusp n)
              do (write-sequence buffer out :end n) (incf total n))
        (format t "~:D bytes copied to ~A~%" total to))))
  (values))
(defcommand del (file) "delete files:  del pattern"
  (dolist (e (files file)) (delete-file (entry-name e)) (format t "deleted ~A~%" (entry-name e)))
  (values))
(defcommand ren (from to) "rename a file:  ren from to" (rename-file from to) (values))
(defcommand md (name) "make a directory:  md name"
  (ensure-directories-exist (format nil "~A\\~A\\" (string-right-trim "\\" (cwd)) name)) (values))
(defcommand cls () "clear the screen" (video-mode 3) (values))
(defcommand ver () "versions"
  (format t "~A ~A~%~A ~A~%" (lisp-implementation-type) (lisp-implementation-version)
          (software-type) (software-version))
  (values))
(defcommand mem () "memory"
  (let ((info (alien-sap (make-alien (unsigned 8) 48))))
    (int86 #x31 :ax #x0500 :di (sap-int info))
    (format t "~12:D KB physical memory free~%~12:D KB Lisp heap in use, of ~:D KB~%"
            (* 4 (sap-ref-32 info #x14)) (floor (sb-kernel:dynamic-usage) 1024)
            (floor (sb-ext:dynamic-space-size) 1024)))
  (values))
(defcommand help () "this list"
  (dolist (c (reverse *commands*)) (format t "  ~(~6A~) ~A~%" (first c) (third c)))
  (format t "  exit   leave the shell~%  (...)  anything in parentheses is Lisp, e.g. (length (files \"*.lsp\"))~%")
  (values))
;;; ---- the read-eval-print loop
(defun words (line)
  (loop with start = 0
        for end = (position #\Space line :start start)
        for word = (subseq line start end)
        unless (string= word "") collect word
        while end do (setf start (1+ end))))
(defun shell ()
  (format t "~&SBCL shell.  DOS commands or (Lisp).  Type help.~%")
  (loop
    (format t "~A> " (cwd))
    (finish-output)
    (let ((line (string-trim '(#\Space #\Return) (or (read-line *standard-input* nil) "exit"))))
      (handler-case
          (cond ((string= line ""))
                ((string-equal line "exit") (return))
                ((find (char line 0) "(#'\"")      ; Lisp
                 (format t "~{~S~%~}" (multiple-value-list (eval (read-from-string line)))))
                (t (let* ((words (words line))
                          (command (find (first words) *commands* :key #'car :test #'string-equal)))
                     (if command
                         (apply (second command) (rest words))
                         (format t "Bad command or file name: ~A~%" (first words))))))
        (error (e) (format t "Error: ~A~%" e))))))

(defun main ()
  "A DOS shell in Lisp."
  (let ((*package* (find-package '#:doslib-examples.shell)))   ; Lisp lines are read here
    (shell)))

(doslib-examples:define-example "shell" 'main
  :category :lisp
  :description "A DOS shell in Lisp.")
