;;;; What the library needs from the Lisp it runs on: SBCL or CLISP.
;;;;
;;;; The library is written with SBCL's names for memory and foreign calls
;;;; (SAP-REF-8, INT-SAP, MAKE-ALIEN, ALIEN-FUNCALL).  On SBCL they are
;;;; SBCL's own.  On CLISP -- the DOS port, built with DJGPP, which has the
;;;; functions SYS::DOS-PEEK, DOS-POKE, DOS-MALLOC, DOS-CALL and DOS-NEAR --
;;;; this file defines as many of them as the library uses.
;;;;
;;;; Addresses.  Everywhere in the library an address is a linear address:
;;;; 0 is the start of the machine's memory, so B8000h is the text screen.
;;;; In SBCL's DOS port that is also the address the program's own code
;;;; uses.  A DJGPP program's segment starts somewhere else, so there the
;;;; two differ, and an address that machine code or a DPMI call is to use
;;;; relative to DS, CS or ES goes through NEAR-ADDRESS first.  ALIEN-FUNCALL
;;;; does that for arguments declared SYSTEM-AREA-POINTER.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS.

(defpackage #:doslib-compat
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-sys)
  (:export #:near-address #:environment-variable #:add-init-hook #:add-exit-hook
           #:interrupt-condition #:foreign-variable-address
           #:copy-memory #:copy-vector-to-memory #:copy-memory-to-vector
           #:with-vector-memory
           #:heap-bytes-used #:heap-bytes-limit #:bytes-allocated #:collect-garbage
           #:code-bytes #:disassembly-string #:time-of-day #:exit-lisp #:save-program #:command-line
           #:current-directory-string #:directory-exists-p #:list-directory
           #:file-kind-and-size #:remove-file #:move-file #:create-directory
           #:write-bytes-to-device #:open-latin-1-file
           #+clisp #:free-alien #+clisp #:sap+ #+clisp #:signed-sap-ref-8
           #+clisp #:signed-sap-ref-16 #+clisp #:signed-sap-ref-32
           #+clisp #:int-sap #+clisp #:sap-int #+clisp #:sap-ref-8
           #+clisp #:sap-ref-16 #+clisp #:sap-ref-32 #+clisp #:make-alien
           #+clisp #:alien-sap #+clisp #:sap-alien #+clisp #:alien-funcall
           #+clisp #:system-area-pointer #+clisp #:unsigned #+clisp #:void
           #+clisp #:int))
(in-package #:doslib-compat)

#-(or sbcl clisp) (error "DOSLIB needs the DOS port of SBCL or of CLISP.")

#+sbcl
(progn
  (declaim (inline near-address))
  (defun near-address (address)
    "ADDRESS (an integer) as the program's own code and the DPMI host know it."
    address)
  (defun copy-memory (from from-offset to to-offset count)
    "Copy COUNT bytes from the address FROM + FROM-OFFSET to TO + TO-OFFSET."
    (sb-kernel:system-area-ub8-copy from from-offset to to-offset count))
  (defun copy-vector-to-memory (vector start sap offset count)
    "Copy COUNT bytes of the (unsigned-byte 8) VECTOR, from START, to the address SAP + OFFSET."
    (with-pinned-objects (vector)
      (sb-kernel:system-area-ub8-copy (vector-sap vector) start sap offset count)))
  (defun copy-memory-to-vector (sap offset vector start count)
    "Copy COUNT bytes at the address SAP + OFFSET into the (unsigned-byte 8) VECTOR, from START."
    (with-pinned-objects (vector)
      (sb-kernel:system-area-ub8-copy sap offset (vector-sap vector) start count)))
  (defmacro with-vector-memory ((sap vector &key (write-back t)) &body body)
    "Run BODY with SAP bound to an address at which the bytes of the simple
(unsigned-byte 8) VECTOR are, for code that works on memory.  What BODY
writes there is in VECTOR afterwards, unless WRITE-BACK is false.  BODY
must not use VECTOR itself."
    (declare (ignore write-back))
    `(with-pinned-objects (,vector)
       (let ((,sap (vector-sap ,vector))) ,@body)))
  (defun environment-variable (name) (sb-ext:posix-getenv name))
  (defun add-init-hook (function) (pushnew function sb-ext:*init-hooks*))
  (defun add-exit-hook (function) (pushnew function sb-ext:*exit-hooks*))
  (deftype interrupt-condition () 'sb-sys:interactive-interrupt)
  (defun foreign-variable-address (name)
    (sb-sys:find-foreign-symbol-address name)))

#+clisp
(progn
  (unless (fboundp 'sys::dos-peek)
    (error "This CLISP has no SYS::DOS-PEEK: DOSLIB needs the DOS port."))
  (deftype system-area-pointer () '(unsigned-byte 32))
  (declaim (inline int-sap sap-int alien-sap near-address
                   sap-ref-8 sap-ref-16 sap-ref-32))
  (defun int-sap (address) address)
  (defun sap-int (sap) sap)
  (defun alien-sap (alien) alien)
  (defun near-address (address)
    "ADDRESS (an integer) as the program's own code and the DPMI host know it."
    (sys::dos-near address))
  (defun sap-ref-8 (sap offset) (sys::dos-peek (+ sap offset) 1))
  (defun sap-ref-16 (sap offset) (sys::dos-peek (+ sap offset) 2))
  (defun sap-ref-32 (sap offset) (sys::dos-peek (+ sap offset) 4))
  (defsetf sap-ref-8 (sap offset) (value) `(sys::dos-poke (+ ,sap ,offset) 1 ,value))
  (defsetf sap-ref-16 (sap offset) (value) `(sys::dos-poke (+ ,sap ,offset) 2 ,value))
  (defsetf sap-ref-32 (sap offset) (value) `(sys::dos-poke (+ ,sap ,offset) 4 ,value))

  (declaim (inline sap+ signed-sap-ref-8 signed-sap-ref-16 signed-sap-ref-32))
  (defun sap+ (sap offset) (+ sap offset))
  (defun signed-sap-ref-8 (sap offset)
    (let ((value (sys::dos-peek (+ sap offset) 1))) (if (logbitp 7 value) (- value #x100) value)))
  (defun signed-sap-ref-16 (sap offset)
    (let ((value (sys::dos-peek (+ sap offset) 2))) (if (logbitp 15 value) (- value #x10000) value)))
  (defun signed-sap-ref-32 (sap offset)
    (let ((value (sys::dos-peek (+ sap offset) 4))) (if (logbitp 31 value) (- value #x100000000) value)))
  (defsetf signed-sap-ref-8 (sap offset) (value) `(sys::dos-poke (+ ,sap ,offset) 1 (ldb (byte 8 0) ,value)))
  (defsetf signed-sap-ref-16 (sap offset) (value) `(sys::dos-poke (+ ,sap ,offset) 2 (ldb (byte 16 0) ,value)))
  (defsetf signed-sap-ref-32 (sap offset) (value) `(sys::dos-poke (+ ,sap ,offset) 4 (ldb (byte 32 0) ,value)))
  (defun free-alien (alien) (sys::dos-free alien) nil)
  (defun copy-memory (from from-offset to to-offset count)
    "Copy COUNT bytes from the address FROM + FROM-OFFSET to TO + TO-OFFSET."
    (sys::dos-move (+ to to-offset) (+ from from-offset) count))
  (defun copy-vector-to-memory (vector start sap offset count)
    "Copy COUNT bytes of the (unsigned-byte 8) VECTOR, from START, to the address SAP + OFFSET."
    (sys::dos-put (+ sap offset) vector start count))
  (defun copy-memory-to-vector (sap offset vector start count)
    "Copy COUNT bytes at the address SAP + OFFSET into the (unsigned-byte 8) VECTOR, from START."
    (sys::dos-get (+ sap offset) vector start count))
  (defmacro with-vector-memory ((sap vector &key (write-back t)) &body body)
    "Run BODY with SAP bound to an address at which the bytes of the simple
(unsigned-byte 8) VECTOR are, for code that works on memory.  What BODY
writes there is in VECTOR afterwards, unless WRITE-BACK is false.  BODY
must not use VECTOR itself.  (CLISP's vectors move, so there the bytes are
copied out of the Lisp heap and back.)"
    (let ((v (gensym "VECTOR")) (n (gensym "BYTES")))
      `(let* ((,v ,vector) (,n (length ,v))
              (,sap (or (sys::dos-malloc ,n) (error "No memory"))))
         (unwind-protect
              (progn (sys::dos-put ,sap ,v 0 ,n)
                     (multiple-value-prog1 (progn ,@body)
                       ,@(when write-back `((sys::dos-get ,sap ,v 0 ,n)))))
           (sys::dos-free ,sap)))))

  (defun type-named-p (type name)
    (and (symbolp type) (string= type name)))
  (defmacro make-alien (type count)
    "Memory for COUNT of TYPE -- (unsigned 8), 16 or 32 -- outside the Lisp heap, zeroed."
    (let ((bytes (if (and (consp type) (type-named-p (first type) "UNSIGNED"))
                     (ceiling (second type) 8)
                     (error "MAKE-ALIEN: ~S is not (unsigned n)" type))))
      `(or (sys::dos-malloc (* ,bytes ,count)) (error "No memory"))))
  (defmacro sap-alien (sap type)
    (declare (ignore type))
    sap)
  (defmacro alien-funcall (function &rest arguments)
    "Call machine code.  FUNCTION must be (sap-alien address (function
result argument-type...)), with as many types as ARGUMENTS."
    (unless (and (consp function) (type-named-p (first function) "SAP-ALIEN")
                 (consp (third function)) (eq (first (third function)) 'function))
      (error "ALIEN-FUNCALL: ~S is not (sap-alien address (function ...))" function))
    (destructuring-bind (result &rest types) (rest (third function))
      (unless (= (length types) (length arguments))
        (error "ALIEN-FUNCALL: ~S does not take ~D arguments" (third function) (length arguments)))
      (let ((call `(sys::dos-call ,(second function)
                                  ,@(mapcar (lambda (argument type)
                                              (if (type-named-p type "SYSTEM-AREA-POINTER")
                                                  `(sys::dos-near ,argument)
                                                  `(ldb (byte 32 0) ,argument)))
                                            arguments types))))
        (cond ((type-named-p result "VOID") `(progn ,call (values)))
              ((type-named-p result "INT")
               `(let ((value ,call))
                  (if (logbitp 31 value) (- value #x100000000) value)))
              ((type-named-p result "SYSTEM-AREA-POINTER")
               `(ldb (byte 32 0) (- ,call (sys::dos-near 0))))
              (t call)))))

  (defun environment-variable (name) (ext:getenv name))
  (defun add-init-hook (function) (pushnew function custom:*init-hooks*))
  (defun add-exit-hook (function) (pushnew function custom:*fini-hooks*))
  (deftype interrupt-condition () 'system::interrupt-condition)
  (defun foreign-variable-address (name)
    (declare (ignore name))
    nil))

;;; ---- the Lisp itself
(defun heap-bytes-used ()
  "How many bytes of Lisp objects there are."
  #+sbcl (sb-kernel:dynamic-usage)
  #+clisp (values (sys::%room)))
(defun heap-bytes-limit ()
  "How large the Lisp heap may get (on CLISP: how large it is now)."
  #+sbcl (sb-ext:dynamic-space-size)
  #+clisp (multiple-value-bind (used free) (sys::%room) (+ used free)))
(defun bytes-allocated ()
  "How many bytes of Lisp objects have been made since Lisp started."
  #+sbcl (sb-ext:get-bytes-consed)
  #+clisp (multiple-value-bind (real1 real2 run1 run2 gc1 gc2 space1 space2) (sys::%%time)
            (declare (ignore real1 real2 run1 run2 gc1 gc2))
            (+ (ash space1 24) space2)))
(defun collect-garbage ()
  "A full garbage collection."
  #+sbcl (sb-ext:gc :full t)
  #+clisp (ext:gc)
  nil)
(defun code-bytes (function)
  "The size of the compiled FUNCTION: bytes of machine code on SBCL, of byte code on CLISP."
  #+sbcl (sb-kernel:%code-text-size (sb-kernel:fun-code-header function))
  #+clisp (length (sys::closure-codevec function)))
(defun disassembly-string (function)
  "What DISASSEMBLE prints for FUNCTION (a function or its name)."
  (with-output-to-string (out)
    #+sbcl (disassemble function :stream out)
    #+clisp (let ((*standard-output* out)) (disassemble function))))
(defun time-of-day ()
  "The time: seconds since 1970, and microseconds (0 where the Lisp does not know them)."
  #+sbcl (sb-ext:get-time-of-day)
  #+clisp (values (- (get-universal-time) 2208988800) 0))
(defun exit-lisp ()
  "Leave Lisp."
  #+sbcl (sb-ext:exit)
  #+clisp (ext:exit))
(defun command-line ()
  "The arguments the program was started with, after Lisp's own."
  #+sbcl (rest sb-ext:*posix-argv*)
  #+clisp ext:*args*)
(defun save-program (name function)
  "Save this Lisp as a program that calls FUNCTION and exits; this ends
the session.  On SBCL NAME is an .EXE; on CLISP it is a memory image, for
LISP.EXE -M NAME [arguments]."
  #+sbcl (sb-ext:save-lisp-and-die name :executable t
                                        :toplevel (lambda () (funcall function) (sb-ext:exit)))
  #+clisp (progn (ext:saveinitmem name :quiet t :norc t :script nil
                                       :init-function (lambda () (funcall function) (ext:exit)))
                 (ext:exit)))

;;; ---- files.  The functions that change something return true, or NIL
;;; and why not.
(defun current-directory-string ()      ; "D:\SBCL\"
  "The current directory, with a \\ at its end."
  (concatenate 'string
               (string-right-trim "\\/" #+sbcl (sb-unix:posix-getcwd)
                                        #+clisp (namestring (ext:default-directory)))
               "\\"))
(defun directory-exists-p (name)
  "Is NAME, with no \\ at its end, a directory?"
  #+sbcl (eq (ignore-errors (sb-impl::native-file-kind name)) :directory)
  #+clisp (and (ignore-errors (ext:probe-directory (concatenate 'string name "\\"))) t))
(defun file-kind-and-size (path)
  "What PATH (no \\ at its end) names -- :FILE or :DIRECTORY -- and a file's
size in bytes; NIL if there is nothing of that name."
  #+sbcl (multiple-value-bind (ok dev ino mode nlink uid gid rdev size) (sb-unix:unix-stat path)
           (declare (ignore dev ino nlink uid gid rdev))
           (cond ((not ok) nil)
                 ((= (logand mode #o170000) #o040000) (values :directory 0))
                 (t (values :file size))))
  #+clisp (cond ((directory-exists-p path) (values :directory 0))
                ((ignore-errors (probe-file path))
                 (values :file (or (ignore-errors
                                    (with-open-file (in path :element-type '(unsigned-byte 8))
                                      (file-length in)))
                                   0)))))
(defun list-directory (dir)
  "What is in DIR, which ends in \\: a list of (name directory-p size universal-time)."
  (let ((entries '()))
    #+sbcl
    (sb-impl::map-directory
     (lambda (path)
       (let* ((native (string-right-trim "\\/" (sb-ext:native-namestring path)))
              (name (subseq native (1+ (or (position-if (lambda (c) (find c "\\/:")) native :from-end t)
                                           -1)))))
         (multiple-value-bind (ok dev ino mode nlink uid gid rdev size atime mtime)
             (sb-unix:unix-stat (concatenate 'string dir name))
           (declare (ignore dev ino nlink uid gid rdev atime))
           (push (list name (and ok (= (logand mode #o170000) #o040000))
                       (if ok size 0) (if ok (+ mtime 2208988800) 0))
                 entries))))
     dir)
    #+clisp
    (flet ((seconds (date) (or (ignore-errors (apply #'encode-universal-time (append date '(0)))) 0)))
      (dolist (file (directory (concatenate 'string dir "*\\") :full t))
        (destructuring-bind (path truename date size) file
          (declare (ignore truename size))
          (push (list (car (last (pathname-directory path))) t 0 (seconds date)) entries)))
      (dolist (file (directory (concatenate 'string dir "*.*") :full t))
        (destructuring-bind (path truename date size) file
          (declare (ignore truename))
          (push (list (file-namestring path) nil size (seconds date)) entries))))
    entries))
#+sbcl
(defun unix-result (ok errno)
  (if ok t (values nil (sb-int:strerror errno))))
#+clisp
(defmacro file-result (&body body)
  `(handler-case (progn ,@body t)
     (error (e) (values nil (substitute #\Space #\Newline (princ-to-string e))))))
(defun remove-file (path directory-p)
  "Delete the file PATH, or the empty directory if DIRECTORY-P."
  #+sbcl (if directory-p
             (if (zerop (alien-funcall (extern-alien "rmdir" (function int c-string)) path))
                 t
                 (values nil (sb-int:strerror (sb-alien:get-errno))))
             (multiple-value-call #'unix-result (sb-unix:unix-unlink path)))
  #+clisp (file-result (if directory-p
                           (ext:delete-directory (concatenate 'string path "\\"))
                           (delete-file path))))
(defun move-file (path target directory-p)
  "Rename PATH, a directory if DIRECTORY-P, to TARGET."
  #+sbcl (declare (ignore directory-p))
  #+sbcl (multiple-value-call #'unix-result (sb-unix:unix-rename path target))
  #+clisp (file-result
            (if directory-p
                (ext:rename-directory (concatenate 'string path "\\") (concatenate 'string target "\\"))
                ;; RENAME-FILE fills in what the new name lacks from the old
                ;; one: a name with no type says so with a dot at its end
                (rename-file path (if (find #\. (file-namestring target))
                                      target
                                      (concatenate 'string target "."))))))
(defun create-directory (path)
  "Make the directory PATH."
  #+sbcl (multiple-value-call #'unix-result (sb-unix:unix-mkdir path #o777))
  #+clisp (file-result (ext:make-directory (concatenate 'string path "\\"))))
(defun write-bytes-to-device (name bytes)
  "Write the vector BYTES to the DOS device (or existing file) NAME, without
making a file of that name.  Returns how many bytes were taken, or NIL if
there is nothing of that name to open."
  #+sbcl (let ((fd (sb-unix:unix-open name sb-unix:o_wronly 0)))
           (and fd (unwind-protect (or (sb-unix:unix-write fd bytes 0 (length bytes)) 0)
                     (sb-unix:unix-close fd))))
  #+clisp (let ((stream (ignore-errors (open name :direction :output :if-exists :overwrite
                                                  :if-does-not-exist nil
                                                  :element-type '(unsigned-byte 8)))))
            (and stream
                 (if (ignore-errors (write-sequence bytes stream) (finish-output stream) t)
                     (progn (ignore-errors (close stream)) (length bytes))
                     (progn (ignore-errors (close stream :abort t)) 0)))))
(defun open-latin-1-file (path)
  "A stream of Latin-1 characters reading the file PATH, or NIL if it cannot be opened."
  #+sbcl (let ((fd (sb-unix:unix-open path sb-unix:o_rdonly 0)))
           (and fd (sb-sys:make-fd-stream fd :input t :buffering :full :external-format :latin-1)))
  #+clisp (ignore-errors (open path :if-does-not-exist nil :external-format charset:iso-8859-1)))
