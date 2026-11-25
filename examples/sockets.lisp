;;;; Sockets for the network examples: SB-BSD-SOCKETS on SBCL, and as much
;;;; of it as the examples use, made from CLISP's SOCKET package, on CLISP.
;;;;
;;;; The examples use the package DOSLIB-SOCKETS.  On SBCL that is
;;;; SB-BSD-SOCKETS under another name, plus SOCKET-READY-P.  On CLISP the
;;;; same names are defined here, with these differences: TCP only (CLISP
;;;; has no datagram sockets), and SOCKET-BIND always binds every address
;;;; of the machine.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS.

(defpackage #:doslib-sockets
  (:use #:cl #+sbcl #:sb-bsd-sockets)
  (:export #:socket-ready-p
           #+clisp #:inet-socket #+clisp #:socket-connect #+clisp #:socket-bind
           #+clisp #:socket-listen #+clisp #:socket-accept #+clisp #:socket-close
           #+clisp #:socket-make-stream #+clisp #:socket-name #+clisp #:socket-peername
           #+clisp #:socket-receive #+clisp #:socket-send #+clisp #:socket-file-descriptor
           #+clisp #:sockopt-reuse-address #+clisp #:get-host-by-name
           #+clisp #:host-ent-address #+clisp #:make-inet-address))
(in-package #:doslib-sockets)

#+sbcl
(progn
  (do-external-symbols (symbol '#:sb-bsd-sockets) (export symbol '#:doslib-sockets))
  (defun socket-ready-p (socket &optional (milliseconds 0))
    "Is there something to read from SOCKET -- or, if it is listening, a
connection to accept -- now or within MILLISECONDS?"
    (sb-unix:unix-simple-poll (socket-file-descriptor socket) :input milliseconds)))

#+clisp
(progn
  (defclass inet-socket ()
    ((type :initarg :type :initform :stream)
     (protocol :initarg :protocol :initform :tcp)
     (port :initform nil :accessor socket-port)
     (server :initform nil :accessor socket-server)
     (stream :initform nil :accessor socket-stream)
     (bytes :initform nil :accessor socket-bytes-p)))    ; is the stream one of bytes now?
  (defmethod initialize-instance :after ((socket inet-socket) &key type &allow-other-keys)
    (when (eq type :datagram)
      (error "CLISP has no datagram (UDP) sockets")))

  (defun make-inet-address (dotted)
    "\"10.0.2.2\" as the vector #(10 0 2 2)."
    (let ((numbers (loop with start = 0
                         for dot = (position #\. dotted :start start)
                         collect (parse-integer dotted :start start :end dot)
                         while dot do (setf start (1+ dot)))))
      (unless (and (= (length numbers) 4) (every (lambda (n) (<= 0 n 255)) numbers))
        (error "~S is not an address" dotted))
      (coerce numbers 'vector)))
  (defun address-string (address)
    (if (stringp address) address (format nil "~{~D~^.~}" (coerce address 'list))))
  (defstruct host-ent address)
  (defun get-host-by-name (name)
    "Look NAME up: a HOST-ENT, whose HOST-ENT-ADDRESS is its address."
    (make-host-ent :address (or (ignore-errors (make-inet-address name))
                                (let ((dotted (sys::dos-resolve name)))
                                  (and dotted (make-inet-address dotted)))
                                (error "There is no host named ~A" name))))

  (defun set-bytes (socket bytes)
    (let ((stream (socket-stream socket)))
      (unless (eq (socket-bytes-p socket) bytes)
        (setf (stream-element-type stream) (if bytes '(unsigned-byte 8) 'character)
              (socket-bytes-p socket) bytes))
      stream))
  (defun socket-connect (socket address port)
    (setf (socket-stream socket)
          (socket:socket-connect port (address-string address)
                                 :element-type 'character :external-format charset:iso-8859-1)
          (socket-bytes-p socket) nil)
    socket)
  (defun socket-bind (socket address port)
    (declare (ignore address))
    (setf (socket-port socket) port))
  (defun socket-listen (socket backlog)
    (setf (socket-server socket) (socket:socket-server (socket-port socket) :backlog backlog))
    socket)
  (defun socket-accept (socket)
    "Wait for a connection to the listening SOCKET: a new socket."
    (let ((new (make-instance 'inet-socket)))
      (setf (socket-stream new)
            (socket:socket-accept (socket-server socket)
                                  :element-type 'character :external-format charset:iso-8859-1))
      new))
  (defun socket-close (socket &key abort)
    (when (socket-stream socket)
      (ignore-errors (close (socket-stream socket) :abort abort))
      (setf (socket-stream socket) nil))
    (when (socket-server socket)
      (ignore-errors (socket:socket-server-close (socket-server socket)))
      (setf (socket-server socket) nil))
    nil)
  (defun socket-make-stream (socket &key (element-type 'character) &allow-other-keys)
    "The stream of the connected SOCKET: of Latin-1 characters, or of bytes
if ELEMENT-TYPE says so."
    (set-bytes socket (not (subtypep element-type 'character))))
  (defun end-of-connection (socket local)
    (multiple-value-bind (host port)
        (if local
            (socket:socket-stream-local (socket-stream socket) t)
            (socket:socket-stream-peer (socket-stream socket) t))
      (values (make-inet-address (subseq host 0 (position #\Space host))) port)))
  (defun socket-name (socket)
    "The address and port of this end of the connected SOCKET."
    (if (socket-stream socket)
        (end-of-connection socket t)
        (values (vector 0 0 0 0) (or (socket-port socket) 0))))
  (defun socket-peername (socket)
    "The address and port of the other end of the connected SOCKET."
    (end-of-connection socket nil))
  (defun socket-receive (socket buffer length &key &allow-other-keys)
    "Read what has arrived on SOCKET, up to LENGTH bytes, waiting for the
first: the buffer (a string or a vector of bytes, as BUFFER is; of bytes if
it is NIL) and how many.  None means the other end has closed."
    (let* ((buffer (or buffer (make-array length :element-type '(unsigned-byte 8))))
           (text (stringp buffer))
           (stream (set-bytes socket t))
           (bytes (if text (make-array length :element-type '(unsigned-byte 8)) buffer))
           (count (ext:read-byte-sequence bytes stream :end length :interactive t)))
      (when text
        (dotimes (i count) (setf (char buffer i) (code-char (aref bytes i)))))
      (values buffer count)))
  (defun socket-send (socket buffer length &key address &allow-other-keys)
    (declare (ignore address))
    (let* ((stream (set-bytes socket t))
           (length (or length (length buffer)))
           (bytes (if (stringp buffer)
                      (map '(vector (unsigned-byte 8)) (lambda (c) (logand (char-code c) 255))
                           (subseq buffer 0 length))
                      buffer)))
      (write-sequence bytes stream :end length)
      (finish-output stream)
      length))
  (defun socket-file-descriptor (socket) socket)
  (defun sockopt-reuse-address (socket) (declare (ignore socket)) t)
  (defun (setf sockopt-reuse-address) (value socket) (declare (ignore socket)) value)
  (defun socket-ready-p (socket &optional (milliseconds 0))
    "Is there something to read from SOCKET -- or, if it is listening, a
connection to accept -- now or within MILLISECONDS?"
    (multiple-value-bind (seconds rest) (floor milliseconds 1000)
      (cond ((socket-stream socket)
             (and (member (socket:socket-status (cons (socket-stream socket) :input)
                                                seconds (* 1000 rest))
                          '(:input :io :eof :append))
                  t))
            ((socket-server socket)
             (and (socket:socket-wait (socket-server socket) seconds (* 1000 rest)) t))))))
