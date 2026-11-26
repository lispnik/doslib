;;;; TLS (what https is made of) over a connected socket, as a Lisp stream.
;;;;
;;;;   (let* ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
;;;;          (config (make-tls-config :ca-file "C:/CACERT.PEM")))
;;;;     (socket-connect socket (host-ent-address (get-host-by-name "example.com")) 443)
;;;;     (with-open-stream (stream (make-tls-stream socket :hostname "example.com"
;;;;                                                       :config config))
;;;;       (format stream "GET / HTTP/1.0~C~CHost: example.com~C~C~C~C"
;;;;               #\Return #\Newline #\Return #\Newline #\Return #\Newline)
;;;;       (finish-output stream)
;;;;       (read-line stream)))
;;;;
;;;; or, when there is nothing unusual about the socket,
;;;;
;;;;   (with-open-stream (stream (open-tls-connection "example.com" 443)) ...)
;;;;
;;;; A TLS-CONFIG says what to insist on: whether the server's certificate
;;;; is checked, against which authorities, and which versions of TLS will
;;;; do.  One configuration serves any number of connections, so a program
;;;; (an HTTP library, say) makes or is given one, and hands it with each
;;;; socket to MAKE-TLS-STREAM.  What comes back is an ordinary stream of
;;;; both bytes and Latin-1 characters -- READ-LINE for the headers,
;;;; READ-SEQUENCE into a byte vector for the body -- and closing it ends
;;;; the TLS conversation and closes the socket.
;;;;
;;;; The work is done by Mbed TLS, in C, which has to be linked into the
;;;; Lisp: (TLS-AVAILABLE-P) says whether it was.  As a client only, TLS 1.2
;;;; and 1.3.  To check certificates it needs a file of the authorities'
;;;; certificates in PEM form, such as https://curl.se/ca/cacert.pem, and
;;;; the PC's clock set to about the right date.  On random numbers, which
;;;; DOS does not provide and TLS depends on, see dos/dostls.c.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS.

(in-package #:doslib-sockets)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(tls-available-p tls-error tls-config make-tls-config tls-config-verify
            tls-config-ca-file tls-config-min-version tls-config-max-version
            free-tls-config *tls-config* *tls-ca-file*
            tls-stream make-tls-stream tls-stream-socket tls-stream-info
            open-tls-connection))
  (import (mapcar (lambda (name)
                    (or (find-symbol name #+sbcl "SB-GRAY" #+clisp "GRAY")
                        (error "No Gray stream symbol ~A" name)))
                  '("FUNDAMENTAL-CHARACTER-INPUT-STREAM" "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
                    "FUNDAMENTAL-BINARY-INPUT-STREAM" "FUNDAMENTAL-BINARY-OUTPUT-STREAM"
                    "STREAM-READ-BYTE" "STREAM-WRITE-BYTE" "STREAM-READ-CHAR"
                    "STREAM-UNREAD-CHAR" "STREAM-READ-CHAR-NO-HANG" "STREAM-PEEK-CHAR"
                    "STREAM-LISTEN" "STREAM-WRITE-CHAR" "STREAM-WRITE-STRING"
                    "STREAM-LINE-COLUMN" "STREAM-FORCE-OUTPUT" "STREAM-FINISH-OUTPUT"
                    "STREAM-CLEAR-INPUT"))))

(define-condition tls-error (error)
  ((message :initarg :message :reader tls-error-message))
  (:report (lambda (condition stream)
             (format stream "TLS: ~A" (tls-error-message condition)))))

;;; The C functions (dos/dostls.c).  Configurations and connections are
;;; addresses there; here they are whatever these return, or NIL for failure.

#+clisp
(progn
  (defun native (name)
    (let ((symbol (find-symbol name "SYSTEM")))
      (and symbol (fboundp symbol) symbol)))
  (defun tls-available-p ()
    "Is TLS linked into this Lisp?"
    (and (native "DOS-TLS-OPEN") t))
  (defun native-error () (funcall (native "DOS-TLS-ERROR")))
  (defun native-config (verify ca-file min max)
    (funcall (native "DOS-TLS-CONFIG") verify ca-file min max))
  (defun native-free (config) (funcall (native "DOS-TLS-FREE") config))
  (defun native-open (config socket hostname)
    (funcall (native "DOS-TLS-OPEN") config (socket-stream socket) hostname))
  (defun native-read (connection vector start count)
    (funcall (native "DOS-TLS-READ") connection vector start count))
  (defun native-write (connection vector start count)
    (funcall (native "DOS-TLS-WRITE") connection vector start count))
  (defun native-ready (connection milliseconds)
    (funcall (native "DOS-TLS-READY") connection milliseconds))
  (defun native-close (connection) (funcall (native "DOS-TLS-CLOSE") connection))
  (defun native-info (connection what) (funcall (native "DOS-TLS-INFO") connection what)))

#+sbcl
(progn
  (defun tls-available-p ()
    "Is TLS linked into this Lisp?"
    (and (sb-sys:find-foreign-symbol-address "dostls_open") t))
  (defmacro call (name result &rest arguments)
    ;; (call "dostls_x" int (type value) ...)
    `(sb-alien:alien-funcall
      (sb-alien:extern-alien ,name (function ,result ,@(mapcar #'first arguments)))
      ,@(mapcar #'second arguments)))
  (defun pointer (sap) (and (/= (sb-sys:sap-int sap) 0) sap))
  (defun native-error () (call "dostls_error" sb-alien:c-string))
  (defun native-config (verify ca-file min max)
    (pointer (call "dostls_config" sb-sys:system-area-pointer
                   (sb-alien:int (if verify 1 0)) (sb-alien:c-string ca-file)
                   (sb-alien:int min) (sb-alien:int max))))
  (defun native-free (config)
    (call "dostls_config_free" sb-alien:void (sb-sys:system-area-pointer config)))
  (defun native-open (config socket hostname)
    (pointer (call "dostls_open" sb-sys:system-area-pointer
                   (sb-sys:system-area-pointer config)
                   (sb-alien:int (socket-file-descriptor socket))
                   (sb-alien:c-string hostname))))
  (defun native-read (connection vector start count)
    (sb-sys:with-pinned-objects (vector)
      (call "dostls_read" sb-alien:int (sb-sys:system-area-pointer connection)
            (sb-sys:system-area-pointer (sb-sys:sap+ (sb-sys:vector-sap vector) start))
            (sb-alien:unsigned count))))
  (defun native-write (connection vector start count)
    (sb-sys:with-pinned-objects (vector)
      (call "dostls_write" sb-alien:int (sb-sys:system-area-pointer connection)
            (sb-sys:system-area-pointer (sb-sys:sap+ (sb-sys:vector-sap vector) start))
            (sb-alien:unsigned count))))
  (defun native-ready (connection milliseconds)
    (/= 0 (call "dostls_ready" sb-alien:int (sb-sys:system-area-pointer connection)
                (sb-alien:int milliseconds))))
  (defun native-close (connection)
    (call "dostls_close" sb-alien:void (sb-sys:system-area-pointer connection)))
  (defun native-info (connection what)
    (call "dostls_info" sb-alien:c-string (sb-sys:system-area-pointer connection)
          (sb-alien:int what))))

(defun tls-fail (&optional (control "~A") &rest arguments)
  (error 'tls-error :message (apply #'format nil control arguments)))
(defun need-tls ()
  (unless (tls-available-p)
    (tls-fail "this Lisp was built without it (it needs Mbed TLS linked in)")))

;;; Configurations

(defvar *tls-ca-file* nil
  "The file of certificate authorities (PEM) for configurations that name
none.  If this is NIL too: the file named by the environment variable
SSL_CERT_FILE, or else CACERT.PEM in the current directory.")

(defstruct (tls-config (:constructor %make-tls-config))
  "What to insist on of a TLS connection.  Made by MAKE-TLS-CONFIG."
  (verify t)
  (ca-file nil)
  (min-version :tls1.2)
  (max-version :tls1.3)
  (native nil))                         ; made when first needed

(defun version-number (version)
  (ecase version ((:tls1.2 12) 12) ((:tls1.3 13) 13) ((nil) 0)))

(defun make-tls-config (&key (verify t) ca-file (min-version :tls1.2) (max-version :tls1.3))
  "A configuration for TLS connections, to give to MAKE-TLS-STREAM as often
as wanted.
VERIFY: is the server's certificate checked -- that an authority we trust
  vouches for it, that it is for the host we asked for, and that its dates
  are good?  Without this anybody may be at the other end.
CA-FILE: the authorities trusted, a file of their certificates in PEM form
  (see *TLS-CA-FILE* for what happens if there is none).  Reading it takes
  a second or two, once for each configuration.
MIN-VERSION, MAX-VERSION: :TLS1.2 or :TLS1.3."
  (version-number min-version) (version-number max-version)
  (%make-tls-config :verify verify :ca-file (and ca-file (namestring ca-file))
                    :min-version min-version :max-version max-version))

(defun ca-file-for (config)
  (let ((file (or (tls-config-ca-file config) *tls-ca-file*)))
    (cond (file (namestring file))
          ((plusp (length (doslib-compat:environment-variable "SSL_CERT_FILE"))) nil)
          ((probe-file "CACERT.PEM") (namestring (truename "CACERT.PEM")))
          (t (tls-fail "there is no file of certificate authorities to check the ~
                        server's certificate against: give MAKE-TLS-CONFIG a :CA-FILE, ~
                        set *TLS-CA-FILE* or the environment variable SSL_CERT_FILE, ~
                        or put CACERT.PEM in the current directory")))))

(defun config-native (config)
  (or (tls-config-native config)
      (setf (tls-config-native config)
            (or (native-config (tls-config-verify config)
                               (and (tls-config-verify config) (ca-file-for config))
                               (version-number (tls-config-min-version config))
                               (version-number (tls-config-max-version config)))
                (tls-fail (native-error))))))

(defun free-tls-config (config)
  "Give back the memory of CONFIG (a megabyte, with the usual authorities).
Only when no stream made with it is still open.  It may be used again."
  (when (tls-config-native config)
    (native-free (shiftf (tls-config-native config) nil)))
  config)

(defvar *tls-config* nil
  "The configuration of MAKE-TLS-STREAM and OPEN-TLS-CONNECTION when they are
given none.  If NIL, one that verifies is made when first wanted.")
(defun default-tls-config ()
  (or *tls-config* (setf *tls-config* (make-tls-config))))

;;; The stream

(defconstant +tls-buffer+ 4096)
(deftype tls-bytes () '(simple-array (unsigned-byte 8) (*)))

(defclass tls-stream (fundamental-character-input-stream fundamental-character-output-stream
                      fundamental-binary-input-stream fundamental-binary-output-stream)
  ((connection :initarg :connection :accessor tls-connection)
   (socket :initarg :socket :reader tls-stream-socket)
   (config :initarg :config :reader tls-stream-config)
   (close-socket :initarg :close-socket :reader tls-close-socket-p)
   (input :initform (make-array +tls-buffer+ :element-type '(unsigned-byte 8)) :reader tls-input)
   (input-start :initform 0 :accessor tls-input-start)
   (input-end :initform 0 :accessor tls-input-end)
   (eof :initform nil :accessor tls-eof)
   (unread :initform nil :accessor tls-unread)
   (output :initform (make-array +tls-buffer+ :element-type '(unsigned-byte 8)) :reader tls-output)
   (output-fill :initform 0 :accessor tls-output-fill)
   (column :initform 0 :accessor tls-column))
  (:documentation "A TLS conversation over a socket, as a stream of bytes and
of Latin-1 characters.  Made by MAKE-TLS-STREAM."))

(defmethod print-object ((stream tls-stream) out)
  (print-unreadable-object (stream out :type t :identity t)
    (if (tls-connection stream)
        (format out "~A" (native-info (tls-connection stream) 0))
        (format out "closed"))))

(defun make-tls-stream (socket &key hostname (config (default-tls-config)) (close-socket t))
  "Start TLS, as the client, on the connected SOCKET: a stream of bytes and
Latin-1 characters for talking to the server in private.  Closing it ends
the conversation and, unless CLOSE-SOCKET is NIL, closes the socket.
HOSTNAME is the server's name: it is told which site is wanted, and (when
CONFIG verifies) its certificate must be for that name.
CONFIG is a TLS-CONFIG (default: *TLS-CONFIG*).
Signals TLS-ERROR if the two cannot agree or the certificate is refused;
the socket is left open then."
  (need-tls)
  (when (and (tls-config-verify config) (not hostname))
    (tls-fail "a verifying configuration needs the :HOSTNAME the certificate is to be for"))
  (let ((connection (or (native-open (config-native config) socket hostname)
                        (tls-fail (native-error)))))
    (make-instance 'tls-stream :connection connection :socket socket :config config
                               :close-socket close-socket)))

(defun open-tls-connection (host port &key (config (default-tls-config)))
  "Connect to PORT of HOST (a name or \"1.2.3.4\") and start TLS: a TLS-STREAM,
which closes the socket when it is closed."
  (need-tls)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        (done nil))
    (unwind-protect
         (progn
           (socket-connect socket (host-ent-address (get-host-by-name host)) port)
           (prog1 (make-tls-stream socket :hostname host :config config)
             (setf done t)))
      (unless done (socket-close socket)))))

(defun tls-stream-info (stream)
  "About the open STREAM, a property list: :PROTOCOL (\"TLSv1.3\"), :CIPHER,
and of the server's certificate :SUBJECT, :ISSUER and :EXPIRES."
  (let ((connection (live-connection stream)))
    (loop for key in '(:protocol :cipher :subject :issuer :expires)
          for what from 0
          collect key collect (native-info connection what))))

(defun live-connection (stream)
  (or (tls-connection stream) (error 'stream-error :stream stream)))

(defmethod stream-element-type ((stream tls-stream)) '(unsigned-byte 8))
(defmethod open-stream-p ((stream tls-stream)) (and (tls-connection stream) t))
;; (CLISP tells the directions from the classes.)
#+sbcl (defmethod input-stream-p ((stream tls-stream)) t)
#+sbcl (defmethod output-stream-p ((stream tls-stream)) t)

;;; Input

(defun fill-input (stream)
  "Make sure there is input in the buffer, waiting for it: true, or false at
the end of the data."
  (cond ((< (tls-input-start stream) (tls-input-end stream)) t)
        ((tls-eof stream) nil)
        (t (let ((count (native-read (live-connection stream) (tls-input stream) 0 +tls-buffer+)))
             (cond ((plusp count)
                    (setf (tls-input-start stream) 0 (tls-input-end stream) count)
                    t)
                   ((zerop count) (setf (tls-eof stream) t) nil)
                   (t (tls-fail (native-error))))))))

(defun input-waiting-p (stream)
  "Can something be read (or the end met) without waiting?"
  (or (< (tls-input-start stream) (tls-input-end stream))
      (tls-eof stream)
      (native-ready (live-connection stream) 0)))

(defmethod stream-read-byte ((stream tls-stream))
  (cond ((tls-unread stream)
         (char-code (shiftf (tls-unread stream) nil)))
        ((fill-input stream)
         (prog1 (aref (tls-input stream) (tls-input-start stream))
           (incf (tls-input-start stream))))
        (t :eof)))

(defmethod stream-read-char ((stream tls-stream))
  (let ((byte (stream-read-byte stream)))
    (if (eq byte :eof) :eof (code-char byte))))
(defmethod stream-unread-char ((stream tls-stream) character)
  (setf (tls-unread stream) character)
  nil)
(defmethod stream-peek-char ((stream tls-stream))
  (let ((character (stream-read-char stream)))
    (unless (eq character :eof) (setf (tls-unread stream) character))
    character))
(defmethod stream-read-char-no-hang ((stream tls-stream))
  (cond ((tls-unread stream) (shiftf (tls-unread stream) nil))
        ((input-waiting-p stream) (stream-read-char stream))
        (t nil)))
(defmethod stream-listen ((stream tls-stream))
  (or (and (tls-unread stream) t)
      (and (input-waiting-p stream) (fill-input stream))))
(defmethod stream-clear-input ((stream tls-stream))
  (setf (tls-unread stream) nil
        (tls-input-start stream) 0 (tls-input-end stream) 0)
  nil)

(defun read-into (stream sequence start end)
  "Fill SEQUENCE (bytes or characters) from START to END, stopping short only
at the end of the data: the index after the last element read."
  (let ((text (stringp sequence))
        (index start))
    (when (and (< index end) (tls-unread stream))
      (let ((character (shiftf (tls-unread stream) nil)))
        (setf (elt sequence index) (if text character (char-code character))))
      (incf index))
    (loop while (and (< index end) (fill-input stream))
          do (let* ((from (tls-input-start stream))
                    (count (min (- end index) (- (tls-input-end stream) from)))
                    (input (tls-input stream)))
               (declare (type tls-bytes input))
               (cond (text
                      (dotimes (i count)
                        (setf (char sequence (+ index i)) (code-char (aref input (+ from i))))))
                     (t (replace sequence input :start1 index :start2 from :end2 (+ from count))))
               (incf index count)
               (setf (tls-input-start stream) (+ from count))))
    index))

;;; Output

(defun flush-output (stream)
  (let ((fill (tls-output-fill stream)))
    (when (plusp fill)
      (setf (tls-output-fill stream) 0)
      (when (minusp (native-write (live-connection stream) (tls-output stream) 0 fill))
        (tls-fail (native-error)))))
  nil)

(defmethod stream-write-byte ((stream tls-stream) byte)
  (when (= (tls-output-fill stream) +tls-buffer+) (flush-output stream))
  (setf (aref (tls-output stream) (tls-output-fill stream)) byte)
  (incf (tls-output-fill stream))
  byte)
(defmethod stream-write-char ((stream tls-stream) character)
  (stream-write-byte stream (logand (char-code character) 255))
  (setf (tls-column stream) (if (char= character #\Newline) 0 (1+ (tls-column stream))))
  character)
(defmethod stream-line-column ((stream tls-stream)) (tls-column stream))
(defmethod stream-force-output ((stream tls-stream)) (flush-output stream))
(defmethod stream-finish-output ((stream tls-stream)) (flush-output stream))

(defun write-from (stream sequence start end)
  (let ((text (stringp sequence))
        (output (tls-output stream)))
    (declare (type tls-bytes output))
    (loop while (< start end)
          do (when (= (tls-output-fill stream) +tls-buffer+) (flush-output stream))
             (let* ((fill (tls-output-fill stream))
                    (count (min (- end start) (- +tls-buffer+ fill))))
               (cond (text
                      (dotimes (i count)
                        (setf (aref output (+ fill i))
                              (logand (char-code (char sequence (+ start i))) 255))))
                     (t (replace output sequence :start1 fill :start2 start :end2 (+ start count))))
               (setf (tls-output-fill stream) (+ fill count))
               (incf start count)))
    (when (and text (< 0 end))
      (let ((newline (position #\Newline sequence :end end :from-end t)))
        (setf (tls-column stream)
              (if newline (- end newline 1) (+ (tls-column stream) end)))))
    sequence))

(defmethod stream-write-string ((stream tls-stream) string &optional (start 0) end)
  (write-from stream string start (or end (length string)))
  string)

;;; READ-SEQUENCE and WRITE-SEQUENCE, which each Lisp spells its own way

#+sbcl
(progn
  (defmethod sb-gray:stream-read-sequence ((stream tls-stream) sequence &optional (start 0) end)
    (read-into stream sequence start (or end (length sequence))))
  (defmethod sb-gray:stream-write-sequence ((stream tls-stream) sequence &optional (start 0) end)
    (write-from stream sequence start (or end (length sequence)))))

#+clisp
(progn
  (defmethod gray:stream-read-byte-sequence ((stream tls-stream) sequence
                                             &optional (start 0) end no-hang interactive)
    (let ((end (or end (length sequence))))
      (cond ((and (or no-hang interactive) (< start end))
             ;; what is there now -- for INTERACTIVE, after waiting for the first
             (if (or interactive (input-waiting-p stream))
                 (if (fill-input stream)
                     (read-into stream sequence start
                                (min end (+ start (- (tls-input-end stream)
                                                     (tls-input-start stream)))))
                     start)
                 start))
            (t (read-into stream sequence start end)))))
  (defmethod gray:stream-read-char-sequence ((stream tls-stream) sequence &optional (start 0) end)
    (read-into stream sequence start (or end (length sequence))))
  (defmethod gray:stream-write-byte-sequence ((stream tls-stream) sequence
                                              &optional (start 0) end no-hang interactive)
    (declare (ignore no-hang interactive))
    (write-from stream sequence start (or end (length sequence))))
  (defmethod gray:stream-write-char-sequence ((stream tls-stream) sequence &optional (start 0) end)
    (write-from stream sequence start (or end (length sequence)))))

(defmethod close ((stream tls-stream) &key abort)
  (let ((connection (shiftf (tls-connection stream) nil)))
    (when connection
      (unwind-protect
           (unless abort
             (setf (tls-connection stream) connection)
             (unwind-protect (ignore-errors (flush-output stream))
               (setf (tls-connection stream) nil)))
        (native-close connection)
        (when (tls-close-socket-p stream)
          (socket-close (tls-stream-socket stream))))
      t)))
