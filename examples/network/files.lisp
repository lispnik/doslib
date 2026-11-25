;;;; A file server: the DOS machine's disk in a Web browser.
;;;;
;;;;   (doslib-examples:run "files")                    serve D:\ on port 8080
;;;;   (doslib-examples:run "files" "C:\\" 8080)        another directory or port
;;;;
;;;; Directories are shown as pages of links and files are sent as they
;;;; are, whatever their size, so this is also a way to get things off the
;;;; machine.  Names are taken relative to the directory being served,
;;;; and a request that tries to climb out of it with ".." is refused.
;;;; Any key on the DOS machine stops the server.
;;;;
;;;; One request is answered at a time, and there are sockets for 28 of
;;;; them in any two minutes (that is how long the network library keeps a
;;;; closed one): plenty for looking around, not for a crawler.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.files
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib
        #:doslib-sockets)
  (:export #:main))
(in-package #:doslib-examples.files)

(defvar *root* "D:\\")
(defvar *served* 0)

(defun ascii (string) (map '(vector (unsigned-byte 8)) (lambda (c) (logand (char-code c) 255)) string))
(defun url-decode (string)
  (with-output-to-string (out)
    (loop with i = 0 while (< i (length string))
          do (let ((c (char string i)))
               (if (and (char= c #\%) (< (+ i 2) (length string))
                        (digit-char-p (char string (+ i 1)) 16) (digit-char-p (char string (+ i 2)) 16))
                   (progn (write-char (code-char (parse-integer string :start (1+ i) :end (+ i 3) :radix 16)) out)
                          (incf i 2))
                   (write-char c out))
               (incf i)))))
(defun url-encode (string)
  (with-output-to-string (out)
    (loop for c across string
          do (if (or (alphanumericp c) (find c "-_.~"))
                 (write-char c out)
                 (format out "%~2,'0X" (char-code c))))))
(defun html-escape (string)
  (with-output-to-string (out)
    (loop for c across string
          do (case c (#\< (write-string "&lt;" out)) (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out)) (t (write-char c out))))))

(defun path-parts (url-path)
  "The names in a request path; DOS takes \\ for a separator too, so it is one here."
  (loop with start = 0
        for slash = (position-if (lambda (c) (find c "/\\")) url-path :start start)
        for part = (subseq url-path start slash)
        unless (or (string= part "") (string= part ".")) collect part
        while slash do (setf start (1+ slash))))

(defun unsafe-part-p (part)
  ;; DOS ignores dots and spaces at the end of a name, so ".. " and "..." must
  ;; go the way of ".."; a colon would name another drive; and a device name
  ;; "exists" in every directory, with or without an extension: opening CON
  ;; would have the server reading the keyboard.
  (let ((stem (string-upcase (subseq part 0 (position #\. part)))))
    (or (string= (string-right-trim ". " part) "")
        (find-if (lambda (c) (or (find c ":*?\"<>|") (< (char-code c) 32))) part)
        (member (string-right-trim " " stem)
                '("CON" "PRN" "AUX" "NUL" "CLOCK$" "COM1" "COM2" "COM3" "COM4" "LPT1" "LPT2" "LPT3")
                :test #'string=))))

(defun local-path (parts)
  "The DOS path the names of a request path lead to, or NIL if that is outside the root."
  (unless (some #'unsafe-part-p parts)
    (format nil "~A~{~A~^\\~}" *root* parts)))

(defun kind-and-size (path)
  (let ((path (string-right-trim "\\" path)))
    (multiple-value-bind (kind size) (doslib-compat:file-kind-and-size path)
      (cond (kind (values kind size))
            ((string= path (string-right-trim "\\" *root*))
             (values :directory 0))))))   ; a drive's root cannot be asked about

(defun content-type (name)
  (let ((type (string-downcase (subseq name (1+ (or (position #\. name :from-end t) (1- (length name))))))))
    (cond ((member type '("txt" "lsp" "lisp" "asd" "md" "bat" "cfg" "log" "c" "h") :test #'string=) "text/plain")
          ((member type '("htm" "html") :test #'string=) "text/html")
          ((string= type "bmp") "image/bmp") ((string= type "png") "image/png")
          ((string= type "gif") "image/gif")
          (t "application/octet-stream"))))

(defvar *head-only* nil)   ; true while answering a HEAD request: headers, no body

(defun send-head (stream status type length)
  (write-sequence (ascii (format nil "HTTP/1.0 ~A~C~CServer: SBCL-on-DOS~C~CContent-Type: ~A~C~C~
Content-Length: ~D~C~CConnection: close~C~C~C~C"
                                 status #\Return #\Newline #\Return #\Newline type #\Return #\Newline
                                 length #\Return #\Newline #\Return #\Newline #\Return #\Newline))
                  stream))
(defun send-text (stream status text &optional (type "text/html"))
  (let ((bytes (ascii text)))
    (send-head stream status type (length bytes))
    (cond (*head-only* 0)
          (t (write-sequence bytes stream) (length bytes)))))

(defun directory-page (path parts)
  (let ((entries '()) (dir (concatenate 'string (string-right-trim "\\" path) "\\")))
    (loop for (name directory size) in (list-directory dir)
          do (push (list name (if directory :directory :file) size) entries))
    (setf entries (sort entries (lambda (a b)
                                  (if (eq (second a) (second b))
                                      (string-lessp (first a) (first b))
                                      (eq (second a) :directory)))))
    ;; The links are made from the names again, not from the path as it came:
    ;; that one is decoded, and may lack its last slash or have too many.
    (let ((base (format nil "/~{~A/~}" (mapcar #'url-encode parts))))
      (with-output-to-string (out)
        (format out "<html><head><title>~A</title></head><body><h1>~A</h1><table>~%"
                (html-escape dir) (html-escape dir))
        (when parts
          (format out "<tr><td><a href=\"/~{~A/~}\">..</a></td><td></td></tr>~%"
                  (mapcar #'url-encode (butlast parts))))
        (dolist (entry entries)
          (destructuring-bind (name kind size) entry
            (format out "<tr><td><a href=\"~A~A~:[~;/~]\">~A~:[~;\\~]</a></td><td align=right>~:[~:D~;~*~]</td></tr>~%"
                    base (url-encode name) (eq kind :directory) (html-escape name) (eq kind :directory)
                    (eq kind :directory) size)))
        (format out "</table><p>~D item~:P.  Served by ~A ~A on DOS.</p></body></html>~%"
                (length entries) (lisp-implementation-type) (lisp-implementation-version))))))

(defun read-request (client)
  "The head of the request, as far as its blank line; what there is of it if
the client says no more for ten seconds, or hangs up."
  ;; Read from the socket itself and only when it has something: there is one
  ;; thread here, and a client that connects and stays silent would otherwise
  ;; hold the server, deaf even to its own keyboard, for as long as it liked.
  (let ((head (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
        (buffer (make-array 1024 :element-type '(unsigned-byte 8)))
        (deadline (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop
      (let ((left (- deadline (get-internal-real-time))))
        (when (or (<= left 0) (> (length head) 8192) (search '(#\Newline #\Return #\Newline) head)
                  (search '(#\Newline #\Newline) head) (key-pressed-p))
          (return))
        (when (socket-ready-p client (min 250 (ceiling (* left 1000) internal-time-units-per-second)))
          (let ((n (nth-value 1 (socket-receive client buffer nil))))
            (when (or (null n) (<= n 0)) (return))
            (dotimes (i n) (vector-push-extend (code-char (aref buffer i)) head))))))
    (coerce head 'simple-string)))

(defun handle (client stream peer)
  (let* ((request (read-request client))
         (line (subseq request 0 (position-if (lambda (c) (find c '(#\Return #\Newline))) request)))
         (complete (find #\Newline request))
         (first-space (position #\Space line))
         (last-space (position #\Space line :from-end t))
         (verb (subseq line 0 first-space))
         (url-path (if (and first-space last-space (< first-space last-space))
                       (let ((p (subseq line (1+ first-space) last-space)))
                         (url-decode (subseq p 0 (position-if (lambda (c) (find c "?#")) p))))
                       "/"))
         (parts (path-parts url-path))
         (path (local-path parts))
         (*head-only* (string= verb "HEAD"))
         (status "200 OK") (sent 0))
    (flet ((refuse (code text)
             (setf status code
                   sent (send-text stream status (format nil "<h1>~A</h1>~A" (subseq code 0 3) text)))))
      (cond ((not complete)   ; nothing to answer, and probably nobody to answer to
             (setf status "---"))
            ((not (or *head-only* (string= verb "GET")))
             (refuse "501 Not Implemented" "Only GET and HEAD are understood here."))
            ((null path)
             (refuse "403 Forbidden" "That is outside what is being served."))
            (t
             (let ((kind (kind-and-size path)))
               (case kind
                 ((nil) (refuse "404 Not Found" (format nil "There is no ~A here." (html-escape url-path))))
                 (:directory (setf sent (send-text stream status (directory-page path parts))))
                 (t (let ((in (open path :element-type '(unsigned-byte 8) :if-does-not-exist nil)))
                      (if (null in)   ; there a moment ago: say so while the head is still unsent
                          (refuse "404 Not Found" (format nil "~A cannot be opened." (html-escape url-path)))
                          (unwind-protect
                               (let ((buffer (make-array 16384 :element-type '(unsigned-byte 8))))
                                 ;; the length is what the open file has, the one sure to be sent
                                 (send-head stream status (content-type path) (file-length in))
                                 (unless *head-only*
                                   (loop for n = (read-sequence buffer in)
                                         until (zerop n)
                                         do (write-sequence buffer stream :end n) (incf sent n))))
                            (close in))))))))))
    (when complete (finish-output stream) (incf *served*))
    (format t "~&~15A ~3A ~10:D  ~A~%" peer (subseq status 0 3) sent
            (if complete (subseq line 0 (min 44 (length line))) "(no request)"))))

(defun main (&optional (root "D:\\") (port 8080))
  "A file server: the DOS machine's disk in a Web browser."
  (setf *root* (concatenate 'string (string-right-trim "\\/" root) "\\") *served* 0)
  (let ((listener (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address listener) t)
    (socket-bind listener #(0 0 0 0) port)
    (socket-listen listener 5)
    (format t "~&Serving ~A on port ~D.  Press a key to stop.~%from            sts      bytes  request~%"
            *root* port)
    (unwind-protect
         (loop until (key-pressed-p)
               do (when (socket-ready-p listener 0)
                    ;; Accepting is under the handler too.  There are sockets for 28 clients
                    ;; and the network library keeps each for two minutes after it is closed,
                    ;; so a burst of requests can find none left: that one goes unanswered
                    ;; and is no reason to stop serving.
                    (let ((client nil) (failed t))
                      (unwind-protect
                           (handler-case
                               (progn
                                 (setf client (socket-accept listener))
                                 (handle client
                                         (socket-make-stream client :output t :buffering :full
                                                                    :element-type '(unsigned-byte 8))
                                         (format nil "~{~D~^.~}" (coerce (socket-peername client) 'list)))
                                 (setf failed nil))
                             (error (e)
                               (if client
                                   (format t "~&  (a request failed: ~A)~%"
                                           (substitute #\Space #\Newline (princ-to-string e)))
                                   (format t "~&  (a client was turned away: no socket is free, ~
                                              they come back two minutes after use)~%"))))
                        ;; after a failure the buffer holds what could not be sent: drop it,
                        ;; or closing would try once more and leave the socket open
                        (when client (ignore-errors (socket-close client :abort failed)))))))
      (when (key-pressed-p) (get-key))
      (socket-close listener))
    (format t "~&Stopped after ~D request~:P.~%" *served*)))

(doslib-examples:define-example "files" 'main
  :category :network
  :description "A file server: the DOS machine's disk in a Web browser.")
