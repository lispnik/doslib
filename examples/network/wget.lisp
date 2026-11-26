;;;; An HTTP client in Lisp on DOS, using SB-BSD-SOCKETS.
;;;; Needs a packet driver loaded and WATTCP.CFG (my_ip = dhcp) in the
;;;; current directory.  Try:  (wget "http://example.com/")
;;;; or, in a Lisp with TLS, with CACERT.PEM in the current directory too:
;;;;                               (wget "https://example.com/")
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.wget
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib
        #:doslib-sockets)
  (:export #:main))
(in-package #:doslib-examples.wget)

(defun parse-url (url)                  ; "http[s]://host[:port]/path" -> host, port, path, tls
  (let* ((tls (eql (search "https://" url) 0))
         (start (cond (tls 8) ((eql (search "http://" url) 0) 7) (t 0)))
         (slash (or (position #\/ url :start start) (length url)))
         (colon (position #\: url :start start :end slash)))
    (values (subseq url start (or colon slash))
            (if colon (parse-integer url :start (1+ colon) :end slash) (if tls 443 80))
            (if (< slash (length url)) (subseq url slash) "/")
            tls)))
(defun dotted (address) (format nil "~{~D~^.~}" (coerce address 'list)))
(defun resolve (host)                   ; a dotted quad, or a name for the DNS
  (if (every (lambda (c) (or (digit-char-p c) (char= c #\.))) host)
      (make-inet-address host)
      (host-ent-address (get-host-by-name host))))
(defun wget (url &key (lines 8) (tls-config *tls-config*))
  "Fetch URL; show the response headers and the first LINES lines of the body.
An https URL is fetched over TLS, as TLS-CONFIG says (see MAKE-TLS-CONFIG)."
  (multiple-value-bind (host port path tls) (parse-url url)
    (let ((address (resolve host))
          (socket (make-instance 'inet-socket :type :stream :protocol :tcp))
          (bytes 0) (shown 0) (in-body nil))
      (format t "~&~A is ~A; connecting to port ~D...~%" host (dotted address) port)
      (socket-connect socket address port)
      (format t "connected from ~A port ~D~%"
              (dotted (socket-name socket)) (nth-value 1 (socket-name socket)))
      (let ((stream (if tls
                        ;; the same socket, with a configuration: a stream again
                        (make-tls-stream socket :hostname host
                                                :config (or tls-config (make-tls-config)))
                        (socket-make-stream socket :input t :output t
                                                   :external-format :latin-1))))
        (when tls
          (format t "~{~*~A~^, ~}~%" (tls-stream-info stream)))
        (unwind-protect
             (progn
               (format stream "GET ~A HTTP/1.0~C~CHost: ~A~C~CUser-Agent: SBCL-on-DOS~C~C~C~C"
                       path #\Return #\Newline host #\Return #\Newline
                       #\Return #\Newline #\Return #\Newline)
               (finish-output stream)
               (loop for line = (read-line stream nil)
                     while line
                     do (setf line (string-right-trim '(#\Return) line))
                        (cond ((not in-body)
                               (if (string= line "")
                                   (setf in-body t)
                                   (format t "< ~A~%" (subseq line 0 (min 76 (length line))))))
                              (t (incf bytes (1+ (length line)))
                                 (when (and (< shown lines) (string/= (string-trim " " line) ""))
                                   (incf shown)
                                   (format t "| ~A~%" (subseq line 0 (min 76 (length line)))))))))
          (close stream)))
      (format t "~D bytes of body.~%" bytes)
      (values))))

(defun main (&optional (url "http://example.com/"))
  "An HTTP client in Lisp on DOS, using SB-BSD-SOCKETS."
  (wget url))

(doslib-examples:define-example "wget" 'main
  :category :network
  :description "An HTTP client in Lisp on DOS, using SB-BSD-SOCKETS.")
