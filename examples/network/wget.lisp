;;;; An HTTP client in Lisp on DOS, using SB-BSD-SOCKETS.
;;;; Needs a packet driver loaded and WATTCP.CFG (my_ip = dhcp) in the
;;;; current directory.  Try:  (wget "http://example.com/")
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.wget
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib #:sb-bsd-sockets)
  (:export #:main))
(in-package #:doslib-examples.wget)

(defun parse-url (url)                  ; "http://host[:port]/path" -> host, port, path
  (let* ((start (if (eql (search "http://" url) 0) 7 0))
         (slash (or (position #\/ url :start start) (length url)))
         (colon (position #\: url :start start :end slash)))
    (values (subseq url start (or colon slash))
            (if colon (parse-integer url :start (1+ colon) :end slash) 80)
            (if (< slash (length url)) (subseq url slash) "/"))))
(defun dotted (address) (format nil "~{~D~^.~}" (coerce address 'list)))
(defun resolve (host)                   ; a dotted quad, or a name for the DNS
  (if (every (lambda (c) (or (digit-char-p c) (char= c #\.))) host)
      (make-inet-address host)
      (host-ent-address (get-host-by-name host))))
(defun wget (url &key (lines 8))
  "Fetch URL; show the response headers and the first LINES lines of the body."
  (multiple-value-bind (host port path) (parse-url url)
    (let ((address (resolve host))
          (socket (make-instance 'inet-socket :type :stream :protocol :tcp))
          (bytes 0) (shown 0) (in-body nil))
      (format t "~&~A is ~A; connecting to port ~D...~%" host (dotted address) port)
      (socket-connect socket address port)
      (format t "connected from ~A port ~D~%"
              (dotted (socket-name socket)) (nth-value 1 (socket-name socket)))
      (let ((stream (socket-make-stream socket :input t :output t :external-format :latin-1)))
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
