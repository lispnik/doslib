;;;; A web server in Lisp on DOS, using SB-BSD-SOCKETS.
;;;;   /            a status page about this machine, with a form
;;;;   /eval?FORM   evaluates FORM in the running Lisp and returns the result
;;;;   /files       the current directory
;;;;   /quit        stops the server
;;;; Needs a packet driver loaded and WATTCP.CFG (my_ip = dhcp) in the
;;;; current directory.  Start with (serve) or (serve :port 8080).
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.websrv
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib #:sb-bsd-sockets)
  (:export #:main))
(in-package #:doslib-examples.websrv)

(defvar *requests* 0)
(defvar *started* (get-universal-time))
(defun ip-string (address) (format nil "~{~D~^.~}" (coerce address 'list)))
(defun clock ()
  (multiple-value-bind (s m h) (get-decoded-time) (format nil "~2,'0D:~2,'0D:~2,'0D" h m s)))
(defun url-decode (string)              ; %28 -> (, + -> space
  (with-output-to-string (out)
    (loop with i = 0 while (< i (length string))
          do (let ((c (char string i)))
               (cond ((and (char= c #\%) (< (+ i 2) (length string)))
                      (write-char (code-char (parse-integer string :start (1+ i) :end (+ i 3) :radix 16)) out)
                      (incf i 2))
                     ((char= c #\+) (write-char #\Space out))
                     (t (write-char c out)))
               (incf i)))))
(defun html-escape (string)
  (with-output-to-string (out)
    (loop for c across string
          do (case c (#\< (write-string "&lt;" out)) (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out)) (t (write-char c out))))))
(defun status-page (socket)
  (format nil "<html><head><title>SBCL on DOS</title></head><body>~
<h1>Hello from Lisp on DOS</h1><table>~
~:{<tr><td>~A</td><td><b>~A</b></td></tr>~}</table>~
<form action=\"/eval\">Evaluate: <input name=\"e\" size=\"50\" value=\"(expt 2 100)\">~
<input type=\"submit\"></form><p><a href=\"/files\">files</a></p></body></html>~%"
          `(("Lisp" ,(format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))
            ("System" ,(format nil "~A ~A" (software-type) (software-version)))
            ("Processor" ,(machine-type))
            ("You are" ,(format nil "~A port ~D" (ip-string (socket-peername socket))
                                (nth-value 1 (socket-peername socket))))
            ("Heap in use" ,(format nil "~:D bytes" (sb-kernel:dynamic-usage)))
            ("Up" ,(format nil "~D seconds" (- (get-universal-time) *started*)))
            ("Requests served" ,*requests*)
            ("Time here" ,(clock)))))
(defun files-page ()
  (with-output-to-string (out)
    (format out "~A~%" (sb-unix:posix-getcwd))
    (dolist (path (directory "*.*"))
      (format out "~10@A  ~A~%"
              (or (ignore-errors (with-open-file (in path :element-type '(unsigned-byte 8))
                                   (file-length in)))
                  "<DIR>")
              (if (pathname-name path)
                  (file-namestring path)
                  (car (last (pathname-directory path))))))))
(defun eval-page (query)                ; "e=FORM" from the form, or just FORM
  (let* ((text (url-decode (if (eql (search "e=" query) 0) (subseq query 2) query)))
         (*package* (find-package :cl-user)))
    (handler-case
        (values 200 (format nil "~A~%=> ~{~S~^ ;~%   ~}~%" text
                            (multiple-value-list (eval (read-from-string text)))))
      (error (e) (values 500 (format nil "~A~%signalled ~A:~%~A~%" text (type-of e) e))))))
(defun respond (stream status type body)
  (format stream "HTTP/1.0 ~D ~A~C~CServer: SBCL-on-DOS~C~CContent-Type: ~A~C~C~
Content-Length: ~D~C~CConnection: close~C~C~C~C~A"
          status (case status (200 "OK") (404 "Not Found") (t "Error"))
          #\Return #\Newline #\Return #\Newline type #\Return #\Newline
          (length body) #\Return #\Newline #\Return #\Newline #\Return #\Newline body)
  (finish-output stream))
(defun handle (client)
  "Answer one request; returns true when the server should stop."
  (let ((stream (socket-make-stream client :input t :output t :external-format :latin-1))
        (peer (ip-string (socket-peername client)))
        (status 200) (type "text/plain") (body "") (quit nil) (request "?"))
    (unwind-protect
         (handler-case
             (let* ((line (string-right-trim '(#\Return) (read-line stream)))
                    (path (subseq line (1+ (or (position #\Space line) -1))
                                  (position #\Space line :from-end t)))
                    (query (let ((q (position #\? path)))
                             (when q (prog1 (subseq path (1+ q)) (setf path (subseq path 0 q)))))))
               (setf request line)
               (loop for header = (read-line stream nil)   ; skip the headers
                     while (and header (string/= (string-right-trim '(#\Return) header) "")))
               (incf *requests*)
               (cond ((string= path "/") (setf type "text/html" body (status-page client)))
                     ((string= path "/eval") (setf (values status body) (eval-page (or query "nil"))))
                     ((string= path "/files") (setf body (files-page)))
                     ((string= path "/quit") (setf quit t body (format nil "Goodbye.~%")))
                     (t (setf status 404 body (format nil "Nothing at ~A~%" path))))
               (respond stream status type body))
           (error (e) (setf status 'failed body (princ-to-string e))))
      (ignore-errors (close stream)))
    (format t "~A  ~15A ~3A ~5D  ~A~%" (clock) peer status (length body)
            (subseq request 0 (min 42 (length request))))
    quit))
(defun serve (&key (port 8080))
  (let ((listener (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address listener) t)
    (socket-bind listener #(0 0 0 0) port)
    (socket-listen listener 5)
    (format t "~&Serving on port ~D.  Request /quit to stop.~%~
time      from            sts bytes  request~%" port)
    (unwind-protect (loop until (handle (socket-accept listener)))
      (socket-close listener))
    (format t "Stopped after ~D requests.~%" *requests*)))

(defun main (&key (port 8080))
  "A web server in Lisp on DOS, using SB-BSD-SOCKETS."
  (serve :port port))

(doslib-examples:define-example "websrv" 'main
  :category :network
  :description "A web server in Lisp on DOS, using SB-BSD-SOCKETS.")
