;;;; A remote REPL into a running graphics program.
;;;;
;;;; The DOS machine shows an animation in mode 13h, computed by calling
;;;; (PIXEL x y time) for each cell of an 80x48 grid.  Meanwhile it listens
;;;; on TCP port 2323: connect from another machine (telnet or nc) and you
;;;; get a Lisp prompt *inside the running program*.  Redefine PIXEL and
;;;; the picture changes on the next frame; make a mistake and the error
;;;; comes back to you while the old picture carries on.
;;;;
;;;; DOS has no threads, so the program takes turns: draw a frame, look at
;;;; the network, draw a frame...
;;;; Needs a packet driver loaded and WATTCP.CFG in the current directory.
;;;; Start with (remote); any key on the DOS machine, or (quit-demo) from
;;;; the remote side, stops it.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.remote
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib
        #:doslib-sockets)
  (:export #:main))
(in-package #:doslib-examples.remote)

(defvar *frames* 0)
(defvar *running* t)
(defvar *listener* nil)
(defvar *client* nil)
                   ; the connected socket and its stream
(defvar *stream* nil)
(defvar *input* (make-string-output-stream))
  ; text received, not yet a whole form
(defvar *pending* "")
(defvar *last-good* nil)
                ; the PIXEL that last drew a whole frame
(defun pixel (x y time)                 ; the picture: a colour 0..255 per cell
  (logand 255 (round (* 40 (+ 3 (sin (/ (+ x time) 9.0)) (sin (/ (- y time) 7.0))
                              (sin (/ (+ x y) 11.0)))))))
(defun quit-demo () (setf *running* nil) :bye)
;;; The screen
(defun status-line (text)               ; BIOS text on the bottom row
  (loop for offset from (* 320 192) below (* 320 200) by 4   ; wipe it first: the BIOS
        do (setf (sap-ref-32 *vga* offset) 0))               ; XORs colours above 127
  (int86 #x10 :ax #x0200 :bx 0 :dx (ash 24 8))
  (loop for c across (format nil "~39A" (subseq text 0 (min 39 (length text))))
        do (int86 #x10 :ax (logior #x0E00 (char-code c)) :bx 255)))
(defun draw-frame ()
  (dotimes (y 48)
    (dotimes (x 80)
      (let ((word (* #x01010101 (logand 255 (pixel x y *frames*))))
            (offset (+ (* 4 x) (* 1280 y))))
        (dotimes (row 4)
          (setf (sap-ref-32 *vga* (+ offset (* 320 row))) word)))))
  (incf *frames*))
;;; The network
(defun say (control &rest arguments)    ; to the remote side, if there is one
  (when *stream*
    (handler-case (progn (apply #'format *stream* control arguments) (finish-output *stream*))
      (error () (hang-up)))))
(defun hang-up ()
  (when *stream* (ignore-errors (close *stream* :abort t)))
  (setf *stream* nil *client* nil *pending* "")
  (status-line "listening on port 2323"))
(defun ready-p (socket)                 ; anything to read or accept, right now?
  (socket-ready-p socket 0))
(defun evaluate (text)
  "Evaluate the first form in TEXT for the remote side; returns what is left of TEXT."
  ;; (read here as well as evaluated here: PIXEL and QUIT-DEMO are this package's)
  (let ((*package* (find-package '#:doslib-examples.remote)))
    (multiple-value-bind (form end)
        (handler-case (read-from-string text)
          (end-of-file () (return-from evaluate text))     ; not all here yet
          (error (e) (say "can't read that: ~A~%DOS> " e) (return-from evaluate "")))
      (status-line (string-trim " " (substitute #\Space #\Newline (subseq text 0 end))))
      (handler-case
          (let ((*standard-output* *stream*) (*error-output* *stream*))   ; warnings too
            (say "~{~S~%~}" (multiple-value-list (eval form))))
        (error (e) (say "~A: ~A~%" (type-of e) e)))
      (say "DOS> ")
      (string-left-trim '(#\Space #\Newline) (subseq text end)))))
(defun poll-network ()
  (when (and (null *client*) (ready-p *listener*))
    (setf *client* (socket-accept *listener*)
          *stream* (socket-make-stream *client* :input t :output t :external-format :latin-1))
    (status-line (format nil "remote: ~{~D~^.~}" (coerce (socket-peername *client*) 'list)))
    (say "~A ~A on ~A, frame ~D.~%(PIXEL x y time) paints the screen; redefine it.~%DOS> "
         (lisp-implementation-type) (lisp-implementation-version) (software-type) *frames*))
  (when *client*
    (loop for c = (handler-case (read-char-no-hang *stream* nil :eof) (error () :eof))
          while c
          do (cond ((eq c :eof) (hang-up) (return))
                   ((char= c #\Return))
                   (t (setf *pending* (concatenate 'string *pending* (string c)))
                      (when (char= c #\Newline)
                        (loop for before = *pending*          ; every complete form
                              do (setf *pending* (evaluate *pending*))
                              until (or (string= *pending* "") (eq *pending* before)))
                        (unless *client* (return))))))))
(defun remote (&key (port 2323))
  (setf *listener* (make-instance 'inet-socket :type :stream :protocol :tcp)
        *running* t *last-good* #'pixel)
  (setf (sockopt-reuse-address *listener*) t)
  (socket-bind *listener* #(0 0 0 0) port)
  (socket-listen *listener* 2)
  (video-mode #x13)
  (rainbow-palette)
  (set-palette 255 63 63 63)
  (status-line "listening on port 2323")
  (unwind-protect
       (loop while (and *running* (not (key-pressed-p)))
             do (handler-case (progn (draw-frame) (setf *last-good* #'pixel))
                  (error (e)            ; a bad PIXEL: say so, go back to the last good one
                    (say "~&PIXEL failed (~A: ~A)~%going back to the previous definition.~%DOS> "
                         (type-of e) (remove #\Newline (princ-to-string e)))
                    (setf (fdefinition 'pixel) *last-good*)))
                (poll-network))
    (say "~&The demo has ended.~%")
    (hang-up)
    (socket-close *listener*)
    (when (key-pressed-p) (wait-key))
    (video-mode 3))
  (format t "~D frames drawn.~%" *frames*))

(defun main (&key (port 2323))
  "A remote REPL into a running graphics program."
  (remote :port port))

(doslib-examples:define-example "remote" 'main
  :category :network
  :description "A remote REPL into a running graphics program.")
