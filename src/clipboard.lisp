;;;; The clipboard, where the machine has one to share
;;;;
;;;; DOS has no clipboard.  Windows gave the DOS programs it ran a way to
;;;; its own, through interrupt 2Fh (functions 1700h to 1708h, "WINOLDAP"),
;;;; and an emulator can answer the same calls with the clipboard of the
;;;; computer it runs on: DOSBox-X does, when its setting "dos clipboard
;;;; api" is true.  So text can go between Lisp on DOS and the programs of
;;;; the host.  On plain DOS nothing answers, and CLIPBOARD-PRESENT-P is
;;;; false.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib)

(defconstant +clipboard-oem-text+ 7 "The clipboard format that is text in the DOS character set.")
(defconstant +clipboard-most+ 60000 "The most text passed in one piece: it goes through DOS memory.")

(defun clipboard-present-p ()
  "Is there a clipboard to share?  (Interrupt 2Fh, AX=1700h: something answers.)"
  (/= (real-int #x2F :ax #x1700) #x1700))

(defmacro with-clipboard-open (&body body)
  `(when (and (clipboard-present-p) (/= (real-int #x2F :ax #x1701) 0))
     (unwind-protect (progn ,@body) (real-int #x2F :ax #x1708))))

(defun clipboard-text ()
  "The text on the clipboard, or NIL if there is none (or no clipboard).
Lines end with a newline, as in Lisp."
  (with-clipboard-open
    (multiple-value-bind (ax bx cx dx) (real-int #x2F :ax #x1704 :dx +clipboard-oem-text+)
      (declare (ignore bx cx))
      (let ((size (+ ax (ash dx 16))))
        (when (< 0 size +clipboard-most+)
          (with-dos-memory (segment (+ size 16))
            (unless (zerop (real-int #x2F :ax #x1705 :dx +clipboard-oem-text+ :es segment :bx 0))
              (let ((base (linear segment)))
                (with-output-to-string (out)
                  (loop for i below size
                        for code = (peek (+ base i))
                        until (zerop code)
                        unless (= code 13)
                          do (write-char (code-char code) out)))))))))))

(defun (setf clipboard-text) (text)
  "Put TEXT on the clipboard, in place of what is there.  Returns TEXT; does
nothing where there is no clipboard, or for more than about 60000 characters."
  (let* ((bytes (with-output-to-string (out)
                  (loop for c across text
                        do (when (char= c #\Newline) (write-char #\Return out))
                           (write-char (if (< (char-code c) 256) c #\?) out))))
         (size (1+ (length bytes))))
    (when (< size +clipboard-most+)
      (with-clipboard-open
        (with-dos-memory (segment (+ size 16))
          (let ((base (linear segment)))
            (dotimes (i (length bytes))
              (setf (sap-ref-8 (int-sap (+ base i)) 0) (char-code (char bytes i))))
            (setf (sap-ref-8 (int-sap (+ base (length bytes))) 0) 0))
          (real-int #x2F :ax #x1702)
          (real-int #x2F :ax #x1703 :dx +clipboard-oem-text+ :es segment :bx 0
                         :si (ash size -16) :cx (logand size #xFFFF))))))
  text)
