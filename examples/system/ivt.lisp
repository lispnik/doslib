;;;; The real-mode interrupt vector table.  The first kilobyte of
;;;; memory holds 256 far pointers, one per interrupt.  This reads them and
;;;; works out who owns each one by matching its segment against DOS's memory
;;;; control blocks and the ROM areas.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.ivt
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:main))
(in-package #:doslib-examples.ivt)

(defun blocks ()                        ; ((first-segment last-segment name) ...)
  (multiple-value-bind (ax bx cx dx es) (real-int #x21 :ax #x5200)
    (declare (ignore ax cx dx))
    (loop with segment = (peek16 (linear es (- bx 2)))
          for type = (peek (linear segment))
          for owner = (peek16 (linear segment 1))
          for size = (peek16 (linear segment 3))
          while (member type '(77 90))
          unless (zerop owner)
            collect (list (1+ segment) (+ segment size)
                          (let ((name-at (linear (if (= owner 8) segment (1- owner)) 8)))
                            (if (= owner 8) "DOS data"
                                (coerce (loop for i below 8 for c = (peek (+ name-at i))
                                              while (<= 33 c 126) collect (code-char c))
                                        'string))))
          until (= type 90)
          do (setf segment (+ segment size 1)))))
(defun owner (segment offset blocks)
  (let ((address (linear segment offset)))
    (cond ((and (zerop segment) (zerop offset)) "(unused)")
          ((>= address #x100000) "DOS in the HMA")
          ((>= address #xF0000) "system BIOS ROM")
          ((>= address #xC0000) (if (< address #xC8000) "video BIOS ROM" "upper memory"))
          (t (or (third (find-if (lambda (b) (<= (first b) (ash address -4) (second b))) blocks))
                 (if (< address #x600) "BIOS data" "DOS kernel"))))))
(defvar *names* '((#x00 "divide error") (#x08 "timer tick, IRQ 0") (#x09 "keyboard, IRQ 1")
                  (#x10 "video services") (#x13 "disk services") (#x15 "system services")
                  (#x16 "keyboard services") (#x1C "user timer tick") (#x21 "DOS services")
                  (#x23 "Ctrl-C handler") (#x24 "critical error") (#x2F "multiplex")
                  (#x31 "DPMI services") (#x33 "mouse driver") (#x67 "EMS")))
(defun ivt ()
  (let ((blocks (blocks)) (tally '()))
    (format t "~&int  vector     owner             what it is for~%")
    (dotimes (n 256)
      (let* ((offset (peek16 (* 4 n))) (segment (peek16 (+ 2 (* 4 n))))
             (who (owner segment offset blocks)))
        (let ((entry (assoc who tally :test #'string=)))
          (if entry (incf (cdr entry)) (push (cons who 1) tally)))
        (let ((name (second (assoc n *names*))))
          (when name
            (format t " ~2,'0X  ~4,'0X:~4,'0X  ~17A ~A~%" n segment offset who name)))))
    (format t "All 256 vectors by owner:~%")
    (format t "~{~<~%~1,78:;  ~A~>~^,~}~%"
            (loop for (who . count) in (sort tally #'> :key #'cdr)
                  collect (format nil "~D ~A" count who)))))

(defun main ()
  "The real-mode interrupt vector table."
  (ivt))

(doslib-examples:define-example "ivt" 'main
  :category :system
  :description "The real-mode interrupt vector table.")
