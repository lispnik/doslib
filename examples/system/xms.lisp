;;;; Memory beyond the first megabyte, the DOS way: XMS and EMS
;;;;
;;;; A real-mode DOS program can address one megabyte.  Two schemes let it
;;;; use more, and by 1990 nearly every large program wanted one of them.
;;;;
;;;; XMS, extended memory (HIMEM.SYS; here HIMEMX).  The memory above 1 MB
;;;; cannot be addressed at all from real mode, so the driver copies: a
;;;; program allocates a block by size, gets a handle, and asks the driver
;;;; to move bytes between the block and its own memory.  The driver is not
;;;; an interrupt but a routine: INT 2Fh function 4310h gives its address,
;;;; and it is called with the function number in AH.  The library's
;;;; REAL-CALL does that from protected mode.
;;;;
;;;; EMS, expanded memory (EMM386, JEMM386; once a card of memory chips).
;;;; Bank switching: a 64K "page frame" in the upper memory area shows four
;;;; 16K pages at a time out of as many as the driver has, and INT 67h
;;;; chooses which.  Nothing is copied -- change the mapping and different
;;;; memory is simply there.
;;;;
;;;; This program asks each driver what it has, then proves it works: a
;;;; megabyte of XMS written in 32K pieces and read back in the opposite
;;;; order, and four EMS pages each seen through the same 16K
;;;; window.  Lisp itself has no need of either -- it is a protected-mode
;;;; program with all the memory there is -- but its DPMI host gets that
;;;; memory from the XMS driver, which is why the free figure is smaller
;;;; than the machine.
;;;;
;;;; The launcher's machine loads HIMEMX and no EMS driver, so there the
;;;; second half only says so: the EMS code here has not been run against a
;;;; driver.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.xms
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib)
  (:export #:main))
(in-package #:doslib-examples.xms)

(defconstant +piece+ 32768)             ; moved at a time, through a buffer in DOS memory
(defconstant +pieces+ 32)               ; a megabyte in all

;;; ---- XMS
(defvar *xms-segment* 0)
(defvar *xms-offset* 0)

(defun xms-present-p ()
  (when (= #x80 (lo (int86 #x2F :ax #x4300)))
    (multiple-value-bind (ax bx cx dx es) (real-int #x2F :ax #x4310)
      (declare (ignore ax cx dx))
      (setf *xms-segment* es *xms-offset* bx))
    t))

(defun xms (function &rest registers)
  "Call the XMS driver.  Returns AX BX DX; AX is 1 for success on most functions."
  (multiple-value-bind (ax bx cx dx)
      (apply #'real-call *xms-segment* *xms-offset* :ax (ash function 8) registers)
    (declare (ignore cx))
    (values ax bx dx)))

(defun xms-move (table-segment length from-handle from-offset to-handle to-offset)
  "Move LENGTH bytes.  A handle of 0 means real-mode memory, its offset a far pointer."
  (let ((table (int-sap (linear table-segment))))
    (setf (sap-ref-32 table 0) length
          (sap-ref-16 table 4) from-handle (sap-ref-32 table 6) from-offset
          (sap-ref-16 table 10) to-handle (sap-ref-32 table 12) to-offset))
  (multiple-value-bind (ax bx) (xms #x0B :ds table-segment :si 0)
    (unless (= ax 1) (error "The XMS driver would not move memory: error ~2,'0Xh" (lo bx)))))

(defun xms-demo ()
  (unless (xms-present-p)
    (format t "~&XMS: no extended memory driver (HIMEM.SYS) is loaded.~%")
    (return-from xms-demo nil))
  (multiple-value-bind (version revision hma) (xms 0)
    (format t "~&XMS ~D.~2,'0D, driver revision ~X.~2,'0X, at ~4,'0X:~4,'0X~:[~;; the high memory area exists~]~%"
            (bcd (hi version)) (bcd (lo version)) (hi revision) (lo revision)
            *xms-segment* *xms-offset* (= hma 1)))
  (multiple-value-bind (largest bx total) (xms 8)
    (declare (ignore bx))
    (format t "  free: ~:D KB, the largest block ~:D KB~%" total largest))
  (xms #x88)                            ; XMS 3: the same in 32 bits (EAX, EDX in the call block)
  (unless (= (lo (sap-ref-16 *rm* #x10)) #x80)
    (format t "  free, by the 32-bit call: ~:D KB (the 16-bit one stops at 65,535)~%"
            (sap-ref-32 *rm* #x14)))
  (multiple-value-bind (ok bx handle) (xms 9 :dx (floor (* +piece+ +pieces+) 1024))
    (unless (= ok 1)
      (format t "  could not allocate a megabyte: error ~2,'0Xh~%" (lo bx))
      (return-from xms-demo nil))
    (unwind-protect
         (with-dos-memory (segment (+ +piece+ 16))      ; the buffer, then the move table
           (let* ((buffer (int-sap (linear segment)))
                  (table-segment (+ segment (floor +piece+ 16)))
                  (far (ash segment 16)) (wrong 0) (start (timer-count)))
             (dotimes (piece +pieces+)                  ; out: each piece filled with its number
               (dotimes (i +piece+)
                 (setf (sap-ref-8 buffer i) (logand (+ piece (* i 7)) 255)))
               (xms-move table-segment +piece+ 0 far handle (* piece +piece+)))
             (loop for piece from (1- +pieces+) downto 0     ; back, last first
                   do (dotimes (i +piece+) (setf (sap-ref-8 buffer i) 0))
                      (xms-move table-segment +piece+ handle (* piece +piece+) 0 far)
                      (dotimes (i +piece+)
                        (unless (= (sap-ref-8 buffer i) (logand (+ piece (* i 7)) 255))
                          (incf wrong))))
             (format t "  handle ~4,'0Xh: ~:D KB out in ~D pieces and back, ~
                        ~:[~:D BYTES WRONG~;every byte right~*~] (~,2F s)~%"
                     handle (floor (* +piece+ +pieces+) 1024) +pieces+ (zerop wrong) wrong
                     (/ (- (timer-count) start) +timer-hz+ 1.0))))
      (xms #x0A :dx handle)))
  t)

;;; ---- EMS
(defun ems-present-p ()
  "The driver is a device called EMMXXXX0, and INT 67h points into it."
  (let ((segment (peek16 (+ (* 4 #x67) 2))))
    (and (plusp segment)
         (string= "EMMXXXX0"
                  (map 'string #'code-char
                       (loop for i below 8 collect (peek (+ (linear segment) 10 i))))))))

(defun ems (function &rest registers)
  "INT 67h.  Returns its status (0 is success), BX and DX."
  (multiple-value-bind (ax bx cx dx) (apply #'real-int #x67 :ax (ash function 8) registers)
    (declare (ignore cx))
    (values (hi ax) bx dx (lo ax))))

(defun ems-demo ()
  (unless (ems-present-p)
    (format t "~&EMS: no expanded memory driver (EMM386, JEMM386) is loaded.~%")
    (return-from ems-demo nil))
  (multiple-value-bind (status bx dx version) (ems #x46)
    (declare (ignore status bx dx))
    (format t "~&EMS ~D.~D" (ash version -4) (logand version 15)))
  (multiple-value-bind (status frame) (ems #x41)
    (unless (zerop status)
      (format t ": no page frame (status ~2,'0Xh)~%" status)
      (return-from ems-demo nil))
    (multiple-value-bind (status free total) (ems #x42)
      (declare (ignore status))
      (format t ", page frame at segment ~4,'0X; ~:D of ~:D pages of 16K free~%" frame free total))
    (multiple-value-bind (status bx handle) (ems #x43 :bx 4)
      (declare (ignore bx))
      (unless (zerop status)
        (format t "  could not allocate four pages: status ~2,'0Xh~%" status)
        (return-from ems-demo nil))
      (unwind-protect
           (let ((window (int-sap (linear frame))) (right 0))
             (dotimes (page 4)                          ; each page in turn, in the same window
               (ems #x44 :bx page :dx handle)           ; (AL = 0: the frame's first 16K)
               (dotimes (i 16384) (setf (sap-ref-8 window i) (logand (+ (* 37 page) i) 255))))
             (loop for page from 3 downto 0
                   do (ems #x44 :bx page :dx handle)
                      (when (dotimes (i 16384 t)
                              (unless (= (sap-ref-8 window i) (logand (+ (* 37 page) i) 255))
                                (return nil)))
                        (incf right)))
             (format t "  handle ~D: four pages written through ~4,'0X:0000 and read back, ~
                        ~D of 4 right~%"
                     handle frame right))
        (ems #x45 :dx handle))))
  t)

(defun main ()
  "Memory beyond the first megabyte, the DOS way: XMS and EMS."
  (xms-demo)
  (ems-demo)
  (values))

(doslib-examples:define-example "xms" 'main
  :category :system
  :description "Memory beyond the first megabyte, the DOS way: XMS and EMS.")
