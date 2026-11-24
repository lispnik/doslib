;;;; A mode for looking at and changing the bytes of a file or a disk sector.
;;;;
;;;;   M-x hex-find-file     the bytes of a file, sixteen to a line: the offset,
;;;;                         the bytes in hex, the same bytes as text
;;;;   M-x hex-disk-sector   one 512-byte sector of a DOS drive, to look at only
;;;;
;;;; The cursor is always on a byte.  The arrows, PgUp/PgDn, Home/End and
;;;; C-Home/C-End move by bytes and lines; g (or M-g) goes to an offset given
;;;; in hex; the echo area says where the cursor is and what is there.  In
;;;; the hex column two hex digits typed over a byte change it; TAB goes over
;;;; to the text column, where any character typed replaces the byte (and
;;;; TAB comes back).  Changed bytes are shown in yellow until they are
;;;; saved.  s (or C-x C-s, F2) writes the file back, u (or C-_) takes back
;;;; the last change, q leaves.  In the text column letters are text, so
;;;; there use C-x C-s, C-_ and M-g.  In a sector, n and p read the next and
;;;; the previous one.  Sectors are never written.
;;;;
;;;; The whole file is read into a vector of bytes; the buffer's lines are
;;;; only its picture.  A small file is drawn in full when it is opened (so
;;;; C-s can search the dump); a big one line by line as the cursor gets
;;;; there, since drawing a megabyte takes far longer than reading it.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

;;; What a hex buffer shows.  It lives in the buffer's ENTRIES slot (Dired's
;;; place for what its lines are drawn from).  The buffer is not given a FILE:
;;; the editor's own save-buffer would then write the dump over the file.
(defstruct (hex-state (:conc-name hex-) (:constructor hex-make-state) (:copier nil))
  (bytes (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (changed (make-array 0 :element-type 'bit) :type simple-bit-vector)  ; 1: differs from the disk
  changes                               ; list of (offset . the byte that was there), newest first
  pending                               ; offset of a byte whose first digit has just been typed
  file                                  ; a file's name, or
  drive sector)                         ; a drive (0 = A:) and the number of the sector shown

(defparameter *hex-eager-bytes* 131072
  "Files up to this size have all their lines drawn when they are opened.")
(defparameter *hex-max-bytes* (* 32 1024 1024) "The largest file hex-find-file will read.")
(defparameter *hex-changed-attribute* #x0E "The colour of a byte that has been changed.")
(defvar *hex-blank* (make-string 0 :element-type 'base-char)
  "Stands in the buffer for every line that has not been drawn yet.")

;;; A line:  00000010  48 65 6C 6C 6F 2C 20 77  6F 72 6C 64 0D 0A 00 FF  Hello, world....
(defparameter *hex-text-column* 60)
(defun hex-byte-column (i) (+ 10 (* 3 i) (if (>= i 8) 1 0)))
(defun hex-render-line (bytes line)
  "The text of line LINE of the dump of BYTES."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (type (unsigned-byte 24) line))
  (let* ((start (* 16 line)) (n (max 0 (min 16 (- (length bytes) start))))
         (digits "0123456789ABCDEF")
         (text (make-string (+ 60 n) :element-type 'base-char :initial-element #\Space)))
    (declare (fixnum start n))
    (dotimes (i 8)
      (setf (schar text i) (schar digits (ldb (byte 4 (* 4 (- 7 i))) start))))
    (dotimes (i n text)
      (let ((byte (aref bytes (+ start i))) (col (+ 10 (* 3 i) (if (>= i 8) 1 0))))
        (setf (schar text col) (schar digits (ash byte -4))
              (schar text (1+ col)) (schar digits (logand byte 15))
              (schar text (+ 60 i)) (if (< 31 byte 127) (code-char byte) #\.))))))
(defun hex-line-count (state) (max 1 (ceiling (length (hex-bytes state)) 16)))
(defun hex-ensure (b from to)
  "Draw the lines FROM to TO of the hex buffer B that are not drawn yet."
  (let ((lines (buf-lines b)) (bytes (hex-bytes (buf-entries b))))
    (loop for i from (max 0 from) to (min to (1- (length lines)))
          do (when (eq (aref lines i) *hex-blank*)
               (setf (aref lines i) (hex-render-line bytes i))))))
(defun hex-redraw-line (b line)         ; after a byte on it has changed
  (unless (eq (aref (buf-lines b) line) *hex-blank*)
    (setf (aref (buf-lines b) line) (hex-render-line (hex-bytes (buf-entries b)) line))))
(defun hex-fill (b)
  "Make the lines of B stand for its bytes; draw them all only if there are few."
  (let* ((state (buf-entries b)) (lines (buf-lines b)) (count (hex-line-count state)))
    (setf (fill-pointer lines) 0)
    (dotimes (i count) (vector-push-extend *hex-blank* lines (max 16 count)))
    (if (<= (length (hex-bytes state)) *hex-eager-bytes*)
        (hex-ensure b 0 count)
        (hex-ensure b 0 64))
    (setf (buf-undo b) nil (buf-redo b) nil (buf-modified b) nil (buf-mark-active b) nil
          (buf-line b) 0 (buf-col b) (hex-byte-column 0))))

;;; Where the cursor is.  Nothing is remembered but the editor's own point,
;;; so that a command of the editor's that moves it (a search, say) leaves
;;; the mode knowing where it is.
(defun hex-state ()
  (if (eq (buf-mode *buf*) 'hex-mode) (buf-entries *buf*) (editor-error "Not a hex buffer")))
(defun hex-where ()
  "The byte under the cursor: its offset, the column it is in (:HEX or :TEXT), which digit (0, 1)."
  (let* ((state (hex-state)) (size (length (hex-bytes state)))
         (col (point-col)) (first (* 16 (point-line)))
         (last (max 0 (min 15 (- size first 1)))))
    (if (>= col *hex-text-column*)
        (values (+ first (min last (- col *hex-text-column*))) :text 0)
        (let* ((c (max 0 (- col 10))) (c (if (>= c 25) (1- c) c)) (i (min 15 (floor c 3))))
          (values (+ first (min last i)) :hex (if (and (<= i last) (= (mod c 3) 1)) 1 0))))))
(defun hex-goto (offset &optional (pane :hex) (digit 0))
  "Put the cursor on the byte at OFFSET (or the nearest there is)."
  (let* ((state (hex-state)) (size (length (hex-bytes state)))
         (offset (max 0 (min offset (1- size)))) (line (floor offset 16)) (i (mod offset 16)))
    (hex-ensure *buf* (- line 30) (+ line 60))           ; all a window could show around it
    (goto line (if (eq pane :text) (+ *hex-text-column* i) (+ (hex-byte-column i) digit)))))
(defun hex-title (state)
  (if (hex-file state)
      (file-name-of (hex-file state))
      (format nil "~C: sector ~D" (code-char (+ 65 (hex-drive state))) (hex-sector state))))
(defun hex-show ()
  "Say in the echo area where the cursor is."
  (let* ((state (hex-state)) (bytes (hex-bytes state)) (offset (hex-where)))
    (if (zerop (length bytes))
        (message "~A is empty" (hex-title state))
        (let ((byte (aref bytes offset)))
          (message "Offset ~8,'0X (~D) of ~D: ~2,'0Xh = ~D~@[ = '~C'~]   ~A" offset offset
                   (length bytes) byte byte (and (< 32 byte 127) (code-char byte))
                   (hex-title state))))))
(defun hex-move (delta)
  (multiple-value-bind (offset pane) (hex-where)
    (let ((target (+ offset delta)) (size (length (hex-bytes (hex-state)))))
      (setf (hex-pending (hex-state)) nil)
      (cond ((and (minusp target) (= (abs delta) 1)) (editor-error "Beginning of file"))
            ((and (>= target size) (= (abs delta) 1)) (editor-error "End of file"))
            ((minusp target) (setf target (mod offset 16)))           ; stay in the column
            ((>= target size)
             (setf target (+ (* 16 (floor (max 0 (1- size)) 16)) (mod offset 16)))
             (when (>= target size) (setf target (max 0 (1- size))))))
      (hex-goto target pane)
      (hex-show))))

;;; Colour.  The editor asks the mode for the colours of each line it draws
;;; (the LINE-ATTRIBUTES property of the mode's name): the offsets are dim,
;;; a changed byte is yellow in both columns, and the byte the cursor is on
;;; is picked out in the column the cursor is not in.
(defun hex-attributes (b line text)
  (let* ((state (buf-entries b)) (n (length text))
         (changed (hex-changed state)) (first (* 16 line)) (size (length (hex-bytes state))))
    (when (>= n 8) (fill *attrs* 8 :end 8))
    (when (hex-changes state)           ; nothing is changed otherwise
      (dotimes (i (min 16 (- size first)))
        (when (= 1 (sbit changed (+ first i)))
          (let ((col (hex-byte-column i)))
            (when (< (+ *hex-text-column* i) n)
              (setf (aref *attrs* col) *hex-changed-attribute*
                    (aref *attrs* (1+ col)) *hex-changed-attribute*
                    (aref *attrs* (+ *hex-text-column* i)) *hex-changed-attribute*))))))
    (when (and (= line (buf-line b)) (plusp size) (> n *hex-text-column*))
      (multiple-value-bind (offset pane) (let ((*buf* b)) (hex-where))
        (let* ((i (- offset first)) (col (hex-byte-column i)))
          (flet ((mark (k) (setf (aref *attrs* k) (logior #x70 (if (= (aref *attrs* k) 7) 0 4)))))
            (if (eq pane :hex)
                (mark (+ *hex-text-column* i))
                (progn (mark col) (mark (1+ col))))))))))
(setf (get 'hex-mode 'line-attributes) 'hex-attributes
      (get 'hex-mode 'ask-before-kill) t)           ; C-x k and C-x C-c ask about unsaved bytes

;;; Changing bytes
(defun hex-writable (state)
  (unless (hex-file state) (editor-error "A disk sector can only be looked at"))
  (when (zerop (length (hex-bytes state))) (editor-error "There is no byte here")))
(defun hex-set-byte (offset new &optional continued)
  "Change the byte at OFFSET, remembering the old one -- unless CONTINUED: the
second digit of a byte belongs to the same change as the first."
  (let ((state (hex-state)))
    (unless continued (push (cons offset (aref (hex-bytes state) offset)) (hex-changes state)))
    (setf (aref (hex-bytes state) offset) new (sbit (hex-changed state) offset) 1
          (buf-modified *buf*) t)
    (hex-redraw-line *buf* (floor offset 16))))
(defun hex-type (char)
  "What a typed character does: in the text column it replaces the byte; in
the hex column a hex digit replaces half of it, and other letters are commands."
  (let ((state (hex-state)))
    (multiple-value-bind (offset pane digit) (hex-where)
      (let ((value (digit-char-p char 16))
            (command (cdr (assoc char '((#\g . hex-goto-offset) (#\s . hex-save) (#\u . hex-undo)
                                        (#\n . hex-next-sector) (#\p . hex-previous-sector)
                                        (#\q . hex-quit) (#\? . hex-help) (#\h . hex-help)
                                        (#\Space . hex-forward))))))
        (cond ((and (eq pane :text) (hex-file state))
               (hex-writable state)
               (hex-set-byte offset (char-code char))
               (setf (hex-pending state) nil)
               (hex-goto (1+ offset) :text))
              (command (funcall command) (return-from hex-type))
              ((null value) (editor-error "~C is not a hex digit (TAB goes to the text column)" char))
              (t (hex-writable state)
                 (let ((old (aref (hex-bytes state) offset)))
                   (cond ((= digit 0)
                          (hex-set-byte offset (logior (ash value 4) (logand old 15)))
                          (setf (hex-pending state) offset)
                          (hex-goto offset :hex 1))
                         (t (hex-set-byte offset (logior (logand old #xF0) value)
                                          (eql (hex-pending state) offset))
                            (setf (hex-pending state) nil)
                            (if (< (1+ offset) (length (hex-bytes state)))
                                (hex-goto (1+ offset) :hex 0)
                                (hex-goto offset :hex 1)))))))
        (hex-show)))))

;;; Files
(defun hex-read-file (name)
  (unless (and (probe-file name) (not (directory-name-p name))) (editor-error "No file ~A" name))
  (with-open-file (in name :element-type '(unsigned-byte 8))
    (when (> (file-length in) *hex-max-bytes*)
      (editor-error "~A is too big (~:D bytes)" name (file-length in)))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))
(defun hex-set-bytes (state bytes)
  (setf (hex-bytes state) bytes (hex-changes state) nil (hex-pending state) nil
        (hex-changed state) (make-array (length bytes) :element-type 'bit :initial-element 0)))
(defun hex-find-buffer (test)
  "The hex buffer whose state satisfies TEST, if there is one."
  (find-if (lambda (b) (and (eq (buf-mode b) 'hex-mode) (funcall test (buf-entries b)))) *buffers*))
(defun hex-make-buffer (name state)
  (let ((b (make-buf name)))
    (setf (buf-read-only b) t (buf-entries b) state)
    (set-major-mode 'hex-mode b)
    (hex-fill b)
    b))

(define-command hex-find-file ()
  "Show the bytes of a file, in hex and as text, and change them."
  (let ((name (string-trim " " (read-string "Hex file: " :complete #'complete-file))))
    (when (string= name "") (error 'quit-command))
    (select-buffer
     (or (hex-find-buffer (lambda (state) (and (hex-file state) (string-equal (hex-file state) name))))
         (let ((state (hex-make-state :file name)))
           (hex-set-bytes state (hex-read-file name))
           (hex-make-buffer (format nil "*hex ~A*" (string-upcase (file-namestring name))) state))))
    (hex-show)))
(define-command hex-save ()
  "Write the bytes back to the file."
  (let ((state (hex-state)))
    (unless (hex-file state) (editor-error "A disk sector can only be looked at"))
    (with-open-file (out (hex-file state) :direction :output :if-exists :supersede
                                          :element-type '(unsigned-byte 8))
      (write-sequence (hex-bytes state) out))
    (fill (hex-changed state) 0)        ; the disk now has them: nothing is "changed"
    (setf (hex-changes state) nil (hex-pending state) nil (buf-modified *buf*) nil)
    (message "Wrote ~:D byte~:P to ~A" (length (hex-bytes state)) (hex-file state))))
(define-command hex-undo ()
  "Take back the last change to a byte."
  (let* ((state (hex-state)) (change (pop (hex-changes state))))
    (unless change (editor-error "No changes to take back"))
    (destructuring-bind (offset . old) change
      (setf (aref (hex-bytes state) offset) old
            (sbit (hex-changed state) offset) (if (assoc offset (hex-changes state)) 1 0)
            (hex-pending state) nil
            (buf-modified *buf*) (and (hex-changes state) t))
      (hex-redraw-line *buf* (floor offset 16))
      (hex-goto offset (nth-value 1 (hex-where)))
      (hex-show))))

;;; Moving
(define-command hex-forward () "Move to the next byte." (hex-move 1))
(define-command hex-backward () "Move to the previous byte." (hex-move -1))
(define-command hex-next-line () "Move down a line: sixteen bytes on." (hex-move 16))
(define-command hex-previous-line () "Move up a line: sixteen bytes back." (hex-move -16))
(define-command hex-scroll-up () "Move forward a screenful of bytes."
  (hex-move (* 16 (max 1 (- (win-height *window*) 2)))))
(define-command hex-scroll-down () "Move back a screenful of bytes."
  (hex-move (* -16 (max 1 (- (win-height *window*) 2)))))
(define-command hex-beginning-of-line () "Move to the first byte of the line."
  (multiple-value-bind (offset pane) (hex-where)
    (hex-goto (* 16 (floor offset 16)) pane)
    (hex-show)))
(define-command hex-end-of-line () "Move to the last byte of the line."
  (multiple-value-bind (offset pane) (hex-where)
    (hex-goto (+ 15 (* 16 (floor offset 16))) pane)
    (hex-show)))
(define-command hex-beginning () "Move to the first byte."
  (hex-goto 0 (nth-value 1 (hex-where)))
  (hex-show))
(define-command hex-end () "Move to the last byte."
  (hex-goto (length (hex-bytes (hex-state))) (nth-value 1 (hex-where)))
  (hex-show))
(define-command hex-goto-offset ()
  "Go to the byte at an offset, given in hex."
  (let* ((text (string-trim " hH" (read-string "Go to offset (hex): ")))
         (offset (and (string/= text "") (ignore-errors (parse-integer text :radix 16)))))
    (unless offset (editor-error "~S is not a hex number" text))
    (when (>= offset (max 1 (length (hex-bytes (hex-state)))))
      (editor-error "Offset ~X is past the end (the last byte is at ~X)" offset
                    (max 0 (1- (length (hex-bytes (hex-state)))))))
    (hex-goto offset (nth-value 1 (hex-where)))
    (hex-show)))
(define-command hex-toggle-column ()
  "Go over to the text column, or back to the hex column."
  (multiple-value-bind (offset pane) (hex-where)
    (setf (hex-pending (hex-state)) nil)
    (hex-goto offset (if (eq pane :hex) :text :hex))
    (hex-show)))
(define-command hex-quit () "Leave the hex buffer (asking first if bytes were changed and not saved)."
  (kill-buffer))
(define-command hex-help () "Say what the keys of the hex mode are."
  (message "0-9 a-f change  TAB text column  g offset  s save  u undo  n/p sector  q quit"))

;;; Disk sectors.  DOS reads a sector of one of its drives, counted from the
;;; start of the volume (so sector 0 is the boot sector, whatever partition
;;; the volume is), with interrupt 25h.  There are three ways of asking, and
;;; which works depends on the volume: a packet with a 32-bit sector number
;;; (CX = FFFFh; volumes over 32MB), interrupt 21h function 7305h (FAT32,
;;; which interrupt 25h refuses), and the original form with the sector in
;;; DX (floppies and small volumes).  The library's DOS-READ-SECTORS finds
;;; out which (src/disk.lisp).
(defvar *hex-sector-method* nil "Which of the three ways read the last sector, for the curious.")
(defun hex-read-sector (drive sector)
  "Sector SECTOR of DRIVE (0 is A:, 2 is C:) as a vector of 512 bytes."
  (catch-critical-errors)               ; an empty drive: no "Abort, Retry, Fail?" on the screen
  (unwind-protect
       (progn
         (critical-error)               ; forget any old one
         (handler-case
             (prog1 (dos-read-sectors drive sector)
               (setf *hex-sector-method*
                     (ecase (drive-read-method drive)
                       (:large "INT 25h, packet") (:fat32 "INT 21h AX=7305h") (:small "INT 25h"))))
           (disk-error (e) (editor-error "~A" e))))
    (release-critical-errors)))
(defun hex-sector-name (drive sector) (format nil "*sector ~C:~D*" (code-char (+ 65 drive)) sector))
(define-command hex-disk-sector ()
  "Show one sector of a DOS drive (A, C, D...), read with interrupt 25h.  n and p step."
  (let* ((letter (string-trim " :" (read-string "Drive (A, C, D): " :initial "D")))
         (drive (and (= (length letter) 1) (alpha-char-p (char letter 0))
                     (- (char-code (char-upcase (char letter 0))) 65)))
         (sector (and drive (parse-integer (string-trim " " (read-string "Sector: " :initial "0"))
                                           :junk-allowed t))))
    (unless drive (editor-error "~S is not a drive letter" letter))
    (unless (and sector (>= sector 0)) (editor-error "Not a sector number"))
    (let ((b (hex-find-buffer (lambda (state) (eql (hex-drive state) drive)))))
      (cond (b (select-buffer b)
               (hex-step-sector (- sector (hex-sector (buf-entries b)))))
            (t (let ((state (hex-make-state :drive drive :sector sector)))
                 (hex-set-bytes state (hex-read-sector drive sector))
                 (select-buffer (hex-make-buffer (hex-sector-name drive sector) state))
                 (hex-show)))))))
(defun hex-step-sector (delta)
  (let* ((state (hex-state)) (sector (and (hex-drive state) (+ (hex-sector state) delta))))
    (unless sector (editor-error "This is a file, not a disk sector"))
    (when (minusp sector) (editor-error "This is the first sector"))
    (hex-set-bytes state (hex-read-sector (hex-drive state) sector))     ; an error leaves all as it was
    (setf (hex-sector state) sector (buf-name *buf*) (hex-sector-name (hex-drive state) sector))
    (hex-fill *buf*)
    (hex-show)))
(define-command hex-next-sector () "Show the next sector of the drive." (hex-step-sector 1))
(define-command hex-previous-sector () "Show the previous sector of the drive." (hex-step-sector -1))

;;; Keys.  Every printing character gets a command of its own that hands the
;;; character to HEX-TYPE (a command is not told which key ran it).  They
;;; are not in *COMMANDS*: ninety-five of them would only clutter M-x.
(setf (get 'hex-mode 'mode-name) "Hex")
(loop for code from 32 to 126
      do (let ((char (code-char code))
               (name (intern (format nil "HEX-TYPE-~2,'0X" code) '#:doslib-emacs)))
           (setf (fdefinition name) (lambda () (hex-type char))
                 (documentation name 'function) "Type this character over the byte.")
           (bind-key (if (char= char #\Space) "SPC" (string char)) name 'hex-mode)))
(dolist (binding
         '(("<right>" hex-forward) ("C-f" hex-forward) ("<left>" hex-backward) ("C-b" hex-backward)
           ("<backspace>" hex-backward)
           ("<down>" hex-next-line) ("C-n" hex-next-line) ("<up>" hex-previous-line) ("C-p" hex-previous-line)
           ("<next>" hex-scroll-up) ("C-v" hex-scroll-up) ("<prior>" hex-scroll-down) ("M-v" hex-scroll-down)
           ("<home>" hex-beginning-of-line) ("C-a" hex-beginning-of-line)
           ("<end>" hex-end-of-line) ("C-e" hex-end-of-line)
           ("C-<home>" hex-beginning) ("M-<" hex-beginning) ("C-<end>" hex-end) ("M->" hex-end)
           ("M-g" hex-goto-offset) ("TAB" hex-toggle-column)
           ("C-x C-s" hex-save) ("<f2>" hex-save) ("C-_" hex-undo) ("C-x u" hex-undo)))
  (bind-key (first binding) (second binding) 'hex-mode))
