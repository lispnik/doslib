;;;; Search
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(defun find-text (text line col direction)
  "The next occurrence of TEXT starting at or after LINE, COL -- or, going
backward, the last one starting at or before it: line and column, or NIL."
  (if (plusp direction)
      (loop for l from line below (nlines)
            for found = (search text (line-text l) :start2 (if (= l line) col 0) :test #'char-equal)
            when found return (values l found))
      (loop for l from line downto 0
            for found = (search text (line-text l) :from-end t :test #'char-equal
                                :end2 (if (= l line) (max 0 (min (length (line-text l)) (+ col (length text))))
                                          (length (line-text l))))
            when found return (values l found))))
(defun isearch (direction)
  (let ((origin-line (point-line)) (origin-col (point-col)) (text "") (failing nil))
    (flet ((look (line col)
             (multiple-value-bind (l c) (and (string/= text "") (find-text text line col direction))
               (setf failing (and (string/= text "") (null l)))
               (when l (goto l (if (plusp direction) (+ c (length text)) c))))))
      (loop
        (message "~:[~;Failing ~]I-search~:[ backward~;~]: ~A" failing (plusp direction) text)
        (redisplay)
        (let ((key (read-key)))
          (cond ((equal key "C-s") (setf direction 1) (look (point-line) (point-col)))
                ((equal key "C-r") (setf direction -1) (look (point-line) (1- (point-col))))
                ((equal key "<backspace>")
                 (setf text (subseq text 0 (max 0 (1- (length text)))))
                 (goto origin-line origin-col) (look origin-line origin-col))
                ((equal key "C-g") (goto origin-line origin-col) (error 'quit-command))
                ((or (= (length key) 1) (equal key "SPC"))
                 (setf text (concatenate 'string text (if (equal key "SPC") " " key)))
                 (if (plusp direction)
                     (look (point-line) (max 0 (- (point-col) (length text) -1)))
                     (look (point-line) (point-col))))
                (t (unless (equal key "RET") (push key *unread*))
                   (message "")
                   (return))))))))
(define-command isearch-forward () "Search forward as you type." (isearch 1))
(define-command isearch-backward () "Search backward as you type." (isearch -1))
(define-command replace-string ()
  "Replace every later occurrence of one string with another."
  (let* ((from (read-string "Replace string: "))
         (to (read-string (format nil "Replace ~A with: " from))) (count 0))
    (when (string= from "") (editor-error "Nothing to replace"))
    (loop (multiple-value-bind (l c) (find-text from (point-line) (point-col) 1)
            (unless l (return))
            (delete-between l c l (+ c (length from)))
            (multiple-value-bind (el ec) (insert-at l c to) (goto el ec))
            (incf count)))
    (message "Replaced ~D occurrence~:P" count)))
