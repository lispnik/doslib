;;;; Keyboard macros: the keys typed between F3 and F4, played back by F4
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

(define-command start-macro () "Start recording a keyboard macro."
  (setf *defining-macro* t *macro-keys* '())
  (message "Defining keyboard macro..."))
(define-command end-macro () "Finish recording a keyboard macro."
  (unless *defining-macro* (editor-error "Not defining a keyboard macro"))
  ;; leave out the keys that ran this command
  (setf *last-macro* (reverse (nthcdr (- (length *macro-keys*) *macro-mark*) *macro-keys*))
        *defining-macro* nil)
  (message "Keyboard macro defined (~D key~:P)" (length *last-macro*)))
(define-command call-macro () "Play back the last keyboard macro."
  (unless *last-macro* (editor-error "No keyboard macro has been defined"))
  (when *defining-macro* (editor-error "Still defining a keyboard macro"))
  (dotimes (i (or *prefix-argument* 1))  ; C-u 5 F4 plays it five times
    (setf *unread* (append *last-macro* *unread*))))
(define-command end-or-call-macro () "Finish recording a keyboard macro, or play the last one back."
  (if *defining-macro* (end-macro) (call-macro)))
(setf (get 'call-macro 'takes-prefix) t (get 'end-or-call-macro 'takes-prefix) t)
