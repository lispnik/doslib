;;;; An Emacs-like text editor for DOS, written in and extended
;;;; with Common Lisp.
;;;;
;;;;   (doslib-emacs:emacs)  or  (doslib-emacs:emacs "file")  or  (ed "file")
;;;;
;;;; It draws straight into text-mode video memory and reads the keyboard
;;;; through the BIOS.  Buffers, windows, a minibuffer with completion, a
;;;; kill ring, undo and redo, incremental search, numeric arguments (C-u),
;;;; keyboard macros (F3 ... F4), Lisp indentation and colouring, and a REPL
;;;; buffer.  An error in evaluated code opens a debugger that lists the
;;;; restarts (type a number to invoke one).  C-x d is Dired: a directory as
;;;; a buffer, to open, copy, rename and delete files from.  Every key runs a Lisp function looked up in a
;;;; keymap, so the editor is extended the way Emacs is -- from inside:
;;;;
;;;;   (define-command insert-date ()
;;;;     "Insert today's date."
;;;;     (multiple-value-bind (s m h day month year) (get-decoded-time)
;;;;       (insert (format nil "~D-~2,'0D-~2,'0D" year month day))))
;;;;   (bind-key "C-c d" 'insert-date)
;;;;
;;;; typed at the REPL (C-c C-z), or evaluated from any buffer with C-x C-e,
;;;; takes effect at once.  EMACSRC.LSP in the current directory is loaded
;;;; at startup.  C-h b lists every key; Alt is Meta (so is an ESC prefix).
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-emacs
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib)
  (:export #:buf #:buf-name #:buf-lines #:buf-line #:buf-col #:buf-goal
          #:buf-mark #:buf-mark-active #:buf-file #:buf-modified #:buf-mode
          #:buf-undo #:buf-redo #:buf-crlf #:buf-prompt #:buf-history
          #:buf-history-index #:buf-read-only #:buf-dir #:buf-entries
          #:buf-sort #:buf-p #:win #:win-buf #:win-top #:win-hscroll #:win-row
          #:win-height #:win-point #:win-p #:*buffers* #:*buf* #:*windows*
          #:*window* #:*commands* #:*message* #:*last-message* #:*kill-ring*
          #:*this-command* #:*last-command* #:*unread* #:*prefix-argument*
          #:*defining-macro* #:*macro-keys* #:*macro-mark* #:*last-macro*
          #:*repl-package* #:quit-command #:*inhibit-read-only*
          #:define-command #:message #:one-line #:editor-error #:make-buf
          #:find-buf #:nlines #:line-text #:current-line #:point-line
          #:point-col #:goto #:split-lines #:vector-insert #:vector-delete
          #:insert-at #:text-between #:delete-between #:insert #:buffer-string
          #:*screen* #:*flush-screen* #:*place-cursor* #:*leave-display* #:reset-display #:*attrs* #:put-string #:set-cursor #:whitespace-p
          #:delimiter-p #:*highlight-words* #:lisp-attributes #:region-bounds
          #:mode-name #:visual-col #:draw-window #:draw-echo #:redisplay
          #:extended-key #:mode-hook #:buffer-precious-p #:*attrs* #:*idle-hooks* #:add-idle-hook #:remove-idle-hook #:read-raw-key #:read-key #:*global-map* #:mode-map
          #:split-keys #:bind-key #:read-command #:read-string #:complete-from
          #:complete-file #:ask-y-or-n #:forward-char #:backward-char
          #:move-lines #:next-line #:previous-line #:beginning-of-line
          #:end-of-line #:beginning-of-buffer #:end-of-buffer #:scroll-up
          #:scroll-down #:recenter #:char-at #:at-end-p #:at-start-p
          #:forward-word #:backward-word #:goto-line #:self-insert #:newline
          #:tab-to-tab-stop #:quoted-insert #:open-line #:delete-char
          #:delete-backward-char #:kill-new #:kill-between #:kill-line
          #:kill-word #:backward-kill-word #:set-mark
          #:exchange-point-and-mark #:mark-whole-buffer #:kill-region
          #:copy-region #:yank #:revert-changes #:*changes* #:undo #:redo
          #:keyboard-quit #:find-text #:isearch #:isearch-forward
          #:isearch-backward #:replace-string #:escaped-p #:safe-start
          #:scan-parens #:find-open-paren #:find-close-paren
          #:matching-paren-position #:sexp-start-before-point #:*body-forms*
          #:body-form-p #:indent-under #:lisp-indentation #:indent-line
          #:newline-and-indent #:minibuffer-stream #:*minibuffer-stream*
          #:*editor-restart* #:*debugged* #:editor-debugger #:eval-captured
          #:read-forms #:printed #:eval-and-show #:eval-last-sexp #:eval-defun
          #:eval-buffer #:eval-expression #:repl-buffer #:repl-emit
          #:repl-prompt #:repl-prompt-line #:repl-input #:repl-return
          #:repl-history #:repl-previous-input #:repl-next-input
          #:repl-beginning-of-line #:select-buffer #:mode-for-file
          #:find-file-buffer #:visit #:find-file #:default-crlf #:name-buffer #:toggle-line-endings #:write-buffer #:save-buffer
          #:write-file #:load-file #:switch-to-buffer #:kill-buffer
          #:show-in-help #:list-buffers #:dentry #:dentry-name #:dentry-dir
          #:dentry-size #:dentry-time #:dentry-flag #:dentry-p
          #:*dired-name-column* #:cwd-directory #:directory-string
          #:parent-directory #:directory-name-p #:read-directory
          #:dired-sorted #:dired-line #:dired-render #:dired-read
          #:dired-buffer #:dired-entry #:dired-path #:dired-real-entry
          #:dired-attributes #:dired-change-directory #:check-unix
          #:dired-target #:file-name-of #:dired #:dired-next-line
          #:dired-previous-line #:dired-find-file
          #:dired-find-file-other-window #:dired-up-directory #:dired-refresh
          #:dired-flag-deletion #:dired-unflag #:dired-unflag-all
          #:dired-delete-entry #:dired-execute #:dired-delete #:dired-rename
          #:dired-copy #:dired-create-directory #:dired-load-file #:dired-sort
          #:dired-quit #:dired-help #:layout-windows #:split-window
          #:delete-other-windows #:other-window #:switch-to-repl #:first-line
          #:command-doc #:describe-key #:describe-bindings #:describe-function
          #:execute-command #:exit-editor #:start-macro #:end-macro
          #:call-macro #:end-or-call-macro #:read-prefix #:run-command #:emacs
          #:build-emacs))

;;; What you type at the editor's REPL, and evaluate from its buffers, is
;;; read in this package: Lisp, DOSLIB and everything the editor defines
;;; are at hand, and what you define cannot replace the editor's own
;;; functions by accident.
(defpackage #:doslib-emacs-user
  (:nicknames #:emacs-user)
  (:use #:cl #:sb-alien #:sb-ext #:sb-sys #:doslib #:doslib-emacs))
