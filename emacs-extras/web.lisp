;;;; The text browser as an editor mode.
;;;;
;;;;   M-x browse-url     asks for an address (http://... or gopher://...; a
;;;;                      bare host name is taken as http://) and shows the
;;;;                      page in a read-only buffer named after it
;;;;
;;;; In that buffer:
;;;;
;;;;   TAB, n             to the next link          S-TAB, p   to the one before
;;;;   RET                follow the link at or after point
;;;;   b                  back to the page before   r          fetch this page again
;;;;   g                  go to another address     q          put the buffer away
;;;;   SPC                a screenful on            ?          this list
;;;;
;;;; Links are the bright numbers in brackets: after the words of a link on a
;;;; Web page ("the manual[3]"), at the start of the line in a Gopher menu.
;;;; The link point is on is shown in reverse.  The first line of the buffer
;;;; is the address.  Everything else is the editor's:
;;;; move about, search with C-s, copy text out with M-w.
;;;;
;;;; The fetching and the boiling down of HTML and Gopher menus are those of
;;;; the "browse" example (examples/network/browse.lisp); only the showing is
;;;; done here.  There is no TLS, so https:// pages cannot be fetched.  While a
;;;; page is being fetched the editor waits: Esc gives up, and a server that
;;;; says nothing for 30 seconds is given up on.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(in-package #:doslib-emacs)

;;; What a browser buffer is showing.  PAGE is a PAGE of the browse example;
;;; LINKS is a vector of (number line start end): where in the buffer each
;;; link's marker is; HISTORY is a list of (page line col), the newest first.
;;; For drawing, which is done a line at a time and often: COLOURS is the
;;; colour of each line of the buffer, MARKS the links on each line.
(defstruct web-view page links history colours marks)
(defvar *web-views* (make-hash-table :test 'eq) "Browser buffer -> its WEB-VIEW.")
(defun web-view (&optional (b *buf*))
  (or (gethash b *web-views*) (editor-error "~A is not a browser buffer" (buf-name b))))

;;; ---- fetching
(defun web-address (text)
  "TEXT as typed, as a whole address."
  (let ((text (string-trim " " text)))
    (when (string= text "") (error 'quit-command))
    (if (search "://" text) text (concatenate 'string "http://" text))))
(defun web-with-search (url)
  "URL -- with the words to look for, asked for now, if it is a Gopher search."
  ;; a Gopher search (type 7) wants its words sent after the selector and a tab
  (if (and (> (length url) 9) (string-equal "gopher://" url :end2 9) (not (find #\Tab url))
           (let ((path (nth-value 3 (doslib-examples.browse::split-url url))))
             (and (> (length path) 1) (char= (char path 1) #\7))))
      (let ((words (string-trim " " (read-string "Search for: "))))
        (when (string= words "") (error 'quit-command))
        (format nil "~A~C~A" url #\Tab words))
      url))
(defun web-failure-p (page)
  "Is PAGE one of LOAD-PAGE's apologies -- nothing but lines in red -- rather than a page?"
  (let ((lines (doslib-examples.browse::page-lines page)))
    (and lines (null (doslib-examples.browse::page-links page))
         (every (lambda (line) (eql (car line) 12)) lines))))
(defun web-fetch (url)
  "The page at URL; an error, to be shown in the echo area, if it cannot be had."
  (when (and (> (length url) 6) (string-equal "https:" url :end2 6))
    (editor-error "There is no TLS on this machine: an https:// address cannot be fetched"))
  (draw-echo (format nil "Fetching ~A ...  (Esc gives up)" (substitute #\Space #\Tab url)))
  (let ((page (doslib-examples.browse::load-page url)))
    (when (web-failure-p page)
      ;; The echo area is one line and the address is already on the screen:
      ;; of "Could not fetch <address>:" and the reason, keep the reason.
      (let ((lines (mapcar #'cdr (doslib-examples.browse::page-lines page))))
        (when (and (rest lines) (eql (search "Could not fetch" (first lines)) 0)) (pop lines))
        (editor-error "~{~A~^ ~}" lines)))
    page))

;;; ---- the page as the text of a buffer
(defun web-find-links (b count)
  "Where the markers of links 1 to COUNT are in the text of B.
A Web page has [3] after the words of its third link, a Gopher menu [ 3] at
the start of the line.  They come in order, so each is looked for after the
one before: a [3] that the page itself happens to say is seldom in the way."
  (let ((found '()) (line 2) (col 0))
    (loop for k from 1 to count
          do (let ((markers (list (format nil "[~D]" k) (format nil "[~2D]" k))))
               (loop for i from line below (nlines b)
                     do (let* ((text (line-text i b))
                               (from (min (length text) (if (= i line) col 0)))
                               (hits (remove nil (mapcar (lambda (marker) (search marker text :start2 from))
                                                         markers)))
                               (start (and hits (reduce #'min hits)))
                               (end (and start (1+ (position #\] text :start start)))))
                          (when start
                            ;; at the start of a line, the whole line is the link (Gopher)
                            (push (list k i start (if (zerop start) (length text) end)) found)
                            (setf line i col end)
                            (return))))))
    (coerce (nreverse found) 'vector)))
(defun web-render (b &optional (line 0) (col 0))
  "Write the page B is showing out as its text, and put point at LINE, COL."
  (let* ((view (web-view b)) (page (web-view-page view)) (lines (buf-lines b))
         (url (substitute #\Space #\Tab (doslib-examples.browse::page-url page)))
         (count (length (doslib-examples.browse::page-links page))))
    (setf (fill-pointer lines) 0)
    (vector-push-extend (format nil "~A   (~D link~:P; ? for help)" url count) lines)
    (vector-push-extend "" lines)
    (dolist (text (or (doslib-examples.browse::page-lines page) '((7 . "(The page is empty.)"))))
      (vector-push-extend (cdr text) lines))
    (setf (web-view-links view) (web-find-links b count))
    (let ((colours (make-array (length lines) :initial-element 7))
          (marks (make-array (length lines) :initial-element nil)))
      (setf (aref colours 0) 11)
      (loop for text in (doslib-examples.browse::page-lines page) for i from 2
            do (setf (aref colours i) (car text)))
      (loop for link across (web-view-links view) do (push link (aref marks (second link))))
      (setf (web-view-colours view) colours (web-view-marks view) marks))
    (setf
          (buf-name b) url
          (buf-undo b) nil (buf-redo b) nil (buf-modified b) nil
          (buf-mark b) nil (buf-mark-active b) nil
          (buf-line b) (max 0 (min line (1- (length lines))))
          (buf-col b) (max 0 (min col (length (aref lines (buf-line b))))))
    ;; windows showing it start again from the top, as for a new buffer
    (dolist (w *windows*)
      (when (eq (win-buf w) b) (setf (win-top w) 0 (win-hscroll w) 0)))))
(defun web-line-attributes (b i text)
  "Colour line I of B as it is drawn: the page's own colour for the line, the
link markers brighter, and the link point is on in reverse."
  (let ((view (gethash b *web-views*)) (n (length text)))
    (when (and view (< i (length (web-view-colours view))))
      (let ((colour (aref (web-view-colours view) i)))
        (unless (= colour 7) (fill *attrs* colour :end n)))
      (dolist (link (aref (web-view-marks view) i))
        (destructuring-bind (start end) (cddr link)
          (let ((end (min end n)))
            (if (and (= i (buf-line b)) (<= start (buf-col b) end))
                (fill *attrs* #x70 :start start :end end)
                ;; a Gopher line has its colour already: only the number is made brighter
                (fill *attrs* 15 :start start
                                 :end (min end (1+ (or (position #\] text :start start) (1- end))))))))))))
(setf (get 'web-mode 'line-attributes) 'web-line-attributes
      (get 'web-mode 'kill-function) (lambda (b) (remhash b *web-views*)))  ; its pages go with it
(defun web-open (url)
  "Fetch URL and show it: in this buffer if it is a browser's, else in a new one."
  (let ((page (web-fetch (web-with-search url))))
    (if (eq (buf-mode *buf*) 'web-mode)
        (let ((view (web-view)))
          (push (list (web-view-page view) (point-line) (point-col)) (web-view-history view))
          (setf (web-view-page view) page))
        (let ((b (make-buf "")))
          (setf (buf-read-only b) t
                (gethash b *web-views*) (make-web-view :page page))
          (set-major-mode 'web-mode b)
          (select-buffer b)))
    (web-render *buf*)
    (message "~D line~:P, ~D link~:P" (length (doslib-examples.browse::page-lines page))
             (length (doslib-examples.browse::page-links page)))))

;;; ---- the commands
(define-command browse-url ()
  "Fetch a Web page (plain HTTP) or a Gopher menu and show it in a buffer."
  (web-open (web-address (read-string "Address (http:// or gopher://): "))))
(defun web-link-after-point (&optional at)
  "The first link after point -- or, with AT, the one point is in or the first after."
  (find-if (lambda (link)
             (destructuring-bind (line start end) (rest link)
               (or (> line (point-line))
                   (and (= line (point-line)) (if at (>= end (point-col)) (> start (point-col)))))))
           (web-view-links (web-view))))
(defun web-link-url (link)
  (nth (1- (first link)) (doslib-examples.browse::page-links (web-view-page (web-view)))))
(defun web-goto-link (link)
  (unless link (editor-error "There are no links on this page"))
  (goto (second link) (third link))
  (message "~A" (substitute #\Space #\Tab (web-link-url link))))      ; where it leads
(define-command web-next-link ()
  "Move to the next link; after the last, to the first."
  (let ((links (web-view-links (web-view))))
    (web-goto-link (or (web-link-after-point) (and (plusp (length links)) (aref links 0))))))
(define-command web-previous-link ()
  "Move to the link before point; before the first, to the last."
  (let ((links (web-view-links (web-view))))
    (web-goto-link (or (find-if (lambda (link)
                                  (or (< (second link) (point-line))
                                      (and (= (second link) (point-line)) (< (third link) (point-col)))))
                                links :from-end t)
                       (and (plusp (length links)) (aref links (1- (length links))))))))
(define-command web-follow-link ()
  "Fetch the page that the link at point, or the first one after it, leads to."
  (let ((link (web-link-after-point t)))
    (unless link (editor-error "No link at or after point"))
    (web-open (web-link-url link))))
(define-command web-back ()
  "Show the page that was in this buffer before, where you left it."
  (let ((view (web-view)))
    (unless (web-view-history view) (editor-error "This is the first page"))
    (destructuring-bind (page line col) (pop (web-view-history view))
      (setf (web-view-page view) page)
      (web-render *buf* line col))))
(define-command web-reload ()
  "Fetch the page in this buffer again."
  (let* ((view (web-view)) (line (point-line)) (col (point-col))
         (page (web-fetch (doslib-examples.browse::page-url (web-view-page view)))))
    (setf (web-view-page view) page)
    (web-render *buf* line col)
    (message "Fetched again: ~D line~:P" (length (doslib-examples.browse::page-lines page)))))
(define-command web-quit ()
  "Put the browser buffer away: show another buffer, and keep this one at the end of the list."
  (let* ((b *buf*)
         (other (or (find-if-not (lambda (o) (or (eq o b) (find o *windows* :key #'win-buf))) *buffers*)
                    (find b *buffers* :test-not #'eq))))
    (unless other (editor-error "There is no other buffer to show"))
    (select-buffer other)
    (setf *buffers* (append (remove b *buffers*) (list b)))))
(define-command web-help () "Say what the browser keys are."
  (message "TAB/n next link  S-TAB/p previous  RET follow  b back  g go to  r reload  q put away"))

(setf (get 'web-mode 'mode-name) "Web")
(dolist (binding
         '(("TAB" web-next-link) ("n" web-next-link) ("<backtab>" web-previous-link) ("p" web-previous-link)
           ("RET" web-follow-link) ("b" web-back) ("g" browse-url) ("r" web-reload) ("q" web-quit)
           ("SPC" scroll-up) ("?" web-help) ("h" web-help)))
  (bind-key (first binding) (second binding) 'web-mode))
