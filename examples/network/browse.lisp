;;;; A text browser for Gopher and plain HTTP.
;;;;
;;;;   (doslib-examples:run "browse")                               a Gopher server
;;;;   (doslib-examples:run "browse" "http://example.com/")
;;;;
;;;; Links are numbered: type a number and Enter to follow one.  The arrow
;;;; keys, PgUp and PgDn scroll; B goes back; G asks for an address; Q or
;;;; Esc leaves.
;;;;
;;;; Gopher is the easy half: a menu is lines of text, one item each.  For
;;;; the Web, HTML is boiled down to its words and its links.  There is no
;;;; TLS, so pages that are only served over HTTPS cannot be shown -- on
;;;; today's Web, that is most of them; Gopher has no such trouble.
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.browse
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib
        #:doslib-sockets)
  (:export #:main))
(in-package #:doslib-examples.browse)

(defstruct page url (lines '()) (links '()))   ; lines: (colour . text); links: urls, in order

;;; ---- fetching
(defparameter *patience* 30 "Seconds to wait for a server that has gone quiet.")
(defun fetch (host port request)
  "Send REQUEST to HOST and return everything that comes back, as a string.
A server that says nothing for *PATIENCE* seconds is given up on; Esc gives
up at once."
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (socket-connect socket
                           (if (every (lambda (c) (or (digit-char-p c) (char= c #\.))) host)
                               (make-inet-address host)
                               (host-ent-address (get-host-by-name host)))
                           port)
           (let ((stream (socket-make-stream socket :output t :external-format :latin-1
                                                    :buffering :full)))
             (write-string request stream)
             (finish-output stream))
           ;; Read from the socket itself, and only when it has something: a
           ;; read from a stream would wait for ever on a server that never
           ;; answers, and there is only this one thread.
           (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                 (quiet-since (get-internal-real-time)))
             (with-output-to-string (out)
               (loop
                 (cond ((socket-ready-p socket 250)
                        (let ((n (nth-value 1 (socket-receive socket buffer nil))))
                          (when (or (null n) (<= n 0)) (return))
                          (dotimes (i n) (write-char (code-char (aref buffer i)) out))
                          (setf quiet-since (get-internal-real-time))))
                       ((and (key-pressed-p) (eql (get-key) (code-char 27)))
                        (error "Stopped: you pressed Esc."))
                       ((> (- (get-internal-real-time) quiet-since)
                           (* *patience* internal-time-units-per-second))
                        (error "The server has said nothing for ~D seconds." *patience*)))))))
      (ignore-errors (socket-close socket)))))

(defun scheme-end (url)
  "Where the colon after URL's scheme is, if it starts with one (letters, then a colon)."
  (let ((colon (position #\: url)))
    (and colon (plusp colon) (every #'alpha-char-p (subseq url 0 colon)) colon)))

(defun split-url (url)
  "URL as scheme, host, port and path."
  (let* ((colon (let ((at (scheme-end url)))       ; "host:8080/x" has a colon but no scheme
                  (and at (eql (search "//" url :start2 at) (1+ at)) at)))
         (scheme (if colon (string-downcase (subseq url 0 colon)) "http"))
         (start (if colon (+ colon 3) 0))
         (slash (or (position-if (lambda (c) (find c "/?#")) url :start start) (length url)))
         (port-colon (position #\: url :start start :end slash)))
    (values scheme
            (subseq url start (or port-colon slash))
            (or (and port-colon (parse-integer url :start (1+ port-colon) :end slash :junk-allowed t))
                (if (string= scheme "gopher") 70 80))
            (if (and (< slash (length url)) (char= (char url slash) #\/))
                (subseq url slash)
                (concatenate 'string "/" (subseq url slash))))))        ; "http://host?q=1"

(defun tidy-path (path)
  "PATH without its . and .. steps: /a/b/../c.html is /a/c.html."
  (let ((query (position #\? path)) (steps '()))
    (loop with start = 1
          for slash = (position #\/ path :start start :end query)
          for step = (subseq path start (or slash query))
          do (cond ((string= step ".") (unless slash (push "" steps)))
                   ((string= step "..") (pop steps) (unless slash (push "" steps)))
                   (t (push step steps)))
          while slash do (setf start (1+ slash)))
    (format nil "~{/~A~}~A" (nreverse steps) (if query (subseq path query) ""))))

(defun join-url (base link)
  "LINK, as written in the page at BASE, as a whole address."
  (multiple-value-bind (scheme host port path) (split-url base)
    (let* ((origin (format nil "~A://~A~:[:~D~;~*~]" scheme host (member port '(70 80)) port))
           ;; where the page is, without its own ?query and #place, which may hold slashes
           (path (subseq path 0 (position-if (lambda (c) (find c "?#")) path)))
           (link (subseq link 0 (position #\# link))))          ; a place within a page: no use here
      (flet ((here (&rest parts) (apply #'concatenate 'string origin parts)))
        (cond ((scheme-end link) link)                          ; http://..., and mailto: and the like
              ((string= link "") (here path))
              ((and (> (length link) 1) (string= (subseq link 0 2) "//"))
               (concatenate 'string scheme ":" link))
              ((char= (char link 0) #\/) (here (tidy-path link)))
              ((char= (char link 0) #\?) (here path link))
              (t (here (tidy-path (concatenate 'string (subseq path 0 (1+ (position #\/ path :from-end t)))
                                               link)))))))))

;;; ---- Gopher: a menu is a line per item: type, text, selector, host, port
(defun split-tabs (line)
  (loop with start = 0
        for tab = (position #\Tab line :start start)
        collect (subseq line start tab)
        while tab do (setf start (1+ tab))))
(defun gopher-page (url text)
  (let ((page (make-page :url url)))
    (dolist (line (split-lines text))
      (unless (or (string= line "") (string= line "."))
        ;; &rest: Gopher+ servers add a fifth field
        (destructuring-bind (label &optional (selector "") (host "") (port "70") &rest more)
            (split-tabs (subseq line 1))
          (declare (ignore more))
          (let ((type (char line 0)))
            (cond ((char= type #\i) (push (cons 7 (format nil "      ~A" label)) (page-lines page)))
                  ((find type "01h7")
                   (push (if (and (char= type #\h) (> (length selector) 4)
                                  (string-equal "URL:" selector :end2 4))
                             (subseq selector 4)
                             (format nil "gopher://~A~:[:~A~;~*~]/~C~A"
                                     host (string= port "70") port type selector))
                         (page-links page))
                   (push (cons (if (char= type #\1) 11 14)
                               (format nil "[~2D] ~A~A" (length (page-links page)) label
                                       (case type (#\1 "/") (#\7 " (search)") (t ""))))
                         (page-lines page)))
                  ((char= type #\3) (push (cons 12 (format nil "      ~A" label)) (page-lines page)))
                  (t (push (cons 8 (format nil "      ~A  (type ~C: not shown)" label type))
                           (page-lines page))))))))
    (setf (page-lines page) (nreverse (page-lines page)) (page-links page) (nreverse (page-links page)))
    page))
(defun split-lines (text)
  (loop with start = 0
        for newline = (position #\Newline text :start start)
        collect (string-right-trim '(#\Return) (subseq text start newline))
        while newline do (setf start (1+ newline))))
(defun text-page (url text)
  (make-page :url url
             ;; a Gopher text file ends with a line holding just a full stop
             :lines (loop for line in (let ((lines (split-lines text)))
                                        (subseq lines 0 (position "." lines :test #'string= :from-end t)))
                          append (mapcar (lambda (l) (cons 7 l))
                                         ;; a tab is not a character the screen can show
                                         (text-wrap (substitute #\Space #\Tab line) 79)))))

;;; ---- HTML, boiled down: words, line breaks, and numbered links
(defun entity (name)
  (flet ((plain (code)                  ; the nearest thing on a plain keyboard, for the commonest
           (cond ((< 31 code 127) (string (code-char code)))
                 ((= code 160) " ") ((<= 8211 code 8212) "-") ((<= 8216 code 8217) "'")
                 ((<= 8220 code 8221) "\"") ((= code 8230) "...") ((= code 169) "(c)")
                 (t "?"))))
    (cond ((string= name "amp") "&") ((string= name "lt") "<") ((string= name "gt") ">")
          ((string= name "quot") "\"") ((string= name "apos") "'")
          ((string= name "nbsp") (plain 160)) ((string= name "copy") (plain 169))
          ((string= name "ndash") (plain 8211)) ((string= name "mdash") (plain 8212))
          ((string= name "lsquo") (plain 8216)) ((string= name "rsquo") (plain 8217))
          ((string= name "ldquo") (plain 8220)) ((string= name "rdquo") (plain 8221))
          ((string= name "hellip") (plain 8230))
          ((and (> (length name) 1) (char= (char name 0) #\#))
           (let ((code (if (char-equal (char name 1) #\x)
                           (parse-integer name :start 2 :radix 16 :junk-allowed t)
                           (parse-integer name :start 1 :junk-allowed t))))
             (if code (plain code) "?")))
          (t "?"))))
(defun attribute (tag name)             ; the value of NAME="..." in the text of a tag
  (let ((at (loop for from = 0 then (1+ at)     ; a whole word: not the href of data-href
                  for at = (search name tag :start2 from :test #'char-equal)
                  while at
                  when (and (plusp at) (find (char tag (1- at)) '(#\Space #\Tab #\Newline #\Return)))
                    return at))
        (space '(#\Space #\Tab #\Newline #\Return)))
    (when at
      (let* ((equals (position-if-not (lambda (c) (find c space)) tag :start (+ at (length name))))
             (start (and equals (char= (char tag equals) #\=)
                         (or (position-if-not (lambda (c) (find c space)) tag :start (1+ equals))
                             (length tag))))
             (quote (and start (< start (length tag)) (find (char tag start) "\"'"))))
        (cond ((not start) nil)
              (quote (subseq tag (1+ start) (or (position quote tag :start (1+ start)) (length tag))))
              (t (subseq tag start (or (position-if (lambda (c) (find c space)) tag :start start)
                                       (length tag)))))))))
(defun html-page (url html)
  (let ((page (make-page :url url)) (paragraphs '()) (text (make-string-output-stream))
        (i 0) (n (length html)) (in-link nil))
    (flet ((break-line () (push (get-output-stream-string text) paragraphs)))
      (loop while (< i n)
            do (let ((c (char html i)))
                 (cond ((and (char= c #\<) (eql (search "<!--" html :start2 i) i))
                        (setf i (let ((end (search "-->" html :start2 i))) (if end (+ end 2) n))))
                       ((char= c #\<)
                        (let* ((end (or (position #\> html :start i) n))
                               (tag (subseq html (1+ i) end))
                               (name (string-downcase
                                      (subseq tag 0 (position-if (lambda (ch) (find ch '(#\Space #\/ #\Tab
                                                                                         #\Newline #\Return)))
                                                                 tag :start (min 1 (length tag)))))))
                          (cond ((member name '("script" "style" "head") :test #'string=)
                                 ;; Nothing in these is for reading.  Look for the end by name: a
                                 ;; script is full of < and > that are not tags.
                                 (let ((close (search (concatenate 'string "</" name) html
                                                      :start2 end :test #'char-equal)))
                                   (cond (close (setf end (or (position #\> html :start close) n)))
                                         ((string/= name "head") (setf end n)))))
                                ((string= name "a")
                                 (let ((href (attribute tag "href")))
                                   (when href
                                     (push (join-url url href) (page-links page))
                                     (setf in-link t))))
                                ((string= name "/a")
                                 (when in-link          ; <a name=...> is a place, not a link
                                   (format text "[~D]" (length (page-links page)))
                                   (setf in-link nil)))
                                ((string= name "li") (break-line) (write-string "* " text))
                                ((member name '("td" "th") :test #'string=) (write-char #\Space text))
                                ((member (string-left-trim "/" name)
                                         '("br" "p" "div" "tr" "h1" "h2" "h3" "h4" "h5" "h6" "ul" "ol" "dl"
                                           "dt" "dd" "table" "hr" "pre" "title" "blockquote" "header"
                                           "footer" "section" "article" "nav" "form" "address" "body")
                                         :test #'string=)
                                 (break-line)))
                          (setf i end)))
                       ((char= c #\&)
                        (let ((end (position #\; html :start i :end (min n (+ i 10)))))
                          ;; a bare & in running text is common, and is just an &
                          (if (and end (> end (1+ i))
                                   (every (lambda (ch) (or (alphanumericp ch) (char= ch #\#)))
                                          (subseq html (1+ i) end)))
                              (progn (write-string (entity (subseq html (1+ i) end)) text) (setf i end))
                              (write-char c text))))
                       ((member c '(#\Newline #\Return #\Tab)) (write-char #\Space text))
                       ((< 31 (char-code c) 127) (write-char c text))
                       (t nil))        ; bytes of characters we have no way to show
                 (incf i)))
      (break-line))
    (setf (page-links page) (nreverse (page-links page)))
    (let ((item nil))                   ; was the paragraph before a list item?
      (dolist (paragraph (nreverse paragraphs))
        (let ((words (string-trim " " paragraph)))
          (unless (string= words "")
            ;; a blank line between paragraphs, but not between the items of a list
            (let ((item-now (and (> (length words) 1) (string= words "* " :end1 2))))
              (when (and (page-lines page) (not (and item item-now))) (push (cons 7 "") (page-lines page)))
              (setf item item-now))
            (dolist (line (text-wrap words 79)) (push (cons 7 line) (page-lines page)))))))
    (setf (page-lines page) (nreverse (page-lines page)))
    page))

(defun message-page (url &rest lines)
  (make-page :url url :lines (mapcar (lambda (l) (cons 12 l)) lines)))

(defun load-page (url &optional (redirects 0))
  (multiple-value-bind (scheme host port path) (split-url url)
    (handler-case
        (cond ((string= scheme "gopher")
               (let* ((type (if (> (length path) 1) (char path 1) #\1))
                      (selector (if (> (length path) 2) (subseq path 2) ""))
                      (text (fetch host port (format nil "~A~C~C" selector #\Return #\Newline))))
                 (if (find type "17") (gopher-page url text) (text-page url text))))
              ((string= scheme "http")
               (let* ((reply (fetch host port
                                    (format nil "GET ~A HTTP/1.0~C~CHost: ~A~:[:~D~;~*~]~C~C~
                                                 User-Agent: SBCL-on-DOS~C~C~C~C"
                                            (subseq path 0 (position #\# path))  ; #place is not the server's
                                            #\Return #\Newline host (= port 80) port #\Return #\Newline
                                            #\Return #\Newline #\Return #\Newline)))
                      (head-end (or (search (coerce '(#\Return #\Newline #\Return #\Newline) 'string) reply)
                                    (length reply)))
                      (head (subseq reply 0 head-end))
                      (body (subseq reply (min (length reply) (+ head-end 4))))
                      (status (or (parse-integer head :start (min (length head) 9) :junk-allowed t) 0))
                      ;; at the start of a line: Content-Location is something else
                      (location (let ((at (search (format nil "~%location:") head :test #'char-equal)))
                                  (and at (string-trim '(#\Space #\Return)
                                                       (subseq head (+ at 10)
                                                               (position #\Newline head :start (1+ at))))))))
                 (cond ((and (<= 300 status 399) location (< redirects 5))
                        (load-page (join-url url location) (1+ redirects)))
                       ((string= reply "") (message-page url (format nil "~A sent nothing back." host)))
                       (t (let ((page (if (search "text/html" head :test #'char-equal)
                                          (html-page url body)
                                          (text-page url body))))
                            (unless (<= 200 status 299)         ; "404 Not Found", above what was sent
                              (setf (page-lines page)
                                    (list* (cons 12 (format nil "The server answered: ~A"
                                                            (string-trim '(#\Return)
                                                                         (subseq head (min (length head) 9)
                                                                                 (position #\Newline head)))))
                                           (cons 7 "") (page-lines page))))
                            page)))))
              ((string= scheme "https")
               (message-page url "This address is served only over HTTPS (encrypted)."
                             "There is no TLS on this machine, so it cannot be shown."))
              (t (message-page url (format nil "I don't know how to fetch ~A addresses." scheme))))
      (error (e) (message-page url (format nil "Could not fetch ~A:" url)
                               (substitute #\Space #\Newline (princ-to-string e)))))))

;;; ---- the screen
(defun show (page top typed)
  (loop for row below 24
        for line = (nth (+ top row) (page-lines page))
        do (text-put 0 row (format nil "~80A" (if line (cdr line) "")) (if line (car line) 7)))
  ;; The bottom line is exactly 80 wide: the address is cut to fit, and a number
  ;; being typed takes the place of the help.
  (text-put 0 24 (format nil "~80A"
                         (format nil " ~A~38T~:[~D link~:P  # Enter  B back  G go  Q quit~;~
                                      ~*Follow link: ~A~]"
                                 (let ((url (substitute #\Space #\Tab (page-url page))))
                                   (subseq url 0 (min 36 (length url))))
                                 (string/= typed "") (length (page-links page)) typed))
            #x70))

(defun ask (prompt)
  "Ask for a line of text on the bottom line; NIL if nothing was typed."
  (text-put 0 24 (format nil "~80A" prompt) #x70)
  (show-text-cursor)
  (let ((answer (text-input (1+ (length prompt)) 24 (- 78 (length prompt)) :attribute #x70)))
    (hide-text-cursor)
    (and answer (string/= answer "") answer)))

(defun main (&optional (url "gopher://gopher.floodgap.com/1/"))
  "A text browser for Gopher and plain HTTP, with numbered links."
  (hide-text-cursor)
  (text-fill 0 0 80 25 7)
  (text-put 0 0 (format nil "Fetching ~A ..." url) 11)
  (let ((page (load-page url)) (history '()) (top 0) (typed ""))
    (flet ((go-to (address)
             ;; a Gopher search (type 7) wants its words sent after the selector and a tab
             (when (and (search "gopher://" address :test #'char-equal)
                        (let ((path (nth-value 3 (split-url address))))
                          (and (> (length path) 1) (char= (char path 1) #\7)))
                        (not (find #\Tab address)))
               (let ((words (ask " Search for:")))
                 (unless words (return-from go-to))
                 (setf address (format nil "~A~C~A" address #\Tab words))))
             (text-put 0 24 (format nil "~80A" (format nil " Fetching ~A ..." address)) #x70)
             (push (cons page top) history)
             (setf page (load-page address) top 0 typed "")))
      (loop
        (show page top typed)
        (let ((key (get-key)) (last (max 0 (- (length (page-lines page)) 24))))
          (cond ((eq key :down) (setf top (min last (1+ top))))
                ((eq key :up) (setf top (max 0 (1- top))))
                ((or (eq key :page-down) (eql key #\Space)) (setf top (min last (+ top 23))))
                ((eq key :page-up) (setf top (max 0 (- top 23))))
                ((eq key :home) (setf top 0))
                ((eq key :end) (setf top last))
                ((and (characterp key) (digit-char-p key))
                 (when (< (length typed) 4) (setf typed (concatenate 'string typed (string key)))))
                ((eql key #\Backspace) (setf typed (subseq typed 0 (max 0 (1- (length typed))))))
                ((eql key #\Return)
                 (let ((link (and (string/= typed "") (plusp (parse-integer typed))
                                  (nth (1- (parse-integer typed)) (page-links page)))))
                   (setf typed "")
                   (when link (go-to link))))
                ((member key '(#\b #\B))
                 (when history
                   (destructuring-bind (was . at) (pop history) (setf page was top at typed ""))))
                ((member key '(#\g #\G))
                 (let ((address (ask " Address:")))
                   (when address
                     (go-to (if (search "://" address) address (concatenate 'string "http://" address))))))
                ((or (member key '(#\q #\Q)) (eql key (code-char 27))) (return)))))))
  (text-fill 0 0 80 25 7)
  (text-cursor 0 0)
  (show-text-cursor)
  (values))                             ; not the registers of the last BIOS call

(doslib-examples:define-example "browse" 'main
  :category :network
  :description "A text browser for Gopher and plain HTTP, with numbered links.")
