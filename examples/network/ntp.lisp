;;;; Set the DOS clock from the Internet, then show it.
;;;;
;;;; Asks an NTP server for the time with one UDP packet (SB-BSD-SOCKETS on
;;;; Watt-32), works out how wrong the DOS clock is, sets it through
;;;; INT 21h, and draws a big clock straight into text-mode video memory.
;;;;
;;;; A pleasant coincidence: NTP counts seconds from 1 January 1900, and so
;;;; does Common Lisp's universal time.  No epoch conversion is needed.
;;;; (DOS keeps no time zone; this demo treats the clock as UTC.)
;;;; Needs a packet driver loaded and WATTCP.CFG in the current directory.
;;;; Start with (ntp-clock) or (ntp-clock "time.cloudflare.com").
;;;;
;;;; Part of DOSLIB: Common Lisp on DOS with SBCL.

(defpackage #:doslib-examples.ntp
  (:use #:cl #+sbcl #:sb-alien #+sbcl #:sb-ext #+sbcl #:sb-sys #:doslib-compat #:doslib
        #:doslib-sockets)
  (:export #:main))
(in-package #:doslib-examples.ntp)

(defun now ()                           ; universal time, with a fraction
  (multiple-value-bind (seconds microseconds) (time-of-day)
    (+ seconds 2208988800 (/ microseconds 1000000))))
(defun timestamp (packet offset)        ; 32 bits of seconds, 32 bits of fraction
  (flet ((word (i) (logior (ash (aref packet i) 24) (ash (aref packet (+ i 1)) 16)
                           (ash (aref packet (+ i 2)) 8) (aref packet (+ i 3)))))
    (+ (word offset) (/ (word (+ offset 4)) (expt 2 32)))))
(defun ntp-query (host)
  "Ask HOST the time.  Returns the offset of our clock in seconds (add it to
get the right time), the round-trip delay, the server's stratum and address."
  (let ((address (host-ent-address (get-host-by-name host)))
        (socket (make-instance 'inet-socket :type :datagram :protocol :udp))
        (packet (make-array 48 :element-type '(unsigned-byte 8) :initial-element 0)))
    (unwind-protect
         (progn
           (setf (aref packet 0) #x23)  ; no leap warning, version 4, mode 3 (client)
           (let ((sent (now)))
             (socket-send socket packet 48 :address (list address 123))
             (unless (socket-ready-p socket 5000)
               (error "No answer from ~A" host))
             (socket-receive socket packet 48)
             (let ((received (now))
                   (server-received (timestamp packet 32))
                   (server-sent (timestamp packet 40)))
               (values (/ (+ (- server-received sent) (- server-sent received)) 2)
                       (- (- received sent) (- server-sent server-received))
                       (aref packet 1)
                       address))))
      (socket-close socket))))
(defun set-dos-clock (universal-time)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (floor universal-time) 0)
    (int86 #x21 :ax #x2B00 :cx year :dx (logior (ash month 8) day))
    (int86 #x21 :ax #x2D00 :cx (logior (ash hour 8) minute)
                :dx (logior (ash second 8) (floor (* 100 (mod universal-time 1)))))))
;;; The display: text mode, written directly into video memory at B800:0
(defvar *text* (int-sap #xB8000))
(defun put (column row string attribute)
  (loop for c across string for x from column
        do (setf (sap-ref-16 *text* (* 2 (+ x (* 80 row))))
                 (logior (char-code c) (ash attribute 8)))))
(defvar *digits*                        ; 5 rows of 5 for 0-9, then the colon
  #("#####.#...#.#...#.#...#.#####" "....#.....#.....#.....#.....#" "#####.....#.#####.#.....#####"
    "#####.....#.#####.....#.#####" "#...#.#...#.#####.....#.....#" "#####.#.....#####.....#.#####"
    "#####.#.....#####.#...#.#####" "#####.....#.....#.....#.....#" "#####.#...#.#####.#...#.#####"
    "#####.#...#.#####.....#.#####" "........#.........#.........."))
(defun big-time (hour minute second row attribute)
  (loop for c across (format nil "~2,'0D:~2,'0D:~2,'0D" hour minute second)
        for column from 14 by 7
        do (let ((shape (aref *digits* (or (digit-char-p c) 10))))
             (dotimes (y 5)
               (dotimes (x 5)
                 (put (+ column x) (+ row y)
                      (string (if (char= (char shape (+ x (* 6 y))) #\#) (code-char 219) #\Space))
                      attribute))))))
(defun date-string (universal-time)
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time universal-time 0)
    (format nil "~A ~D ~A ~D  ~2,'0D:~2,'0D:~2,'0D UTC"
            (nth weekday '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
            day (nth (1- month) '("January" "February" "March" "April" "May" "June" "July"
                                  "August" "September" "October" "November" "December"))
            year hour minute second)))
(defun ntp-clock (&optional (host "pool.ntp.org"))
  (let ((before (get-universal-time)))
    (ntp-query host)                    ; once to get acquainted (name and ARP lookups)
    (multiple-value-bind (offset delay stratum address) (ntp-query host)
      (set-dos-clock (+ (now) offset))
      (video-mode 3)
      (put 2 1 (format nil "NTP server   ~A (~{~D~^.~}), stratum ~D" host (coerce address 'list) stratum) 7)
      (put 2 2 (format nil "Round trip   ~D ms" (round (* 1000 delay))) 7)
      (put 2 3 (format nil "DOS clock    ~A" (date-string before)) 12)
      (put 2 4 (format nil "was wrong by ~:D seconds~:[~; (~,1F years)~]"
                       (round offset) (> (abs offset) 86400) (/ offset 31557600.0)) 12)
      (put 2 5 (format nil "DOS clock    ~A   (set through INT 21h)"
                       (date-string (get-universal-time))) 10)
      (put 27 21 "press a key to stop" 8)
      (loop until (key-pressed-p)
            do (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time) 0)
                 (big-time hour minute second 10 11)
                 (put 20 17 (format nil "~40A" (date-string (get-universal-time))) 15))
               (wait-tick 2))
      (wait-key)
      (video-mode 3)
      (values))))

(defun main (&optional (host "pool.ntp.org"))
  "Set the DOS clock from the Internet, then show it."
  (ntp-clock host))

(doslib-examples:define-example "ntp" 'main
  :category :network
  :description "Set the DOS clock from the Internet, then show it.")
