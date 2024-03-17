(defpackage xmpp-repl
  (:use :cl :metabang-bind))
(in-package :xmpp-repl)


;; (defun write-to-xmpp (object)
;;   (let ((process (uiop:launch-program '("/usr/bin/xmpp-bridge")
;;                                       :input :stream
;;                                       :output :stream
;;                                       :error-output :stream
;;                                       :wait nil
;;                                       :search t)))
;;     (print object (uiop:process-info-input process))
;;     (uiop:terminate-process process)))

(defun get-this-pid ()
  ;; https://stackoverflow.com/a/52790432
  "Return PID of this current lisp process."
  (parse-integer (with-open-file (in #P"/proc/self/status")
                   (loop for line = (read-line in nil)
                         while line
                         when (ppcre:scan "^Pid" line)
                           do (return (car
                                       (ppcre:all-matches-as-strings "\\d+" line)))))))
(defvar *connection* nil)

(defun connect-and-auth (host username password)
  (setf *connection* (xmpp:connect-tls :hostname host))
  (xmpp:auth *connection* username password "resource" :mechanism :sasl-plain)
  (bt:make-thread (lambda () (xmpp:receive-stanza-loop *connection*)) :name "xmpp-repl-stanza-loop"))


(defun rep (input output)
                                        ; from jackdaniel
  (format output "~&~a>~%" (package-name *package*))
  (shiftf +++ ++ + - (read input nil '%quit))
  (when (eq - '%quit)
    (throw :exit "bye!"))
  (shiftf /// // / (multiple-value-list (eval -)))
  (shiftf *** ** * (first /))
  (format output "~&~{~s ~^~%~}~%" /))

(defun repl (input output)
  (with-simple-restart (exit-xmpp-repl "Exit XMPP repl for PID ~a." (get-this-pid))
    (catch :exit
      (unwind-protect
           (loop do
             (with-simple-restart (abort "Abort current XMPP repl request.")
               (trivial-custom-debugger:with-debugger
                   ((lambda (condition hook)
                      (declare (ignore hook))
                      (format output "Condition ~a was signalled.~%" condition)
                      (format output "Choose a restart: ~%")
                      (let ((restarts (compute-restarts condition)))
                        (loop for i from 0
                              for restart in restarts
                              do (format output "~a: [~a] ~a~%" i (string-upcase (restart-name restart)) restart))
                        (loop do
                          (let ((index (parse-integer (read-line input))))
                            (if (and index (<= 0 index) (< index (length restarts)))
                                (invoke-restart (nth index restarts))
                                (format output "Choose a valid restart.")))))))
                 (rep input output))))
        (format output "; Closing XMPP repl~%") ))))

(defun xmpp-repl (recipient)
  (repl (make-instance 'xmpp-input-stream :recipient recipient :connection *connection*)
        (make-instance 'xmpp-output-stream :recipient recipient :connection *connection*)))

(defclass xmpp-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((connection :initarg :connection :accessor connection)
   (recipient :initarg :recipient :accessor recipient)
   (buffer :initarg :buffer :accessor buffer :type string :initform (make-array (list 0)
                                                                                :element-type 'character
                                                                                :adjustable t
                                                                                :fill-pointer 0))))

(defclass xmpp-input-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((connection :initarg :connection :accessor connection)
   (recipient :initarg :recipient :accessor recipient)
   (lock :initarg :lock :accessor lock :initform (bt:make-lock "xmpp-input-stream"))
   (buffer :initarg :buffer :accessor buffer :type string :initform "")))

(defvar *input-streams* nil)

(defmethod initialize-instance :after ((input-stream xmpp-input-stream) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push input-stream *input-streams*))

(defmethod trivial-gray-streams:stream-read-char ((stream xmpp-input-stream))
  (bind (((:accessors connection recipient buffer lock) stream))
    (loop named inner do
      (bt:with-lock-held (lock)
        (if (zerop (length buffer))
            ;; Buffer is empty; check if we are connected. If so, continue to next iteration. If not, return eof
            (if (xmpp:connectedp connection)
                (sleep 0.001)
                (return-from inner :eof))

            ;; Buffer has a character; return it and remove the character from the buffer
            (let ((return-char (aref buffer 0)))
              (setf buffer (subseq buffer 1))
              (return-from inner return-char)))))))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream xmpp-input-stream))
  (bind (((:accessors connection buffer lock) stream))
    (bt:with-lock-held (lock)
      (if (zerop (length buffer))
          (if (xmpp:connectedp connection) nil :eof)
          (let ((return-char (aref buffer 0)))
            (setf buffer (subseq buffer 1))
            return-char))))

  ;; (defmethod trivial-gray-streams:stream-read-line ((stream xmpp-input-stream))
  ;;   (error "not implemented yet"))

  (defmethod trivial-gray-streams:stream-read-sequence ((stream xmpp-input-stream)
                                                        sequence start end &key &allow-other-keys)
    (error "not implemented yet")))


(defmethod trivial-gray-streams:stream-unread-char ((stream xmpp-input-stream) character)
  (bind (((:accessors connection buffer lock) stream))
    (bt:with-lock-held (lock)
      (setf buffer (concatenate 'string (make-string 1 :initial-element character) buffer)))))

(defmethod trivial-gray-streams:stream-listen ((stream xmpp-input-stream))
  (not (zerop (length (buffer stream)))))


(defmethod trivial-gray-streams:stream-write-char ((stream xmpp-output-stream) character)
  (bind (((:accessors connection recipient buffer) stream))
    (setf buffer (concatenate 'string buffer (make-string 1 :initial-element character)))
    (when (char= #\Newline character)
      (maybe-send-message stream))))

(defmethod trivial-gray-streams:stream-fresh-line ((stream xmpp-output-stream))
  (trivial-gray-streams:stream-write-char stream #\Newline))

(defmethod trivial-gray-streams:stream-write-string ((stream xmpp-output-stream) string &optional (start 0) end)
  (bind (((:accessors connection recipient) stream))
    (add-to-buffer stream (subseq string start end))
    (maybe-send-message stream)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream xmpp-output-stream) string start end &key &allow-other-keys)
  (bind (((:accessors connection recipient) stream))
    (add-to-buffer stream (coerce (subseq string start end) 'string))
    (maybe-send-message stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream xmpp-output-stream)) nil)
(defmethod trivial-gray-streams:stream-start-line-p ((stream xmpp-output-stream)) nil)
(defmethod trivial-gray-streams:stream-read-byte ((stream xmpp-output-stream))
  (error "not implemented yet"))
(defmethod trivial-gray-streams:stream-write-byte ((stream xmpp-output-stream) integer)
  (error "not implemented yet"))

(defmethod trivial-gray-streams:stream-finish-output ((stream xmpp-output-stream))
  (bind (((:accessors connection recipient buffer) stream))
    (xmpp:message connection recipient buffer)
    (setf buffer (make-array (list 0) :element-type 'character :adjustable t :fill-pointer 0))))

(defmethod add-to-buffer ((stream xmpp-output-stream) message)
  (bind (((:accessors connection recipient buffer) stream))
    (setf buffer (concatenate 'string buffer message))))

(defmethod maybe-send-message ((stream xmpp-output-stream))
  (bind (((:accessors connection recipient buffer) stream)
         (position (position #\Newline buffer)))
    (when position
      (xmpp:message connection recipient (subseq buffer 0 position))
      (setf buffer (subseq buffer (1+ position))))))

(defmethod xmpp:handle ((connection xmpp:connection) (message xmpp:message))
  (bind (((:accessors xmpp:from xmpp:id xmpp:body) message))
    (when xmpp:id
      (loop for input-stream in *input-streams*
            do (bind (((:accessors recipient buffer lock) input-stream))
                 (when (str:starts-with-p recipient xmpp:from)
                   (bt:with-lock-held (lock)
                     (setf buffer (concatenate 'string buffer xmpp:body  (make-string 1 :initial-element #\Newline)))
                     (format t "new buffer: ~a~%" buffer))))))))

#|
'(#:stream-read-char
#:stream-unread-char #:stream-read-char-no-hang
#:stream-peek-char #:stream-listen #:stream-read-line
#:stream-clear-input #:stream-write-char #:stream-line-column
#:stream-start-line-p #:stream-write-string #:stream-terpri
#:stream-fresh-line #:stream-finish-output #:stream-force-output
#:stream-clear-output #:stream-advance-to-column
#:stream-read-byte #:stream-write-byte)
|#
