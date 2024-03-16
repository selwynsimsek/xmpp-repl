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
                                       (ppcre:all-matches-as-strings "\\d+" 
                                                                     line)))))))
(defvar *connection* nil)

;(xmpp:auth connection "password" "resource" :mechanism :sasl-plain)

(defun connect-and-auth (host username password)
  (setf *connection* (xmpp:connect-tls :hostname host))
  (xmpp:auth *connection* username password "resource" :mechanism :sasl-plain))


(defun rep (input output)
                                        ; from jackdaniel
  (format t "~&~a> " (package-name *package*))
  (shiftf +++ ++ + - (read input nil '%quit))
  (when (eq - '%quit)
    (throw :exit "bye!"))
  (shiftf /// // / (multiple-value-list (eval -)))
  (shiftf *** ** * (first /))
  (format output "~&~{ ~s~^~%~}~%" /))

(defun repl (input output)
  (catch :exit
    (loop (handler-case (rep input output)
            (condition (c)
              (format *error-output* "~&~a~%~a~%" (class-name (class-of c)) c))))))

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
   (buffer :initarg :buffer :accessor buffer :type string :initform "")))

(defvar *input-streams* nil)

(defmethod initialize-instance :after ((input-stream xmpp-input-stream) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (push input-stream *input-streams*))

(defmethod trivial-gray-streams:stream-read-char ((stream xmpp-input-stream))
  )

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream xmpp-input-stream))
  (error "not implemented yet"))

;; (defmethod trivial-gray-streams:stream-read-line ((stream xmpp-input-stream))
;;   (error "not implemented yet"))

(defmethod trivial-gray-streams:stream-read-sequence ((stream xmpp-input-stream)
                                 sequence start end &key &allow-other-keys)
  (error "not implemented yet"))


(defmethod trivial-gray-streams:stream-unread-char ((stream xmpp-input-stream) character)
  (error "not implemented yet"))

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
            do (bind (((:accessors recipient buffer) input-stream))
                 (when (string= xmpp:from recipient)
                   (setf buffer (concatenate 'string buffer "#\Newline" message))))))))

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
