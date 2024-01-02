(defpackage xmpp-repl
  (:use :cl))
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

(defun write-to-xmpp (object)
  (with-input-from-string (input "")
    (with-output-to-string (output)
      (uiop:run-program `("bash" "-c" ,(format nil "echo '~a: ~a' | xmpp-bridge"
                                               (get-this-pid)
                                               (princ-to-string object)))
                        :output output
                        :input input
                        :error-output *error-output*)))
  t)



(defmacro echo (&rest body)
  "Evaluates the forms in body and prints the results to XMPP."
  (let ((form-name (gensym))
        (form-value (gensym))
        (body-name (gensym)))
    `(progn
       (let ((*print-case* :downcase)) (loop for ,body-name in ',body do (write-to-xmpp ,body-name)))
       (write-to-xmpp "=>")
       (let ((,form-name (multiple-value-list (progn ,@body))))
         (loop for ,form-value in ,form-name do (write-to-xmpp ,form-value))))))

(export 'echo)
