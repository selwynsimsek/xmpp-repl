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

(defun write-to-xmpp (object)
  (with-input-from-string (input "")
    (with-output-to-string (output)
      (uiop:run-program `("bash" "-c" ,(format nil "echo '~a' | xmpp-bridge" (princ-to-string object)))
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
       (loop for ,body-name in ',body do (write-to-xmpp ,body-name))
       (write-to-xmpp "=>")
       (let ((,form-name (multiple-value-list (progn ,@body))))
         (loop for ,form-value in ,form-name do (write-to-xmpp ,form-value))))))

(export 'echo)
;; blah blah blah.
