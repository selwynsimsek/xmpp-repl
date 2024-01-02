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
  (let ((form-name (gensym)))
    `(let ((,form-name (multiple-value-list (progn ,@body))))
       (write-to-xmpp ,form-name))))

(export 'echo)
;; blah blah blah.
