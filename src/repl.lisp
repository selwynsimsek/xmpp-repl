(in-package #:xmpp-repl)

#|Copyright 2022 Daniel 'jackdaniel' Kochmański
                                        ;
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: ;
                                        ;
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
                                        ;
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.|#

(defun rep (input output)
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
