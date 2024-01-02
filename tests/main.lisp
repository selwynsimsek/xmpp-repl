(defpackage xmpp-repl/tests/main
  (:use :cl
        :xmpp-repl
        :rove))
(in-package :xmpp-repl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :xmpp-repl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
