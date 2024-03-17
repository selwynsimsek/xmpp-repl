(defsystem "xmpp-repl"
  :version "0.1.0"
  :author "Selwyn Simsek"
  :license "BSD-2"
  :depends-on ("cl-ppcre"
               "cl-xmpp-tls"
               "bordeaux-threads"
               "str"
               "trivial-garbage"
               "metabang-bind"
               "trivial-custom-debugger"
               "trivial-gray-streams"
               "cl-xmpp-sasl")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "repl"))))
  :description "Communicate with a Lisp process over XMPP"
  :in-order-to ((test-op (test-op "xmpp-repl/tests"))))

(defsystem "xmpp-repl/tests"
  :author "Selwyn Simsek"
  :license "BSD-2"
  :depends-on ("xmpp-repl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for xmpp-repl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
