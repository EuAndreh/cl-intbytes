(defsystem cl-intbytes-test
  :name "cl-intbytes-test"
  :version "0.1.0"
  :author "André Miranda"
  :maintainer "André Miranda"
  :mailto "andremiramor@gmail.com"
  :homepage "https://github.com/EuAndreh/cl-intbytes"
  :bug-tracker "https://github.com/EuAndreh/cl-intbytes/issues"
  :source-control (:git "git@github.com:EuAndreh/cl-intbytes.git")
  :license "LLGPL"
  :description "Test system for cl-intbytes."
  :depends-on (cl-intbytes prove)
  :components ((:test-file "t/cl-intbytes"))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op (op c)
                    (symbol-call :prove-asdf :run-test-system c)))
