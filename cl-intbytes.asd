(defsystem cl-intbytes
  :name "cl-intbytes"
  :version "0.1.0"
  :author "André Miranda"
  :maintainer "André Miranda"
  :mailto "andremiramor@gmail.com"
  :homepage "https://github.com/EuAndreh/cl-intbytes"
  :bug-tracker "https://github.com/EuAndreh/cl-intbytes/issues"
  :source-control (:git "git@github.com:EuAndreh/cl-intbytes.git")
  :license "LLGPL"
  :description "Encode/decode any-base integers and byte arrays interchangeably."
  :depends-on (fast-io)
  :components ((:module "src"
                        :components ((:file "cl-intbytes")))
               (:static-file "README.md"))
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-truename* "README.md"))
  :in-order-to ((test-op (test-op cl-intbytes-test))))
