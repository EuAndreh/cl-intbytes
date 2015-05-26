(defsystem cl-intbytes-test
  :depends-on (cl-intbytes
               prove)
  :components ((:module "t"
                        :components
                        ((:test-file "cl-intbytes"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern "RUN-TEST-SYSTEM" :prove-asdf) c)
                    (asdf:clear-system c)))
