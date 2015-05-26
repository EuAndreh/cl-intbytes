(defpackage cl-intbytes-test
  (:use cl prove cl-intbytes))
(in-package cl-intbytes-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-intbytes)' in your Lisp.

(plan 2)

(deftest bundled-functions-test
  (is (int32->octets 84215045)
      #(5 5 5 5)
      :test #'equalp)
  (is (octets->int32 #(5 5 5 5))
      84215045)
  (is (octets->int32 #(0 5 5 5 5) 1)
      84215045)
  (is (int64->octets 578437695752307201)
      #(1 2 3 4 5 6 7 8)
      :test #'equalp)
  (is (octets->int64 #(1 2 3 4 5 6 7 8))
      578437695752307201)
  (is (int32->octets -123)
      #(133 255 255 255)
      :test #'equalp)
  (is (octets->uint32 #(133 255 255 255))
      4294967173)
  (is (int64->octets -1)
      #(255 255 255 255 255 255 255 255)
      :test #'equalp)
  (is (octets->uint64 #(255 255 255 255 255 255 255 255))
      18446744073709551615)
  (is (octets->int64 #(255 255 255 255 255 255 255 255))
      -1))

(deftest define-and-use-functions-test
  (labels ((int16->octets (int16)
             (int->octets int16 2))
           (octets->int16 (array &optional (start 0))
             (octets->int array 2 start))
           (octets->uint16 (array &optional (start 0))
             (octets->uint array 2 start)))
    (is (int16->octets -3)
        #(253 255)
        :test #'equalp)
    (is (octets->uint16 #(253 255))
        65533)
    (is (octets->int16 #(253 255))
        -3
        :test #'equalp)
    (is (octets->int16 #(0 253 255) 1)
        -3
        :test #'equalp)))

(run-test-all)