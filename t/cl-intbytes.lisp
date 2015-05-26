(defpackage cl-intbytes-test
  (:use cl prove cl-intbytes))
(in-package cl-intbytes-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-intbytes)' in your Lisp.

(plan 2)

(deftest bundled-functions-test
  (is (int32->octets 84215045)
      #(5 5 5 5)
      :test #'equalp
      "INT32->OCTETS")
  (is (octets->int32 #(5 5 5 5))
      84215045
      "OCTETS->INT32")
  (is (octets->int32 #(0 5 5 5 5) 1)
      84215045
      "OCTETS->INT32 :start 1")
  (is (int64->octets 578437695752307201)
      #(1 2 3 4 5 6 7 8)
      :test #'equalp
      "INT64->OCTETS")
  (is (octets->int64 #(1 2 3 4 5 6 7 8))
      578437695752307201
      "OCTETS->INT64")
  (is (int32->octets -123)
      #(133 255 255 255)
      :test #'equalp
      "INT32->OCTETS")
  (is (octets->uint32 #(133 255 255 255))
      4294967173
      "INT32->OCTETS")
  (is (int64->octets -1)
      #(255 255 255 255 255 255 255 255)
      :test #'equalp
      "INT64->OCTETS")
  (is (octets->uint64 #(255 255 255 255 255 255 255 255))
      18446744073709551615
      "OCTETS->UINT64")
  (is (octets->int64 #(255 255 255 255 255 255 255 255))
      -1
      "OCTETS->INT64"))

(deftest define-and-use-functions-test
  (labels ((int16->octets (int16)
             (int->octets int16 2))
           (octets->int16 (array &optional (start 0))
             (octets->int array 2 start))
           (octets->uint16 (array &optional (start 0))
             (octets->uint array 2 start)))
    (is (int16->octets -3)
        #(253 255)
        :test #'equalp
        "\"Custom\" INT16->OCTETS")
    (is (octets->uint16 #(253 255))
        65533
        "\"Custom\" OCTETS->UINT16")
    (is (octets->int16 #(253 255))
        -3
        "\"Custom\" OCTETS->UINT16")
    (is (octets->int16 #(0 253 255) 1)
        -3
        "\"Custom\" OCTETS->UINT :start 1")))

(run-test-all)
