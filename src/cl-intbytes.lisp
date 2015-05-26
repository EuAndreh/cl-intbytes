(defpackage cl-intbytes
  (:use cl)
  (:nicknames intbytes)
  (:import-from fast-io
                fast-write-byte
                with-fast-output)
  (:documentation "This package defines general-purpose functions to encode integers into octets-vectors of given sizes and functions decode them back. Also, it defines functions for common encoding formats (`INT32->OCTETS`, `INT64->OCTETS`) and functions for common decoding formats (`OCTETS->INT32`, `OCTETS->INT64`, `OCTETS->UINT32`, `OCTETS->UINT64`)."))
(in-package cl-intbytes)
(annot:enable-annot-syntax)

(defun uint->int (uint int-size)
  "Converts a given `UINT` into a signed int based on the given `INT-SIZE`."
  (let ((sign-bit-position (1- int-size))
        (max-uint (expt 2 int-size)))
    (cond ((and (< 0 uint)
                 (logbitp (1- sign-bit-position) uint)
                 (- uint max-uint)))
         ((<= max-uint uint) 0)
         (t uint))))

@export
(defun octets->uint (array n-bytes &optional (start 0))
  "Interprets `N-BYTES` of a given `ARRAY` as an unsigned integer."
  (let ((int 0))
    (loop
       for byte-position from 0 below (* 8 n-bytes) by 8
       for array-position from 0 below n-bytes
       do (setf (ldb (byte 8 byte-position) int)
                (aref array (+ start array-position))))
    int))

@export
(defun octets->int (array n-bytes &optional (start 0))
  "Interprets `N-BYTES` of a given `ARRAY` as an unsigned integer using `OCTETS->UINT`, then converts to a signed value using `UINT->INT`."
  (uint->int (octets->uint array n-bytes start)
             (* 8 n-bytes)))

@export
(defun int->octets (int n-bytes)
  "Writes all the bytes of a given `INT` into an array of type '(UNSIGNED-BYTE 8) with length of `N-BYTES`."
  (with-fast-output (buffer)
    (loop for byte-position from 0 below (* n-bytes 8) by 8 do
         (fast-write-byte (ldb (byte 8 byte-position)
                               int)
                          buffer))))

@export
(defun octets->int32 (array &optional (start 0))
  "Interprets 4 bytes of the given `ARRAY` as a signed integer, starting from `(START 0)`."
  (octets->int array 4 start))

@export
(defun octets->int64 (array &optional (start 0))
  "Inteprets 8 bytes of the given `ARRAY` as a signed integer, starting from `(START 0)`."
  (octets->int array 8 start))

@export
(defun octets->uint32 (array &optional (start 0))
  "Inteprets 4 bytes of the given `ARRAY` as a unsigned integer, starting from `(START 0)`."
  (octets->uint array 4 start))

@export
(defun octets->uint64 (array &optional (start 0))
  "Interprets 8 bytes of the given `ARRAY` as a unsigned integer, starting from `(START 0)."
  (octets->uint array 8 start))

@export
(defun int32->octets (int32)
  "Writes `INT32` into an array of type '(UNSIGNED-BYTE 8) with 4 elements."
  (int->octets int32 4))

@export
(defun int64->octets (int64)
  "Writes `INT64` into an array of type '(UNSIGNED-BYTE 8) with 8 elements."
  (int->octets int64 8))
