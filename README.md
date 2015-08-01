# cl-intbytes - Convert between any-base integers and byte arrays interchangeably
[![Quicklisp](http://quickdocs.org/badge/cl-intbytes.svg)](http://quickdocs.org/cl-intbytes/)
[![Build Status](https://travis-ci.org/EuAndreh/cl-intbytes.svg?branch=master)](https://travis-ci.org/EuAndreh/cl-intbytes)
[![Circle CI](https://circleci.com/gh/EuAndreh/cl-intbytes.svg?style=svg)](https://circleci.com/gh/EuAndreh/cl-intbytes)
[![Coverage Status](https://coveralls.io/repos/EuAndreh/cl-intbytes/badge.svg?branch=master)](https://coveralls.io/r/EuAndreh/cl-intbytes?branch=master)

Inspired by [cl-mongo](https://github.com/fons/cl-mongo/blob/bb1f807a17c960dc81bebd5e8a2df5d0886d422a/src/octets.lisp).

## Usage
```lisp
* (ql:quickload :cl-intbytes)
; => (:CL-INTBYTES)
* (use-package :intbytes)
; => T
```

For easy/ready encoding/decoding, use `int32->octets`/`octets->int32` and `int64->octets`/`octets->int64`. All accept an `&optional (start 0)` value:
```lisp
* (int32->octets 84215045)
; => #(5 5 5 5)
* (octets->int32 *)
; => 84215045
* (octets->int32 #(0 5 5 5 5) 1)
; => 84215045
* (int64->octets 578437695752307201)
; => #(1 2 3 4 5 6 7 8)
* (octets->int64 *)
; => 578437695752307201
```

For unsigned values, there are equivalent unsigned functions `octets->uint32` and `octets->uint64`:
```lisp
* (int32->octets -123)
; => #(133 255 255 255)
* (octets->uint32 *)
; => 4294967173
* (int64->octets -1)
; => #(255 255 255 255 255 255 255 255)
* (octets->uint64 *)
; => 18446744073709551615
* (octets->int64 **)
; => -1
```

You can create your own functions with `int->octets` and `octets->int`:
```lisp
* (defun int16->octets (int16)
    (int->octets int16 2))
; => INT16->OCTETS
* (defun octets->int16 (array &optional (start 0))
    (octets->int array 2 start))
; => OCTETS->INT16
* (defun octets->uint16 (array &optional (start 0))
    (octets->uint array 2 start))
; => OCTETS->UINT16
* (int16->octets -3)
; => #(253 255)
* (octets->uint16 *)
; => 65533
* (octets->int16 **)
; => -3
* (octets->int16 #(0 253 255) 1)
; => -3
```

If you want to encode floats to bytes, use [`ieee-floats`](https://github.com/marijnh/ieee-floats):
```lisp
* (int64->octets (ieee-floats:encode-float64 1.5d0))
; => #(0 0 0 0 0 0 248 63)
* (ieee-floats:decode-float64 (octets->int64 *))
; => 1.5d0
```
## Dependencies
This library depends on [fast-io](https://github.com/rpav/fast-io).

The test package uses the [prove](https://github.com/fukamachi/prove) test library.

## Installation
```lisp
(ql:quickload :cl-intbytes)
```

## Bugs
If you find any bug or inconsistency in the code, or if you find it too hard to use, please, feel free to open an issue.

## Tests
This library is tested under [SBCL](http://www.sbcl.org/) and [CCL](http://ccl.clozure.com/) Common Lisp implementations.

To run all the defined tests, use:
```lisp
* (asdf:test-system :cl-intbytes)
; prints lots of (colorful) stuff...
; => T
```

Tests are ran with [Travis CI](https://travis-ci.org/EuAndreh/cl-intbytes) and [Circle CI](https://circleci.com/gh/EuAndreh/cl-intbytes) using [cl-travis](https://github.com/luismbo/cl-travis), [CIM](https://github.com/KeenS/CIM), [cl-coveralls](https://github.com/fukamachi/cl-coveralls) and [Roswell](https://github.com/snmsts/roswell). Check it out!
## Author
+ [André Miranda](https://github.com/EuAndreh)

## License
[LLGPL](https://tldrlegal.com/license/lisp-lesser-general-public-license#fulltext).
