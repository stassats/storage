(in-package #:storage)

(defparameter *codes*
  #(ascii-string
    identifiable
    cons
    string
    null
    t
    fixnum
    bignum
    fixnum-ratio
    ratio
    double-float
    single-float
    complex
    list-of-objects
    symbol
    intern-package-and-symbol
    intern-symbol
    character
    simple-vector
    vector
    array
    hash-table
    pathname
    fixnum-1
    fixnum-2
    fixnum-3))

(defun type-code (type)
  (position type *codes*))

(defconstant +sequence-length+ 2)
(defconstant +fixnum-length+ 4)
(defconstant +char-length+ 3)
(defconstant +id-length+ 3)
(defconstant +hash-table-length+ 3)
(defconstant +vector-length+ 4)
(defconstant +slots-length+ 1)

(defconstant +end+ 255)
(defconstant +improper-list-end+ 254)

(defconstant +ascii-char-limit+ (code-char 128))

(deftype ascii-string ()
  '(or
    #+sb-unicode simple-base-string ; on #-sb-unicode the limit is 255
    (satisfies ascii-string-p)))

(deftype storage-fixnum ()
  `(signed-byte ,(* +fixnum-length+ 8)))
