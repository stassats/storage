;;; -*- Mode: Lisp -*-

(asdf:defsystem #:storage
  :name "storage"
  :serial t
  :depends-on (closer-mop ieee-floats)
  :components ((:file "packages")
               #+(and sbcl (or x86 x86-64))
               (:file "io-sbcl")
               #-(and sbcl (or x86 x86-64))
               (:file "io-generic")
               (:file "mop")
               (:file "kmp")
               (:file "storage")
               (:file "disk")))
