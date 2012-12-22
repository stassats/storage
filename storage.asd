;;; -*- Mode: Lisp -*-

(asdf:defsystem #:storage
  :name "storage"
  :serial t
  :depends-on (alexandria
               #-abcl closer-mop
               #-sbcl
               ieee-floats)
  :components ((:file "packages")
               (:file "parameters")
               #+(and sbcl (or x86 x86-64))
               (:file "io-sbcl")
               #+(and sb-unicode (or x86 x86-64))
               (:file "io-sbcl-strings")
               #-(and sbcl (or x86 x86-64))
               (:file "io-generic")
               #-(and sb-unicode (or x86 x86-64))
               (:file "io-generic-strings")
               #+(and sbcl (or x86 x86-64))
               (:file "util-sbcl")
               #-(and sbcl (or x86 x86-64))
               (:file "util-generic")
               (:file "mop")
               (:file "kmp")
               (:file "storage")
               (:file "disk")))
