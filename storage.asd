;;; -*- Mode: Lisp -*-

(asdf:defsystem #:storage
  :name "storage"
  :serial t
  :depends-on (alexandria
               closer-mop
               #-sbcl ieee-floats)
  :components ((:file "packages")
               #+(and sbcl (or x86 x86-64))
               (:file "io-sbcl")
               #+(and sbcl (or x86 x86-64))
               (:file "io-sbcl-strings")
               #-(and sbcl (or x86 x86-64))
               (:file "io-generic")
               (:file "mop")
               (:file "kmp")
               (:file "storage")
               (:file "compression")
               (:file "disk")))
