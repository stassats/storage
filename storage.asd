;;; -*- Mode: Lisp -*-

(asdf:defsystem #:storage
  :name "storage"
  :serial t
  :depends-on (closer-mop)
  :components ((:file "packages")
               #+(and sbcl (or x86 x86-64))
               (:file "io-sbcl")
               #-(and sbcl (or x86 x86-64))
               (:file "io-generic")
               (:file "mop")
               (:file "storage")))
