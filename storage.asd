;;; -*- Mode: Lisp -*-

(asdf:defsystem #:storage
  :name "storage"
  :serial t
  :depends-on (alexandria
               closer-mop
               #-sbcl ieee-floats
               #+(and sbcl (or x86 x86-64 arm)
                      (not win32))
               sb-posix)
  :components ((:file "packages")
               (:file "parameters")
               #+(and sbcl (or x86 x86-64 arm))
               (:module "sbcl"
                :pathname ""
                :serial t
                :components
                ((:file "io-sbcl")
                 #+sb-unicode
                 (:file "io-sbcl-strings")
                 #-sb-unicode
                 (:file "io-generic-strings")
                 (:file "util-sbcl")))
               #-(and sbcl (or x86 x86-64 arm))
               (:module "generic"
                :pathname ""
                :serial t
                :components
                ((:file "io-generic")
                 (:file "io-generic-strings")
                 (:file "util-generic")))
               (:file "mop")
               (:file "kmp")
               (:file "storage")
               (:file "query")
               (:file "disk")))
