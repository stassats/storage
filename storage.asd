;;; -*- Mode: Lisp -*-

(asdf:defsystem #:storage
  :name "storage"
  :serial t
  :depends-on (alexandria
               closer-mop
               #-sbcl-unsafe ieee-floats
               #+(and sbcl-unsafe (or x86 x86-64 arm arm64)
                      (not win32))
               sb-posix)
  :components ((:file "packages")
               (:file "parameters")
               #+(and sbcl-unsafe (or x86 x86-64 arm arm64))
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
               #-(and sbcl-unsafe (or x86 x86-64 arm arm64))
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
