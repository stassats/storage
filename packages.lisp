;;; -*- Mode: Lisp -*-

(defpackage #:storage
  (:use #:closer-common-lisp)
  (:shadow #:count #:delete)
  (:export
   #:id
   #:add
   #:where
   #:delete
   #:objects-of-type
   #:map-type

   #:storable-class
   #:identifiable
   #:lookup
   #:interlink-objects
   #:load-data
   #:save-data
   #:with-storage
   #:storage))

