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
   #:storage
   #:relations
   #:relation
   #:storage-data
   #:slot-db-type
   #:slot-read-only-p
   #:map-data
   #:search-key
   #:make-kmp-searcher
   #:count
   #:slot-unit
   #:storage-file
   #:find-slot
   #:modified
   #:objects-of-class))

