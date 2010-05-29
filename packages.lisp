;;; -*- Mode: Lisp -*-

(defpackage #:storage
  (:use #:closer-common-lisp)
  (:shadow #:count #:delete)
  (:export
   #:add
   #:where
   #:delete
   #:objects-of-type
   #:map-type

   #:storable-class
   #:identifiable))

(defpackage #:movies
  (:use #:closer-common-lisp)
  (:export


   #:person
   #:male
   #:female
   #:movie
   #:movies
   #:short-movie
   #:feature-movie
   #:unwatched-movie
   #:theatre
   #:view
   #:views

   #:title
   #:date
   #:theatre
   #:animated-p
   #:duration
   #:documentary-p
   #:directors
   #:writers
   #:producers
   #:cast
   #:countries
   #:year
   #:imdb-id
   #:alternate-titles
   #:name
   #:died
   #:born
   #:imdb-id

   #:save-data
   #:load-data

   #:lookup
   #:count
   #:id
   #:format-date
   #:month
   #:sort-hash-table-to-alist
   #:identifiable
   #:imdb-url
   #:last-views
   #:print-movies

   #:movies-by-decade
   #:views-per-month
   #:top-years
   #:top-persons
   #:top-countries))

(defpackage #:iso-3166-1
  (:use #:cl)
  (:export
   #:code-country
   #:country-code))

(defpackage #:imdb
  (:use #:cl)
  (:shadow #:search)
  (:export #:search
           #:parse-page))

(defpackage #:ascii-graph
  (:use #:cl)
  (:export #:bar-graph))

(defpackage #:freebase
  (:use #:cl))

(defpackage #:http
  (:use #:cl)
  (:export #:request
           #:*stream*))
