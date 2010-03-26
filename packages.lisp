;;; -*- Mode: Lisp -*-

(defpackage #:movies
  (:use #:cl)
  (:shadow #:count #:delete #:delete-if)
  (:export
   #:add
   #:where
   #:delete
   #:delete-if

   #:*data-file*
   #:*data*

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
   #:delete
   #:delete-if
   #:sort-hash-table-to-alist
   #:identifiable
   #:imdb-url))

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

(defpackage #:graphs
  (:use #:cl #:movies)
  (:shadowing-import-from #:movies #:count #:delete #:delete-if)
  (:export #:movies-by-decade
           #:views-per-month
           #:top-years
           #:top-persons
           #:top-countries))

(defpackage #:freebase
  (:use #:cl))

(defpackage #:http
  (:use #:cl)
  (:export #:request
           #:*stream*))
