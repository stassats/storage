;;; -*- Mode: Lisp -*-

(defpackage #:movies
  (:use #:cl)
  (:shadow #:count #:delete #:delete-if)
  (:export
   #:add-person
   #:add-view
   #:add-movie
   #:where
   #:delete
   #:delete-if

   #:*movies*
   #:*views*
   #:*persons*
   #:*theatres*
   #:*data-file*

   #:person
   #:movie
   #:short-movie
   #:feature-movie
   #:unwatched-movie
   #:theatre
   #:view

   #:title
   #:date
   #:theatre
   #:animated-p
   #:duration
   #:documentary-p
   #:directors
   #:year
   #:imdb-id
   #:alternate-titles
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
   #:delete-if))

(defpackage #:imdb
  (:use #:cl)
  (:shadow #:search)
  (:export #:search))

(defpackage #:ascii-graph
  (:use #:cl)
  (:export #:bar-graph))

(defpackage #:graphs
  (:use #:cl #:movies)
  (:shadow #:count #:delete #:delete-if)
  (:export #:movies-by-decade
           #:views-per-month
           #:top-years))

