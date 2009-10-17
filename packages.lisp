;;; -*- Mode: Lisp -*-

(defpackage #:movies
  (:use #:cl)
  (:export
   #:add-person
   #:add-view
   #:where

   #:*movies*
   #:*views*
   #:*persons*
   #:*theatres*
   #:*data-file*

   #:person
   #:movie
   #:short-movie
   #:feature-movie
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
   #:list-all
   #:count-
   #:id
   #:format-date
   #:month))

(defpackage #:imdb
  (:use #:cl))

(defpackage #:ascii-graph
  (:use #:cl)
  (:export :bar-graph))

(defpackage #:graphs
  (:use #:cl #:movies)
  (:export #:movies-by-decade
           #:views-per-month))

