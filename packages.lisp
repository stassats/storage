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
   #:count-))

(defpackage #:imdb
  (:use #:cl))

(defpackage #:graphs
  (:use #:cl #:movies)
  (:export #:movies-by-decade
           #:views-per-month))
