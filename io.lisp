;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:io)

(defmacro with-mmap-file ((stream file) &body body)
  (let ((fd-stream (gensym)))
    `(with-open-file (,fd-stream ,file)
       (let ((,stream (mmap ,fd-stream)))
         (unwind-protect
              (progn ,@body)
           (munmap ,stream))))))
