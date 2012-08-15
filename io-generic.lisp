;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(define-compiler-macro read-n-bytes (&whole form bytes stream)
  (case bytes
    (1 `(read-byte ,stream))
    (2 `(read-2-bytes ,stream))
    (3 `(read-3-bytes ,stream))
    (4 `(read-4-bytes ,stream))
    (8 `(read-8-bytes ,stream))
    (t form)))

(declaim (inline read-n-bytes))
(defun read-n-bytes (n stream)
  (ecase n
    (1 (read-byte stream))
    (2 (read-2-bytes stream))
    (3 (read-3-bytes stream))
    (4 (read-4-bytes stream))
    (8 (read-8-bytes stream))))

(declaim (inline read-n-signed-bytes))
(defun read-n-signed-bytes (n stream)
  (let ((byte (read-n-bytes n stream)))
    (logior byte (- (mask-field (byte 1 (1- (* n 8))) byte)))))

(declaim (inline read-2-bytes read-3-bytes read-4-bytes
                 read-8-bytes))
(defun read-2-bytes (stream)
  (declare (optimize speed))
  (let ((1-byte (read-byte stream))
        (2-byte (read-byte stream)))
    (logior (ash 2-byte 8) 1-byte)))

(defun read-3-bytes (stream)
  (declare (optimize speed))
  (let ((1-byte (read-byte stream))
        (2-byte (read-byte stream))
        (3-byte (read-byte stream)))
    (logior (ash 3-byte 16) (ash 2-byte 8) 1-byte)))

(defun read-4-bytes (stream)
  (declare (optimize speed))
  (let ((1-byte (read-byte stream))
        (2-byte (read-byte stream))
        (3-byte (read-byte stream))
        (4-byte (read-byte stream)))
    (logior (ash 4-byte 24) (ash 3-byte 16) (ash 2-byte 8) 1-byte)))

(defun read-8-bytes (stream)
  (logior (read-4-bytes stream)
          (ash (read-4-bytes stream) 32)))

(declaim (inline write-n-bytes))
(defun write-n-bytes (integer n stream)
  (loop for low-bit to (* 8 (1- n)) by 8
        do (write-byte (ldb (byte 8 low-bit) integer) stream)))

(declaim (inline write-n-signed-bytes))
(defun write-n-signed-bytes (integer n stream)
  (write-n-bytes (ldb (byte (* n 8) 0) integer) n stream))

;;; floats

(declaim (inline read-single-float read-double-float
                 write-single-float write-double-float))

(defun read-single-float (stream)
  (ieee-floats:decode-float32 (read-n-bytes 4 stream)))

(defun read-double-float (stream)
  (ieee-floats:decode-float64 (read-n-bytes 8 stream)))

(defun write-single-float (value stream)
  (write-n-bytes (ieee-floats:encode-float32 value) 4 stream))

(defun write-double-float (value stream)
  (write-n-bytes (ieee-floats:encode-float64 value) 8 stream))

;;;

(defmacro with-io-file ((stream file &key (direction :input) size) &body body)
  (declare (ignore size))
  `(with-open-file (,stream ,file
                            :element-type '(unsigned-byte 8)
                            :direction ,direction
                            :if-exists :supersede)
     (unwind-protect
          (progn ,@body)
       (when (eql ,direction :output)
         (finish-output ,stream)))))

(declaim (inline stream-end-of-file-p))
(defun stream-end-of-file-p (stream)
  (>= (file-position stream)
      (file-length stream)))
