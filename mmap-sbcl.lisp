;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defstruct mmap-stream
  (sap nil :type sb-sys:system-area-pointer)
  (position 0 :type fixnum)
  (length 0 :type fixnum))

(defun mmap (file-stream)
  (make-mmap-stream
   :sap (sb-posix:mmap nil
                       (file-length file-stream)
                       sb-posix:prot-read
                       sb-posix:map-shared
                       (sb-sys:fd-stream-fd file-stream)
                       0)
   :length (file-length file-stream)))

(defun munmap (mmap-stream)
  (sb-posix:munmap (mmap-stream-sap mmap-stream)
                   (mmap-stream-length mmap-stream)))


(declaim (inline sap-ref-24))
(defun sap-ref-24 (sap offset)
  (declare (optimize speed (safety 0))
           (fixnum offset))
  (mask-field (byte 24 0) (sb-sys:sap-ref-32 sap offset)))

(declaim (inline read-n-bytes))
(defun read-n-bytes (n stream &optional (eof-error-p t))
  (declare (optimize speed)
           (fixnum n))
  (let ((new-position (+ (mmap-stream-position stream) n)))
    (when (> new-position
            (mmap-stream-length stream))
     (if eof-error-p
         (error "End of file ~a" stream)
         (return-from read-n-bytes)))
   (prog1
       (funcall (ecase n
                  (1 #'sb-sys:sap-ref-8)
                  (2 #'sb-sys:sap-ref-16)
                  (3 #'sap-ref-24)
                  (4 #'sb-sys:sap-ref-32))
                (mmap-stream-sap stream)
                (mmap-stream-position stream))
     (setf (mmap-stream-position stream)
           new-position))))

(defmacro with-io-file ((stream file) &body body)
  (let ((fd-stream (gensym)))
    `(with-open-file (,fd-stream ,file)
       (let ((,stream (mmap ,fd-stream)))
         (unwind-protect
              (progn ,@body)
           (munmap ,stream))))))
