;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defstruct mmap-stream
  (sap (sb-sys:int-sap 0)
       :type sb-sys:system-area-pointer)
  (position 0 :type fixnum)
  (length 0 :type fixnum :read-only t))

(defun scale-file (stream size)
  (file-position stream (1- size))
  (write-byte 0 stream)
  (finish-output stream))

(defun mmap (file-stream
             &key direction size)
  (when (eql direction :output)
    (scale-file file-stream size))
  (make-mmap-stream
   :sap (sb-posix:mmap nil
                       (or size (file-length file-stream))
                       (ecase direction
                         (:input sb-posix:prot-read)
                         (:output sb-posix:prot-write))
                       sb-posix:map-shared
                       (sb-sys:fd-stream-fd file-stream)
                       0)
   :length (or size (file-length file-stream))))

(defun munmap (mmap-stream)
  (sb-posix:munmap (mmap-stream-sap mmap-stream)
                   (mmap-stream-length mmap-stream))
  (setf (mmap-stream-sap mmap-stream) (sb-sys:int-sap 0)))

(declaim (inline stream-position))
(defun stream-position (stream)
  (mmap-stream-position stream))

(declaim (inline stream-length))
(defun stream-length (stream)
  (mmap-stream-length stream))

;;;

(declaim (inline sap-ref-24))
(defun sap-ref-24 (sap offset)
  (declare (optimize speed (safety 0))
           (fixnum offset))
  (mask-field (byte 24 0) (sb-sys:sap-ref-32 sap offset)))

(declaim (inline advance-stream))
(defun advance-stream (n stream)
  (let* ((position (mmap-stream-position stream))
         (new-position (+ position n)))
    (when (> new-position
             (mmap-stream-length stream))
      (error "End of file ~a" stream))
    (setf (mmap-stream-position stream)
          new-position)
    (sb-sys:sap+ (mmap-stream-sap stream) position)))

(declaim (inline read-n-bytes))
(defun read-n-bytes (n stream)
  (declare (optimize speed)
           (fixnum n))
  (funcall (ecase n
             (1 #'sb-sys:sap-ref-8)
             (2 #'sb-sys:sap-ref-16)
             (3 #'sap-ref-24)
             (4 #'sb-sys:sap-ref-32))
           (advance-stream n stream)
           0))

(declaim (inline write-n-bytes))
(defun write-n-bytes (value n stream)
  (declare (optimize speed)
           (fixnum n))
  (setf (sb-sys:sap-ref-32 (advance-stream n stream) 0) value)
  t)

(declaim (inline copy-mem))
(defun copy-mem (from to length)
  (loop for i below length by sb-vm:n-word-bytes
        do
        #+x86
        (setf (sb-sys:sap-ref-32 to i)
              (sb-sys:sap-ref-32 from i))
        #+x86-64
        (setf (sb-sys:sap-ref-64 to i)
              (sb-sys:sap-ref-64 from i))))

(defun read-ascii-string-optimized (length string stream)
  (declare (type fixnum length))
  (sb-sys:with-pinned-objects (string)
    (let ((mmap-sap (advance-stream length stream))
          (string-sap (sb-sys:vector-sap string)))
      (copy-mem mmap-sap string-sap length)))
  string)

(defun write-ascii-string-optimized (length string stream)
  (declare (type fixnum length))
  (sb-sys:with-pinned-objects (string)
    (let ((mmap-sap (advance-stream length stream))
          (string-sap (sb-sys:vector-sap string)))
      (copy-mem string-sap mmap-sap length))))

(defmacro with-io-file ((stream file &key (direction :input) size)
                        &body body)
  (let ((fd-stream (gensym)))
    `(with-open-file (,fd-stream ,file
                                 :direction (if (eql ,direction :output)
                                                :io
                                                ,direction)
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
       (let ((,stream (mmap ,fd-stream :direction ,direction :size ,size)))
         (unwind-protect
              (progn ,@body)
           (progn (munmap ,stream)
                  (when (eql ,direction :output)
                    (sb-posix:fdatasync
                     (sb-sys:fd-stream-fd ,fd-stream)))))))))
