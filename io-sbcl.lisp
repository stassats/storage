;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(deftype word ()
  'sb-vm:word)

(defstruct mmap-stream
  (beginning)
  (length 0 :type word :read-only t)
  (sap 0 :type word)
  (end 0 :type word :read-only t))

(defun scale-file (stream size)
  (file-position stream (1- size))
  (write-byte 0 stream)
  (finish-output stream))

(defun mmap (file-stream
             &key direction size)
  (when (eql direction :output)
    (scale-file file-stream size))
  (let* ((sap (sb-posix:mmap nil
                             (or size (file-length file-stream))
                             (ecase direction
                               (:input sb-posix:prot-read)
                               (:output sb-posix:prot-write))
                             sb-posix:map-shared
                             (sb-sys:fd-stream-fd file-stream)
                             0))
         (sap-int (sb-sys:sap-int sap)))
    (make-mmap-stream
     :beginning sap
     :sap sap-int
     :length (or size (file-length file-stream))
     :end (+ sap-int (or size (file-length file-stream))))))

(defun munmap (mmap-stream)
  (sb-posix:munmap (mmap-stream-beginning mmap-stream)
                   (mmap-stream-length mmap-stream))
  (setf (mmap-stream-sap mmap-stream) 0))

(declaim (inline stream-end-of-file-p))
(defun stream-end-of-file-p (stream)
  (>= (mmap-stream-sap stream)
      (mmap-stream-end stream)))

;;;

(declaim (inline sap-ref-24))
(defun sap-ref-24 (sap offset)
  (declare (optimize speed (safety 0))
           (fixnum offset))
  (mask-field (byte 24 0) (sb-sys:sap-ref-32 sap offset)))

(defun signal-end-of-file (stream)
  (error "End of file ~a" stream))

(declaim (inline advance-stream))
(defun advance-stream (n stream)
  (declare (optimize (space 0))
           (type word n))
  (let* ((sap (mmap-stream-sap stream))
         (new-position (sb-ext:truly-the word (+ sap n))))
    (when (> new-position
             (mmap-stream-end stream))
      (signal-end-of-file stream))
    (setf (mmap-stream-sap stream) new-position)
    (sb-sys:int-sap sap)))

(declaim (inline read-n-bytes))
(defun read-n-bytes (n stream)
  (declare (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (type (integer 1 4) n))
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
        do (setf (sb-sys:sap-ref-word to i)
                 (sb-sys:sap-ref-word from i))))

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
