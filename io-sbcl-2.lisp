;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage-test)

(defun open-file (file-stream
                  &key direction size)
  (declare (ignore size))
  (when (eql direction :output)
    (warn "Not implemented."))
  (make-st-stream
   :fd (sb-sys:fd-stream-fd file-stream)
   :left (file-length file-stream)))

(defun close-file (stream)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "free"
                          (function (values) sb-alien:int))
   (st-stream-buffer-start stream)))

(defconstant +buffer-size+ 8192)

(deftype word ()
  'sb-vm:word)

(defstruct st-stream
  (fd nil :type word)
  (left 0 :type word)
  (buffer-start (sb-sys:sap-int
                 (sb-alien::%make-alien (* sb-vm:n-byte-bits +buffer-size+)))
                :type word)
  (buffer-end 0 :type word)
  (buffer-position 0 :type word))

(declaim (notinline sap-ref-24))
(defun sap-ref-24 (sap offset)
  (declare (optimize speed (safety 0))
           (fixnum offset))
  (mask-field (byte 24 0) (sb-sys:sap-ref-32 sap offset)))

(declaim (inline n-sap-ref))
(defun n-sap-ref (n sap &optional (offset 0))
  (funcall (ecase n
             (1 #'sb-sys:sap-ref-8)
             (2 #'sb-sys:sap-ref-16)
             (3 #'sap-ref-24)
             (4 #'sb-sys:sap-ref-32))
           (sb-sys:int-sap sap)
           offset))

(declaim (inline unix-read))
(defun unix-read (fd buf len)
  (declare (optimize (sb-c::float-accuracy 0)
                     (space 0)))
  (declare (type sb-unix::unix-fd fd)
           (type word len))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "read"
                          (function sb-alien:int
                                    sb-alien:int sb-alien:int sb-alien:int))
   fd buf len))

(defun unix-write (fd buf len)
  (declare (type sb-unix::unix-fd fd)
           (type word len))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "write"
                          (function sb-alien:int
                                    sb-alien:int sb-alien:int sb-alien:int))
   fd buf len))

(declaim (inline fill-buffer))
(defun fill-buffer (stream offset)
  (let ((length (unix-read (st-stream-fd stream)
                           (+ (st-stream-buffer-start stream) offset)
                           (- +buffer-size+ offset))))
    (setf (st-stream-buffer-end stream)
          (+ (st-stream-buffer-start stream) (+ length offset)))
    (decf (st-stream-left stream) length))
  t)

(declaim (notinline refill-buffer))
(defun refill-buffer (n stream)
  (declare (optimize speed (safety 0))
           (type (integer 0 4)  n)
           (st-stream stream))
  (let ((left-n-bytes (sb-ext:truly-the word (- (st-stream-buffer-end stream)
                                   (st-stream-buffer-position stream)))))
    (when (> (the word (- n left-n-bytes))
             (st-stream-left stream))
      (error "End of file ~a" stream))
    (unless (zerop left-n-bytes)
      (setf (sb-sys:sap-ref-word (sb-sys:int-sap (st-stream-buffer-start stream)) 0)
            (n-sap-ref left-n-bytes (st-stream-buffer-position stream))))
    (fill-buffer stream left-n-bytes))
  (let ((start (st-stream-buffer-start stream)))
    (setf (st-stream-buffer-position stream)
          (sb-ext:truly-the u32 (+ start n))))
  t)

(declaim (inline advance-stream))
(defun advance-stream (n stream)
  (declare (optimize (space 0))
           (type word n)
           (type st-stream stream))
  (let* ((sap (st-stream-buffer-position stream))
         (new-sap (sb-ext:truly-the word (+ sap n))))
    (declare (word sap new-sap))
    (cond ((> new-sap (st-stream-buffer-end stream))
           (- new-sap (st-stream-buffer-end stream))
           (refill-buffer n stream)
           (st-stream-buffer-start stream))
          (t
           (setf (st-stream-buffer-position stream)
                 new-sap)
           sap))))

(declaim (inline read-n-bytes))
(defun read-n-bytes (n stream)
  (declare (optimize (space 0))
           (type word n))
  (n-sap-ref n (advance-stream n stream)))

(defun flush-buffer (stream))

(defun write-n-bytes (n stream)
  (declare (optimize (space 0))
           (type word n))
  (n-sap-ref n (advance-stream n stream)))

;;;

(defmacro with-io-file ((stream file &key (direction :input) size)
                        &body body)
  (let ((fd-stream (gensym)))
    `(with-open-file (,fd-stream ,file
                                 :direction (if (eql ,direction :output)
                                                :io
                                                ,direction)
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
       (let ((,stream (open-file ,fd-stream :direction ,direction :size ,size)))
         (unwind-protect
              (progn ,@body)
           (close-file ,stream)
           (when (eql ,direction :output)
             (sb-posix:fdatasync
              (sb-sys:fd-stream-fd ,fd-stream))))))))

(defun test ()
  (declare (optimize speed (safety 0)))
  (storage-test::with-io-file (stream "/home/stas/test-io-file")
    (loop repeat 6276930
          with count fixnum
          if (evenp
              (storage-test::read-n-bytes 1 stream))
          do (setf count (sb-ext:truly-the fixnum (1+ count)))
          finally (return count))))

(defun test-2 ()
  (declare (optimize speed (safety 0)))
  (storage::with-io-file (stream "/home/stas/test-io-file")
    (loop repeat 6276930
          with count fixnum
          if (evenp
              (storage::read-n-bytes 1 stream))
          do (setf count (sb-ext:truly-the fixnum (1+ count)))
          finally (return count))))

