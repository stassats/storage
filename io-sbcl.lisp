;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(defconstant +buffer-size+ 8192)

(deftype buffer-length ()
  '(integer 0 #.+buffer-size+))

(deftype word () 'sb-vm:word)

;;; sap wrappers

(defmacro define-sap-ref-wrapper (bits &key name prefix)
  (let ((name (alexandria:format-symbol t "~@[~a-~]~a-~a"
                                        prefix 'mem-ref (or name bits)))
        (sb-sys (alexandria:format-symbol 'sb-sys
                                          "~@[~a-~]~a-~a"
                                          prefix 'sap-ref (or name bits))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (address &optional (offset 0))
         (declare (type word address)
                  (fixnum offset)
                  (optimize speed)
                  (sb-ext:muffle-conditions sb-ext:compiler-note))
         (,sb-sys (sb-sys:int-sap address) offset))

       (declaim (inline (setf ,name)))
       (defun (setf ,name) (value address &optional (offset 0))
         (declare (type (unsigned-byte ,bits) value)
                  (type word address)
                  (fixnum offset)
                  (optimize speed)
                  (sb-ext:muffle-conditions sb-ext:compiler-note))
         (setf (,sb-sys (sb-sys:int-sap address) offset) value)))))

(define-sap-ref-wrapper 8)
(define-sap-ref-wrapper 16)
(define-sap-ref-wrapper 32)
(define-sap-ref-wrapper #.sb-vm:n-word-bits :name word)

(define-sap-ref-wrapper 8 :prefix signed)
(define-sap-ref-wrapper 16 :prefix signed)
(define-sap-ref-wrapper 32 :prefix signed)
(define-sap-ref-wrapper #.sb-vm:n-word-bits :name word :prefix signed)

(declaim (inline mem-ref-24))
(defun mem-ref-24 (address offset)
  (declare (optimize speed (safety 0))
           (fixnum offset))
  (mask-field (byte 24 0) (mem-ref-32 address offset)))

(declaim (inline n-mem-ref))
(defun n-mem-ref (n address &optional (offset 0))
  (funcall (ecase n
             (1 #'mem-ref-8)
             (2 #'mem-ref-16)
             (3 #'mem-ref-24)
             (4 #'mem-ref-32))
           address
           offset))

(declaim (inline n-signed-mem-ref (setf n-signed-mem-ref)))
(defun n-signed-mem-ref (n address &optional (offset 0))
  (funcall (ecase n
             (1 #'signed-mem-ref-8)
             (2 #'signed-mem-ref-16)
             (4 #'signed-mem-ref-32))
           address
           offset))

(defun (setf n-signed-mem-ref) (value n address &optional (offset 0))
  (funcall (ecase n
             (1 #'(setf signed-mem-ref-8))
             (2 #'(setf signed-mem-ref-16))
             (4 #'(setf signed-mem-ref-32)))
           value
           address
           offset))

(declaim (inline vector-address))
(defun vector-address (vector)
  (sb-sys:sap-int (sb-sys:vector-sap vector)))

;;;

(defun allocate-buffer ()
  (sb-sys:sap-int
   (sb-alien:alien-sap
    (sb-alien:make-alien char
                         ;; alignment
                         (+ +buffer-size+
                            sb-vm:n-word-bytes)))))

(defstruct (input-stream
            (:predicate nil))
  (fd nil :type word)
  (left 0 :type word)
  (buffer-start (allocate-buffer)
   :type word)
  (buffer-end 0 :type word)
  (buffer-position 0 :type word))

(defstruct (output-stream
            (:predicate nil))
  (fd nil :type word)
  (buffer-start (allocate-buffer)
                :type word)
  (buffer-end 0 :type word)
  (buffer-position 0 :type word))

(defun open-file (file-stream
                  &key direction)
  (if (eql direction :output)
      (let ((output (make-output-stream
                     :fd (sb-sys:fd-stream-fd file-stream))))
        (setf (output-stream-buffer-position output)
              (output-stream-buffer-start output)
              (output-stream-buffer-end output)
              (+ (output-stream-buffer-start output)
                 +buffer-size+))
        output)
      (make-input-stream
       :fd (sb-sys:fd-stream-fd file-stream)
       :left (file-length file-stream))))

(defun close-input-stream (stream)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "free"
                          (function (values) sb-alien:long))
   (input-stream-buffer-start stream)))

(defun close-output-stream (stream)
  (flush-buffer stream)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "free"
                          (function (values) sb-alien:long))
   (output-stream-buffer-start stream)))

(declaim (inline stream-end-of-file-p))
(defun stream-end-of-file-p (stream)
  (and (>= (input-stream-buffer-position stream)
           (input-stream-buffer-end stream))
       (zerop (input-stream-left stream))))

(declaim (inline unix-read))
(defun unix-read (fd buf len)
  (declare (type word fd len))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "read"
                          (function sb-alien:int
                                    sb-alien:int sb-alien:long sb-alien:int))
   fd buf len))

(declaim (inline unix-write))
(defun unix-write (fd buf len)
  (declare (type word fd len))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "write"
                          (function sb-alien:int
                                    sb-alien:int sb-alien:long sb-alien:int))
   fd buf len))

(defun fill-buffer (stream offset)
  (let ((length (unix-read (input-stream-fd stream)
                           (+ (input-stream-buffer-start stream) offset)
                           (- +buffer-size+ offset))))
    (setf (input-stream-buffer-end stream)
          (+ (input-stream-buffer-start stream) (+ length offset)))
    (decf (input-stream-left stream) length))
  t)

(defun refill-buffer (n stream)
  (declare (type word n)
           (input-stream stream))
  (let ((left-n-bytes (- (input-stream-buffer-end stream)
                         (input-stream-buffer-position stream))))
    (when (> (- n left-n-bytes)
             (input-stream-left stream))
      (error "End of file ~a" stream))
    (unless (zerop left-n-bytes)
      (setf (mem-ref-word (input-stream-buffer-start stream))
            (n-mem-ref left-n-bytes
                       (input-stream-buffer-position stream))))
    (fill-buffer stream left-n-bytes))
  (let ((start (input-stream-buffer-start stream)))
    (setf (input-stream-buffer-position stream)
          (+ start n)))
  t)

(declaim (inline advance-input-stream))
(defun advance-input-stream (n stream)
  (declare (type (integer 1 4) n)
           (type input-stream stream))
  (let* ((sap (input-stream-buffer-position stream))
         (new-sap (sb-ext:truly-the word (+ sap n))))
    (declare (word sap new-sap))
    (cond ((> new-sap (input-stream-buffer-end stream))
           (refill-buffer n stream)
           (input-stream-buffer-start stream))
          (t
           (setf (input-stream-buffer-position stream)
                 new-sap)
           sap))))

(declaim (inline read-n-bytes))
(defun read-n-bytes (n stream)
  (declare (type (integer 1 4) n))
  (n-mem-ref n (advance-input-stream n stream)))

(declaim (inline read-n-signed-bytes))
(defun read-n-signed-bytes (n stream)
  (declare (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (type (integer 1 4) n))
  (n-signed-mem-ref n (advance-input-stream n stream)))

(declaim (inline write-n-signed-bytes))
(defun write-n-signed-bytes (value n stream)
  (declare (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (fixnum n))
  (setf (n-signed-mem-ref n (advance-output-stream n stream)) value)
  t)

(defun flush-buffer (stream)
  (unix-write (output-stream-fd stream)
              (output-stream-buffer-start stream)
              (- (output-stream-buffer-position stream)
                 (output-stream-buffer-start stream))))

(declaim (inline advance-output-stream))
(defun advance-output-stream (n stream)
  (declare (optimize (safety 0))
           (type output-stream stream)
           ((integer 1 4) n))
  (let* ((sap (output-stream-buffer-position stream))
         (new-sap (+ sap n)))
    (declare (word sap new-sap))
    (cond ((> new-sap (output-stream-buffer-end stream))
           (flush-buffer stream)
           (setf (output-stream-buffer-position stream)
                 (the word (+ (output-stream-buffer-start stream)
                              n)))
           (output-stream-buffer-start stream))
          (t
           (setf (output-stream-buffer-position stream)
                 new-sap)
           sap))))

(declaim (inline write-n-bytes))
(defun write-n-bytes (value n stream)
  (declare (optimize (space 0))
           (type (integer 1 4) n))
  (setf (mem-ref-32 (advance-output-stream n stream)) value))

;;;

(defmacro with-io-file ((stream file
                         &key append (direction :input))
                        &body body)
  (let ((fd-stream (gensym)))
    `(with-open-file (,fd-stream ,file
                                 :element-type '(unsigned-byte 8)
                                 :direction ,direction
                                 ,@(and (eql direction :output)
                                        `(:if-exists ,(if append
                                                          :append
                                                          :supersede))))
       (let ((,stream (open-file ,fd-stream :direction ,direction)))
         (unwind-protect
              (progn ,@body)
           ,@(ecase direction
               (:output
                `((close-output-stream ,stream)
                  (sb-posix:fdatasync
                   (sb-sys:fd-stream-fd ,fd-stream))))
               (:input
                `((close-input-stream ,stream)))))))))
