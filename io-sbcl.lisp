;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(defconstant +buffer-size+ 8192)

(deftype buffer-length ()
  '(integer 0 #.(* +buffer-size+ 2)))

(deftype word () 'sb-vm:word)

(declaim (ftype (function * (values * &optional))
                fill-input-buffer refill-input-buffer
                flush-output-buffer))

;;; sap wrappers

(defmacro define-sap-ref-wrapper (bits &key name signed)
  (let ((name (alexandria:format-symbol t "~@[~a-~]~a-~a"
                                        (if signed
                                            'signed)
                                        'mem-ref (or name bits)))
        (sb-sys (alexandria:format-symbol 'sb-sys
                                          "~@[~a-~]~a-~a"
                                          (if signed
                                              'signed)
                                          'sap-ref (or name bits))))
    `(progn
       (declaim (inline ,name))
       (defun ,name (address &optional (offset 0))
         (declare (type word address)
                  (fixnum offset)
                  (sb-ext:muffle-conditions sb-ext:compiler-note))
         (,sb-sys (sb-sys:int-sap address) offset))

       (declaim (inline (setf ,name)))
       (defun (setf ,name) (value address &optional (offset 0))
         (declare (type ,(cond
                          ((eql bits 'double)
                           'double-float)
                          ((eql bits 'single)
                           'single-float)
                          (signed
                           `(signed-byte ,bits))
                          (t
                           `(unsigned-byte ,bits)))
                        value)
                  (type word address)
                  (fixnum offset)
                  (sb-ext:muffle-conditions sb-ext:compiler-note))
         (setf (,sb-sys (sb-sys:int-sap address) offset) value)))))

(define-sap-ref-wrapper 8)
(define-sap-ref-wrapper 16)
(define-sap-ref-wrapper 32)
(define-sap-ref-wrapper #.sb-vm:n-word-bits :name word)
(define-sap-ref-wrapper single)
(define-sap-ref-wrapper double)

(define-sap-ref-wrapper 8 :signed t)
(define-sap-ref-wrapper 16 :signed t)
(define-sap-ref-wrapper 32 :signed t)
(define-sap-ref-wrapper #.sb-vm:n-word-bits :name word :signed t)

(declaim (inline mem-ref-24))
(defun mem-ref-24 (address &optional (offset 0))
  (declare (word address)
           (fixnum offset))
  (mask-field (byte 24 0) (mem-ref-32 address offset)))

(declaim (inline signed-mem-ref-24))
(defun signed-mem-ref-24 (address &optional (offset 0))
  (declare (type word address)
           (fixnum offset)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (optimize speed))
  (let ((byte (mask-field (byte 24 0)
                          (mem-ref-32 address offset))))
    (logior byte (- (mask-field (byte 1 23) byte)))))

(declaim (inline (setf signed-mem-ref-24)))
(defun (setf signed-mem-ref-24) (value address &optional (offset 0))
  (declare (type (signed-byte 24) value)
           (type word address)
           (fixnum offset)
           (sb-ext:muffle-conditions sb-ext:compiler-note)
           (optimize speed))
  (setf (mem-ref-32 address offset)
        (ldb (byte 24 0) value)))

(declaim (inline n-mem-ref))
(defun n-mem-ref (n address &optional (offset 0))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (ecase n
    (1 (mem-ref-8 address offset))
    (2 (mem-ref-16 address offset))
    (3 (mem-ref-24 address offset))
    (4 (mem-ref-32 address offset))))

(declaim (inline n-signed-mem-ref (setf n-signed-mem-ref)))
(defun n-signed-mem-ref (n address &optional (offset 0))
  (ecase n
    (1 (signed-mem-ref-8 address offset))
    (2 (signed-mem-ref-16 address offset))
    (3 (signed-mem-ref-24 address offset))
    (4 (signed-mem-ref-32 address offset))))

(defun (setf n-signed-mem-ref) (value n address &optional (offset 0))
  (ecase n
    (1 (setf (signed-mem-ref-8 address offset) value))
    (2 (setf (signed-mem-ref-16 address offset) value))
    (3 (setf (signed-mem-ref-24 address offset) value))
    (4 (setf (signed-mem-ref-32 address offset) value))))

(declaim (inline vector-address))
(defun vector-address (vector)
  (sb-sys:sap-int (sb-sys:vector-sap vector)))

(defmacro truly-the (type form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl the
    ,type ,form))

;;;

(defun allocate-buffer (&optional twice)
  (sb-sys:sap-int
   (sb-alien:alien-sap
    (sb-alien:make-alien char
                         (+ +buffer-size+
                            (if twice
                                +buffer-size+
                                0)
                            ;; alignment
                            8)))))

(defstruct (input-stream
            (:predicate nil)
            (:copier nil)
            (:constructor make-input-stream (fd left)))
  (fd nil :type word :read-only t)
  (left 0 :type word)
  (buffer-start (allocate-buffer)
   :type word
   :read-only t)
  (buffer-end 0 :type word)
  (buffer-position 0 :type word))

(defstruct (output-stream
            (:predicate nil)
            (:copier nil)
            (:constructor make-output-stream (fd)))
  (fd nil :type word :read-only t)
  (buffer-start (allocate-buffer t)
   :type word
   :read-only t)
  (buffer-end 0 :type word)
  (buffer-position 0 :type word))

(declaim (sb-ext:freeze-type input-stream output-stream))

(defun open-file (file-stream
                  &key direction)
  (if (eql direction :output)
      (let ((output (make-output-stream
                     (sb-sys:fd-stream-fd file-stream))))
        (setf (output-stream-buffer-position output)
              (output-stream-buffer-start output)
              (output-stream-buffer-end output)
              (+ (output-stream-buffer-start output)
                 +buffer-size+))
        output)
      (make-input-stream
       (sb-sys:fd-stream-fd file-stream)
       (file-length file-stream))))

(defun close-input-stream (stream)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "free"
                          (function (values) sb-alien:unsigned-long))
   (input-stream-buffer-start stream)))

(defun close-output-stream (stream)
  (flush-output-buffer stream 0)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "free"
                          (function (values) sb-alien:unsigned-long))
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
   (sb-alien:extern-alien #-win32 "read"
                          #+win32 "win32_unix_read"
                          (function sb-alien:int
                                    sb-alien:int sb-alien:unsigned-long
                                    sb-alien:int))
   fd buf len))

(declaim (inline unix-write))
(defun unix-write (fd buf len)
  (declare (type word fd len))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien #-win32 "write"
                          #+win32 "win32_unix_write"
                          (function sb-alien:int
                                    sb-alien:int sb-alien:unsigned-long
                                    sb-alien:int))
   fd buf len))

(defun fill-input-buffer (stream offset)
  (let ((length (unix-read (input-stream-fd stream)
                           (+ (input-stream-buffer-start stream) offset)
                           (- +buffer-size+ offset))))
    (setf (input-stream-buffer-end stream)
          (+ (input-stream-buffer-start stream) (+ length offset)))
    (decf (input-stream-left stream) length))
  t)

(defun refill-input-buffer (n stream)
  (declare (type word n)
           (input-stream stream))
  (let ((left-n-bytes (- (input-stream-buffer-end stream)
                         (input-stream-buffer-position stream))))
    (when (> (- n left-n-bytes)
             (input-stream-left stream))
      (error "End of file ~a" stream))
    (loop for start from (input-stream-buffer-start stream)
          by sb-vm:n-word-bytes
          for position from (input-stream-buffer-position stream)
          below (input-stream-buffer-end stream)
          by sb-vm:n-word-bytes
          do
          (setf (mem-ref-word start)
                (mem-ref-word position)))
    (fill-input-buffer stream left-n-bytes))
  (let ((start (input-stream-buffer-start stream)))
    (setf (input-stream-buffer-position stream)
          (+ start n)))
  t)

(declaim (inline advance-input-stream))
(defun advance-input-stream (n stream)
  (declare (type (integer 1 8) n)
           (type input-stream stream))
  (let* ((sap (input-stream-buffer-position stream))
         (new-sap (truly-the word (+ sap n))))
    (declare (word sap new-sap))
    (cond ((> new-sap (input-stream-buffer-end stream))
           (refill-input-buffer n stream)
           (input-stream-buffer-start stream))
          (t
           (setf (input-stream-buffer-position stream)
                 new-sap)
           sap))))

(declaim (inline read-n-bytes))
(defun read-n-bytes (n stream)
  (declare (type (integer 1 4) n)
           (optimize speed))
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

(defun flush-output-buffer (stream n &optional count)
  (unix-write (output-stream-fd stream)
              (output-stream-buffer-start stream)
              (or count
                  (- (output-stream-buffer-position stream)
                     (output-stream-buffer-start stream))))
  (setf (output-stream-buffer-position stream)
        (truly-the word (+ n (output-stream-buffer-start stream))))
  t)

(declaim (inline advance-output-stream))
(defun advance-output-stream (n stream)
  (declare (optimize (safety 0))
           (type output-stream stream)
           ((integer 1 8) n))
  (let* ((sap (output-stream-buffer-position stream))
         (new-sap (truly-the word (+ sap n))))
    (declare (word sap new-sap))
    (cond ((> new-sap (output-stream-buffer-end stream))
           (flush-output-buffer stream n)
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

(declaim (inline read-single-float))
(defun read-single-float (stream)
  (declare (optimize speed))
  (mem-ref-single (advance-input-stream 4 stream)))

(declaim (inline read-double-float))
(defun read-double-float (stream)
  (declare (optimize speed)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (mem-ref-double (advance-input-stream 8 stream)))

(declaim (inline write-single-float))
(defun write-single-float (value stream)
  (declare (optimize speed (safety 0)))
  (setf (mem-ref-single (advance-output-stream 4 stream)) value)
  t)

(declaim (inline write-double-float))
(defun write-double-float (value stream)
  (declare (optimize speed (safety 0)))
  (setf (mem-ref-double (advance-output-stream 8 stream)) value)
  t)

;;;
#+win32
(defun flush-file-buffers (fd)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "FlushFileBuffers"
                          (function (boolean) sb-alien:unsigned-long))
   fd))

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
                  #-win32
                  (sb-posix:fdatasync
                   (sb-sys:fd-stream-fd ,fd-stream))
                  #+win32
                  (flush-file-buffers
                   (sb-sys:fd-stream-fd ,fd-stream))))
               (:input
                `((close-input-stream ,stream)))))))))
