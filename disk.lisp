;;; -*- Mode: Lisp -*-

(in-package #:storage)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *codes*
    #(ascii-string
      identifiable cons
      string null symbol
      storable-class
      standard-object
      fixnum bignum ratio)))

(declaim (type simple-vector *codes*))

(defvar *statistics* ())
(defun collect-stats (code)
  (let* ((type (aref *codes* code))
         (cons (assoc type *statistics*)))
    (if cons
        (incf (cdr cons))
        (push (cons type 1) *statistics*))
    type))

(defvar *indexes* #())
(declaim (simple-vector *indexes*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-code (type)
    (position type *codes*)))

(defparameter *readers* (make-array (length *codes*)))
(declaim (type (simple-array function (*)) *readers*))

(defmacro defreader (type (stream) &body body)
  (let ((name (intern (format nil "~a-~a" type '#:reader))))
    `(progn
       (defun ,name (,stream)
         ,@body)
       (setf (aref *readers* ,(type-code type))
             #',name))))

(declaim (inline call-reader))
(defun call-reader (code stream)
  ;; (collect-stats code)
  (funcall (aref *readers* code) stream))

;;;

(defconstant +sequence-length+ 2)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fixnum-length+ 4))
(defconstant +char-length+ 2)
(defconstant +id-length+ 3)

(defconstant +end+ 255)

(defconstant +ascii-char-limit+ (code-char 128))

(deftype ascii-string ()
  '(or 
    #+sb-unicode simple-base-string ; on #-sb-unicode the limit is 255
    (and simple-string
     (satisfies ascii-string-p))))

(defun ascii-string-p (string)
  (declare (simple-string string))
  (loop for char across string
        always (char< char +ascii-char-limit+)))

(deftype storage-fixnum ()
  `(signed-byte ,(* +fixnum-length+ 8)))

;;;

(defun slot-effective-definition (class slot-name)
  (find slot-name (class-slots class) :key #'slot-definition-name))

(defgeneric write-object (object stream))
(defgeneric object-size (object))

(defun measure-size ()
  (let ((result +sequence-length+)) ;; number of classes
    (map-data (lambda (class objects)
                (when objects
                  (incf result (object-size class))
                  (dolist (object objects)
                    (incf result
                          (standard-object-size object))))))
    result))

(defun assign-ids ()
  (let ((last-id 0))
    (declare (fixnum last-id))
    (map-data
     (lambda (class objects)
       (declare (ignore class))
       (dolist (object objects)
         (setf (id object) last-id)
         (incf last-id))))))

(defun number-of-non-empty-classes (storage)
  (count-if #'objects-of-class
            (storage-data storage)))

(defun write-classes-info (stream)
  (assign-ids)
  (write-n-bytes (number-of-non-empty-classes *storage*)
		 +sequence-length+ stream)
  (map-data (lambda (class objects)
              (when objects
                (write-object class stream)
                (write-n-bytes (length objects)
                               +id-length+ stream)))))

(defun dump-data (stream)
  (write-classes-info stream)
  (map-data (lambda (class objects)
              (declare (ignore class))
              (dolist (object objects)
                (write-standard-object object stream)))))

(declaim (inline read-next-object))
(defun read-next-object (stream)
  (call-reader (read-n-bytes 1 stream) stream))

;;; NIL

(defmethod object-size ((object null))
  1)

(defmethod write-object ((object null) stream)
  (write-n-bytes #.(type-code 'null) 1 stream))

(defreader null (stream)
  (declare (ignore stream)
           (optimize speed))
  nil)

;;; Symbol

(defun short-package-name (package)
  (if (eq package (load-time-value (find-package :cl)))
      (symbol-name :cl)
      (package-name package)))

(defmethod object-size ((object symbol))
  (+ 3 ;; type + package name length + symbol length
     (length (short-package-name (symbol-package object)))
     (length (symbol-name object))))

(defmethod write-object ((object symbol) stream)
  (write-n-bytes #.(type-code 'symbol) 1 stream)
  (let ((name (symbol-name object))
        (package (short-package-name (symbol-package object))))
    (write-n-bytes (length name) 1 stream)
    (write-ascii-string name stream)
    (write-n-bytes (length package) 1 stream)
    (write-ascii-string package stream)))

(defreader symbol (stream)
  (intern (read-ascii-string (read-n-bytes 1 stream) stream)
          (read-ascii-string (read-n-bytes 1 stream) stream)))

;;; Integer

(defmethod object-size ((object integer))
  (+ 1 ;; tag
     1 ;; sign
     (typecase object
       (storage-fixnum +fixnum-length+)
       (t (+ 1 ;; size
             (* (ceiling (integer-length (abs object))
                         (* +fixnum-length+ 8))
                +fixnum-length+))))))

(declaim (inline sign))
(defun sign (n)
  (if (minusp n)
      1
      0))

(defun write-fixnum (n stream)
  (declare (storage-fixnum n))
  (write-n-bytes #.(type-code 'fixnum) 1 stream)
  (write-n-bytes (sign n) 1 stream)
  (write-n-bytes (abs n) +fixnum-length+ stream))

(defun write-bignum (n stream)
  (declare ((and integer (not storage-fixnum)) n))
  (write-n-bytes #.(type-code 'bignum) 1 stream)
  (write-n-bytes (sign n) 1 stream)
  (let* ((fixnum-bits (* +fixnum-length+ 8))
         (n (abs n))
         (size (ceiling (integer-length n) fixnum-bits)))
    (write-n-bytes size 1 stream)
    (loop for position by fixnum-bits below (* size fixnum-bits)
          do
          (write-n-bytes (ldb (byte fixnum-bits position) n)
                         +fixnum-length+ stream))))

(defmethod write-object ((object integer) stream)
  (typecase object
    (storage-fixnum
     (write-fixnum object stream))
    (t (write-bignum object stream))))

(declaim (inline read-sign))
(defun read-sign (stream)
  (if (plusp (read-n-bytes 1 stream))
      -1
      1))

(defreader bignum (stream)
  (let ((fixnum-bits (* +fixnum-length+ 8))
        (sign (read-sign stream))
        (size (read-n-bytes 1 stream))
        (integer 0))
    (loop for position by fixnum-bits below (* size fixnum-bits)
          do
          (setf (ldb (byte fixnum-bits position) integer)
                (read-n-bytes +fixnum-length+ stream)))
    (* sign integer)))

(defreader fixnum (stream)
  (* (read-sign stream)
     (read-n-bytes +fixnum-length+ stream)))

;;; Ratio

(defmethod object-size ((object ratio))
  (+ 1
     (object-size (numerator object))
     (object-size (denominator object))))

(defmethod write-object ((object ratio) stream)
  (write-n-bytes #.(type-code 'ratio) 1 stream)
  (write-object (numerator object) stream)
  (write-object (denominator object) stream))

(defreader ratio (stream)
  (/ (read-next-object stream)
     (read-next-object stream)))

;;; Strings

(defmethod object-size ((string string))
  (+ 1
     +sequence-length+
     (etypecase string
       (ascii-string (length string))
       (string (* (length string)
                  +char-length+)))))

(defun write-ascii-string (string stream)
  (declare (simple-string string))
  (loop for char across string
        do (write-n-bytes (char-code char) 1 stream)))

(defun write-multibyte-string (string stream)
  (declare (simple-string string))
  (loop for char across string
        do (write-n-bytes (char-code char) +char-length+ stream)))

(defmethod write-object ((string string) stream)
  (etypecase string
    #+sb-unicode
    (simple-base-string
     (write-n-bytes #.(type-code 'ascii-string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-ascii-string-optimized (length string) string stream))
    (ascii-string
     (write-n-bytes #.(type-code 'ascii-string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-ascii-string string stream))
    (string
     (write-n-bytes #.(type-code 'string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-multibyte-string string stream))))

(declaim (inline read-ascii-string))
(defun read-ascii-string (length stream)
  (let ((string (make-string length :element-type 'base-char)))
    #-sbcl
    (loop for i below length
          do (setf (schar string i)
                   (code-char (read-n-bytes 1 stream))))
    #+(and sbcl (or x86 x86-64))
    (read-ascii-string-optimized length string stream)
    string))

(defreader ascii-string (stream)
  (read-ascii-string (read-n-bytes +sequence-length+ stream) stream))

(defreader string (stream)
  (let* ((length (read-n-bytes +sequence-length+ stream))
         (string (make-string length :element-type 'character)))
    (loop for i below length
          do (setf (schar string i)
                   (code-char (read-n-bytes +char-length+ stream))))
    string))

;;; cons

(defmethod object-size ((list cons))
  (let ((count (+ 1 1))) ;; type + +end+
    (mapc (lambda (x)
            (incf count (object-size x)))
          list)
    count))

(defmethod write-object ((list cons) stream)
  (write-n-bytes #.(type-code 'cons) 1 stream)
  (dolist (item list)
    (write-object item stream))
  (write-n-bytes +end+ 1 stream))

(defreader cons (stream)
  (loop for code = (read-n-bytes 1 stream)
        until (= code +end+)
        collect (call-reader code stream)))

;;; storable-class

(defmethod object-size ((class storable-class))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (+ 1 ;; type
     (object-size (class-name class))
     1                 ;; class-id
     +sequence-length+ ;; list length
     (reduce #'+ (slots-to-store class)
             :key (lambda (x)
                    (object-size (slot-definition-name x))))
     +id-length+)) ;; size of objects

(defmethod write-object ((class storable-class) stream)
  (write-n-bytes #.(type-code 'storable-class) 1 stream)
  (write-object (class-name class) stream)
  (write-n-bytes (class-id class) 1 stream)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((slots (slots-to-store class)))
    (write-n-bytes (length slots) +sequence-length+ stream)
    (loop for slot across slots
          do (write-object (slot-definition-name slot)
                           stream))))

(defreader storable-class (stream)
  (let ((class (find-class (read-next-object stream)))
        (id (read-n-bytes 1 stream)))
    (cache-class class id)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (pushnew class (storage-data (class-storage class)))
    (setf (objects-of-class class) nil)
    (let* ((length (read-n-bytes +sequence-length+ stream))
           (vector (make-array length)))
      (loop for i below length
            do (setf (aref vector i)
                     (slot-effective-definition class
                                                (read-next-object stream))))
      (setf (slots-to-store class) vector)
      (initialize-class-slots class :slots-to-store-only t))
    class))

;;; identifiable

(defmethod object-size ((object identifiable))
  (+ 1 ;; type
     +id-length+))

(defmethod write-object ((object identifiable) stream)
  (write-n-bytes #.(type-code 'identifiable) 1 stream)
  (write-n-bytes (id object) +id-length+ stream))

(declaim (inline get-instance))
(defun get-instance (id)
  (aref *indexes* id))

(defreader identifiable (stream)
  (get-instance (read-n-bytes +id-length+ stream)))

;;; standard-object

(defun standard-object-size (object)
  (let ((slots (slot-locations-and-initforms (class-of object))))
    (declare (simple-vector slots))
    (+ 1           ;; data type
       +id-length+ ;; id
       (loop for (location . initform) across slots
             sum (let ((value (standard-instance-access object
                                                        location)))
                   (if (eql value initform)
                       0
                       (+ 1 ;; slot id
                          (object-size value)))))
       1))) ;; end-of-slots

;;; Can't use write-object method, because it would conflict with
;;; writing a pointer to a standard object
(defun write-standard-object (object stream)
  (write-n-bytes #.(type-code 'standard-object) 1 stream)
  (let* ((class (class-of object))
         (slots (slot-locations-and-initforms class)))
    (declare (simple-vector slots))
    (write-n-bytes (id object) +id-length+ stream)
    (loop for id below (length slots)
          for (location . initform) = (aref slots id)
          for value = (standard-instance-access object location)
          unless (eql value initform)
          do
          (write-n-bytes id 1 stream)
          (write-object value stream))
    (write-n-bytes +end+ 1 stream)))

(defreader standard-object (stream)
  (let* ((instance (get-instance
		    (read-n-bytes +id-length+ stream)))
	 (class (class-of instance))
         (slots (slot-locations-and-initforms class)))
    (declare (simple-vector slots))
    (loop for slot-id = (read-n-bytes 1 stream)
          until (= slot-id +end+)
          do (setf (standard-instance-access instance
                                             (car (aref slots slot-id)))
                   (read-next-object stream)))
    instance))

;;;

#+sbcl (declaim (inline fast-allocate-instance))

#+sbcl
(defun fast-allocate-instance (wrapper initforms)
  (declare (simple-vector initforms))
  (let ((instance (sb-pcl::%make-standard-instance
		   (copy-seq initforms) (sb-pcl::get-instance-hash-code))))
    (setf (sb-pcl::std-instance-wrapper instance)
	  wrapper)
    instance))

(defvar *number-of-threads* 4)

(defun min-position (vector)
  (loop with min-position = 0
        with min = (aref vector 0)
        for i from 0
        until (= i (length vector))
        for value = (aref vector i)
        when (< value min)
        do (setf min value
                 min-position i)
        finally (return min-position)))

(defun lpt (info)
  (let* ((threads (make-array *number-of-threads* :initial-element 0))
         (threads-info (make-array *number-of-threads* :initial-element ())))
    (loop for info in info
          for position = (min-position threads)
          do (incf (aref threads position) (second info))
             (push info (aref threads-info position)))
    threads-info))

(defun preallocate-objects-worker (array info)
  (declare (simple-vector array)
           (optimize speed))
  (loop for (class length index) in info
	for initforms = (class-initforms class)
	for wrapper = (sb-pcl::class-wrapper class)
        do
        (setf (objects-of-class class)
              (loop repeat (the fixnum length)
                    for instance = (fast-allocate-instance wrapper initforms)
                    collect instance
                    do
                    (setf (aref array index) instance)
                    (incf index)))))

#+sbcl
(defun preallocate-objects (array info)
  (let ((threads (lpt info)))
    (mapcar
     #'sb-thread:join-thread
     (loop for info across threads
           collect (sb-thread:make-thread
                    (lambda (info)
                      (preallocate-objects-worker array info))
                    :arguments (list info))))))

#-sbcl
(defun initialize-slots (instance slot-cache)
  (loop for (location . value)
        across slot-cache
        do (setf (standard-instance-access instance location)
                 value))
  instance)

#-sbcl
(defun preallocate-objects (array info)
  (declare (simple-array array))
  (loop with index = 0
	for (class . length) in info
	for slot-cache = (all-slot-locations-and-initforms class)
	do
        (setf (objects-of-class class)
              (loop repeat length
                    for instance = (allocate-instance class)
                    collect instance
                    do (initialize-slots instance slot-cache)
                       (setf (aref array index) instance)
                       (incf index)))))

(defun prepare-classes (stream)
  (loop repeat (read-n-bytes +sequence-length+ stream)
        for start = 0 then (+ start length)
        for class = (read-next-object stream)
        for length = (read-n-bytes +id-length+ stream)
        collect (list class length start) into info
        sum length into array-length
        finally
        (time (preallocate-objects (setf *indexes* (make-array array-length))
                              info))))

(defun read-file (file)
  (with-io-file (stream file)
    (unwind-protect
	 (progn (prepare-classes stream)
		(loop until (stream-end-of-file-p stream)
		      do (read-next-object stream)))
      (setf *indexes* #()))))

(defun load-data (storage &optional file)
  (let ((*storage* storage)
        (*indexes* *indexes*))
    (clear-cashes)
    (read-file (or file (storage-file *storage*)))
    (interlink-all-objects-first-time)))

(defun save-data (storage &optional file)
  (let ((*storage* storage))
    (when (storage-data storage)
      (with-io-file (stream (or file (storage-file storage))
                            :direction :output
                            :size (measure-size))
        (dump-data stream)))))
