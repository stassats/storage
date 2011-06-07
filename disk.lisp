;;; -*- Mode: Lisp -*-

(in-package #:storage)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *codes* #(ascii-string
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-code (type)
    (position type *codes*)))

(defvar *code-functions* (make-array (length *codes*)))
(declaim (type (simple-array function (*)) *code-functions*))

(defmacro defreader (type (stream) &body body)
  (let ((name (intern (format nil "~a-~a" type '#:reader))))
    `(progn
       (defun ,name (,stream)
         ,@body)
       (setf (aref *code-functions* ,(type-code type))
             #',name))))

(declaim (inline call-reader))
(defun call-reader (code stream)
  ;; (collect-stats code)
  (funcall (aref *code-functions* code) stream))

;;;

(defconstant +sequence-length+ 2)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fixnum-length+ 4))
(defconstant +char-length+ 2)
(defconstant +id-length+ 3)

(defconstant +end-of-slots+ 255)

(defconstant +ascii-char-limit+ (code-char 128))

(deftype ascii-string ()
  '(or #+sb-unicode simple-base-string ; on #-sb-unicode the limit is 255
    (and simple-string
     (satisfies ascii-string-p))))

(defun ascii-string-p (string)
  (declare (simple-string string))
  (loop for char across string
        always (char< char +ascii-char-limit+)))

;;;

(defun slot-effective-definition (class slot-name)
  (find slot-name (class-slots class) :key #'slot-definition-name))

(defgeneric write-object (object stream))
(defgeneric object-size (object))

(defun measure-size ()
  (let ((result (+ +id-length+		;; number of objects
		   +sequence-length+))) ;; number of classes
    (map-data (lambda (class objects)
                (incf result (object-size class))
                (dolist (object objects)
                  (incf result
                        (standard-object-size object)))))
    result))

(defun assign-ids ()
  (let ((last-id 0))
    (declare (fixnum last-id))
    (map-data
     (lambda (class objects)
       (declare (ignore class))
       (dolist (object objects)
         (setf (id object) last-id)
         (incf last-id))))
    last-id))

(defun write-classes-info (stream)
  (write-n-bytes (assign-ids) +id-length+ stream)
  (write-n-bytes (length (storage-data *storage*))
		 +sequence-length+ stream)
  (map-data (lambda (class objects)
              (write-object class stream)
	      (write-n-bytes (length objects)
			     +id-length+ stream))))

(defun dump-data (stream)
  (write-classes-info stream)
  (map-data (lambda (class objects)
              (declare (ignore class))
              (dolist (object objects)
                (write-standard-object object stream)))))

(defun read-next-object (stream)
  (call-reader (read-n-bytes 1 stream) stream))

;;; NIL

(defmethod object-size ((object null))
  1)

(defmethod write-object ((object null) stream)
  (write-n-bytes #.(type-code 'null) 1 stream))

(defreader null (stream)
  (declare (ignore stream))
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
       (#.`(signed-byte ,(* +fixnum-length+ 8))
        +fixnum-length+)
       (t (+ 1 ;; size
             (* (ceiling (integer-length (abs object))
                         (* +fixnum-length+ 8)) +fixnum-length+))))))

(defun write-fixnum (n stream)
  (write-n-bytes #.(type-code 'fixnum) 1 stream)
  (write-n-bytes (if (minusp n) 1 0) 1 stream)
  (write-n-bytes (abs n) +fixnum-length+ stream))

(defun write-bignum (n stream)
  (write-n-bytes #.(type-code 'bignum) 1 stream)
  (write-n-bytes (if (minusp n) 1 0) 1 stream)
  (let* ((n (abs n))
         (size (ceiling (integer-length n)
                        (* +fixnum-length+ 8))))
    (write-n-bytes size 1 stream)
    (loop for position to size
          do
          (write-n-bytes (ldb (byte (* +fixnum-length+ 8)
                                    (* position (* +fixnum-length+ 8)))
                              n)
                         +fixnum-length+ stream))))

(defmethod write-object ((object integer) stream)
  (typecase object
    (#.`(signed-byte ,(* +fixnum-length+ 8))
     (write-fixnum object stream))
    (t (write-bignum object stream))))

(defreader bignum (stream)
  (* (if (plusp (read-n-bytes 1 stream))
         -1
         1)
     (loop with integer = 0
           for position to (read-n-bytes 1 stream)
           do
           (setf (ldb (byte (* +fixnum-length+ 8)
                            (* position (* +fixnum-length+ 8)))
                      integer)
                 (read-n-bytes +fixnum-length+ stream))
           finally (return integer))))

(defreader fixnum (stream)
  (* (if (plusp (read-n-bytes 1 stream))
         -1
         1)
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

(defun read-ascii-string (length stream)
  (let ((string (make-string length :element-type 'base-char)))
    #-sbcl
    (loop for i below length
          do (setf (char string i)
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
          do (setf (char string i)
                   (code-char (read-n-bytes +char-length+ stream))))
    string))

;;; cons

(defmethod object-size ((list cons))
  (let ((count (+ 1 +sequence-length+)))
    (mapc (lambda (x)
            (incf count (object-size x)))
          list)
    count))

(defmethod write-object ((list cons) stream)
  (write-n-bytes #.(type-code 'cons) 1 stream)
  (write-n-bytes (length list) +sequence-length+ stream)
  (dolist (item list)
    (write-object item stream)))

(defreader cons (stream)
  (loop repeat (read-n-bytes +sequence-length+ stream)
        collect (read-next-object stream)))

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
  (let ((index (indexes *storage*)))
    (declare (simple-vector index))
    (aref index id)))

(defreader identifiable (stream)
  (let ((id (read-n-bytes +id-length+ stream)))
    (get-instance id)))

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
    (write-n-bytes +end-of-slots+ 1 stream)))

(defreader standard-object (stream)
  (let* ((instance (get-instance
		    (read-n-bytes +id-length+ stream)))
	 (class (class-of instance))
         (slots (slot-locations-and-initforms class)))
    (declare (simple-vector slots))
    (loop for slot-id = (read-n-bytes 1 stream)
          until (= slot-id +end-of-slots+)
          do (setf (standard-instance-access instance
                                             (car (aref slots slot-id)))
                   (read-next-object stream)))
    instance))

;;;

(defun initialize-slots (instance slot-cache)
  (loop for (location . value)
        across slot-cache
        do (setf (standard-instance-access instance location)
                 value))
  instance)

#+sbcl (declaim (inline fast-allocate-instance))

#+sbcl
(defun fast-allocate-instance (wrapper initforms)
  (declare (simple-vector initforms))
  (let ((instance (sb-pcl::%make-standard-instance
		   (copy-seq initforms) (sb-pcl::get-instance-hash-code))))
    (setf (sb-pcl::std-instance-wrapper instance)
	  wrapper)
    instance))

#+sbcl
(defun preallocate-objects (array info)
  (declare (simple-vector array))
  (loop with index = 0
	for (class . length) in info
	for initforms = (class-initforms class)
	for wrapper = (sb-pcl::class-wrapper class)
        do
        (setf (objects-of-class class)
              (loop repeat length
                    for instance = (fast-allocate-instance wrapper initforms)
                    collect instance
                    do
                    (setf (aref array index) instance)
                    (incf index)))))

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
  (let ((array (make-array (read-n-bytes +id-length+ stream)
			   :initial-element nil)))
    (setf (indexes *storage*) array)
    (loop repeat (read-n-bytes +sequence-length+ stream)
	  for class = (read-next-object stream)
	  for length = (read-n-bytes +id-length+ stream)
	  collect (cons class length) into info
	  finally (preallocate-objects array info))))

(defun read-file (file)
  (with-io-file (stream file)
    (unwind-protect
	 (progn (prepare-classes stream)
		(loop until (stream-end-of-file-p stream)
		      do (read-next-object stream)))
      (setf (indexes *storage*) nil))))

(defun load-data (storage &optional file)
  (let ((*storage* storage))
    (clear-cashes)
    (read-file (or file (storage-file *storage*)))
    (interlink-all-objects)))

(defun save-data (storage &optional file)
  (let ((*storage* storage))
    (when (storage-data storage)
      (with-io-file (stream (or file (storage-file storage))
                            :direction :output
                            :size (measure-size))
        (dump-data stream)))))
