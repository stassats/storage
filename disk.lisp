;;; -*- Mode: Lisp -*-

(in-package #:storage)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *codes*
    #(ascii-string
      identifiable
      cons
      string
      null
      storable-class
      standard-object
      fixnum
      bignum
      ratio
      list-of-objects
      symbol
      intern-package-and-symbol
      intern-symbol)))

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
    (satisfies ascii-string-p)))

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
    (setf (fill-pointer *packages*) 0)
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
  (declare (ignore stream))
  nil)

;;; Symbol

(defvar *packages* #())
(declaim (vector *packages*))

(defun make-s-packages ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defun make-s-package (package)
  (let ((symbols (make-array 100 :adjustable t :fill-pointer 0)))
    (values (vector-push-extend (cons package symbols) *packages*)
            symbols
            t)))

(defun find-s-package (package)
  (loop for i below (length *packages*)
        for (stored-package . symbols) = (aref *packages* i)
        when (eq package stored-package)
        return (values i symbols)
        finally (return (make-s-package package))))

(defun s-intern (symbol)
  (multiple-value-bind (package-id symbols new-package)
      (find-s-package (symbol-package symbol))
    (let* ((existing (and (not new-package)
                          (position symbol symbols)))
           (symbol-id (or existing
                          (vector-push-extend symbol symbols))))
      (values package-id symbol-id new-package (not existing)))))

(defun s-intern-existing (symbol symbols)
  (vector-push-extend symbol symbols))

(defmethod object-size ((symbol symbol))
  (+ 1 ;; type
     (multiple-value-bind (package-id symbol-id
                           new-package new-symbol) (s-intern symbol)
       (declare (ignore package-id symbol-id))
       (cond ((and new-package new-symbol)
              (+ (object-size (package-name (symbol-package symbol)))
                 (object-size (symbol-name symbol))))
             (new-symbol
              (+ +sequence-length+
                 (object-size (symbol-name symbol))))
             (t
              (+ +sequence-length+
                 +sequence-length+))))))

(defmethod write-object ((symbol symbol) stream)
  (multiple-value-bind (package-id symbol-id
                        new-package new-symbol) (s-intern symbol)
    (cond ((and new-package new-symbol)
           (write-n-bytes #.(type-code 'intern-package-and-symbol) 1 stream)
           (write-object (package-name (symbol-package symbol)) stream)
           (write-object (symbol-name symbol) stream))
          (new-symbol
           (write-n-bytes #.(type-code 'intern-symbol) 1 stream)
           (write-n-bytes package-id +sequence-length+ stream)
           (write-object (symbol-name symbol) stream))
          (t
           (write-n-bytes #.(type-code 'symbol) 1 stream)
           (write-n-bytes package-id +sequence-length+ stream)
           (write-n-bytes symbol-id +sequence-length+ stream)))))

(defreader symbol (stream)
  (let* ((package-id (read-n-bytes +sequence-length+ stream))
         (symbol-id (read-n-bytes +sequence-length+ stream))
         (package (or (aref *packages* package-id)
                      (error "Package with id ~a not found" package-id)))
         (symbol (aref (cdr package) symbol-id)))
    (or symbol
        (error "Symbol with id ~a in package ~a not found"
               symbol-id (car package)))))

(defreader intern-package-and-symbol (stream)
  (let* ((package-name (read-next-object stream))
         (symbol-name (read-next-object stream))
         (package (or (find-package package-name)
                      (error "Package ~a not found" package-name)))
         (symbol (intern symbol-name package))
         (s-package (nth-value 1 (make-s-package package))))
    (s-intern-existing symbol s-package)
    symbol))

(defreader intern-symbol (stream)
  (let* ((package-id (read-n-bytes +sequence-length+ stream))
         (symbol-name (read-next-object stream))
         (package (or (aref *packages* package-id)
                      (error "Package with id ~a for symbol ~a not found"
                             package-id symbol-name)))
         (symbol (intern symbol-name (car package))))
    (s-intern-existing symbol (cdr package))
    symbol))

;;; Integer

(defmethod object-size ((object integer))
  (+ 1 ;; tag
     (typecase object
       (storage-fixnum +fixnum-length+)
       (t (+ 1 ;; sign
             1 ;; size
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
  (write-n-signed-bytes n +fixnum-length+ stream))

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
  (read-n-signed-bytes +fixnum-length+ stream))

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
    (cond ((list-of-objects-p list)
           (incf count (+ (* (length list)
                             +id-length+)
                          (1- +sequence-length+)))) ;; sans +end+
          (t
           (mapc (lambda (x)
                   (incf count (object-size x)))
                 list)))
    count))

(defmethod write-object ((list cons) stream)
  (cond ((list-of-objects-p list)
         (write-list-of-objects list stream))
        (t
         (write-n-bytes #.(type-code 'cons) 1 stream)
         (dolist (item list)
           (write-object item stream))
         (write-n-bytes +end+ 1 stream))))

(defreader cons (stream)
  (loop for code = (read-n-bytes 1 stream)
        until (= code +end+)
        collect (call-reader code stream)))

;;; list-of-objects

(defun list-of-objects-p (list)
  (loop for i in list
        always (typep i 'standard-object)))

(defun write-list-of-objects (list stream)
  (write-n-bytes #.(type-code 'list-of-objects) 1 stream)
  (write-n-bytes (length list) +sequence-length+ stream)
  (dolist (object list)
    (write-n-bytes (id object) +id-length+ stream)))

(defreader list-of-objects (stream)
  (loop repeat (read-n-bytes +sequence-length+ stream)
        for id = (read-n-bytes +id-length+ stream)
        collect (get-instance id)))

;;; storable-class

(defmethod object-size ((class storable-class))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (+ 1 ;; type
     (object-size (class-name class))
     +sequence-length+ ;; list length
     (reduce #'+ (slots-to-store class)
             :key (lambda (x)
                    (object-size (slot-definition-name x))))
     +id-length+)) ;; size of objects

(defmethod write-object ((class storable-class) stream)
  (write-n-bytes #.(type-code 'storable-class) 1 stream)
  (write-object (class-name class) stream)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((slots (slots-to-store class)))
    (write-n-bytes (length slots) +sequence-length+ stream)
    (loop for slot across slots
          do (write-object (slot-definition-name slot)
                           stream))))

(defreader storable-class (stream)
  (let ((class (find-class (read-next-object stream))))
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (setf (objects-of-class class) nil)
    (let* ((length (read-n-bytes +sequence-length+ stream))
           (vector (make-array length)))
      (loop for i below length
            for slot-d =
            (slot-effective-definition class (read-next-object stream))
            do (setf (aref vector i)
                     (cons (slot-definition-location slot-d)
                           (slot-definition-initform slot-d))))
      (setf (slot-locations-and-initforms-read class) vector))
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
         (slots (slot-locations-and-initforms-read class)))
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

#+sbcl
(defun preallocate-objects (array info)
  (declare (simple-vector array)
           (optimize speed))
  (loop with index = 0
	for (class . length) in info
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

#-sbcl
(defun initialize-slots (instance slot-cache)
  (loop for (location . value) across slot-cache
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
        for class = (read-next-object stream)
        for length = (read-n-bytes +id-length+ stream)
        collect (cons class length) into info
        sum length into array-length
        finally
        (preallocate-objects (setf *indexes* (make-array array-length)) info)))

(defun read-file (file)
  (with-io-file (stream file)
    (unwind-protect
	 (progn (prepare-classes stream)
		(loop until (stream-end-of-file-p stream)
		      do (read-next-object stream)))
      (setf *indexes* #()))))

(defun load-data (storage &optional file)
  (let ((*storage* storage)
        (*indexes* *indexes*)
        (*packages* (make-s-packages)))
    (clear-cashes)
    (read-file (or file (storage-file *storage*)))
    (interlink-all-objects-first-time)))

(defun save-data (storage &optional file)
  (let ((*storage* storage)
        (*packages* (make-s-packages)))
    (when (storage-data storage)
      (with-io-file (stream (or file (storage-file storage))
                            :direction :output
                            :size (measure-size))
        (dump-data stream)))))
