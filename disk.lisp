;;; -*- Mode: Lisp -*-

(in-package #:storage)

(defvar *statistics* ())
(defun collect-stats (code)
  (let* ((type (aref *codes* code))
         (cons (assoc type *statistics*)))
    (if cons
        (incf (cdr cons))
        (push (cons type 1) *statistics*))
    type))

(defvar *indexes*)
(declaim (simple-vector *indexes*))

(defvar *read-packages*)
(defvar *read-symbols*)
(declaim (vector *read-packages* *read-symbols*))

(defvar *write-packages*)
(defvar *write-symbols*)
(declaim (hash-table *write-packages* *write-symbols*))

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

(defun slot-effective-definition (class slot-name)
  (find slot-name (class-slots class) :key #'slot-definition-name))

(defgeneric write-object (object stream))

(defun assign-ids ()
  (let ((last-id 0))
    (declare (fixnum last-id))
    (map-all-data
     (lambda (class objects)
       (let ((slot (slot-definition-location (find-slot 'id class))))
         (dolist (object objects)
           (setf (standard-instance-access object slot) last-id)
           (incf last-id)))))))

(defun number-of-non-empty-classes (storage)
  (count-if #'objects-of-class
            (storage-data storage)))

(defun write-classes-info (stream)
  (write-n-bytes (number-of-non-empty-classes *storage*)
                 +sequence-length+ stream)
  (map-all-data
   (lambda (class objects)
     (write-storable-class class stream)
     (write-n-bytes (length objects)
                    +id-length+ stream))))

(defun dump-data (stream)
  (write-classes-info stream)
  (assign-ids)
  (map-all-data
   (lambda (class objects)
     (let ((slots (slot-locations-and-initforms class))
           (bytes-for-slots (number-of-bytes-for-slots class)))
       (dolist (object objects)
         (write-standard-object object slots bytes-for-slots stream))))))

(declaim (inline read-next-object))
(defun read-next-object (stream)
  (call-reader (read-n-bytes 1 stream) stream))

;;; NIL

(defmethod write-object ((object null) stream)
  (write-n-bytes #.(type-code 'null) 1 stream))

(defreader null (stream)
  (declare (ignore stream))
  nil)

;;; T

(defmethod write-object ((object (eql t)) stream)
  (write-n-bytes #.(type-code t) 1 stream))

(defreader t (stream)
  (declare (ignore stream))
  t)

;;; Symbol

(defmacro with-writing-packages (&body body)
  `(let ((*write-packages* (make-hash-table :test #'eq))
         (*write-symbols* (make-hash-table :test #'eq
                                           :size 256)))
     ,@body))

(defmacro with-reading-packages (&body body)
  `(let ((*read-packages* (make-s-packages))
         (*read-symbols* (make-s-symbols)))
     ,@body))

(defun make-s-packages ()
  (make-array 16 :adjustable t :fill-pointer 0))

(defun make-s-symbols ()
  (make-array 256 :adjustable t :fill-pointer 0))

(defun make-s-package (package)
  (vector-push-extend package *read-packages*))

(defun find-s-package-for-writing (package)
  (or (gethash package *write-packages*)
      (values (setf (gethash package *write-packages*)
                    (hash-table-count *write-packages*))
              t)))

(defun s-intern-for-writing (symbol)
  (multiple-value-bind (package-id new-package)
      (find-s-package-for-writing (symbol-package symbol))
    (let* ((existing (and (not new-package)
                          (gethash symbol *write-symbols*)))
           (symbol-id (or existing
                          (setf (gethash symbol *write-symbols*)
                                (hash-table-count *write-symbols*)))))
      (values package-id symbol-id
              new-package
              (not existing)))))

(defun s-intern-existing (symbol)
  (vector-push-extend symbol *read-symbols*)
  symbol)

(defmethod write-object ((symbol symbol) stream)
  (multiple-value-bind (package-id symbol-id new-package new-symbol)
      (s-intern-for-writing symbol)
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
           (write-n-bytes symbol-id +sequence-length+ stream)))))

(defreader symbol (stream)
  (let* ((symbol-id (read-n-bytes +sequence-length+ stream))
         (symbol (aref *read-symbols* symbol-id)))
    (or symbol
        (error "Symbol with id ~a is not found" symbol-id))))

(defreader intern-package-and-symbol (stream)
  (let* ((package-name (read-next-object stream))
         (symbol-name (read-next-object stream))
         (package (or (find-package package-name)
                      (error "Package ~a not found" package-name))))
    (make-s-package package)
    (s-intern-existing (intern symbol-name package))))

(defreader intern-symbol (stream)
  (let* ((package-id (read-n-bytes +sequence-length+ stream))
         (symbol-name (read-next-object stream))
         (package (or (aref *read-packages* package-id)
                      (error "Package with id ~a for symbol ~a not found"
                             package-id symbol-name))))
    (s-intern-existing (intern symbol-name package))))

;;; Integer

(declaim (inline write-fixnum))
(defun write-fixnum (n stream)
  (declare (storage-fixnum n))
  (write-n-signed-bytes n +fixnum-length+ stream))

(defun write-fixnum-1 (n stream)
  (declare (storage-fixnum n))
  (write-n-bytes #.(type-code 'fixnum-1) 1 stream)
  (write-n-signed-bytes n 1 stream))

(defun write-fixnum-2 (n stream)
  (declare (storage-fixnum n))
  (write-n-bytes #.(type-code 'fixnum-2) 1 stream)
  (write-n-signed-bytes n 2 stream))

(defun write-fixnum-3 (n stream)
  (declare (storage-fixnum n))
  (write-n-bytes #.(type-code 'fixnum-3) 1 stream)
  (write-n-signed-bytes n 3 stream))

(declaim (inline sign))
(defun sign (n)
  (if (minusp n)
      1
      0))

(defun write-bignum (n stream)
  (declare (type (and integer (not storage-fixnum)) n))
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
    ((signed-byte 8)
     (write-fixnum-1 object stream))
    ((signed-byte 16)
     (write-fixnum-2 object stream))
    ((signed-byte 24)
     (write-fixnum-3 object stream))
    (storage-fixnum
     (write-n-bytes #.(type-code 'fixnum) 1 stream)
     (write-fixnum object stream))
    (t
     (write-n-bytes #.(type-code 'bignum) 1 stream)
     (write-bignum object stream))))

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

(defreader fixnum-1 (stream)
  (read-n-signed-bytes 1 stream))

(defreader fixnum-2 (stream)
  (read-n-signed-bytes 2 stream))

(defreader fixnum-3 (stream)
  (read-n-signed-bytes 3 stream))

;;; Ratio

(defmethod write-object ((n ratio) stream)
  (let ((numerator (numerator n))
        (denominator (denominator n)))
    (cond ((and (typep numerator 'storage-fixnum)
                (typep denominator 'storage-fixnum))
           (write-n-bytes #.(type-code 'fixnum-ratio) 1 stream)
           (write-fixnum numerator stream)
           (write-fixnum denominator stream))
          (t
           (write-n-bytes #.(type-code 'ratio) 1 stream)
           (write-object numerator stream)
           (write-object denominator stream)))))

(defreader fixnum-ratio (stream)
  (/ (the storage-fixnum (fixnum-reader stream))
     (the storage-fixnum (fixnum-reader stream))))

(defreader ratio (stream)
  (/ (read-next-object stream)
     (read-next-object stream)))

;;; Float

(defmethod write-object ((float float) stream)
  (etypecase float
    (single-float
     (write-n-bytes #.(type-code 'single-float) 1 stream)
     (write-single-float float stream))
    (double-float
     (write-n-bytes #.(type-code 'double-float) 1 stream)
     (write-double-float float stream))))

(defreader single-float (stream)
  (read-single-float stream))

(defreader double-float (stream)
  (read-double-float stream))

;;; Complex

(defmethod write-object ((complex complex) stream)
  (write-n-bytes #.(type-code 'complex) 1 stream)
  (write-object (realpart complex) stream)
  (write-object (imagpart complex) stream))

(defreader complex (stream)
  (complex (read-next-object stream)
           (read-next-object stream)))

;;; Characters

(defmethod write-object ((character character) stream)
  (write-n-bytes #.(type-code 'character) 1 stream)
  (write-n-bytes (char-code character) +char-length+ stream))

(defreader character (stream)
  (code-char (read-n-bytes +char-length+ stream)))

;;; Strings

(defun write-ascii-string (string stream)
  (declare (simple-string string))
  (write-n-bytes #.(type-code 'ascii-string) 1 stream)
  (write-n-bytes (length string) +sequence-length+ stream)
  (write-ascii-non-base-string-optimized string stream))

(defun write-multibyte-string (string stream)
  (declare (simple-string string))
  (write-n-bytes #.(type-code 'string) 1 stream)
  (write-n-bytes (length string) +sequence-length+ stream)
  (write-multibyte-string-optimized string stream))

(defmethod write-object ((string string) stream)
  (etypecase string
    ((not simple-string)
     (call-next-method))
    #+(and sb-unicode (or x86 x86-64))
    (simple-base-string
     (write-n-bytes #.(type-code 'ascii-string) 1 stream)
     (write-n-bytes (length string) +sequence-length+ stream)
     (write-ascii-string-optimized string stream))
    (ascii-string
     (write-ascii-string string stream))
    (string
     (write-multibyte-string string stream))))

(defreader ascii-string (stream)
  (let* ((length (read-n-bytes +sequence-length+ stream))
         (string (make-string length :element-type 'base-char)))
    (read-ascii-string-optimized length string stream)
    string))

(defreader string (stream)
  (let* ((length (read-n-bytes +sequence-length+ stream))
         (string (make-string length :element-type 'character)))
    (read-multibyte-string-optimized length string stream)
    string))

;;; Pathname

(defmethod write-object ((pathname pathname) stream)
  (write-n-bytes #.(type-code 'pathname) 1 stream)
  (write-object (pathname-name pathname) stream)
  (write-object (pathname-directory pathname) stream)
  (write-object (pathname-device pathname) stream)
  (write-object (pathname-type pathname) stream)
  (write-object (pathname-version pathname) stream))

(defreader pathname (stream)
  (make-pathname
   :name (read-next-object stream)
   :directory (read-next-object stream)
   :device (read-next-object stream)
   :type (read-next-object stream)
   :version (read-next-object stream)))

;;; Cons

(defun circular-list-p (list)
  (or (eq list (cdr list))
      (do ((fast (cdr list) (cddr fast))
           (slow list (cdr slow)))
          (nil)
        (unless (and (consp fast) (listp (cdr fast)))
          (return))
        (when (eq fast slow)
          (return t)))))

(defmethod write-object ((list cons) stream)
  (cond ((circular-list-p list)
         (error "Can't store circular lists"))
        ((list-of-objects-p list)
         (write-list-of-objects list stream))
        (t
         (write-n-bytes #.(type-code 'cons) 1 stream)
         (write-cons list stream))))

(defun write-cons (cons stream)
  (loop for cdr = cons then (cdr cdr)
        do
        (typecase cdr
          (cons
           (write-object (car cdr) stream))
          (null
           (write-n-bytes +end+ 1 stream)
           (return))
          (t
           (write-n-bytes +improper-list-end+ 1 stream)
           (write-object cdr stream)
           (return)))))

(defreader cons (stream)
  (let ((first-cons (list (read-next-object stream))))
    (loop for previous-cons = first-cons then new-cons
          for car = (let ((id (read-n-bytes 1 stream)))
                      (cond ((eq id +improper-list-end+)
                             (setf (cdr previous-cons)
                                   (read-next-object stream))
                             (return))
                            ((eq id +end+)
                             (return))
                            ((call-reader id stream))))
          for new-cons = (list car)
          do (setf (cdr previous-cons) new-cons))
    first-cons))

;;; list-of-objects

(defun list-of-objects-p (list)
  (loop for cdr = list then (cdr cdr)
        while cdr
        always (and (consp cdr) (typep (car cdr) 'standard-object))))

(defun write-list-of-objects (list stream)
  (write-n-bytes #.(type-code 'list-of-objects) 1 stream)
  (write-n-bytes (length list) +sequence-length+ stream)
  (dolist (object list)
    (write-n-bytes (fast-id object)
                   +id-length+ stream)))

(defreader list-of-objects (stream)
  (loop repeat (read-n-bytes +sequence-length+ stream)
        for id = (read-n-bytes +id-length+ stream)
        collect (get-instance id)))

;;; vector

(declaim (inline bit-test))
(defun bit-test (byte index)
  (declare (type (unsigned-byte 32) byte)
           (type (integer 0 31) index))
  (ldb-test (byte 1 index) byte))

(declaim (inline read-fill-pointer))
(defun read-fill-pointer (byte stream)
  (declare (type (unsigned-byte 8) byte))
  (and (bit-test byte 0)
       (read-n-bytes +vector-length+ stream)))

(defmethod write-object ((vector vector) stream)
  (typecase vector
    (simple-vector
     (write-simple-vector vector stream))
    (t
     (write-vector vector stream))))

(defun write-vector (vector stream)
  (write-n-bytes #.(type-code 'vector) 1 stream)
  (let ((byte 0)
        (fp (array-has-fill-pointer-p vector))
        (type (array-element-type vector)))
    (declare (type (unsigned-byte 8) byte))
    (when fp
      (setf byte 1))
    (when (adjustable-array-p vector)
      (setf (ldb (byte 1 1) byte) 1))
    (when (eq type t)
      (setf (ldb (byte 1 2) byte) 1))
    (write-n-bytes byte 1 stream)
    (when fp
      (write-n-bytes (fill-pointer vector) +vector-length+ stream))
    (unless (eq type t)
      (write-object (array-element-type vector) stream))
    (write-n-bytes (length vector) +vector-length+ stream)
    (loop for i below (length vector)
          do (write-object (aref vector i) stream))))

(defreader vector (stream)
  (let* ((byte (read-n-bytes 1 stream))
         (fill-pointer (read-fill-pointer byte stream))
         (length (read-n-bytes +vector-length+ stream))
         (vector (make-array length
                             :fill-pointer fill-pointer
                             :element-type (if (bit-test byte 2)
                                               t
                                               (read-next-object stream))
                             :adjustable (bit-test byte 1))))
    (loop for i below length
          do (setf (aref vector i) (read-next-object stream)))
    vector))

;;; Simple-vector

(defun write-simple-vector (vector stream)
  (declare (simple-vector vector))
  (write-n-bytes #.(type-code 'simple-vector) 1 stream)
  (write-n-bytes (length vector) +sequence-length+ stream)
  (loop for elt across vector
        do (write-object elt stream)))

(defreader simple-vector (stream)
  (let ((vector (make-array (read-n-bytes +sequence-length+ stream))))
    (loop for i below (length vector)
          do (setf (svref vector i) (read-next-object stream)))
    vector))

;;; Array

(defmethod write-object ((array array) stream)
  (write-n-bytes #.(type-code 'array) 1 stream)
  (let ((byte 0)
        (type (array-element-type array)))
    (declare (type (unsigned-byte 8) byte))
    (when (adjustable-array-p array)
      (setf (ldb (byte 1 0) byte) 1))
    (when (eq type t)
      (setf (ldb (byte 1 1) byte) 1))
    (write-n-bytes byte 1 stream)
    (unless (eq type t)
      (write-object (array-element-type array) stream))
    (write-cons (array-dimensions array) stream)
    (loop for i below (array-total-size array)
          do (write-object (row-major-aref array i) stream))))

(defreader array (stream)
  (let* ((byte (read-n-bytes 1 stream))
         (array (make-array (cons-reader stream)
                            :element-type (if (bit-test byte 1)
                                              t
                                              (read-next-object stream))
                            :adjustable (bit-test byte 0))))
    (loop for i below (array-total-size array)
          do (setf (row-major-aref array i) (read-next-object stream)))
    array))

;;; Hash-table

(defvar *hash-table-tests* #(eql equal equalp eq))
(declaim (simple-vector *hash-table-tests*))

(defun check-hash-table-test (hash-table)
  (let* ((test (hash-table-test hash-table))
         (test-id (position test *hash-table-tests*)))
   (unless test-id
     (error "Only standard hashtable tests are supported, ~a has ~a"
            hash-table test))
    test-id))

(defmethod write-object ((hash-table hash-table) stream)
  (write-n-bytes #.(type-code 'hash-table) 1 stream)
  (write-n-bytes (check-hash-table-test hash-table) 1 stream)
  (write-n-bytes (hash-table-size hash-table) +hash-table-length+ stream)
  (loop for key being the hash-keys of hash-table
        using (hash-value value)
        do
        (write-object key stream)
        (write-object value stream))
  (write-n-bytes +end+ 1 stream))

(defreader hash-table (stream)
  (let* ((test (svref *hash-table-tests* (read-n-bytes 1 stream)))
         (size (read-n-bytes +hash-table-length+ stream))
         (table (make-hash-table :test test :size size)))
    (loop for id = (read-n-bytes 1 stream)
          until (eq id +end+)
          do (setf (gethash (call-reader id stream) table)
                   (read-next-object stream)))
    table))

;;; storable-class

(defun prepare-classes (stream)
  (loop repeat (read-n-bytes +sequence-length+ stream)
        for class = (read-storable-class stream)
        for length = (read-n-bytes +id-length+ stream)
        collect (cons class length) into info
        sum length into array-length
        finally
        (let ((array (make-array array-length)))
          (preallocate-objects array info)
          (return (values array info)))))

(defun write-storable-class (class stream)
  (write-object (class-name class) stream)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((slots (slots-to-store class)))
    (write-n-bytes (length slots) +slots-length+ stream)
    (loop for slot across slots
          do (write-object (slot-definition-name slot)
                           stream))))

(defun read-storable-class (stream)
  (let ((class (find-class (read-next-object stream))))
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (setf (objects-of-class class) nil)
    (let* ((length (read-n-bytes +slots-length+ stream))
           (old-vector (slot-locations-and-initforms-read class))
           (vector (if (= length (length old-vector))
                       old-vector
                       (make-array length))))
      (loop for i below length
            for slot-name = (read-next-object stream)
            for slot-d = (or (slot-effective-definition class slot-name)
                             (cerror "Skip this slot"
                                     "No slot named ~a in ~a"
                                     class slot-name))
            for location = (and slot-d (slot-definition-location slot-d))
            for initiform = (and slot-d (slot-definition-initform slot-d))
            for old-value = (aref vector i)
            unless (and (consp old-value)
                        (eql (car old-value) location)
                        (eql (cdr old-value) initiform))
            do
            (setf (aref vector i) (cons location initiform)))
      (setf (slot-locations-and-initforms-read class) vector))
    class))

;;; identifiable

(defmethod write-object ((object identifiable) stream)
  (write-n-bytes #.(type-code 'identifiable) 1 stream)
  (write-n-bytes (fast-id object) +id-length+ stream))

(declaim (inline get-instance))
(defun get-instance (id)
  (aref *indexes* id))

(defreader identifiable (stream)
  (get-instance (read-n-bytes +id-length+ stream)))

;;; standard-object

(declaim (inline make-slot-map))
(defun make-slot-map (object slots)
  (declare (simple-vector slots))
  (let ((map 0))
    (declare (type (unsigned-byte 32) map))
    (loop for i from (1- (length slots)) downto 0
          for (location . initform) = (aref slots i)
          for value = (standard-instance-access object location)
          do (setf map
                   (if (eql value initform)
                       (the (unsigned-byte 32) (ash map 1))
                       (the (unsigned-byte 32) (+ map map 1)))))
    map))

(defun write-standard-object (object slots bytes-for-slots stream)
  (declare (simple-vector slots))
  (let ((map (make-slot-map object slots)))
    (declare (type (unsigned-byte 32) map))
    (write-n-bytes map bytes-for-slots stream)
    (loop for slot-id of-type (integer 0 32) from 0
          while (plusp map)
          when (oddp map)
          do
          (write-object
           (standard-instance-access object (car (aref slots slot-id)))
           stream)
          do (setf map (ash map -1)))))

(defun read-standard-object (object slots bytes-for-slots stream)
  (declare (simple-vector slots))
  (let ((map (read-n-bytes bytes-for-slots stream)))
    (declare (type (unsigned-byte 32) map))
    (loop for slot-id of-type (integer 0 32) from 0
          while (plusp map)
          when (oddp map)
          do (setf (standard-instance-access object
                                             (car (aref slots slot-id)))
                   (read-next-object stream))
          do (setf map (ash map -1)))))

;;;

(defun read-file (file)
  (with-io-file (stream file)
    (multiple-value-bind (array info) (prepare-classes stream)
      (declare (simple-vector array))
      (let ((*indexes* array))
        (loop with i = 0
              for (class . n) of-type (t . fixnum) in info
              for slots = (slot-locations-and-initforms-read class)
              for bytes-for-slots = (number-of-bytes-for-slots class)
              do
              (loop repeat n
                    for instance = (aref array i)
                    do
                    (incf i)
                    (read-standard-object instance slots bytes-for-slots
                                          stream)))))))

(defun db-changed-after-load-p (storage)
  (> (file-write-date (storage-file storage))
     (load-time storage)))

(defun load-data (storage &optional file)
  (let* ((*storage* storage)
         (file (or file (storage-file *storage*)))
         (time (or (file-write-date file)
                   (get-universal-time))))
    (with-reading-packages
      (read-file file))
    (interlink-all-objects-first-time)
    (setf (modified storage) nil
          (load-time storage) time)
    t))

(defun save-data (storage &optional file)
  (let ((*storage* storage))
    (when (storage-data storage)
      (with-writing-packages
        (with-io-file (stream (or file (storage-file storage))
                       :direction :output)
          (dump-data stream)))
      (setf (modified storage) nil
            (load-time storage) (get-universal-time))
      t)))
