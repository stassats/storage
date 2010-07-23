;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:storage)

(defvar *storage* nil)

(defmacro with-storage (class-name &body body)
  `(let ((*storage* (find-class ,class-name)))
     ,@body))

(defun objects-of-type (type)
  (objects-of-class (find-class type)))

(defun (setf objects-of-type) (value type)
  (setf (objects-of-class (find-class type)) value))

(defun store-object (object)
  (push object (objects-of-class (class-of object)))
  t)

(defun clear-data-cache ()
  (setf (storage-data *storage*) nil))

(defun delete (object)
  (setf (objects-of-class (class-of object))
        (cl:delete object (objects-of-class (class-of object))))
  (when (typep object 'identifiable)
    (setf (id object) -1))
  t)

(defun map-data (function)
  (dolist (class (storage-data *storage*))
    (funcall function
             class (objects-of-class class))))

(defun map-type (type function)
  (dolist (class (storage-data *storage*))
    (when (subtypep class type)
      (map nil function
           (objects-of-class class)))))
;;;

(defvar *last-id* -1)

(defclass identifiable ()
  ((id :accessor id
       :initarg :id
       :initform nil
       :storep nil))
  (:metaclass storable-class))

(defmethod update-instance-for-different-class
    :after ((previous identifiable) (current identifiable) &key)
  (delete previous)
  (store-object current))

(defmethod initialize-instance :after ((object identifiable)
                                       &key id)
  (if (integerp id)
      (setf *last-id* (max *last-id* id))
      (setf (id object) (incf *last-id*))))

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *codes* #(integer ascii-string
                    identifiable cons
                    string symbol
                    storable-class
                    standard-object)))

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
(declaim (type (simple-array function (*))))

(defmacro defreader (type (stream) &body body)
  (let ((name (intern (format nil "~a-~a" type '#:reader))))
    `(progn
       (defun ,name (,stream)
         ,@body)
       (setf (aref *code-functions* ,(type-code type))
             #',name))))

(defun call-reader (code stream)
  ;; (collect-stats code)
  (funcall (aref *code-functions* code) stream))

;;;

(defconstant +sequence-length+ 2)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +integer-length+ 3))
(defconstant +char-length+ 2)

(defconstant +end-of-slots+ 255)

(defconstant +ascii-char-limit+ (code-char 128))

(deftype ascii-string ()
  '(or #+sb-unicode simple-base-string  ; on #-sb-unicode the limit is 255
    (satisfies ascii-string-p)))

(defun ascii-string-p (string)
  (and (stringp string)
       (every (lambda (x)
                (char< x +ascii-char-limit+))
              string)))
;;;

(defvar *indexes* (make-hash-table))

(defun index-object (object)
  (setf (gethash (id object) *indexes*) object))

;;;

(defun slot-effective-definition (class slot-name)
  (find slot-name (class-slots class) :key #'slot-definition-name))

;;;

(defgeneric write-object (object stream))
(defgeneric object-size (object))

(defun measure-size ()
  (let ((result 0))
    (map-data (lambda (class objects)
                (incf result (object-size class))
                (dolist (object objects)
                  (incf result
                        (standard-object-size object)))))
    result))

(defun dump-data (stream)
  (map-data (lambda (class objects)
              (declare (ignore objects))
              (write-object class stream)))
  (map-data (lambda (class objects)
              (declare (ignore class))
              (dolist (object objects)
                (write-standard-object object stream)))))

(defun read-next-object (stream)
  (call-reader (read-n-bytes 1 stream) stream))

;;; Symbol

(defmethod object-size ((object symbol))
  (+ 3 ;; type + package name length + symbol length
     (length (package-name (symbol-package object)))
     (length (symbol-name object))))

(defmethod write-object ((object symbol) stream)
  (write-n-bytes #.(type-code 'symbol) 1 stream)
  (let ((name (symbol-name object))
        (package (package-name (symbol-package object))))
    (write-n-bytes (length name) 1 stream)
    (write-ascii-string name stream)
    (write-n-bytes (length package) 1 stream)
    (write-ascii-string package stream)))

(defreader symbol (stream)
  (intern (read-ascii-string (read-n-bytes 1 stream) stream)
          (read-ascii-string (read-n-bytes 1 stream) stream)))

;;; Integer

(defmethod object-size ((object integer))
  (+ 1 +integer-length+))

(defmethod write-object ((object integer) stream)
  (assert (typep object #.`'(unsigned-byte ,(* +integer-length+ 8))))
  (write-n-bytes #.(type-code 'integer) 1 stream)
  (write-n-bytes object +integer-length+ stream))

(defreader integer (stream)
  (read-n-bytes +integer-length+ stream))

;;; Strings

(defmethod object-size ((string string))
  (+ 1
     +sequence-length+
     (etypecase string
       (ascii-string (length string))
       (string (* (length string)
                  +char-length+)))))

(defun write-ascii-string (string stream)
  (loop for char across string
        do (write-n-bytes (char-code char) 1 stream)))

(defun write-multibyte-string (string stream)
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
  (+ 1 ;; type
     (object-size (class-name class))
     1                 ;; class-id
     +sequence-length+ ;; list length
     (reduce #'+ (slots-to-store class)
             :key (lambda (x)
                    (object-size (slot-definition-name x))))))

(defmethod write-object ((class storable-class) stream)
  (write-n-bytes #.(type-code 'storable-class) 1 stream)
  (write-object (class-name class) stream)
  (write-n-bytes (class-id class) 1 stream)
  (let ((slots (slots-to-store class)))
    (write-n-bytes (length slots) +sequence-length+ stream)
    (loop for slot across slots
          do (write-object (slot-definition-name slot)
                           stream))))

(defreader storable-class (stream)
  (let ((class (find-class (read-next-object stream)))
        (id (read-n-bytes 1 stream)))
    (cache-class-with-id class id)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (push class (storage-data class))
    (setf (objects-of-class class) nil)
    (let* ((length (read-n-bytes +sequence-length+ stream))
           (vector (make-array length)))
      (loop for i below length
            do (setf (aref vector i)
                     (slot-effective-definition class
                                                (read-next-object stream))))
      (setf (slots-to-store class) vector))
    class))

;;; identifiable

(defmethod object-size ((object identifiable))
  (+ 2 ;; type + class id
     +integer-length+))

(defmethod write-object ((object identifiable) stream)
  (write-n-bytes #.(type-code 'identifiable) 1 stream)
  (write-n-bytes (id object) +integer-length+ stream)
  (write-n-bytes (class-id (class-of object)) 1 stream))

(defreader identifiable (stream)
  (let ((id (read-n-bytes +integer-length+ stream))
        (class-id (read-n-bytes 1 stream)))
    (or (gethash id *indexes*)
        (setf (gethash id *indexes*)
              (make-instance (find-class-by-id class-id)
                             :id id)))))

;;; standard-object

(defun standard-object-size (object)
  (let* ((class (class-of object)))
    (+ 1                ;; data type
       1                ;; class id
       +integer-length+ ;; id
       (loop for slot-def across (slots-to-store class)
             sum (let ((value (slot-value-using-class class object slot-def)))
                   (if (eql value (slot-definition-initform slot-def))
                       0
                       (+ 1 ;; slot id
                          (object-size value)))))
       1))) ;; end-of-slots

;;; Can't use write-object method, because it would conflict with
;;; writing a pointer to a standard object
(defun write-standard-object (object stream)
  (write-n-bytes #.(type-code 'standard-object) 1 stream)
  (let ((class (class-of object)))
    (write-n-bytes (class-id class) 1 stream)
    (write-n-bytes (id object) +integer-length+ stream)
    (loop for slot-def across (slots-to-store class)
          for i from 0
          for value = (slot-value-using-class class object slot-def)
          unless (eql value (slot-definition-initform slot-def))
          do
          (write-n-bytes i 1 stream)
          (write-object value stream))
    (write-n-bytes +end-of-slots+ 1 stream)))

(declaim (inline get-instance))
(defun get-instance (id class)
  (or (gethash id *indexes*)
      (setf (gethash id *indexes*)
            (make-instance class :id id))))

(defreader standard-object (stream)
  (let* ((class (find-class-by-id (read-n-bytes 1 stream)))
         (id (read-n-bytes +integer-length+ stream))
         (instance (get-instance id class))
         (slots (slots-to-store class)))
    (loop for slot-id = (read-n-bytes 1 stream)
          until (= slot-id +end-of-slots+)
          do (setf (slot-value-using-class class instance
                                           (aref slots slot-id))
                   (read-next-object stream)))
    (setf *last-id* (max *last-id* (id instance)))
    (push instance (objects-of-class class))
    instance))

;;;

(defgeneric interlink-objects (object))

(defmethod interlink-objects ((object t))
  nil)

;;;

(defun read-file (file)
  (with-io-file (stream file)
    (loop until (stream-end-of-file-p stream)
          do (read-next-object stream))))

(defun clear-cashes ()
  (clear-data-cache)
  (clrhash *indexes*))

(defun load-data (storage-name &optional file)
  (with-storage storage-name
    (clear-cashes)
    (read-file (or file (storage-file *storage*)))
    (map-data (lambda (type objects)
                (declare (ignore type))
                (mapc #'interlink-objects objects)))))

(defun save-data (storage-name &optional file)
  (with-storage storage-name
    (when (storage-data *storage*)
      (with-io-file (stream (or file (storage-file *storage*))
                            :direction :output
                            :size (measure-size))
        (dump-data stream)))))

;;; Data manipulations

(defgeneric add (class &rest args &key &allow-other-keys))

(defmethod add (class &rest args &key &allow-other-keys)
  (let* ((class (if (classp class)
                    class
                    (find-class class)))
         (object (apply #'make-instance class args)))
    (pushnew class (storage-data class) :test #'eq)
    (store-object object)
    object))

(defun where (&rest clauses)
  (let ((slots (loop for slot in clauses by #'cddr
                     collect (intern (symbol-name slot))))
        (values (loop for value in (cdr clauses) by #'cddr collect value)))
    (compile
     nil
     `(lambda (object)
        (with-slots ,slots object
          (and
           ,@(mapcar (lambda (slot value)
                       (typecase value
                         (function
                          `(funcall ,value ,slot))
                         (string
                          `(search ,value ,slot :test #'char-equal))
                         (t
                          `(equalp ,value ,slot))))
                     slots values)))))))

(defun type-and-test (type test)
  (lambda (object) (and (typep object type)
                        (funcall test object))))

(defun lookup (type &optional test)
  (let (results)
    (map-data (lambda (key objects)
                (when (subtypep key type)
                  (setf results
                        (append (if test
                                    (remove-if-not test objects)
                                    objects)
                                results)))))
    (if (= (length results) 1)
        (car results)
        results)))

(defun count (type &optional test)
  (let ((count 0))
    (map-data (lambda (key objects)
                (when (subtypep key type)
                  (incf count
                        (if (null test)
                            (length objects)
                            (count-if test objects))))))
    count))
