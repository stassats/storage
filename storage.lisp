;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defvar *data-file* (merge-pathnames "doc/movies.db" (user-homedir-pathname)))
(defvar *data* nil)

(defun objects-of-type (type)
  (objects-of-class (find-class type)))

(defun (setf objects-of-type) (value type)
  (setf (objects-of-class (find-class type)) value))

(defun store-object (object)
  (push object (objects-of-class (class-of object))))

(defun clear-data-cache ()
  (setf *data* nil))

(defun delete (object)
  (setf (objects-of-class (class-of object))
        (cl:delete object (objects-of-class (class-of object))))
  (when (typep object 'identifiable)
    (setf (id object) -1))
  t)

(defun map-data (function)
  (dolist (class *data*)
    (funcall function
             class (objects-of-class class))))

(defun map-type (type function)
  (dolist (class *data*)
    (when (subtypep class type)
      (map nil function
           (objects-of-class class)))))
;;;

(defvar *last-id* -1)

(defclass identifiable ()
  ((id :accessor id
       :initarg :id
       :initform nil))
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


;; (defvar *statistics* ())
;; (defun code-type (code)
;;   (let* ((type (aref *codes* code))
;;          (cons (assoc type *statistics*)))
;;     (if cons
;;         (incf (cdr cons))
;;         (push (cons type 1) *statistics*))
;;     type))

(defun code-type (code)
  (aref *codes* code))

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

(defstruct pointer (id 0 :type fixnum))

(defvar *indexes* (make-hash-table))

(defun object-from-pointer (pointer)
  (gethash (pointer-id pointer) *indexes*))

(defun index-object (object)
  (setf (gethash (id object) *indexes*) object))

;;;

(defun slots (class)
  (coerce (remove-if-not #'store-slot-p (class-slots class))
          'simple-vector))

(defun slot-effective-definition (class slot-name)
  (find slot-name (class-slots class) :key #'slot-definition-name))

(defvar *class-cache* (make-array 20 :initial-element nil))
(defvar *class-last-id* 0)

(defun clear-class-cache ()
  (fill *class-cache* nil)
  (setf *class-last-id* 0))

(defun assign-id-to-class (class)
  (prog1 (setf (class-id class) *class-last-id*)
    (incf *class-last-id*)))

(defun cache-class-with-id (class id)
  (setf (aref *class-cache* id) class))

(defun find-class-by-id (id)
  (aref *class-cache* id))

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
  (clear-class-cache)
  (map-data (lambda (class objects)
              (write-object class stream)
              (dolist (object objects)
                (write-standard-object object stream)))))

(defun read-next-object (stream)
  (call-reader (read-n-bytes 1 stream) stream))

;;; Symbol

(defmethod object-size ((object symbol))
  (+ 2 ;; type + length
     (length (symbol-name object))))

(defmethod write-object ((object symbol) stream)
  (write-n-bytes #.(type-code 'symbol) 1 stream)
  (let ((name (symbol-name object)))
    (write-n-bytes (length name) 1 stream)
    (write-ascii-string name stream)))

(defreader symbol (stream)
  (intern (read-ascii-string (read-n-bytes 1 stream) stream)
          *package*))

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
  (+ 1 ;;type
     1 ;; class-id
     (object-size (class-name class))
     +sequence-length+ ;; length of list
     (let ((slots (slots class)))
       (setf (slots-to-store class) slots)
       (reduce #'+ slots
               :key (lambda (x)
                      (object-size (slot-definition-name x)))))))

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
  (let ((class (find-class (read-next-object stream))))
    (cache-class-with-id class
                         (read-n-bytes 1 stream))
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (push class *data*)
    (setf (objects-of-class class) nil)
    (let* ((length (read-n-bytes +sequence-length+ stream))
           (vector (make-array length)))
      (loop for i below length
            do (setf (aref vector i)
                     (slot-effective-definition class
                                                (read-next-object stream))))
      (setf (slots-to-store class)
            vector))
    class))

;;; identifiable

(defmethod object-size ((object identifiable))
  (+ 1 +integer-length+))

(defmethod write-object ((object identifiable) stream)
  (write-n-bytes #.(type-code 'identifiable) 1 stream)
  (write-n-bytes (id object) +integer-length+ stream))

(defreader identifiable (stream)
  (make-pointer :id (read-n-bytes  +integer-length+ stream)))

;;; standard-object

(defun standard-object-size (object)
  (let* ((class (class-of object))
         (slots (slots-to-store class)))
    (declare (type (simple-array t (*)) slots))
    (+ 1 ;; data type
       1 ;; class id
       (loop for slot-def across slots
             for i from 0
             for value = (slot-value-using-class class object slot-def)
             unless (eql value (slot-definition-initform slot-def))
             sum (+ 1 ;; slot id
                    (object-size value)))
       1))) ;; end-of-slots

;;; Can't use write-object method, because it would conflict with
;;; writing a pointer to a standard class
(defun write-standard-object (object stream)
  (write-n-bytes #.(type-code 'standard-object) 1 stream)
  (let ((class (class-of object)))
    (write-n-bytes (class-id class) 1 stream)
    (loop for slot-def across (slots-to-store class)
          for i from 0
          for value = (slot-value-using-class class object slot-def)
          unless (eql value (slot-definition-initform slot-def))
          do
          (write-n-bytes i 1 stream)
          (write-object value stream))
    (write-n-bytes +end-of-slots+ 1 stream)))

(defreader standard-object (stream)
  (let* ((class (find-class-by-id (read-n-bytes 1 stream)))
         (instance (make-instance class :id 0))
         (slots (slots-to-store class)))
    (loop for slot-id = (read-n-bytes 1 stream)
          until (= slot-id +end-of-slots+)
          do (setf (slot-value-using-class class instance
                                           (aref slots slot-id))
                   (read-next-object stream)))
    (index-object instance)
    (setf *last-id* (max *last-id* (id instance)))
    (push instance (objects-of-class class))
    instance))

;;;

(defun replace-pointers-in-slot (value)
  (flet ((replace-recursively (x)
           (setf (car x)
                 (replace-pointers-in-slot (car x)))))
    (typecase value
      (pointer
       (object-from-pointer value))
      (cons
       (loop for x on value
             do (replace-recursively x))
       value)
      (t value))))

(defun replace-pointers (object)
  (loop with class = (class-of object)
        for slot across (slots-to-store class)
        do (setf (slot-value-using-class class object slot)
                 (replace-pointers-in-slot
                  (slot-value-using-class class object slot)))))

(defgeneric interlink-objects (object))

(defmethod interlink-objects ((object t))
  nil)

;;;

(defun read-file (file)
  (let ((*package* (find-package 'movies)))
    (with-io-file (stream file)
      (loop with length = (stream-length stream)
            while (< (stream-position stream)
                     length)
            do (read-next-object stream)))))

(defun clear-cashes ()
  (clear-class-cache)
  (clear-data-cache)
  (clrhash *indexes*))

(defun load-data (&optional (file *data-file*))
  (clear-cashes)
  (read-file file)
  (map-data (lambda (type objects)
              (declare (ignore type))
              (dolist (object objects)
                (replace-pointers object)
                (interlink-objects object)))))

(defun save-data (&optional (file *data-file*))
  (with-io-file (stream file :direction :output
                        :size (measure-size))
    (dump-data stream)))

;;; Data manipulations

(defgeneric add (type &rest args &key &allow-other-keys))

(defmethod add (type &rest args &key &allow-other-keys)
  (let ((object (apply #'make-instance type args)))
    (store-object object)
    object))

(defun where (&rest clauses)
  (let ((slots (loop for slot in clauses by #'cddr
                     collect (intern (symbol-name slot)
                                     'movies)))
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
