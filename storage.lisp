;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defvar *data-file* (merge-pathnames "doc/movies.db" (user-homedir-pathname)))
(defvar *data* ())

(defvar *last-id* -1)

(defclass identifiable ()
  ((id :accessor id
       :initarg :id
       :initform nil)))

(defmethod initialize-instance :after ((object identifiable)
                                       &key id)
  (if (integerp id)
      (setf *last-id* (max *last-id* id))
      (setf (id object) (incf *last-id*))))

(defstruct class-description id name slots)

(defvar *codes* #(keyword integer unibyte-string string
                  standard-object identifiable cons symbol
                  class-description))

(defconstant +sequence-length+ 2)
(defconstant +integer-length+ 3)
(defconstant +char-length+ 2)

(defconstant +end-of-line+ 255)

(deftype unibyte-string ()
  '(satisfies unibyte-string-p))

(defconstant +char-limit+ (code-char 255))

(defun unibyte-string-p (string)
  (and (stringp string)
       (every (lambda (x)
                (char< x +char-limit+))
              string)))

(defun type-code (object)
  (position-if (lambda (x) (typep object x))
               *codes*))

(defun code-type (code)
  (aref *codes* code))

(declaim (inline read-integer))
(defun read-integer (bytes stream)
  (declare (type (integer 1 4) bytes)
           (optimize speed))
  (loop with value of-type fixnum = 0
        for low-bit downfrom (* 8 (1- bytes)) to 0 by 8 do
        (setf (ldb (byte 8 low-bit) value)
              (read-byte stream))
        finally (return value)))

(defun write-integer (integer bytes stream)
  (loop for low-bit downfrom (* 8 (1- bytes)) to 0 by 8
        do (write-byte (ldb (byte 8 low-bit) integer) stream)))

;;;

(defun dump-object (object stream)
  (unless (typep object 'standard-object)
    (write-byte (type-code object) stream))
  (write-object object stream)
  object)

(defgeneric write-object (object stream))

(defmethod write-object ((object symbol) stream)
  (let ((name (symbol-name object)))
    (write-byte (length name) stream)
    (write-unibyte-string name stream)))

(defmethod write-object ((object integer) stream)
  (assert (typep object `(unsigned-byte ,(* +integer-length+ 8))))
  (write-integer object +integer-length+ stream))

(defun write-unibyte-string (string stream)
  (loop for char across string
        do (write-byte (char-code char) stream)))

(defun write-multibyte-string (string stream)
  (loop for char across string
        do (write-integer (char-code char) +char-length+ stream)))

(defmethod write-object ((string string) stream)
  (write-integer (length string) +sequence-length+ stream)
  (etypecase string
    (unibyte-string (write-unibyte-string string stream))
    (string (write-multibyte-string string stream))))

(defmethod write-object ((list cons) stream)
  (write-integer (length list) +sequence-length+ stream)
  (dolist (item list)
    (dump-object item stream)))

(defun write-pointer-to-object (object stream)
  (write-byte (position 'identifiable *codes*) stream)
  (write-integer (id object) +integer-length+ stream))

(defmethod write-object ((description class-description) stream)
  (write-byte (class-description-id description) stream)
  (write-object (class-description-name  description) stream)
  (write-object (class-description-slots description) stream))

(defvar *writing-standard-object* nil)

(defmethod write-object ((object standard-object) stream)
  (if *writing-standard-object*
      (write-pointer-to-object object stream)
      (let ((*writing-standard-object* t))
        (write-standard-object object stream))))

(defun write-standard-object (object stream)
  (let* ((class (class-of object))
         (description (ensure-class-id class stream))
         (slots (class-description-slots description)))
    (write-byte (type-code object) stream)
    (write-byte (class-description-id description) stream)
    (loop for slot-def in (class-slots class)
          for slot = (slot-definition-name slot-def)
          for value = (slot-value object slot)
          unless (or (eql slot 'movies)
                     (eql value (slot-definition-initform slot-def)))
          do
          (write-byte (position slot slots) stream)
          (dump-object value stream))
    (write-byte +end-of-line+ stream)))

;;;

(defun read-next-object (stream &optional (eof-error-p t))
  (let ((code (read-byte stream eof-error-p)))
    (unless (or (not code)
                (= code +end-of-line+))
      (read-object (code-type code) stream))))

(defgeneric read-object (type stream))

(defun read-symbol (keyword-p stream)
  (intern (read-unibyte-string (read-byte stream)
                             stream)
          (if keyword-p
              :keyword
              *package*)))

(defmethod read-object ((type (eql 'keyword)) stream)
  (read-symbol t stream))

(defmethod read-object ((type (eql 'symbol)) stream)
  (read-symbol nil stream))

(defun read-unibyte-string (length stream)
  (let ((string (make-string length)))
    (loop for i below length
          do (setf (char string i)
                   (code-char (read-byte stream))))
    string))

(defun read-multibyte-string (stream)
  (let* ((length (read-integer +sequence-length+ stream))
         (string (make-string length)))
    (loop for i below length
          do (setf (char string i)
                   (code-char (read-integer +char-length+ stream))))
    string))

(defmethod read-object ((type (eql 'unibyte-string)) stream)
  (read-unibyte-string (read-integer +sequence-length+ stream) stream))

(defmethod read-object ((type (eql 'string)) stream)
  (read-multibyte-string stream))

(defmethod read-object ((type (eql 'cons)) stream)
  (loop repeat (read-integer +sequence-length+ stream)
        collect (read-next-object stream)))

(defmethod read-object ((type (eql 'integer)) stream)
  (read-integer +integer-length+ stream))

(defmethod read-object ((type (eql 'identifiable)) stream)
  (make-pointer :id (read-integer  +integer-length+ stream)))

(defmethod read-object ((type (eql 'standard-object)) stream)
  (let* ((description (id-class (read-byte stream)))
         (instance (make-instance (class-description-name description)
                                  :id 0))
         (slots (class-description-slots description)))
    (loop for slot-id = (read-byte stream)
          until (= slot-id +end-of-line+)
          do (setf (slot-value instance (elt slots slot-id))
                   (read-next-object stream)))
    (setf (index) instance)
    (setf *last-id* (max *last-id* (id instance)))
    (push instance *data*)
    instance))

(defmethod read-object ((type (eql 'class-description)) stream)
  (let ((id (read-byte stream)))
    (setf (id-class id)
          (make-class-description
           :id id
           :name (read-object 'symbol stream)
           :slots (read-object 'cons stream)))))

;;;

(defstruct pointer (id 0 :type fixnum))

(defvar *indexes* (make-hash-table))

(defun index (id)
  (gethash id *indexes*))

(defun (setf index) (object)
  (setf (gethash (id object) *indexes*) object))

(declaim (inline class-slots))
(defun class-slots (class)
  #+ccl (ccl:class-slots class)
  #+sbcl (sb-mop:class-slots class))

(declaim (inline slot-definition-name))
(defun slot-definition-name (slot)
  #+ccl (ccl:slot-definition-name slot)
  #+sbcl (sb-mop:slot-definition-name slot))

(declaim (inline slot-definition-initform))
(defun slot-definition-initform (slot)
  #+ccl (ccl:slot-definition-initform slot)
  #+sbcl (sb-mop:slot-definition-initform slot))

(defun slots (class)
  (mapcar #'slot-definition-name
          (class-slots class)))

(defvar *class-cache* (make-array 20 :initial-element nil))
(defvar *class-cache-size* 0)

(defun clear-class-cache ()
  (fill *class-cache* nil)
  (setf *class-cache-size* 0))

(defun class-id (class-name)
  (loop for i below *class-cache-size*
        for class across *class-cache*
        when (eql class-name (class-description-name class))
        return class))

(defun (setf class-id) (class)
  (let ((description
         (make-class-description
          :id *class-cache-size*
          :name (class-name class)
          :slots (slots class))))
    (setf (aref *class-cache* *class-cache-size*)
          description)
    (incf *class-cache-size*)
    description))

(defun ensure-class-id (class stream)
  (or (class-id (class-name class))
      (dump-object (setf (class-id) class) stream)))

(defun id-class (id)
  (aref *class-cache* id))

(defun (setf id-class) (description id)
  (setf (aref *class-cache* id) description))

(defun dump-data (stream)
  (clear-class-cache)
  (dolist (object *data*)
    (dump-object object stream)))

(defun find-object (id)
  (index id))

(defun %deidentify (value)
  (typecase value
    (pointer
     (find-object (pointer-id value)))
    (cons
     (mapl (lambda (x)
             (setf (car x)
                   (%deidentify (car x))))
           value))
    (t value)))

(defun deidentify (object)
  (loop for slot-def in (class-slots (class-of object))
        for slot = (slot-definition-name slot-def)
        do (setf (slot-value object slot)
                 (%deidentify (slot-value object slot))))
  object)

(defgeneric interlink-objects (object))

(defmethod interlink-objects (object)
  nil)

(defun read-file (file)
  (clear-class-cache)
  (let ((*package* (find-package 'movies)))
    (with-open-file (stream file :element-type 'unsigned-byte)
      (loop while (read-next-object stream nil)))))

(defun load-data (&optional (file *data-file*))
  (setf *data* nil)
  (read-file file)
  (dolist (object *data*)
    (deidentify object)
    (interlink-objects object)))

(defun save-data (&optional (file *data-file*))
  (when *data*
    (let ((*package* (find-package 'movies)))
      (with-open-file (stream file :direction :output :if-exists :supersede
                              :element-type 'unsigned-byte)
        (dump-data stream)))))

;;; Data manipulations

(defgeneric add (type &rest args &key &allow-other-keys))

(defmethod add (type &rest args &key &allow-other-keys)
  (let ((object (apply #'make-instance type args)))
    (push object *data*)
    object))

(defun delete (object)
  (setf *data* (remove object *data*))
  (when (typep object 'identifiable)
    (setf (id object) -1))
  t)

(defun delete-if (type test)
  (setf *data* (remove-if (type-and-test type test) *data*))
  t)

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

(defun lookup (type &optional (test #'identity))
  (let ((result (remove-if-not (type-and-test type test) *data*)))
    (if (= (length result) 1)
        (car result)
        result)))

(defun count (type &optional (test #'identity))
  (count-if (type-and-test type test) *data*))
