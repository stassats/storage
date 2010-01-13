;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defstruct class-description id name slots)

(defvar *codes* #(keyword integer ascii-string string
                  standard-object identifiable cons symbol
                  class-description))

(defvar *sequence-length* 2)
(defvar *integer-length* 3)
(defvar *char-length* 2)

(defconstant +end-of-line+ 255)

(deftype ascii-string ()
  '(satisfies ascii-string-p))

(defvar +char-limit+ (code-char 255))

(defun ascii-string-p (string)
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
    (write-ascii-string name stream)))

(defmethod write-object ((object integer) stream)
  (assert (typep object `(unsigned-byte ,(* *integer-length* 8))))
  (write-integer object *integer-length* stream))

(defun write-ascii-string (string stream)
  (loop for char across string
        do (write-byte (char-code char) stream)))

(defun write-non-ascii-string (string stream)
  (loop for char across string
        do (write-integer (char-code char) *char-length* stream)))

(defmethod write-object ((string string) stream)
  (write-integer (length string) *sequence-length* stream)
  (etypecase string
    (ascii-string (write-ascii-string string stream))
    (string (write-non-ascii-string string stream))))

(defmethod write-object ((list cons) stream)
  (write-integer (length list) *sequence-length* stream)
  (dolist (item list)
    (dump-object item stream)))

(defun write-pointer-to-object (object stream)
  (write-byte (position 'identifiable *codes*) stream)
  (write-integer (id object) *integer-length* stream))

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
  (intern (read-ascii-string (read-byte stream)
                             stream)
          (if keyword-p
              :keyword
              *package*)))

(defmethod read-object ((type (eql 'keyword)) stream)
  (read-symbol t stream))

(defmethod read-object ((type (eql 'symbol)) stream)
  (read-symbol nil stream))

(defun read-ascii-string (length stream)
  (let ((string (make-string length)))
    (loop for i below length
          do (setf (char string i)
                   (code-char (read-byte stream))))
    string))

(defun read-non-ascii-string (stream)
  (let* ((length (read-integer *sequence-length* stream))
         (string (make-string length)))
    (loop for i below length
          do (setf (char string i)
                   (code-char (read-integer *char-length* stream))))
    string))

(defmethod read-object ((type (eql 'ascii-string)) stream)
  (read-ascii-string (read-integer *sequence-length* stream) stream))

(defmethod read-object ((type (eql 'string)) stream)
  (read-non-ascii-string stream))

(defmethod read-object ((type (eql 'cons)) stream)
  (loop repeat (read-integer *sequence-length* stream)
        collect (read-next-object stream)))

(defmethod read-object ((type (eql 'integer)) stream)
  (read-integer *integer-length* stream))

(defmethod read-object ((type (eql 'identifiable)) stream)
  (make-pointer :id (read-integer  *integer-length* stream)))

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
(defvar *class-cache-fill-pointer* 0)

(defun clear-class-cache ()
  (fill *class-cache* nil)
  (setf *class-cache-fill-pointer* 0))

(defun class-id (class-name)
  (loop for i below *class-cache-fill-pointer*
        for class across *class-cache*
        when (eql class-name (class-description-name class))
        return class))

(defun (setf class-id) (class)
  (let ((description
         (make-class-description
          :id *class-cache-fill-pointer*
          :name (class-name class)
          :slots (slots class))))
    (setf (aref *class-cache* *class-cache-fill-pointer*)
          description)
    (incf *class-cache-fill-pointer*)
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

(defun interlink-objects (movie)
  (dolist (director (directors movie))
    (push movie (getf (movies director) :director)))
  (dolist (producer (producers movie))
    (push movie (getf (movies producer) :producer)))
  (dolist (writer (writers movie))
    (push movie (getf (movies writer) :writer)))
  (dolist (producer (producers movie))
    (push movie
          (getf (movies producer) :producer)))
  (dolist (role (cast movie))
    (push (list movie (second role))
          (getf (movies (car role)) :actor)))
  movie)

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
    (typecase object
      (movie (interlink-objects object)))))

(defun save-data (&optional (file *data-file*))
  (when *data*
    (let ((*package* (find-package 'movies)))
      (with-open-file (stream file :direction :output :if-exists :supersede
                              :element-type 'unsigned-byte)
        (dump-data stream)))))
#+nil
(eval-when (:execute :load-toplevel)
  (load-data))
