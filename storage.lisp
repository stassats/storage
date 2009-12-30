;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defvar *codes* #(keyword integer ascii-string string
                  standard-object identifiable cons symbol
                  class-description))

(defvar *sequence-length* 2)
(defvar *integer-length* 3)
(defvar *char-length* 3)

(defconstant +end-of-line+ 255)

(deftype ascii-string ()
  '(satisfies ascii-string-p))

(defvar *ascii-limit* (code-char 255))

(defun ascii-string-p (string)
  (and (stringp string)
       (every (lambda (x)
                (char< x *ascii-limit*))
              string)))

(defun type-code (object)
  (position-if (lambda (x) (typep object x))
               *codes*))

(defun code-type (code)
  (aref *codes* code))

(defun read-integer (bytes stream)
  (loop with value = 0
        for low-bit downfrom (* 8 (1- bytes)) to 0 by 8 do
        (setf (ldb (byte 8 low-bit) value) (read-byte stream))
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
    (loop for (slot . initform) in (slots-with-initform class)
          for value = (slot-value object slot)
          unless (eql value initform) do
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
  (let* ((string (make-string length)))
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
         (instance (make-instance (class-description-name description)))
         (slots (class-description-slots description)))
    (loop for slot-id = (read-byte stream)
          until (= slot-id +end-of-line+)
          do (setf (slot-value instance (elt slots slot-id))
                   (read-next-object stream)))
    (setf (index) instance)
    (push instance (data (class-description-name description)))
    instance))

(defmethod read-object ((type (eql 'class-description)) stream)
  (let ((id (read-byte stream)))
    (setf (id-class id)
          (make-class-description
           :id id
           :name (read-object 'symbol stream)
           :slots (read-object 'cons stream)))))

;;;

(defstruct pointer id)

(defvar *indexes* (make-hash-table))

(defun index (id)
  (gethash id *indexes*))

(defun (setf index) (object)
  (setf (gethash (id object) *indexes*) object))

(defun slots (class)
  #+ccl
  (mapcar #'ccl:slot-definition-name
          (ccl:class-slots class))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-slots class)))

(defun slots-with-initform (class)
  #+ccl
  (mapcar (lambda (slot)
            (cons (ccl:slot-definition-name slot)
                  (ccl:slot-definition-initform slot)))
          (ccl:class-slots class))
  #+sbcl
  (mapcar (lambda (slot)
            (cons (sb-mop:slot-definition-name slot)
                  (sb-mop:slot-definition-initform slot)))
          (sb-mop:class-slots class)))

(defvar *class-cache* (make-array 20 :initial-element nil))
(defvar *class-cache-fill-pointer* 0)

(defun clear-class-cache ()
  (fill *class-cache* nil)
  (setf *class-cache-fill-pointer* 0))

(defstruct class-description id name slots)

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

(defun dump-class (class stream)
  (dolist (object (data class))
    (dump-object object stream)))

(defun dump-data (stream)
  (clear-class-cache)
  (loop for (class . nil) in *data*
        do (dump-class class stream)))

(defun find-object (id)
  (index id))

(defun %deidentify (value)
  (typecase value
    (pointer
     (find-object (pointer-id value)))
    (cons
     (mapcar '%deidentify value))
    (t value)))

(defun deidentify (object)
  (dolist (slot (slots (class-of object)))
    (setf (slot-value object slot)
          (%deidentify (slot-value object slot))))
  object)

(defun deidentify-all (list)
  (loop for (nil . objects) in list
        do (mapcar 'deidentify (symbol-value objects))))

(defun read-file (file)
  (clear-class-cache)
  (let ((*package* (find-package 'movies)))
    (with-open-file (stream file :element-type 'unsigned-byte)
      (loop while (read-next-object stream nil)))))

(defun load-data (&optional (file *data-file*))
  (dolist (cons *data*) (setf (data (car cons)) nil))
  (read-file file)
  (deidentify-all *data*))

(defun save-data (&optional (file *data-file*))
  (let ((*package* (find-package 'movies)))
    (with-open-file (stream file :direction :output :if-exists :supersede
                            :element-type 'unsigned-byte)
      (dump-data stream))))

(eval-when (:execute :load-toplevel)
  (load-data))
