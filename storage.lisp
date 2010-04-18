;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defvar *data-file* (merge-pathnames "doc/movies.db" (user-homedir-pathname)))
(defvar *data* ())

(defvar *last-id* -1)

;;

(defclass identifiable ()
  ((id :accessor id
       :initarg :id
       :initform nil))
  (:metaclass storable-class))

(defmethod initialize-instance :after ((object identifiable)
                                       &key id)
  (if (integerp id)
      (setf *last-id* (max *last-id* id))
      (setf (id object) (incf *last-id*))))

(defvar *codes* #(integer ascii-string
                  identifiable cons
                  string symbol
                  standard-object))

(defconstant +sequence-length+ 2)
(defconstant +integer-length+ 3)
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

(defun type-code (object)
  (position-if (lambda (x) (typep object x))
               *codes*))

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

(defun write-integer (integer bytes stream)
  (loop for low-bit to (* 8 (1- bytes)) by 8
        do (write-byte (ldb (byte 8 low-bit) integer) stream)))

;;;

(defstruct pointer (id 0 :type fixnum))

(defvar *indexes* (make-hash-table))

(defun index (id)
  (gethash id *indexes*))

(defun (setf index) (object)
  (setf (gethash (id object) *indexes*) object))

(defun slots (class)
  (coerce (remove-if-not #'store-slot-p (class-slots class))
          'simple-vector))

(defun slot-effective-definition (class slot-name)
  (find slot-name (class-slots class) :key #'slot-definition-name))

(defvar *class-cache* (make-array 20 :initial-element nil))
(defvar *class-cache-size* 0)

(defun clear-class-cache ()
  (fill *class-cache* nil)
  (setf *class-cache-size* 0))

(defun class-id (class)
  (position class *class-cache* :end *class-cache-size*))

(defun (setf class-id) (class)
  (setf
   (slots-to-store class) (slots class)
   (aref *class-cache* *class-cache-size*) class)
  (prog1 *class-cache-size*
    (incf *class-cache-size*)))

(defun id-class (id)
  (aref *class-cache* id))

(defun (setf id-class) (class id)
  (setf (aref *class-cache* id) class
        *class-cache-size*
        (max *class-cache-size* (1+ id)))
  class)

;;;

(defun dump-object (object stream)
  (write-byte (type-code object) stream)
  (write-object object stream)
  object)

(defgeneric write-object (object stream))

(defmethod write-object ((object symbol) stream)
  (let ((name (symbol-name object)))
    (write-byte (length name) stream)
    (write-ascii-string name stream)))

(defmethod write-object ((object integer) stream)
  (assert (typep object `(unsigned-byte ,(* +integer-length+ 8))))
  (write-integer object +integer-length+ stream))

(defun write-ascii-string (string stream)
  (loop for char across string
        do (write-byte (char-code char) stream)))

(defun write-multibyte-string (string stream)
  (loop for char across string
        do (write-integer (char-code char) +char-length+ stream)))

(defmethod write-object ((string string) stream)
  (write-integer (length string) +sequence-length+ stream)
  (etypecase string
    (ascii-string (write-ascii-string string stream))
    (string (write-multibyte-string string stream))))

(defmethod write-object ((list cons) stream)
  (write-integer (length list) +sequence-length+ stream)
  (dolist (item list)
    (dump-object item stream)))

(defmethod write-object ((class storable-class) stream)
  (write-object (class-name class) stream)
  (write-object (map 'list #'slot-definition-name
                     (slots-to-store class))
                stream))

(defmethod write-object ((object identifiable) stream)
  (write-integer (id object) +integer-length+ stream))

(defun ensure-write-class (class stream)
  (let ((id (class-id class)))
    (cond (id (write-byte id stream))
          (t (setf id (setf (class-id) class))
             (write-byte id stream)
             (write-object class stream)))
    class))

(defun write-standard-object (object stream)
  (write-byte (position 'standard-object *codes*) stream)
  (let ((class (class-of object)))
    (ensure-write-class class stream)
    (loop for slot-def across (slots-to-store class)
          for i from 0
          for value = (slot-value-using-class class object slot-def)
          unless (eql value (slot-definition-initform slot-def))
          do
          (write-byte i stream)
          (dump-object value stream))
    (write-byte +end-of-slots+ stream)))

;;;

(defmethod read-object ((type (eql 'storable-class)) stream)
  (let ((class (find-class (read-object 'symbol stream))))
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (setf (slots-to-store class)
          (map 'simple-vector
               (lambda (slot)
                 (slot-effective-definition class slot))
               (read-object 'cons stream)))
    class))

(defun ensure-read-class (stream)
  (let ((id (read-n-bytes 1 stream)))
    (or (id-class id)
        (setf (id-class id)
              (read-object 'storable-class stream)))))

(defmethod read-object ((type (eql 'standard-object)) stream)
  (let* ((class (ensure-read-class stream))
         (instance (make-instance class :id 0))
         (slots (slots-to-store class)))
    (loop for slot-id = (read-n-bytes 1 stream)
          until (= slot-id +end-of-slots+)
          do (setf (slot-value-using-class class instance
                                           (aref slots slot-id))
                   (read-next-object stream)))
    (setf (index) instance)
    (setf *last-id* (max *last-id* (id instance)))
    (push instance *data*)
    instance))

(defun read-next-object (stream &optional (eof-error-p t))
  (let ((code (read-n-bytes 1 stream eof-error-p)))
    (when code
      (read-object (code-type code)
                   stream))))

(defgeneric read-object (type stream))

(defun read-symbol (keyword-p stream)
  (intern (read-ascii-string (read-n-bytes 1 stream) stream)
          (if keyword-p
              :keyword
              *package*)))

(defmethod read-object ((type (eql 'keyword)) stream)
  (read-symbol t stream))

(defmethod read-object ((type (eql 'symbol)) stream)
  (read-symbol nil stream))

(defun read-ascii-string (length stream)
  (let ((string (make-string length :element-type 'base-char)))
    (loop for i below length
          do (setf (char string i)
                   (code-char (read-n-bytes 1 stream))))
    string))

(defmethod read-object ((type (eql 'ascii-string)) stream)
  (read-ascii-string (read-n-bytes +sequence-length+ stream) stream))

(defmethod read-object ((type (eql 'string)) stream)
  (let* ((length (read-n-bytes +sequence-length+ stream))
         (string (make-string length :element-type 'character)))
    (loop for i below length
          do (setf (char string i)
                   (code-char (read-n-bytes +char-length+ stream))))
    string))

(defmethod read-object ((type (eql 'cons)) stream)
  (loop repeat (read-n-bytes +sequence-length+ stream)
        collect (read-next-object stream)))

(defmethod read-object ((type (eql 'integer)) stream)
  (read-n-bytes +integer-length+ stream))

(defmethod read-object ((type (eql 'identifiable)) stream)
  (make-pointer :id (read-n-bytes  +integer-length+ stream)))

;;;


(defun dump-data (stream)
  (clear-class-cache)
  (dolist (object *data*)
    (write-standard-object object stream)))

(defun replace-pointers-in-slot (value)
  (typecase value
    (pointer
     (index (pointer-id value)))
    (cons
     (mapl (lambda (x)
             (setf (car x)
                   (replace-pointers-in-slot (car x))))
           value))
    (t value)))

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
      (loop while (read-next-object stream nil)))))

(defun load-data (&optional (file *data-file*))
  (setf *data* nil)
  (clear-class-cache)
  (clrhash *indexes*)
  (read-file file)
  (dolist (object *data*)
    (replace-pointers object)
    (interlink-objects object)))

(defun save-data (&optional (file *data-file*))
  (when *data*
    (with-open-file (stream file :direction :output :if-exists :supersede
                            :element-type 'unsigned-byte)
      (dump-data stream))))

;;; Data manipulations

(defgeneric add (type &rest args &key &allow-other-keys))

(defmethod add (type &rest args &key &allow-other-keys)
  (let ((object (apply #'make-instance type args)))
    (push object *data*)
    object))

(defun delete (object)
  (setf *data* (cl:delete object *data*))
  (when (typep object 'identifiable)
    (setf (id object) -1))
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
