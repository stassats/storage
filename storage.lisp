;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:movies)

(defvar *codes* #(keyword integer ascii-string string
                  identifiable cons symbol))

(defvar *sequence-length* 2)
(defvar *integer-length* 3)
(defvar *char-length* 3)

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

(defgeneric write-object (object stream))

(defmethod write-object ((object symbol) stream)
  (let ((name (symbol-name object)))
    (write-byte (type-code object) stream)
    (write-byte (length name) stream)
    (write-ascii-string name stream)))

(defmethod write-object ((object integer) stream)
  (assert (typep object `(unsigned-byte ,(* *integer-length* 8))))
  (write-byte (type-code object) stream)
  (write-integer object *integer-length* stream))

(defun write-ascii-string (string stream)
  (loop for char across string
        do (write-byte (char-code char) stream)))

(defun write-non-ascii-string (string stream)
  (loop for char across string
        do (write-integer (char-code char) *char-length* stream)))

(defmethod write-object ((string string) stream)
  (write-byte (type-code string) stream)
  (write-integer (length string) *sequence-length* stream)
  (etypecase string
    (ascii-string (write-ascii-string string stream))
    (string (write-non-ascii-string string stream))))

(defmethod write-object ((list cons) stream)
  (write-byte (type-code list) stream)
  (write-integer (length list) *sequence-length* stream)
  (dolist (item list)
    (write-object item stream)))

(defmethod write-object ((object identifiable) stream)
  (write-byte (type-code object) stream)
  (write-integer (id object) *integer-length* stream))

;;; 

(defun read-next-object (stream &optional (eof-error-p t))
  (let ((code (read-byte stream eof-error-p)))
    (unless (or (not code)
                (= code (char-code #\Newline)))
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
        collect (read-object (code-type (read-byte stream))
                             stream)))

(defmethod read-object ((type (eql 'integer)) stream)
  (read-integer *integer-length* stream))

(defmethod read-object ((type (eql 'identifiable)) stream)
  (make-pointer :id (read-integer  *integer-length* stream)))

;;;

(defstruct pointer id)

(defvar *dump-classes*
  '((person . *persons*)
    (movie . *movies*)
    (theatre . *theatres*)
    (view . *views*)))

(defun data (type)
  (when type
    (symbol-value (cdr (assoc type *dump-classes* :test #'subtypep)))))

(defun (setf data) (list type)
  (when type
    (setf (symbol-value (cdr (assoc type *dump-classes* :test #'subtypep)))
          list)))

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

(defun dump-object (object stream)
  (let ((class (class-of object)))
    (write-object (class-name class) stream)
    (loop with *print-pretty*
          for (slot . initform) in (slots-with-initform class)
          for value = (slot-value object slot)
          unless (equalp value initform) do
          (write-object slot stream)
          (write-object value stream))
    (write-byte (char-code #\Newline) stream)))

(defun dump-class (class stream)
  (dolist (object (data class))
    (dump-object object stream)))

(defun dump-data (stream)
  (loop for (class . nil) in *dump-classes*
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

(defun read-instance (class-name stream)
  (let ((instance (make-instance class-name)))
    (loop for slot-name = (read-next-object stream)
          while slot-name
          do (setf (slot-value instance slot-name)
                   (read-next-object stream)))
    (setf (index) instance)
    (push instance (data class-name))))

(defun read-file (file)
  (with-standard-io-syntax
    (let ((*package* (find-package 'movies)))
      (with-open-file (stream file :element-type 'unsigned-byte)
        (loop for class-name = (read-next-object stream nil)
              while class-name
              do (read-instance class-name stream))))))

(defun load-data (&optional (file *data-file*))
  (dolist (cons *dump-classes*) (setf (data (car cons)) nil))
  (read-file file)
  (deidentify-all *dump-classes*))

(defun save-data (&optional (file *data-file*))
  (with-standard-io-syntax
    (let ((*package* (find-package 'movies)))
      (with-open-file (stream file :direction :output :if-exists :supersede
                              :element-type 'unsigned-byte)
        (dump-data stream)))
    t))

(eval-when (:execute :load-toplevel)
  (load-data))
